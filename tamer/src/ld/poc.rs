// Proof-of-concept TAME linker
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
//
//  This file is part of TAME.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

//! **This contains the remaining portions of the proof-of-concept linker.**
//! It is feature-complete and just needs final refactoring.

use super::xmle::{
    lower::{sort, SortError},
    xir::lower_iter,
    XmleSections,
};
use crate::{
    asg::{
        air::{AirState, AirToken},
        Asg, AsgError, DefaultAsg, Ident, Object,
    },
    diagnose::{AnnotatedSpan, Diagnostic},
    fs::{
        Filesystem, FsCanonicalizer, PathFile, VisitOnceFile,
        VisitOnceFilesystem,
    },
    ld::xmle::Sections,
    obj::xmlo::{
        XmloAirContext, XmloAirError, XmloError, XmloReader, XmloToAir,
        XmloToken,
    },
    parse::{Lower, ParseError, Parsed, ParsedObject, UnknownToken},
    sym::{GlobalSymbolResolve, SymbolId},
    xir::reader::XmlXirReader,
    xir::{
        flat::{self, Object as XirfToken, StateError as XirfError},
        writer::{Error as XirWriterError, XmlWriter},
        DefaultEscaper, Error as XirError, Escaper, Token as XirToken,
    },
};
use fxhash::FxBuildHasher;
use petgraph_graphml::GraphMl;
use std::{
    error::Error,
    fmt::{self, Display},
    fs,
    io::{self, BufReader, BufWriter, Write},
    path::{Path, PathBuf},
};

type LinkerAsg = DefaultAsg;

pub fn xmle(package_path: &str, output: &str) -> Result<(), TameldError> {
    let mut fs = VisitOnceFilesystem::new();
    let escaper = DefaultEscaper::default();

    let (depgraph, state) = load_xmlo(
        package_path,
        &mut fs,
        LinkerAsg::with_capacity(65536, 65536),
        &escaper,
        XmloAirContext::default(),
    )?;

    let XmloAirContext {
        prog_name: name,
        relroot,
        ..
    } = state;

    let sorted = match sort(&depgraph, Sections::new()) {
        Ok(sections) => sections,
        Err(SortError::Cycles(cycles)) => {
            return Err(TameldError::CycleError(
                cycles
                    .into_iter()
                    .map(|cycle| {
                        let mut path: Vec<SymbolId> = cycle
                            .into_iter()
                            .filter_map(|obj| {
                                depgraph
                                    .get(obj)
                                    .unwrap()
                                    .as_ident_ref()
                                    .map(Ident::name)
                            })
                            .collect();

                        path.reverse();
                        path.push(path[0].clone());
                        path
                    })
                    .collect(),
            ))
        }
        Err(e) => return Err(e.into()),
    };

    output_xmle(
        sorted,
        name.expect("missing root package name"),
        relroot.expect("missing root package relroot"),
        output,
        &escaper,
    )?;

    Ok(())
}

// TODO: This needs to be further generalized.
pub fn graphml(package_path: &str, output: &str) -> Result<(), TameldError> {
    let mut fs = VisitOnceFilesystem::new();
    let escaper = DefaultEscaper::default();

    let (depgraph, _) = load_xmlo(
        package_path,
        &mut fs,
        LinkerAsg::with_capacity(65536, 65536),
        &escaper,
        XmloAirContext::default(),
    )?;

    // if we move away from petgraph, we will need to abstract this away
    let g = depgraph.into_inner();
    let graphml =
        GraphMl::new(&g)
            .pretty_print(true)
            .export_node_weights(Box::new(|node| {
                let (name, kind, generated) = match node {
                    Some(Object::Ident(n)) => {
                        let generated = match n.src() {
                            Some(src) => src.generated,
                            None => false,
                        };

                        (
                            n.name().lookup_str().into(),
                            n.kind().unwrap().as_sym(),
                            format!("{}", generated),
                        )
                    }
                    // TODO: We want these filtered.
                    Some(_) => (
                        String::from("non-ident"),
                        "non-ident".into(),
                        "false".into(),
                    ),
                    None => (
                        String::from("missing"),
                        "missing".into(),
                        "false".into(),
                    ),
                };

                vec![
                    ("label".into(), name.into()),
                    ("kind".into(), kind.lookup_str().into()),
                    ("generated".into(), generated.into()),
                ]
            }));

    fs::write(output, graphml.to_string())?;

    Ok(())
}

fn load_xmlo<'a, P: AsRef<Path>, S: Escaper>(
    path_str: P,
    fs: &mut VisitOnceFilesystem<FsCanonicalizer, FxBuildHasher>,
    asg: Asg,
    escaper: &S,
    state: XmloAirContext,
) -> Result<(Asg, XmloAirContext), TameldError> {
    let PathFile(path, file, ctx): PathFile<BufReader<fs::File>> =
        match fs.open(path_str)? {
            VisitOnceFile::FirstVisit(file) => file,
            VisitOnceFile::Visited => return Ok((asg, state)),
        };

    // TODO: This entire block is a WIP and will be incrementally
    //   abstracted away.
    let (mut asg, mut state) =
        Lower::<ParsedObject<XirToken, XirError>, flat::State<64>>::lower::<
            _,
            TameldError,
        >(&mut XmlXirReader::new(file, escaper, ctx), |toks| {
            Lower::<flat::State<64>, XmloReader>::lower(toks, |xmlo| {
                let mut iter = xmlo.scan(false, |st, rtok| match st {
                    true => None,
                    false => {
                        *st = matches!(
                            rtok,
                            Ok(Parsed::Object(XmloToken::Eoh(..)))
                        );
                        Some(rtok)
                    }
                });

                Lower::<XmloReader, XmloToAir>::lower_with_context(
                    &mut iter,
                    state,
                    |air| {
                        let (_, asg) =
                            Lower::<XmloToAir, AirState>::lower_with_context(
                                air,
                                asg,
                                |end| {
                                    end.fold(
                                        Result::<(), TameldError>::Ok(()),
                                        |x, _| x,
                                    )
                                },
                            )?;

                        Ok(asg)
                    },
                )
            })
        })?;

    let mut dir: PathBuf = path.clone();
    dir.pop();

    let found = state.found.take().unwrap_or_default();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        let str: &str = &relpath.lookup_str();
        path_buf.push(str);
        path_buf.set_extension("xmlo");

        (asg, state) = load_xmlo(path_buf, fs, asg, escaper, state)?;
    }

    Ok((asg, state))
}

fn output_xmle<'a, X: XmleSections<'a>, S: Escaper>(
    sorted: X,
    name: SymbolId,
    relroot: SymbolId,
    output: &str,
    escaper: &S,
) -> Result<(), TameldError> {
    let file = fs::File::create(output)?;
    let mut buf = BufWriter::new(file);

    lower_iter(sorted, name, relroot).write(
        &mut buf,
        Default::default(),
        escaper,
    )?;
    buf.flush()?;

    Ok(())
}

// TODO: This, like everything else here, needs a home.
// TODO: Better encapsulation for `*ParseError` types.
/// Linker (`tameld`) error.
///
/// This represents the aggregation of all possible errors that can occur
///   during link-time.
/// This cannot include panics,
///   but efforts have been made to reduce panics to situations that
///   represent the equivalent of assertions.
#[derive(Debug)]
pub enum TameldError {
    Io(io::Error),
    SortError(SortError),
    XirParseError(ParseError<UnknownToken, XirError>),
    XirfParseError(ParseError<XirToken, XirfError>),
    XmloParseError(ParseError<XirfToken, XmloError>),
    XmloLowerError(ParseError<XmloToken, XmloAirError>),
    AirLowerError(ParseError<AirToken, AsgError>),
    XirWriterError(XirWriterError),
    CycleError(Vec<Vec<SymbolId>>),
    Fmt(fmt::Error),
}

impl From<io::Error> for TameldError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<SortError> for TameldError {
    fn from(e: SortError) -> Self {
        Self::SortError(e)
    }
}

impl From<ParseError<UnknownToken, XirError>> for TameldError {
    fn from(e: ParseError<UnknownToken, XirError>) -> Self {
        Self::XirParseError(e)
    }
}

impl From<ParseError<XirfToken, XmloError>> for TameldError {
    fn from(e: ParseError<XirfToken, XmloError>) -> Self {
        Self::XmloParseError(e)
    }
}

impl From<ParseError<XirToken, XirfError>> for TameldError {
    fn from(e: ParseError<XirToken, XirfError>) -> Self {
        Self::XirfParseError(e)
    }
}

impl From<ParseError<XmloToken, XmloAirError>> for TameldError {
    fn from(e: ParseError<XmloToken, XmloAirError>) -> Self {
        Self::XmloLowerError(e)
    }
}

impl From<ParseError<AirToken, AsgError>> for TameldError {
    fn from(e: ParseError<AirToken, AsgError>) -> Self {
        Self::AirLowerError(e)
    }
}

impl From<XirWriterError> for TameldError {
    fn from(e: XirWriterError) -> Self {
        Self::XirWriterError(e)
    }
}

impl From<fmt::Error> for TameldError {
    fn from(e: fmt::Error) -> Self {
        Self::Fmt(e)
    }
}

impl Display for TameldError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => Display::fmt(e, f),
            Self::SortError(e) => Display::fmt(e, f),
            Self::XirParseError(e) => Display::fmt(e, f),
            Self::XirfParseError(e) => Display::fmt(e, f),
            Self::XmloParseError(e) => Display::fmt(e, f),
            Self::XmloLowerError(e) => Display::fmt(e, f),
            Self::AirLowerError(e) => Display::fmt(e, f),
            Self::XirWriterError(e) => Display::fmt(e, f),
            Self::CycleError(cycles) => {
                for cycle in cycles {
                    writeln!(
                        f,
                        "cycle: {}",
                        cycle
                            .iter()
                            .map(|sym| sym.lookup_str())
                            .collect::<Vec<&str>>()
                            .join(" -> ")
                    )?;
                }

                Ok(())
            }
            Self::Fmt(e) => Display::fmt(e, f),
        }
    }
}

impl Error for TameldError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::SortError(e) => Some(e),
            Self::XirParseError(e) => Some(e),
            Self::XirfParseError(e) => Some(e),
            Self::XmloParseError(e) => Some(e),
            Self::XmloLowerError(e) => Some(e),
            Self::AirLowerError(e) => Some(e),
            Self::XirWriterError(e) => Some(e),
            Self::CycleError(..) => None,
            Self::Fmt(e) => Some(e),
        }
    }
}

impl Diagnostic for TameldError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::XirParseError(e) => e.describe(),
            Self::XirfParseError(e) => e.describe(),
            Self::XmloParseError(e) => e.describe(),
            Self::XmloLowerError(e) => e.describe(),
            Self::AirLowerError(e) => e.describe(),

            // TODO (will fall back to rendering just the error `Display`)
            _ => vec![],
        }
    }
}
