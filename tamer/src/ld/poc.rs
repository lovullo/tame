// Proof-of-concept TAME linker
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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
    asg::{Asg, DefaultAsg, IdentObject},
    fs::{
        Filesystem, FsCanonicalizer, PathFile, VisitOnceFile,
        VisitOnceFilesystem,
    },
    iter::into_iter_while_ok,
    ld::xmle::Sections,
    obj::xmlo::{
        AsgBuilder, AsgBuilderError, AsgBuilderState, XmloError, XmloReader,
    },
    parse::ParseError,
    parse::{ParseState, Parsed},
    sym::{GlobalSymbolIntern, GlobalSymbolResolve, SymbolId},
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
    fmt::Display,
    fs,
    io::{self, BufReader, BufWriter, Write},
    path::{Path, PathBuf},
};

type LinkerAsg = DefaultAsg<IdentObject>;
type LinkerAsgBuilderState = AsgBuilderState<FxBuildHasher>;

pub fn xmle(package_path: &str, output: &str) -> Result<(), TameldError> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let escaper = DefaultEscaper::default();

    let state = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
        &escaper,
        AsgBuilderState::new(),
    )?;

    let AsgBuilderState {
        mut roots,
        name,
        relroot,
        found: _,
    } = state;

    roots.extend(
        vec!["___yield", "___worksheet"]
            .iter()
            .map(|name| name.intern())
            .filter_map(|sym| depgraph.lookup(sym)),
    );

    let sorted = match sort(&depgraph, &roots, Sections::new()) {
        Ok(sections) => sections,
        Err(SortError::Cycles(cycles)) => {
            return Err(TameldError::CycleError(
                cycles
                    .into_iter()
                    .map(|cycle| {
                        let mut path: Vec<SymbolId> = cycle
                            .into_iter()
                            .map(|obj| depgraph.get(obj).unwrap().name())
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

pub fn graphml(package_path: &str, output: &str) -> Result<(), TameldError> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let escaper = DefaultEscaper::default();

    let _ = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
        &escaper,
        AsgBuilderState::new(),
    )?;

    // if we move away from petgraph, we will need to abstract this away
    let g = depgraph.into_inner();
    let graphml =
        GraphMl::new(&g)
            .pretty_print(true)
            .export_node_weights(Box::new(|node| {
                let (name, kind, generated) = match node {
                    Some(n) => {
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
                    None => (
                        String::from("missing"),
                        "missing".into(),
                        format!("{}", false),
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
    depgraph: &mut LinkerAsg,
    escaper: &S,
    state: LinkerAsgBuilderState,
) -> Result<LinkerAsgBuilderState, TameldError> {
    let PathFile(path, file, ctx): PathFile<BufReader<fs::File>> =
        match fs.open(path_str)? {
            VisitOnceFile::FirstVisit(file) => file,
            VisitOnceFile::Visited => return Ok(state),
        };

    // TODO: This entire block is a WIP and will be incrementally
    //   abstracted away.
    let mut state =
        into_iter_while_ok(XmlXirReader::new(file, escaper, ctx), |toks| {
            flat::State::<64>::parse(toks).lower_while_ok::<XmloReader, _>(
                |xirf| {
                    into_iter_while_ok(xirf, |xmlo_out| {
                        // TODO: Transitionary---we do not want to filter.
                        depgraph.import_xmlo(
                            xmlo_out.filter_map(|parsed| match parsed {
                                Parsed::Incomplete => None,
                                Parsed::Object(obj) => Some(Ok(obj)),
                            }),
                            state,
                        )
                    })
                },
            )
        })????;

    let mut dir: PathBuf = path.clone();
    dir.pop();

    let found = state.found.take().unwrap_or_default();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        let str: &str = &relpath.lookup_str();
        path_buf.push(str);
        path_buf.set_extension("xmlo");

        state = load_xmlo(path_buf, fs, depgraph, escaper, state)?;
    }

    Ok(state)
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
    XirError(XirError),
    XirfParseError(ParseError<XirToken, XirfError>),
    XmloParseError(ParseError<XirfToken, XmloError>),
    AsgBuilderError(AsgBuilderError),
    XirWriterError(XirWriterError),

    CycleError(Vec<Vec<SymbolId>>),
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

impl From<XirError> for TameldError {
    fn from(e: XirError) -> Self {
        Self::XirError(e)
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

impl From<AsgBuilderError> for TameldError {
    fn from(e: AsgBuilderError) -> Self {
        Self::AsgBuilderError(e)
    }
}

impl From<XirWriterError> for TameldError {
    fn from(e: XirWriterError) -> Self {
        Self::XirWriterError(e)
    }
}

impl Display for TameldError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => Display::fmt(e, f),
            Self::SortError(e) => Display::fmt(e, f),
            Self::XirError(e) => Display::fmt(e, f),
            Self::XirfParseError(e) => Display::fmt(e, f),
            Self::XmloParseError(e) => Display::fmt(e, f),
            Self::AsgBuilderError(e) => Display::fmt(e, f),
            Self::XirWriterError(e) => Display::fmt(e, f),

            TameldError::CycleError(cycles) => {
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
        }
    }
}
