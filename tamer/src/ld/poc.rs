// Proof-of-concept TAME linker
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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
        air::{Air, AirAggregate},
        Asg, AsgError, DefaultAsg,
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
    parse::{
        lowerable, FinalizeError, Lower, ParseError, Parsed, ParsedObject,
        UnknownToken,
    },
    sym::{GlobalSymbolResolve, SymbolId},
    xir::{
        flat::{PartialXirToXirf, Text, XirToXirfError, XirfToken},
        reader::XmlXirReader,
        writer::{Error as XirWriterError, XmlWriter},
        DefaultEscaper, Error as XirError, Escaper, Token as XirToken,
    },
};
use fxhash::FxBuildHasher;
use std::{
    error::Error,
    fmt::{self, Display},
    fs,
    io::{self, BufReader, BufWriter, Write},
    path::Path,
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

    let sorted = sort(&depgraph, Sections::new())?;

    output_xmle(
        sorted,
        name.expect("missing root package name"),
        relroot.expect("missing root package relroot"),
        output,
        &escaper,
    )?;

    Ok(())
}

fn load_xmlo<P: AsRef<Path>, S: Escaper>(
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

    let src = &mut lowerable(XmlXirReader::new(file, escaper, ctx))
        .map(|result| result.map_err(TameldError::from));

    // TODO: This entire block is a WIP and will be incrementally
    //   abstracted away.
    let (mut asg, mut state) = Lower::<
        ParsedObject<UnknownToken, XirToken, XirError>,
        PartialXirToXirf<4, Text>,
        _,
    >::lower(src, |toks| {
        Lower::<PartialXirToXirf<4, Text>, XmloReader, _>::lower(toks, |xmlo| {
            let mut iter = xmlo.scan(false, |st, rtok| match st {
                true => None,
                false => {
                    *st =
                        matches!(rtok, Ok(Parsed::Object(XmloToken::Eoh(..))));
                    Some(rtok)
                }
            });

            Lower::<XmloReader, XmloToAir, _>::lower_with_context(
                &mut iter,
                state,
                |air| {
                    let (_, asg) =
                            Lower::<XmloToAir, AirAggregate, _>::lower_with_context(
                                air,
                                asg,
                                |end| {
                                    end.fold(
                                        Result::<(), TameldError>::Ok(()),
                                        |x, _| x,
                                    )
                                },
                            )?;

                    Ok::<_, TameldError>(asg)
                },
            )
        })
    })?;

    let mut dir = path;
    dir.pop();

    let found = state.found.take().unwrap_or_default();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        path_buf.push(relpath.lookup_str());
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
    XirfParseError(ParseError<XirToken, XirToXirfError>),
    XmloParseError(ParseError<XirfToken<Text>, XmloError>),
    XmloLowerError(ParseError<XmloToken, XmloAirError>),
    AirLowerError(ParseError<Air, AsgError>),
    XirWriterError(XirWriterError),
    FinalizeError(FinalizeError),
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

impl From<ParseError<XirfToken<Text>, XmloError>> for TameldError {
    fn from(e: ParseError<XirfToken<Text>, XmloError>) -> Self {
        Self::XmloParseError(e)
    }
}

impl From<ParseError<XirToken, XirToXirfError>> for TameldError {
    fn from(e: ParseError<XirToken, XirToXirfError>) -> Self {
        Self::XirfParseError(e)
    }
}

impl From<ParseError<XmloToken, XmloAirError>> for TameldError {
    fn from(e: ParseError<XmloToken, XmloAirError>) -> Self {
        Self::XmloLowerError(e)
    }
}

impl From<ParseError<Air, AsgError>> for TameldError {
    fn from(e: ParseError<Air, AsgError>) -> Self {
        Self::AirLowerError(e)
    }
}

impl From<FinalizeError> for TameldError {
    fn from(e: FinalizeError) -> Self {
        Self::FinalizeError(e)
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
            Self::FinalizeError(e) => Display::fmt(e, f),
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
            Self::FinalizeError(e) => Some(e),
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
            Self::FinalizeError(e) => e.describe(),
            Self::SortError(e) => e.describe(),

            Self::Io(_) | Self::XirWriterError(_) | Self::Fmt(_) => vec![],
        }
    }
}
