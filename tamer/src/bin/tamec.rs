// TAME compiler
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

// Use your judgment;
//   a `match` may be more clear within a given context.
#![allow(clippy::single_match)]
#![feature(assert_matches)]

//! This is the TAME compiler.
//!
//! `tamec` compiles source code into object files that are later linked
//!   into a final executable using [`tameld`](../tameld).

extern crate tamer;

use getopts::{Fail, Options};
use std::{
    convert::Infallible,
    env,
    error::Error,
    fmt::{self, Display, Write},
    fs::{self, File},
    io::{self, BufReader, BufWriter},
    path::Path,
};
use tamer::{
    asg::DefaultAsg,
    diagnose::{
        AnnotatedSpan, Diagnostic, FsSpanResolver, Reporter, VisualReporter,
    },
    nir::NirToAirParseType,
    parse::{lowerable, FinalizeError, ParseError, Token},
    pipeline::{parse_package_xml, LowerXmliError, ParsePackageXmlError},
    xir::{self, reader::XmlXirReader, writer::XmlWriter, DefaultEscaper},
};

/// Types of commands
#[derive(Debug, PartialEq)]
enum Command {
    Compile(String, ObjectFileKind, String),
    Usage,
}

/// The type of object file to output.
///
/// While TAMER is under development,
///   object files serve as a transition between the new compiler and the
///   old.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ObjectFileKind {
    /// Produce something akin to an object file.
    ///
    /// During TAME's development,
    ///   this is an `xmli` file that is passed to the old compiler to pick
    ///   up where this one left off.
    ///
    /// This is the stable feature set,
    ///   expected to work with any package.
    XmloStable,

    /// Enable experimental flag(s),
    ///   attempting to build the given package with an system that has not
    ///   yet stabalized and is bound to fail on some packages.
    ///
    /// This is intentionally vague.
    /// It should be used only for testing.
    XmloExperimental,
}

/// Create a [`XmlXirReader`] for a source file.
///
/// The provided escaper must be shared between all readers and writers in
///   order to benefit from its caching.
fn src_reader<'a>(
    input: &'a String,
    escaper: &'a DefaultEscaper,
) -> Result<XmlXirReader<'a, BufReader<File>>, UnrecoverableError> {
    use tamer::fs::{File, PathFile};

    let source = Path::new(input);

    let PathFile(_, file, ctx): PathFile<BufReader<fs::File>> =
        PathFile::open(source)?;

    Ok(XmlXirReader::new(file, escaper, ctx))
}

/// Write each parsed token to the provided buffer.
///
/// This is intended to be a temporary function that exists during a
///   transition period between the XSLT-based TAME and TAMER.
/// Writing XIR proves that the source file is being successfully parsed and
///   helps to evaluate system performance.
fn copy_xml_to<'e, W: io::Write + 'e>(
    mut fout: Option<W>,
    escaper: &'e DefaultEscaper,
) -> impl FnMut(&Result<tamer::xir::Token, tamer::xir::Error>) + 'e {
    let mut xmlwriter = Default::default();

    move |tok_result| match (fout.as_mut(), tok_result) {
        (Some(mut dest), Ok(tok)) => {
            xmlwriter = tok.write(&mut dest, xmlwriter, escaper).unwrap();
        }
        _ => (),
    }
}

/// Compile a source file,
///   writing to the provided destination path.
///
/// NB: Output is presently a _copy_ of the input,
///   with formatting partially removed.
fn compile<R: Reporter>(
    src_path: &String,
    dest_path: &String,
    reporter: &mut R,
    kind: ObjectFileKind,
) -> Result<(), UnrecoverableError> {
    let dest = Path::new(&dest_path);

    let (fcopy, fout, parse_type) = match kind {
        // Parse XML and re-emit into target verbatim
        //   (but missing some formatting).
        // Tokens will act as no-ops after NIR.
        ObjectFileKind::XmloStable => (
            Some(BufWriter::new(fs::File::create(dest)?)),
            None,
            NirToAirParseType::Noop,
        ),

        // Parse sources into ASG and re-generate sources from there.
        // This will fail if the source package utilize features that are
        //   not yet supported.
        ObjectFileKind::XmloExperimental => (
            None,
            Some(BufWriter::new(fs::File::create(dest)?)),
            NirToAirParseType::LowerKnownErrorRest,
        ),
    };

    let escaper = DefaultEscaper::default();
    let mut ebuf = String::new();

    let report_err = |result: Result<(), ParsePackageXmlError<_>>| {
        result.or_else(|e| {
            // See below note about buffering.
            ebuf.clear();
            writeln!(ebuf, "{}", reporter.render(&e))?;
            println!("{ebuf}");

            Ok::<_, UnrecoverableError>(())
        })
    };

    let src = &mut lowerable(
        src_reader(src_path, &escaper)?.inspect(copy_xml_to(fcopy, &escaper)),
    );

    // TODO: Determine a good default capacity once we have this populated
    //   and can come up with some heuristics.
    let (_, air_ctx) = parse_package_xml(
        parse_type,
        DefaultAsg::with_capacity(1024, 2048),
    )(src, report_err)?;

    if reporter.has_errors() {
        Err(UnrecoverableError::ErrorsDuringLowering(
            reporter.error_count(),
        ))
    } else if let Some(dest) = fout {
        let asg = air_ctx.finish();
        derive_xmli(asg, dest, &escaper)
    } else {
        Ok(())
    }
}

/// Derive an `xmli` file from the ASG.
///
/// The `xmli` file is an intermediate file that allows us to incrementally
///   transition responsibilities away from the old XSLT-based compiler and
///   into TAMER.
/// It will represent a program that is derived from the program the user
///   originally defined,
///     and must be an equivalent program,
///     but will look different;
///       TAMER reasons about the system using a different paradigm.
fn derive_xmli(
    asg: tamer::asg::Asg,
    mut fout: impl std::io::Write,
    escaper: &DefaultEscaper,
) -> Result<(), UnrecoverableError> {
    use tamer::{
        asg::visit::tree_reconstruction, pipeline, xir::writer::WriterState,
    };

    let src = lowerable(tree_reconstruction(&asg).map(Ok));

    // TODO: Remove bad file?
    //   Let make do it?
    let mut st = WriterState::default();
    let (_asg,) = pipeline::lower_xmli(&asg)(src, |result| {
        // Write failures should immediately bail out;
        //   we can't skip writing portions of the file and
        //   just keep going!
        result
            .map_err(Into::<UnrecoverableError>::into)
            .and_then(|tok| {
                tok.write(&mut fout, st, escaper)
                    .map(|newst| st = newst)
                    .map_err(Into::<UnrecoverableError>::into)
            })
    })?;

    Ok(())
}

/// Entrypoint for the compiler
pub fn main() -> Result<(), UnrecoverableError> {
    let args: Vec<String> = env::args().collect();
    let program = &args[0];
    let opts = get_opts();
    let usage = opts.usage(&format!("Usage: {program} [OPTIONS] INPUT"));

    match parse_options(opts, args) {
        Ok(Command::Compile(src_path, kind, dest_path)) => {
            let mut reporter = VisualReporter::new(FsSpanResolver);

            compile(&src_path, &dest_path, &mut reporter, kind).map_err(
                |e: UnrecoverableError| {
                    // Rendering to a string ensures buffering so that we
                    //   don't interleave output between processes.
                    let report = reporter.render(&e).to_string();
                    println!(
                        "{report}\nfatal: failed to compile `{dest_path}`",
                    );

                    std::process::exit(1);
                },
            )
        }
        Ok(Command::Usage) => {
            println!("{usage}");
            std::process::exit(exitcode::OK);
        }
        Err(e) => {
            eprintln!("{e}");
            println!("{usage}");
            std::process::exit(exitcode::USAGE);
        }
    }
}

/// Get 'Options'
///
/// ```
/// use getopts::Options;
///
/// let opts = get_opts();
/// ```
fn get_opts() -> Options {
    let mut opts = Options::new();
    opts.optopt("o", "output", "set output file name", "NAME");
    opts.optopt("", "emit", "set output type", "xmlo");
    opts.optflag("h", "help", "print this help menu");

    opts
}

/// Option parser
fn parse_options(opts: Options, args: Vec<String>) -> Result<Command, Fail> {
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            return Err(f);
        }
    };

    if matches.opt_present("h") {
        return Ok(Command::Usage);
    }

    let input = match matches.free.len() {
        0 => return Err(Fail::OptionMissing(String::from("INPUT"))),
        1 => matches.free[0].clone(),
        _ => return Err(Fail::UnrecognizedOption(matches.free[1].clone())),
    };

    let emit = match matches.opt_str("emit") {
        Some(m) => match &m[..] {
            "xmlo" => Ok(ObjectFileKind::XmloStable),
            "xmlo-experimental" => Ok(ObjectFileKind::XmloExperimental),
            _ => Err(Fail::ArgumentMissing(String::from("--emit xmlo"))),
        },
        None => Err(Fail::OptionMissing(String::from("--emit xmlo"))),
    }?;

    let output = match matches.opt_str("o") {
        Some(m) => m,
        // we check that the extension is "xml" later
        None => format!("{input}o"),
    };

    Ok(Command::Compile(input, emit, output))
}

/// Toplevel `tamec` error representing a failure to complete the requested
///   operation successfully.
///
/// These are errors that will result in aborting execution and exiting with
///   a non-zero status.
/// Contrast this with recoverable errors in [`tamer::pipeline`],
///   which is reported real-time to the user and _does not_ cause the
///   program to abort until the end of the compilation unit.
///
/// Note that an recoverable error,
///   under a normal compilation strategy,
///   will result in an [`UnrecoverableError::ErrorsDuringLowering`] at the
///     end of the compilation unit.
#[derive(Debug)]
pub enum UnrecoverableError {
    Io(io::Error),
    Fmt(fmt::Error),
    XirWriterError(xir::writer::Error),
    LowerXmliError(LowerXmliError<Infallible>),
    ErrorsDuringLowering(ErrorCount),
    FinalizeError(FinalizeError),
}

/// Number of errors that occurred during this compilation unit.
///
/// Let's hope that this is large enough for the number of errors you may
///   have in your code.
type ErrorCount = usize;

impl From<io::Error> for UnrecoverableError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<fmt::Error> for UnrecoverableError {
    fn from(e: fmt::Error) -> Self {
        Self::Fmt(e)
    }
}

impl From<xir::writer::Error> for UnrecoverableError {
    fn from(e: xir::writer::Error) -> Self {
        Self::XirWriterError(e)
    }
}

impl From<LowerXmliError<Infallible>> for UnrecoverableError {
    fn from(e: LowerXmliError<Infallible>) -> Self {
        Self::LowerXmliError(e)
    }
}

impl From<FinalizeError> for UnrecoverableError {
    fn from(e: FinalizeError) -> Self {
        Self::FinalizeError(e)
    }
}

impl<T: Token> From<ParseError<T, Infallible>> for UnrecoverableError {
    fn from(_: ParseError<T, Infallible>) -> Self {
        unreachable!(
            "<UnrecoverableError as From<ParseError<T, Infallible>>>::from"
        )
    }
}

impl Display for UnrecoverableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnrecoverableError::*;

        match self {
            Io(e) => Display::fmt(e, f),
            Fmt(e) => Display::fmt(e, f),
            LowerXmliError(e) => Display::fmt(e, f),
            XirWriterError(e) => Display::fmt(e, f),
            FinalizeError(e) => Display::fmt(e, f),

            // TODO: Use formatter for dynamic "error(s)"
            ErrorsDuringLowering(err_count) => {
                write!(f, "aborting due to previous {err_count} error(s)",)
            }
        }
    }
}

impl Error for UnrecoverableError {}

impl Diagnostic for UnrecoverableError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use UnrecoverableError::*;

        match self {
            LowerXmliError(e) => e.describe(),
            FinalizeError(e) => e.describe(),

            // Fall back to `Display`
            Io(_) | Fmt(_) | XirWriterError(_) | ErrorsDuringLowering(_) => {
                vec![]
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::assert_matches::assert_matches;

    #[test]
    fn parse_options_help() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![String::from("program"), String::from("-h")],
        );

        match result {
            Ok(Command::Usage) => {}
            _ => panic!("Long help option did not parse"),
        }
    }

    #[test]
    fn parse_options_help_long() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![String::from("program"), String::from("--help")],
        );

        match result {
            Ok(Command::Usage) => {}
            _ => panic!("Help option did not parse"),
        }
    }

    #[test]
    fn parse_options_invalid() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![String::from("program"), String::from("-q")],
        );

        match result {
            Err(Fail::UnrecognizedOption(_)) => {}
            _ => panic!("Invalid option not caught"),
        }
    }

    #[test]
    fn parse_options_missing_input() {
        let opts = get_opts();
        let result = parse_options(opts, vec![String::from("program")]);

        match result {
            Err(Fail::OptionMissing(message)) => {
                assert_eq!("INPUT", message);
            }
            _ => panic!("Missing input not caught"),
        }
    }

    #[test]
    fn parse_options_missing_emit() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![String::from("program"), String::from("filename")],
        );

        match result {
            Err(Fail::OptionMissing(message)) => {
                assert_eq!("--emit xmlo", message);
            }
            _ => panic!("Missing emit not caught"),
        }
    }

    #[test]
    fn parse_options_invalid_emit() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("filename.xml"),
                String::from("--emit"),
                String::from("foo"),
            ],
        );

        match result {
            Err(Fail::ArgumentMissing(message)) => {
                assert_eq!("--emit xmlo", message);
            }
            _ => panic!("Invalid emit not caught"),
        }
    }

    #[test]
    fn parse_options_too_many_args() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo"),
                String::from("--emit"),
                String::from("bar"),
                String::from("baz"),
            ],
        );

        match result {
            Err(Fail::UnrecognizedOption(message)) => {
                assert_eq!("baz", message);
            }
            _ => panic!("Extra option not caught"),
        }
    }

    #[test]
    fn parse_options_valid() {
        let opts = get_opts();
        let xmlo = String::from("xmlo");
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo.xml"),
                String::from("--emit"),
                xmlo,
            ],
        );

        match result {
            Ok(Command::Compile(infile, xmlo, outfile)) => {
                assert_eq!("foo.xml", infile);
                assert_eq!("foo.xmlo", outfile);
                assert_eq!(ObjectFileKind::XmloStable, xmlo);
            }
            _ => panic!("Unexpected result"),
        }
    }

    #[test]
    fn parse_options_valid_custom_out() {
        let opts = get_opts();
        let xmlo = String::from("xmlo");
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo.xml"),
                String::from("--emit"),
                xmlo,
                String::from("-o"),
                String::from("foo.xmli"),
            ],
        );

        match result {
            Ok(Command::Compile(infile, xmlo, outfile)) => {
                assert_eq!("foo.xml", infile);
                assert_eq!("foo.xmli", outfile);
                assert_eq!(ObjectFileKind::XmloStable, xmlo);
            }
            _ => panic!("Unexpected result"),
        }
    }

    #[test]
    fn parse_options_valid_custom_out_long() {
        let opts = get_opts();
        let xmlo = String::from("xmlo");
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo.xml"),
                String::from("--emit"),
                xmlo,
                String::from("--output"),
                String::from("foo.xmli"),
            ],
        );

        match result {
            Ok(Command::Compile(infile, xmlo, outfile)) => {
                assert_eq!("foo.xml", infile);
                assert_eq!("foo.xmli", outfile);
                assert_eq!(ObjectFileKind::XmloStable, xmlo);
            }
            _ => panic!("Unexpected result"),
        }
    }

    #[test]
    fn parse_options_xmlo_experimetal() {
        let opts = get_opts();
        let xmlo = String::from("xmlo-experimental");
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo.xml"),
                String::from("--emit"),
                xmlo,
                String::from("--output"),
                String::from("foo.xmli"),
            ],
        );

        assert_matches!(
            result,
            Ok(Command::Compile(_, ObjectFileKind::XmloExperimental, _)),
        );
    }
}
