// TAME compiler
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

//! This is the TAME compiler.
//!
//! `tamec` compiles source code into object files that are later linked
//!   into a final executable using [`tameld`](../tameld).

extern crate tamer;

use getopts::{Fail, Options};
use std::{
    env,
    error::Error,
    fmt::{self, Display, Write},
    fs::{self, File},
    io::{self, BufReader, BufWriter},
    path::Path,
};
use tamer::{
    diagnose::{
        AnnotatedSpan, Diagnostic, FsSpanResolver, Reporter, VisualReporter,
    },
    nir::{InterpError, InterpolateNir, Nir, XirfToNir, XirfToNirError},
    parse::{
        Lower, ParseError, Parsed, ParsedObject, ParsedResult, UnknownToken,
    },
    xir::{
        self,
        flat::{RefinedText, XirToXirf, XirToXirfError, XirfToken},
        reader::XmlXirReader,
        writer::XmlWriter,
        DefaultEscaper, Error as XirError, Token as XirToken,
    },
};

/// Types of commands
enum Command {
    Compile(String, String, String),
    Usage,
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
    mut fout: W,
    escaper: &'e DefaultEscaper,
) -> impl FnMut(&ParsedResult<ParsedObject<XirToken, XirError>>) + 'e {
    let mut xmlwriter = Default::default();

    move |tok_result| match tok_result {
        Ok(Parsed::Object(tok)) => {
            xmlwriter = tok.write(&mut fout, xmlwriter, escaper).unwrap();
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
) -> Result<(), UnrecoverableError> {
    let dest = Path::new(&dest_path);
    let fout = BufWriter::new(fs::File::create(dest)?);

    let escaper = DefaultEscaper::default();

    let mut ebuf = String::new();

    fn report_err<R: Reporter>(
        e: &RecoverableError,
        reporter: &mut R,
        ebuf: &mut String,
    ) -> Result<(), UnrecoverableError> {
        // See below note about buffering.
        ebuf.clear();
        writeln!(ebuf, "{}", reporter.render(e))?;
        println!("{ebuf}");

        Ok(())
    }

    // TODO: We're just echoing back out XIR,
    //   which will be the same sans some formatting.
    let src = &mut src_reader(src_path, &escaper)?
        .inspect(copy_xml_to(fout, &escaper))
        .map(|result| result.map_err(RecoverableError::from));

    let _ = Lower::<
        ParsedObject<XirToken, XirError>,
        XirToXirf<64, RefinedText>,
        _,
    >::lower::<_, UnrecoverableError>(src, |toks| {
        Lower::<XirToXirf<64, RefinedText>, XirfToNir, _>::lower(toks, |nir| {
            Lower::<XirfToNir, InterpolateNir, _>::lower(nir, |nir| {
                nir.fold(Ok(()), |x, result| match result {
                    Ok(_) => x,
                    Err(e) => {
                        report_err(&e, reporter, &mut ebuf)?;
                        x
                    }
                })
            })
        })
    })?;

    match reporter.has_errors() {
        false => Ok(()),
        true => Err(UnrecoverableError::ErrorsDuringLowering(
            reporter.error_count(),
        )),
    }
}

/// Entrypoint for the compiler
pub fn main() -> Result<(), UnrecoverableError> {
    let args: Vec<String> = env::args().collect();
    let program = &args[0];
    let opts = get_opts();
    let usage = opts.usage(&format!("Usage: {} [OPTIONS] INPUT", program));

    match parse_options(opts, args) {
        Ok(Command::Compile(src_path, _, dest_path)) => {
            let mut reporter = VisualReporter::new(FsSpanResolver);

            compile(&src_path, &dest_path, &mut reporter).or_else(
                |e: UnrecoverableError| {
                    // Rendering to a string ensures buffering so that we
                    //   don't interleave output between processes.
                    let report = reporter.render(&e).to_string();
                    println!(
                        "{report}\nfatal: failed to compile `{}`",
                        dest_path
                    );

                    std::process::exit(1);
                },
            )
        }
        Ok(Command::Usage) => {
            println!("{}", usage);
            std::process::exit(exitcode::OK);
        }
        Err(e) => {
            eprintln!("{}", e);
            println!("{}", usage);
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
            "xmlo" => m,
            _ => {
                return Err(Fail::ArgumentMissing(String::from("--emit xmlo")))
            }
        },
        None => {
            return Err(Fail::OptionMissing(String::from("--emit xmlo")));
        }
    };

    let output = match matches.opt_str("o") {
        Some(m) => m,
        // we check that the extension is "xml" later
        None => format!("{}o", input),
    };

    Ok(Command::Compile(input, emit, output))
}

/// Toplevel `tamec` error representing a failure to complete the requested
///   operation successfully.
///
/// These are errors that will result in aborting execution and exiting with
///   a non-zero status.
/// Contrast this with [`RecoverableError`],
///   which is reported real-time to the user and _does not_ cause the
///   program to abort until the end of the compilation unit.
#[derive(Debug)]
pub enum UnrecoverableError {
    Io(io::Error),
    Fmt(fmt::Error),
    XirWriterError(xir::writer::Error),
    ErrorsDuringLowering(ErrorCount),
}

/// Number of errors that occurred during this compilation unit.
///
/// Let's hope that this is large enough for the number of errors you may
///   have in your code.
type ErrorCount = usize;

/// An error that occurs during the lowering pipeline that may be recovered
///   from to continue parsing and collection of additional errors.
///
/// This represents the aggregation of all possible errors that can occur
///   during lowering.
/// This cannot include panics,
///   but efforts have been made to reduce panics to situations that
///   represent the equivalent of assertions.
///
/// These errors are distinct from [`UnrecoverableError`],
///   which represents the errors that could be returned to the toplevel
///     `main`,
///   because these errors are intended to be reported to the user _and then
///     recovered from_ so that compilation may continue and more errors may
///     be collected;
///       nobody wants a compiler that reports one error at a time.
///
/// Note that an recoverable error,
///   under a normal compilation strategy,
///   will result in an [`UnrecoverableError::ErrorsDuringLowering`] at the
///     end of the compilation unit.
#[derive(Debug)]
pub enum RecoverableError {
    XirParseError(ParseError<UnknownToken, xir::Error>),
    XirfParseError(ParseError<XirToken, XirToXirfError>),
    NirParseError(ParseError<XirfToken<RefinedText>, XirfToNirError>),
    InterpError(ParseError<Nir, InterpError>),
}

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

impl From<ParseError<UnknownToken, xir::Error>> for RecoverableError {
    fn from(e: ParseError<UnknownToken, xir::Error>) -> Self {
        Self::XirParseError(e)
    }
}

impl From<ParseError<XirToken, XirToXirfError>> for RecoverableError {
    fn from(e: ParseError<XirToken, XirToXirfError>) -> Self {
        Self::XirfParseError(e)
    }
}

impl From<ParseError<XirfToken<RefinedText>, XirfToNirError>>
    for RecoverableError
{
    fn from(e: ParseError<XirfToken<RefinedText>, XirfToNirError>) -> Self {
        Self::NirParseError(e)
    }
}

impl From<ParseError<Nir, InterpError>> for RecoverableError {
    fn from(e: ParseError<Nir, InterpError>) -> Self {
        Self::InterpError(e)
    }
}

impl Display for UnrecoverableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => Display::fmt(e, f),
            Self::Fmt(e) => Display::fmt(e, f),
            Self::XirWriterError(e) => Display::fmt(e, f),

            // TODO: Use formatter for dynamic "error(s)"
            Self::ErrorsDuringLowering(err_count) => {
                write!(f, "aborting due to previous {err_count} error(s)",)
            }
        }
    }
}

impl Display for RecoverableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::XirParseError(e) => Display::fmt(e, f),
            Self::XirfParseError(e) => Display::fmt(e, f),
            Self::NirParseError(e) => Display::fmt(e, f),
            Self::InterpError(e) => Display::fmt(e, f),
        }
    }
}

impl Error for UnrecoverableError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Fmt(e) => Some(e),
            Self::XirWriterError(e) => Some(e),
            Self::ErrorsDuringLowering(_) => None,
        }
    }
}

impl Error for RecoverableError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::XirParseError(e) => Some(e),
            Self::XirfParseError(e) => Some(e),
            Self::NirParseError(e) => Some(e),
            Self::InterpError(e) => Some(e),
        }
    }
}

impl Diagnostic for UnrecoverableError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            // Fall back to `Display`
            _ => vec![],
        }
    }
}

impl Diagnostic for RecoverableError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::XirParseError(e) => e.describe(),
            Self::XirfParseError(e) => e.describe(),
            Self::NirParseError(e) => e.describe(),
            Self::InterpError(e) => e.describe(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
                assert_eq!("xmlo", xmlo);
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
                assert_eq!("xmlo", xmlo);
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
                assert_eq!("xmlo", xmlo);
            }
            _ => panic!("Unexpected result"),
        }
    }
}
