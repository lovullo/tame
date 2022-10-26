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
    cell::RefCell,
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
    nir::{XirfToNir, XirfToNirError},
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
) -> Result<XmlXirReader<'a, BufReader<File>>, TamecError> {
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
    reporter: &R,
) -> Result<(), TamecError> {
    let dest = Path::new(&dest_path);
    let fout = BufWriter::new(fs::File::create(dest)?);

    let escaper = DefaultEscaper::default();

    let ebuf = RefCell::new(String::new());

    fn report_err<R: Reporter>(
        e: &TamecError,
        reporter: &R,
        ebuf: &mut String,
    ) -> Result<(), TamecError> {
        // See below note about buffering.
        ebuf.clear();
        writeln!(ebuf, "{}", reporter.render(e))?;
        println!("{ebuf}");

        Ok(())
    }

    // TODO: You have landed on a commit that is in the middle of
    //   refactoring this lowering pipline;
    //     congratulations!
    //   Specifically,
    //     this is beginning to derive what these lowering steps have in
    //     common so that they can be factored out,
    //       and committing this intermediate state helps with rationalizing
    //       and understanding the changes.

    // TODO: We're just echoing back out XIR,
    //   which will be the same sans some formatting.
    let src = &mut src_reader(src_path, &escaper)?
        .inspect(copy_xml_to(fout, &escaper))
        .map(|result| result.map_err(TamecError::from));

    let _ = Lower::<
        ParsedObject<XirToken, XirError>,
        XirToXirf<64, RefinedText>,
        _,
    >::lower(src, |toks| {
        Lower::<XirToXirf<64, RefinedText>, XirfToNir, _>::lower(toks, |nir| {
            nir.fold(Ok(()), |x, result| match result {
                Ok(_) => x,
                Err(e) => {
                    report_err(&e, reporter, &mut ebuf.borrow_mut())?;
                    x
                }
            })
        })
    })?;

    // TODO: Proper error summary and exit in `main`.
    if reporter.has_errors() {
        println!(
            "fatal: failed to compile `{}` due to previous {} error(s)",
            dest_path,
            reporter.error_count(),
        );
        std::process::exit(1);
    }

    Ok(())
}

/// Entrypoint for the compiler
pub fn main() -> Result<(), TamecError> {
    let args: Vec<String> = env::args().collect();
    let program = &args[0];
    let opts = get_opts();
    let usage = opts.usage(&format!("Usage: {} [OPTIONS] INPUT", program));

    match parse_options(opts, args) {
        Ok(Command::Compile(src_path, _, dest_path)) => {
            let reporter = VisualReporter::new(FsSpanResolver);

            compile(&src_path, &dest_path, &reporter).or_else(
                |e: TamecError| {
                    // POC: Rendering to a string ensures buffering so that we don't
                    //   interleave output between processes.
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

/// Compiler (`tamec`) error.
///
/// This represents the aggregation of all possible errors that can occur
///   during compile-time.
/// This cannot include panics,
///   but efforts have been made to reduce panics to situations that
///   represent the equivalent of assertions.
#[derive(Debug)]
pub enum TamecError {
    Io(io::Error),
    XirParseError(ParseError<UnknownToken, xir::Error>),
    XirfParseError(ParseError<XirToken, XirToXirfError>),
    NirParseError(ParseError<XirfToken<RefinedText>, XirfToNirError>),
    XirWriterError(xir::writer::Error),
    Fmt(fmt::Error),
}

impl From<io::Error> for TamecError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<ParseError<UnknownToken, xir::Error>> for TamecError {
    fn from(e: ParseError<UnknownToken, xir::Error>) -> Self {
        Self::XirParseError(e)
    }
}

impl From<ParseError<XirToken, XirToXirfError>> for TamecError {
    fn from(e: ParseError<XirToken, XirToXirfError>) -> Self {
        Self::XirfParseError(e)
    }
}

impl From<ParseError<XirfToken<RefinedText>, XirfToNirError>> for TamecError {
    fn from(e: ParseError<XirfToken<RefinedText>, XirfToNirError>) -> Self {
        Self::NirParseError(e)
    }
}

impl From<xir::writer::Error> for TamecError {
    fn from(e: xir::writer::Error) -> Self {
        Self::XirWriterError(e)
    }
}

impl From<fmt::Error> for TamecError {
    fn from(e: fmt::Error) -> Self {
        Self::Fmt(e)
    }
}

impl Display for TamecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => Display::fmt(e, f),
            Self::XirParseError(e) => Display::fmt(e, f),
            Self::XirfParseError(e) => Display::fmt(e, f),
            Self::NirParseError(e) => Display::fmt(e, f),
            Self::XirWriterError(e) => Display::fmt(e, f),
            Self::Fmt(e) => Display::fmt(e, f),
        }
    }
}

impl Error for TamecError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::XirParseError(e) => Some(e),
            Self::XirfParseError(e) => Some(e),
            Self::NirParseError(e) => Some(e),
            Self::XirWriterError(e) => Some(e),
            Self::Fmt(e) => Some(e),
        }
    }
}

impl Diagnostic for TamecError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::XirParseError(e) => e.describe(),
            Self::XirfParseError(e) => e.describe(),
            Self::NirParseError(e) => e.describe(),

            // TODO (will fall back to rendering just the error `Display`)
            _ => vec![],
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
