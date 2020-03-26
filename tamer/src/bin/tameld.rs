// TAME linker
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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

//! This is the TAME linker, so named after the traditional `ld` Unix
//! utility.  Its job is to take each of the compiled object files and
//! produce a final executable.
//!
//! For more information about the linker,
//!   see the [`tamer::ld`] module.

extern crate tamer;

use getopts::{Fail, Options};
use std::env;
use std::error::Error;
use tamer::ld::poc;

/// Types of commands
enum Command {
    Link(String, String),
    Usage,
}

/// Entrypoint for the linker
pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let program = &args[0];
    let opts = get_opts();
    let usage = opts.usage(&format!("Usage: {} -o OUTPUT FILE", program));

    match parse_options(opts, args) {
        Ok(Command::Link(input, output)) => match poc::main(&input, &output) {
            Err(e) => {
                eprintln!("error: {}", e);
                eprintln!("fatal: failed to link `{}`", output);

                std::process::exit(1);
            }
            ok => ok,
        },
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
        0 => return Err(Fail::OptionMissing(String::from("FILE"))),
        1 => matches.free[0].clone(),
        _ => return Err(Fail::UnrecognizedOption(matches.free[1].clone())),
    };

    let output = match matches.opt_str("o") {
        Some(m) => m,
        None => {
            return Err(Fail::OptionMissing(String::from("-o OUTPUT")));
        }
    };

    Ok(Command::Link(input, output))
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
                assert_eq!("FILE", message);
            }
            _ => panic!("Missing input not caught"),
        }
    }

    #[test]
    fn parse_options_missing_output() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![String::from("program"), String::from("foo")],
        );

        match result {
            Err(Fail::OptionMissing(message)) => {
                assert_eq!("-o OUTPUT", message);
            }
            _ => panic!("Missing output not caught"),
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
                String::from("-o"),
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
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo"),
                String::from("-o"),
                String::from("bar"),
            ],
        );

        match result {
            Ok(Command::Link(infile, outfile)) => {
                assert_eq!("foo", infile);
                assert_eq!("bar", outfile);
            }
            _ => panic!("Unexpected result"),
        }
    }

    #[test]
    fn parse_options_valid_long() {
        let opts = get_opts();
        let result = parse_options(
            opts,
            vec![
                String::from("program"),
                String::from("foo"),
                String::from("--output"),
                String::from("bar"),
            ],
        );

        match result {
            Ok(Command::Link(infile, outfile)) => {
                assert_eq!("foo", infile);
                assert_eq!("bar", outfile);
            }
            _ => panic!("Unexpected result"),
        }
    }
}
