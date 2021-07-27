// TAME frontends
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

//! Frontends for the TAME programming language.
//!
//! A _frontend_ represents a source language.
//! The principal frontend for TAME is the XML-based package specification
//!   language ([`XmlFrontendParser`]).
//!
//! Parsing
//! =======
//! [Parsers](parser) for frontends are expected to fulfill three primary
//!   roles:
//!
//!   1. Produce a sequence tokens from a source input (see [`Token`]);
//!   2. Perform no implicit copying of source buffer data (zero-copy); and
//!   3. Attempt recovery to continue parsing in the event of an error.
//!
//! Recovery allows the parser to find and report more errors at once,
//!   rather than requiring a developer to correct and recompile one error
//!   at a time.
//! Recovery further makes parsers suitable for static analysis in
//!   situations where correctness is non-critical,
//!     such as for linting; checkstyle; and language servers.
//!
//! Parsers are expected to be scannerless
//!   (that is, not require a separate scanning/lexing process),
//!     or to at least encapsulate lexing.
//!
//! *TODO*: Mention IR and guide reader to the next steps in the pipeline.

mod parser;
mod xml;

pub use parser::{
    ClosedByteInterval, FrontendError, FrontendEvent, FrontendParser,
    FrontendResult, Token,
};

pub use xml::XmlFrontendParser;
