// XIR errors
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

//! XIR error information.

use std::{fmt::Display, str::Utf8Error};

/// Error attempting to produce a XIR object.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Provided name contains a `':'`.
    NCColon(Vec<u8>),
    /// Provided string contains non-ASCII-whitespace characters.
    NotWhitespace(String),
    /// Provided QName is not valid.
    InvalidQName(Vec<u8>),
    // A UTF-8 error together with the byte slice that caused it.
    //
    // By storing the raw bytes instead of a string,
    //   we allow the displayer to determine how to handle invalid UTF-8
    //   encodings.
    InvalidUtf8(Utf8Error, Vec<u8>),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NCColon(bytes) => {
                write!(
                    f,
                    "NCName `{}` cannot contain ':'",
                    String::from_utf8_lossy(bytes)
                )
            }
            Self::NotWhitespace(s) => {
                write!(f, "string contains non-ASCII-whitespace: `{}`", s)
            }
            Self::InvalidQName(bytes) => {
                write!(f, "invalid QName `{}`", String::from_utf8_lossy(bytes))
            }
            Self::InvalidUtf8(inner, bytes) => {
                write!(
                    f,
                    "{} for string `{}`",
                    inner,
                    String::from_utf8_lossy(bytes)
                )
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidUtf8(err, ..) => Some(err),
            _ => None,
        }
    }
}

impl From<(Utf8Error, &[u8])> for Error {
    fn from((err, bytes): (Utf8Error, &[u8])) -> Self {
        Self::InvalidUtf8(err, bytes.to_owned())
    }
}

impl From<quick_xml::Error> for Error {
    fn from(err: quick_xml::Error) -> Self {
        // These need to be translated to provide our own errors.
        todo!("quick_xml::Error: {:?}", err)
    }
}
