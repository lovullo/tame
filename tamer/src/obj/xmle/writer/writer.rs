// xmle object file writer
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

use crate::ir::asg::Sections;
use crate::sym::Symbol;
use quick_xml::Error as XmlError;
use std::io::{Error as IoError, Write};
use std::result;
use std::str::Utf8Error;

pub type Result<T = ()> = result::Result<T, WriterError>;

/// A wrapper around a `Write` object
///
/// This is used to take the [`Sections`] and write out the xmle files.
pub trait Writer<W: Write> {
    fn write(
        &mut self,
        sections: &Sections,
        name: Symbol,
        relroot: &str,
    ) -> Result<()>
    where
        Self: Sized;
}

/// Error implementations for the writer
#[derive(Debug)]
pub enum WriterError {
    /// Propagated IO error
    Io(IoError),
    /// Propagated UTF8 error
    Utf8(Utf8Error),
    /// Propagated XML error
    XmlError(XmlError),
    /// Something other than a fragment was given when a fragment was expected
    ExpectedFragment(String),
}

impl std::fmt::Display for WriterError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Io(inner) => inner.fmt(fmt),
            Self::Utf8(inner) => inner.fmt(fmt),
            Self::XmlError(inner) => inner.fmt(fmt),
            Self::ExpectedFragment(msg) => {
                write!(fmt, "expected fragment: {}", msg)
            }
        }
    }
}

impl std::error::Error for WriterError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl From<IoError> for WriterError {
    fn from(err: IoError) -> Self {
        WriterError::Io(err)
    }
}

impl From<Utf8Error> for WriterError {
    fn from(err: Utf8Error) -> Self {
        WriterError::Utf8(err)
    }
}

impl From<XmlError> for WriterError {
    fn from(err: XmlError) -> Self {
        WriterError::XmlError(err)
    }
}
