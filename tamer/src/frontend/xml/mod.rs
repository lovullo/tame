// XML frontend
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

//! XML frontend for the TAME programming language.

use super::{FrontendEvent, FrontendParser, FrontendResult};
use quick_xml::{Error as XmlError, Reader as XmlReader};
use std::fmt::Display;
use std::io::BufRead;

/// Parser for XML-based sources.
pub struct XmlFrontendParser<B>
where
    B: BufRead,
{
    _reader: XmlReader<B>,
}

impl<B> XmlFrontendParser<B>
where
    B: BufRead,
{
    pub fn new(buf_read: B) -> Self {
        let reader = XmlReader::from_reader(buf_read);

        Self { _reader: reader }
    }
}

impl<'l, B> FrontendParser<'l, XmlToken, XmlFrontendError>
    for XmlFrontendParser<B>
where
    B: BufRead,
{
    fn desc() -> &'static str {
        "XML-based package specification language"
    }

    fn parse_next(&mut self) -> XmlFrontendResult<XmlFrontendEvent<'l>> {
        Ok(FrontendEvent::Eof)
    }
}

pub type XmlFrontendEvent<'l> = FrontendEvent<'l, XmlToken, XmlFrontendError>;

pub enum XmlToken {}

#[derive(Debug)]
pub enum XmlFrontendError {
    XmlError(XmlError),
}

impl Display for XmlFrontendError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "TODO fmt")
    }
}

impl std::error::Error for XmlFrontendError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

pub type XmlFrontendResult<T> = FrontendResult<T, XmlFrontendError>;

#[cfg(test)]
mod test;
