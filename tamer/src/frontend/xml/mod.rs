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

use super::{
    ClosedByteInterval, FrontendError, FrontendEvent, FrontendParser,
    FrontendResult, Token,
};
use crate::tpwrap::quick_xml::Error as XmlError;
use quick_xml::events::Event as XmlEvent;
use quick_xml::Reader as XmlReader;
use std::fmt::Display;
use std::io::BufRead;

/// Parser for XML-based sources.
pub struct XmlFrontendParser<B>
where
    B: BufRead,
{
    /// XML parser.
    reader: XmlReader<B>,

    /// Buffer for all XML data besides namespaces.
    buf: Vec<u8>,

    /// Buffer for namespace data.
    nsbuf: Vec<u8>,
}

impl<B> XmlFrontendParser<B>
where
    B: BufRead,
{
    pub fn new(buf_read: B) -> Self {
        let reader = XmlReader::from_reader(buf_read);

        Self {
            reader,
            buf: Vec::new(),
            nsbuf: Vec::new(),
        }
    }

    /// Calculate the closed byte interval representing the bytes associated
    ///   with a given [`XmlEvent`].
    fn calc_interval(
        pos_start: usize,
        pos_cur: usize,
        ev: &XmlEvent,
    ) -> ClosedByteInterval {
        match ev {
            XmlEvent::Empty(_) => ClosedByteInterval(pos_start, pos_cur - 1),

            _ => ClosedByteInterval(pos_start, pos_start),
        }
    }
}

impl<'l, B> FrontendParser<'l, XmlToken<'l>, XmlFrontendError>
    for XmlFrontendParser<B>
where
    B: BufRead,
{
    fn desc() -> &'static str {
        "XML-based package specification language"
    }

    fn parse_next(&'l mut self) -> XmlFrontendResult<XmlFrontendEvent<'l>> {
        let reader = &mut self.reader;
        let pos_start = reader.buffer_position();

        reader
            .read_namespaced_event(&mut self.buf, &mut self.nsbuf)
            .map(|(ns, ev)| match ev {
                XmlEvent::Eof => FrontendEvent::Eof,
                _ => {
                    let interval = Some(Self::calc_interval(
                        pos_start,
                        reader.buffer_position(),
                        &ev,
                    ));

                    FrontendEvent::Token(Token {
                        kind: XmlToken::RawXmlEvent((ns, ev)),
                        lexeme: None,
                        interval,
                    })
                }
            })
            .map_err(|e| FrontendError::UnrecoverableError {
                source: XmlFrontendError::XmlError(e.into()),
                interval: ClosedByteInterval(
                    pos_start,
                    reader.buffer_position(),
                ),
            })
    }
}

pub type XmlFrontendEvent<'l> =
    FrontendEvent<'l, XmlToken<'l>, XmlFrontendError>;

type Namespace<'a> = &'a [u8];
type NamespacedXmlEvent<'a> = (Option<Namespace<'a>>, XmlEvent<'a>);

#[derive(Debug)]
pub enum XmlToken<'l> {
    RawXmlEvent(NamespacedXmlEvent<'l>),
}

#[derive(Debug, PartialEq)]
pub enum XmlFrontendError {
    XmlError(XmlError),
}

impl Display for XmlFrontendError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::XmlError(e) => e.fmt(fmt),
        }
    }
}

impl std::error::Error for XmlFrontendError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::XmlError(e) => Some(e),
        }
    }
}

impl<E: Into<XmlError>> From<E> for XmlFrontendError {
    fn from(err: E) -> Self {
        Self::XmlError(err.into())
    }
}

pub type XmlFrontendResult<T> = FrontendResult<T, XmlFrontendError>;

#[cfg(test)]
mod test;
