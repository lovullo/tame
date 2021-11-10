// XIR reader
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

//! Parse XML files into a XIR [`Token`] stream.
//!
//! This uses [`quick_xml`] as the parser.

use super::{Error, Token, XirString};
use crate::{span::DUMMY_SPAN, sym::GlobalSymbolInternBytes, xir::Text};
use quick_xml::{
    self,
    events::{attributes::Attributes, BytesStart, Event as QuickXmlEvent},
};
use std::{collections::VecDeque, io::BufRead, result};

pub type Result<T> = result::Result<T, Error>;

/// Parse XML into a XIR [`Token`] stream.
///
/// This reader is intended to be used as an [`Iterator`].
///
/// The underlying reader produces events in chunks that are far too
///   large for XIR,
///     so most [`Token`]s retrieved via this call are buffered.
/// Parsing takes place when that buffer is exhausted and the next event
///   is requested from the underlying reader
///     (see [`XmlXirReader::refill_buf`]).
/// Errors can only occur during parsing,
///   and will never occur on buffered tokens.
///
/// [`None`] is returned only on EOF,
///   not on error.
pub struct XmlXirReader<B: BufRead> {
    /// Inner parser.
    reader: quick_xml::Reader<B>,

    /// Buffer for [`quick_xml::Reader`].
    readbuf: Vec<u8>,

    /// [`Token`] buffer populated upon receiving a new event from
    ///   `reader`.
    ///
    /// This buffer serves [`Iterator::next`] requests until it is
    ///   depleted,
    ///     after which [`XmlXirReader::refill_buf`] requests another token
    ///     from `reader`.
    tokbuf: VecDeque<Token>,
}

impl<B: BufRead> XmlXirReader<B> {
    pub fn new(reader: B) -> Self {
        let mut reader = quick_xml::Reader::from_reader(reader);

        // XIR must support mismatched tags so that we are able to represent
        //   and reconstruct malformed inputs.
        // XIRT will handle mismatch errors itself.
        reader.check_end_names(false);

        Self {
            reader,
            readbuf: Vec::new(),
            // This capacity is largely arbitrary,
            //   but [`Token`]s are small enough that it likely does not
            //   matter much.
            tokbuf: VecDeque::with_capacity(32),
        }
    }

    /// Parse using the underlying [`quick_xml::Reader`] and populate the
    ///   [`Token`] buffer.
    ///
    /// This is intended to be invoked once the buffer has been depleted by
    ///   [`XmlXirReader::next`].
    pub fn refill_buf(&mut self) -> Option<Result<Token>> {
        // Clear any previous buffer to free unneeded data.
        self.tokbuf.clear();

        match self.reader.read_event(&mut self.readbuf) {
            // This is the only time we'll consider the iterator to be done.
            Ok(QuickXmlEvent::Eof) => None,

            Err(inner) => Some(Err(inner.into())),

            Ok(ev) => match ev {
                QuickXmlEvent::Empty(ele) => Some(
                    Self::parse_element_open(&mut self.tokbuf, ele).and_then(
                        |open| {
                            // Tag is self-closing, but this does not yet
                            //   handle whitespace before the `/`.
                            self.tokbuf
                                .push_front(Token::Close(None, DUMMY_SPAN));

                            Ok(open)
                        },
                    ),
                ),

                QuickXmlEvent::Start(ele) => {
                    Some(Self::parse_element_open(&mut self.tokbuf, ele))
                }

                QuickXmlEvent::End(ele) => {
                    Some(ele.name().try_into().map_err(Error::from).and_then(
                        |qname| Ok(Token::Close(Some(qname), DUMMY_SPAN)),
                    ))
                }

                // quick_xml emits a useless text event if the first byte is
                //   a '<'.
                QuickXmlEvent::Text(bytes) if bytes.escaped().is_empty() => {
                    self.refill_buf()
                }

                // quick_xml gives us escaped bytes for CData,
                //   so handle them identically.
                // The question is whether we'll want to distinguish the two
                //   in the future to reproduce the source document on write.
                QuickXmlEvent::Text(bytes) | QuickXmlEvent::CData(bytes) => {
                    Some(bytes.intern_utf8().map_err(Error::from).and_then(
                        |text| Ok(Token::Text(Text::Escaped(text), DUMMY_SPAN)),
                    ))
                }

                // Comments are _not_ returned escaped.
                QuickXmlEvent::Comment(bytes) => Some(
                    bytes.intern_utf8().map_err(Error::from).and_then(|text| {
                        Ok(Token::Comment(Text::Unescaped(text), DUMMY_SPAN))
                    }),
                ),

                x => todo!("event: {:?}", x),
            },
        }
    }

    /// Parse opening element and its attributes into a XIR [`Token`]
    ///   stream.
    ///
    /// The opening element is returned rather than being added to the token
    ///   buffer,
    ///     since the intent is to provide that token immediately.
    fn parse_element_open(
        tokbuf: &mut VecDeque<Token>,
        ele: BytesStart,
    ) -> Result<Token> {
        ele.name()
            .try_into()
            .map_err(Error::from)
            .and_then(|qname| {
                Self::parse_attrs(tokbuf, ele.attributes())?;

                // The first token will be immediately returned
                //   via the Iterator.
                Ok(Token::Open(qname, DUMMY_SPAN))
            })
    }

    /// Parse attributes into a XIR [`Token`] stream.
    ///
    /// The order of attributes will be maintained.
    ///
    /// This does not yet handle whitespace between attributes,
    ///   or around `=`.
    fn parse_attrs<'a>(
        tokbuf: &mut VecDeque<Token>,
        mut attrs: Attributes<'a>,
    ) -> Result<()> {
        // Disable checks to allow duplicate attributes;
        //   XIR does not enforce this,
        //     because it needs to accommodate semantically invalid XML for
        //     later analysis.
        for result in attrs.with_checks(false) {
            let attr = result?;

            // The name must be parsed as a QName.
            let name = attr.key.try_into()?;

            // The attribute value,
            //   having just been read from XML,
            //   must have been escaped to be parsed properly.
            // If it parsed but it's not technically escaped according to
            //   the spec,
            //     that's okay as long as we can read it again,
            //       but we probably should still throw an error if we
            //       encounter such a situation.
            let value =
                XirString::from_escaped_raw(attr.value.as_ref())?.into();

            tokbuf.push_front(Token::AttrName(name, DUMMY_SPAN));
            tokbuf.push_front(Token::AttrValue(value, DUMMY_SPAN));
        }

        // Indicate the end of attributes even if no attributes were output.
        // This allows for a reliable delimiter that can be used without
        //   lookahead for streaming attribute parsing.
        tokbuf.push_front(Token::AttrEnd);

        Ok(())
    }
}

impl<B: BufRead> Iterator for XmlXirReader<B> {
    type Item = Result<Token>;

    /// Produce the next XIR [`Token`] from the input.
    ///
    /// For more information on how this reader operates,
    ///   see [`XmlXirReader`].
    fn next(&mut self) -> Option<Self::Item> {
        self.tokbuf
            .pop_back()
            .map(Result::Ok)
            .or_else(|| self.refill_buf())
    }
}

impl<B: BufRead> From<B> for XmlXirReader<B> {
    fn from(reader: B) -> Self {
        Self::new(reader)
    }
}

#[cfg(test)]
mod test;
