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

use super::{error::SpanlessError, DefaultEscaper, Error, Escaper, Token};
use crate::{
    span::Context,
    sym::{st::raw::WS_EMPTY, GlobalSymbolInternBytes},
};
use quick_xml::{
    self,
    events::{
        attributes::Attributes, BytesDecl, BytesStart, Event as QuickXmlEvent,
    },
    Error as QuickXmlError,
};
use std::{borrow::Cow, collections::VecDeque, io::BufRead, result};

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
pub struct XmlXirReader<'s, B, S = DefaultEscaper>
where
    B: BufRead,
    S: Escaper,
{
    /// Inner parser.
    reader: quick_xml::Reader<B>,

    /// Parsing context for reader.
    ctx: Context,

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

    /// System for unescaping string data.
    escaper: &'s S,
}

impl<'s, B: BufRead, S: Escaper> XmlXirReader<'s, B, S> {
    pub fn new(reader: B, escaper: &'s S, ctx: Context) -> Self {
        let mut reader = quick_xml::Reader::from_reader(reader);

        // XIR must support mismatched tags so that we are able to represent
        //   and reconstruct malformed inputs.
        // XIRT will handle mismatch errors itself.
        reader.check_end_names(false);

        Self {
            reader,
            ctx,
            readbuf: Vec::new(),
            // This capacity is largely arbitrary,
            //   but [`Token`]s are small enough that it likely does not
            //   matter much.
            tokbuf: VecDeque::with_capacity(32),

            escaper,
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
        self.readbuf.clear();

        let ctx = self.ctx;
        let prev_pos = self.reader.buffer_position();

        match self.reader.read_event(&mut self.readbuf) {
            // TODO: To provide better spans and error messages,
            //   we need to map specific types of errors.
            // But we don't encounter much of anything here with how we make
            //   use of quick-xml.
            Err(inner) => Some(Err({
                let span = ctx.span_or_zz(prev_pos, 0);
                SpanlessError::from(inner).with_span(span)
            })),

            Ok(ev) => match ev {
                // This is the only time we'll consider the iterator to be
                //   done.
                QuickXmlEvent::Eof => None,

                QuickXmlEvent::Empty(ele) => Some(
                    Self::parse_element_open(
                        &self.escaper,
                        &mut self.tokbuf,
                        ele,
                        prev_pos,
                        ctx,
                    )
                    .and_then(|open| {
                        let new_pos = self.reader.buffer_position();

                        // `<tag ... />`
                        //           ||
                        let span = ctx.span_or_zz(new_pos - 2, 2);

                        // Tag is self-closing, but this does not yet
                        //   handle whitespace before the `/`
                        //     (as indicated in the span above).
                        self.tokbuf.push_front(Token::Close(None, span));

                        Ok(open)
                    }),
                ),

                QuickXmlEvent::Start(ele) => Some(Self::parse_element_open(
                    &self.escaper,
                    &mut self.tokbuf,
                    ele,
                    prev_pos,
                    ctx,
                )),

                QuickXmlEvent::End(ele) => Some({
                    // </foo>
                    // |----|  name + '<' + '/' + '>'
                    let span = ctx.span_or_zz(prev_pos, ele.name().len() + 3);

                    ele.name()
                        .try_into()
                        .map_err(Error::from_with_span(span))
                        .and_then(|qname| Ok(Token::Close(Some(qname), span)))
                }),

                // quick_xml emits a useless text event if the first byte is
                //   a '<'.
                QuickXmlEvent::Text(bytes) if bytes.escaped().is_empty() => {
                    self.refill_buf()
                }

                // quick_xml _escapes_ the unescaped CData before handing it
                //   off to us,
                //     which is a complete waste since we'd just have to
                //     unescape it again.
                QuickXmlEvent::CData(bytes) => todo!("CData: {:?}", bytes),

                QuickXmlEvent::Text(bytes) => Some({
                    // <text>foo bar</text>
                    //       |-----|
                    let span = ctx.span_or_zz(prev_pos, bytes.len());

                    bytes
                        .intern_utf8()
                        .map_err(Into::into)
                        .and_then(|sym| self.escaper.unescape(sym))
                        .map_err(Error::from_with_span(span))
                        .and_then(|unesc| Ok(Token::Text(unesc, span)))
                }),

                // Comments are _not_ returned escaped.
                QuickXmlEvent::Comment(bytes) => Some({
                    // <!-- foo -->
                    // |----------|  " foo " + "<!--" + "-->"
                    let span = ctx.span_or_zz(prev_pos, bytes.len() + 7);

                    bytes
                        .intern_utf8()
                        .map_err(Error::from_with_span(span))
                        .and_then(|comment| Ok(Token::Comment(comment, span)))
                }),

                // TODO: This must appear in the Prolog.
                QuickXmlEvent::Decl(decl) => {
                    match Self::validate_decl(&decl, prev_pos, ctx) {
                        Err(x) => Some(Err(x)),
                        Ok(()) => self.refill_buf(),
                    }
                }

                // We do not support processor instructions or doctypes.
                // TODO: Convert this into an error/warning?
                // Previously `xml-stylesheet` was present in some older
                //   source files and may linger for a bit after cleanup.
                QuickXmlEvent::PI(..) | QuickXmlEvent::DocType(..) => {
                    self.refill_buf()
                }
            },
        }
    }

    /// Validate an that an XML declaration contains expected values.
    ///
    /// A declaration looks like `<?xml version="1.0" encoding="utf-8"?>`,
    ///   where `@encoding` is optional but `@version` is not.
    /// It may also contain `@standalone`,
    ///   but we do not check for that.
    ///
    /// We expect version 1.0 and UTF-8 encoding.
    /// Failing when these expectations are voilated helps to ensure that
    ///   people unfamiliar with the system do not have expectations that
    ///   are going to be unmet,
    ///     which may result in subtle (or even serious) problems.
    fn validate_decl(decl: &BytesDecl, pos: usize, ctx: Context) -> Result<()> {
        // Starts after `<?`, which we want to include.
        let decl_ptr = decl.as_ptr() as usize - 2 + pos;

        // Fallback span that covers the entire declaration.
        let decl_span = ctx.span_or_zz(pos, decl.len() + 4);

        let ver =
            &decl.version().map_err(Error::from_with_span(decl_span))?[..];

        // NB: `quick-xml` docs state that `version` returns the quotes,
        //   but it does not.
        if ver != b"1.0" {
            // <?xml version="X.Y"?>
            //                |-|
            let ver_pos = (ver.as_ptr() as usize) - decl_ptr;
            let span = ctx.span_or_zz(ver_pos, ver.len());

            Err(Error::UnsupportedXmlVersion(
                ver.intern_utf8().map_err(Error::from_with_span(span))?,
                span,
            ))?
        }

        if let Some(enc) = decl.encoding() {
            match &enc.map_err(Error::from_with_span(decl_span))?[..] {
                b"utf-8" | b"UTF-8" => (),
                invalid => {
                    let enc_pos = (invalid.as_ptr() as usize) - decl_ptr;
                    let span = ctx.span_or_zz(enc_pos, invalid.len());

                    Err(Error::UnsupportedEncoding(
                        invalid
                            .intern_utf8()
                            .map_err(Error::from_with_span(span))?,
                        span,
                    ))?
                }
            }
        }

        Ok(())
    }

    /// Parse opening element and its attributes into a XIR [`Token`]
    ///   stream.
    ///
    /// The opening element is returned rather than being added to the token
    ///   buffer,
    ///     since the intent is to provide that token immediately.
    fn parse_element_open(
        escaper: &'s S,
        tokbuf: &mut VecDeque<Token>,
        ele: BytesStart,
        pos: usize,
        ctx: Context,
    ) -> Result<Token> {
        // Starts after the opening tag `<`, so adjust.
        let addr = ele.as_ptr() as usize - 1;
        let len = ele.name().len();

        match ele.name().last() {
            None => {
                // TODO: QName should be self-validating.  Move this.
                return Err(Error::InvalidQName(
                    WS_EMPTY,
                    // <>
                    //  |  where QName should be
                    ctx.span_or_zz(pos + 1, 0),
                ));
            }

            // Quick-and-dirty guess as to whether they may have missed the
            //   element name and included an attribute instead,
            //     which quick-xml does not check for.
            Some(b'"' | b'\'') => {
                return Err({
                    // <foo="bar" ...>
                    //  |-------|
                    let span = ctx.span_or_zz(pos + 1, len);

                    Error::InvalidQName(
                        ele.name()
                            .intern_utf8()
                            .map_err(Error::from_with_span(span))?,
                        span,
                    )
                });
            }

            _ => (),
        };

        // `ele` contains every byte up to the [self-]closing tag.
        ele.name()
            .try_into()
            .map_err(Error::from_with_span(ctx.span_or_zz(pos + 1, len)))
            .and_then(|qname| {
                let has_attrs = ele.attributes_raw().len() > 0;
                let noattr_add: usize = (!has_attrs).into();

                // <tag ... />
                // |--|  name + '<'
                //
                // <tag>..</tag>
                // |---| name + '<' + '>'
                let span = ctx.span_or_zz(pos, len + 1 + noattr_add);

                if has_attrs {
                    let found = Self::parse_attrs(
                        escaper,
                        tokbuf,
                        ele.attributes(),
                        addr - pos, // offset relative to _beginning_ of buf
                        pos,
                        ctx,
                    )?;

                    // Given this input, quick-xml ignores the bytes entirely:
                    //   <foo bar>
                    //        ^^^| missing `="value"`
                    //
                    // The whitespace check is to handle input like this:
                    //   <foo />
                    //       ^ whitespace making `attributes_raw().len` > 0
                    if !found
                        && ele
                            .attributes_raw()
                            .iter()
                            .find(|b| !Self::is_whitespace(**b))
                            .is_some()
                    {
                        return Err(Error::AttrValueExpected(
                            None,
                            ctx.span_or_zz(pos + ele.len() + 1, 0),
                        ));
                    }
                }

                // The first token will be immediately returned
                //   via the Iterator.
                Ok(Token::Open(qname, span))
            })
    }

    /// quick-xml's whitespace predicate.
    fn is_whitespace(b: u8) -> bool {
        match b {
            b' ' | b'\r' | b'\n' | b'\t' => true,
            _ => false,
        }
    }

    /// Parse attributes into a XIR [`Token`] stream.
    ///
    /// The order of attributes will be maintained.
    ///
    /// This does not yet handle whitespace between attributes,
    ///   or around `=`.
    ///
    /// Note About Pointer Arithmetic
    /// =============================
    /// `ele_ptr` is expected to be a pointer to the buffer containing the
    ///   bytes read from the source file.
    /// Attributes reference this buffer,
    ///   so we can use pointer arithmetic to determine the offset within
    ///   the buffer relative to the node.
    /// This works because the underlying buffer is a `Vec`,
    ///   which is contiguous in memory.
    ///
    /// However, since this is a `Vec`,
    ///   it is important that the address be retrieved _after_ quick-xml
    ///   read events,
    ///     otherwise the buffer may be expanded and will be reallocated.
    fn parse_attrs<'a>(
        escaper: &'s S,
        tokbuf: &mut VecDeque<Token>,
        mut attrs: Attributes<'a>,
        ele_ptr: usize,
        ele_pos: usize,
        ctx: Context,
    ) -> Result<bool> {
        let mut found = false;

        // Disable checks to allow duplicate attributes;
        //   XIR does not enforce this,
        //     because it needs to accommodate semantically invalid XML for
        //     later analysis.
        for result in attrs.with_checks(false) {
            found = true;

            let attr = result.map_err(|e| match e {
                QuickXmlError::NoEqAfterName(pos) => {
                    // TODO: quick-xml doesn't give us the name,
                    //   but we should discover it.
                    Error::AttrValueExpected(
                        None,
                        ctx.span_or_zz(ele_pos + pos, 0),
                    )
                }

                QuickXmlError::UnquotedValue(pos) => {
                    // TODO: name and span length
                    Error::AttrValueUnquoted(
                        None,
                        ctx.span_or_zz(ele_pos + pos, 0),
                    )
                }

                // fallback
                e => Error::from_with_span(ctx.span_or_zz(ele_pos, 0))(e),
            })?;

            let keyoffset = attr.key.as_ptr() as usize;
            let name_offset = keyoffset - ele_ptr;

            // Accommodates zero-length values (e.g. `key=""`) with a
            //   zero-length span at the location the value _would_ be.
            let valoffset = match attr.value {
                Cow::Borrowed(b) => b.as_ptr() as usize,

                // This should never happen since we have a reference to the
                //   underlying buffer.
                Cow::Owned(_) => unreachable!(
                    "internal error: unexpected owned attribute value"
                ),
            };

            let value_offset = valoffset - ele_ptr;

            let span_name = ctx.span_or_zz(name_offset, attr.key.len());
            let span_value = ctx.span_or_zz(value_offset, attr.value.len());

            // The name must be parsed as a QName.
            let name = attr
                .key
                .try_into()
                .map_err(Error::from_with_span(span_name))?;

            // The attribute value,
            //   having just been read from XML,
            //   must have been escaped to be parsed properly.
            // If it parsed but it's not technically escaped according to
            //   the spec,
            //     that's okay as long as we can read it again,
            //       but we probably should still throw an error if we
            //       encounter such a situation.
            let value = escaper
                .unescape(
                    attr.value
                        .as_ref()
                        .intern_utf8()
                        .map_err(Error::from_with_span(span_value))?,
                )
                .map_err(Error::from_with_span(span_value))?
                .into();

            tokbuf.push_front(Token::AttrName(name, span_name));
            tokbuf.push_front(Token::AttrValue(value, span_value));
        }

        Ok(found)
    }
}

impl<'s, B, S> Iterator for XmlXirReader<'s, B, S>
where
    B: BufRead,
    S: Escaper,
{
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

#[cfg(test)]
mod test;
