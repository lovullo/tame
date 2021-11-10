// XIR writer
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

//! Lower XIR stream into an XML byte stream via [`Write`].

use super::{Error as XirError, QName, Token, TokenStream};
use crate::sym::GlobalSymbolResolve;
use crate::xir::{AttrValue, Text};
use std::io::{Error as IoError, Write};
use std::result;

pub type Result<T = WriterState> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Io(IoError),
    Xir(XirError),
    // TODO: No String here, since then we cannot resolve inner symbols for
    // display to the user
    UnexpectedToken(String, WriterState),
    Todo(String, WriterState),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => e.fmt(f),
            Self::Xir(e) => e.fmt(f),
            Self::UnexpectedToken(tok, state) => write!(
                f,
                "Invalid token {} at XML writer state {:?}",
                tok, state
            ),
            Self::Todo(tok, state) => write!(
                f,
                "Unexpected {} at XML writer state {:?}; TAMER intends to \
                 support this operation, but it was not yet thought to be \
                 needed.  More development is needed to support it, but \
                 hopefully very little.  However, if you hit this during \
                 normal use of TAME, then this represents a bug.",
                tok, state
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Xir(e) => Some(e),
            _ => None,
        }
    }
}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self {
        Self::Io(e)
    }
}

impl From<XirError> for Error {
    fn from(e: XirError) -> Self {
        Self::Xir(e)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriterState {
    /// A node is expected to be output next.
    NodeExpected,
    /// A node is currently being output and has not yet been closed.
    NodeOpen,
    /// Cursor is adjacent to an attribute name within an element.
    AttrNameAdjacent,
    /// Cursor is adjacent to an attribute fragment within an element.
    AttrFragmentAdjacent,
}

impl Default for WriterState {
    fn default() -> Self {
        Self::NodeExpected
    }
}

impl WriterState {
    #[inline]
    fn close_tag_if_open<W: Write>(&self, sink: &mut W) -> Result<()> {
        Ok(match *self {
            Self::NodeOpen => {
                sink.write(b">")?;
            }
            _ => (),
        })
    }
}

/// Write an XML representation.
///
/// This trait is intended for use with [XIR](super) [`Token`] stream.
/// It uses a finate state machine (FSM),
///   where states are represented by [`WriterState`],
///   to avoid lookahead requirements.
pub trait XmlWriter: Sized {
    /// Write XML representation into the provided buffer.
    ///
    /// The writer acts as a state machine to determine whether previous
    ///   output requires closing.
    /// Each write operation takes a previous [`WriterState`],
    ///   and transitions to a new [`WriterState`] after performing the
    ///   write operation
    ///     (which may be the same as the previous state).
    /// This returned state must be provided to the next `write` operation
    ///   to produce valid output.
    ///
    /// If you have a series of writes to perform,
    ///   consider using an [`Iterator`] implementing [`XmlWriter`].
    #[must_use = "Write operation may fail"]
    fn write<W: Write>(self, sink: &mut W, prev_state: WriterState) -> Result;

    /// Allocate a new buffer and write into it,
    ///   returning both the new buffer and the writer state.
    ///
    /// See [`write`](XmlWriter::write) for more information.
    ///
    /// This is intended primarily for testing;
    ///   it is recommended that you use [`write`](XmlWriter::write) instead,
    ///     unless you _really_ need a new owned `Vec<u8>`.
    #[must_use]
    fn write_new(
        self,
        prev_state: WriterState,
    ) -> Result<(Vec<u8>, WriterState)> {
        let mut buf = Vec::<u8>::new();
        let state = self.write(&mut buf, prev_state)?;

        Ok((buf, state))
    }
}

impl XmlWriter for QName {
    #[inline]
    fn write<W: Write>(self, sink: &mut W, prev_state: WriterState) -> Result {
        if let Some(prefix) = self.prefix() {
            sink.write(prefix.lookup_str().as_bytes())?;
            sink.write(b":")?;
        }

        sink.write(self.local_name().lookup_str().as_bytes())?;
        Ok(prev_state)
    }
}

impl XmlWriter for Token {
    fn write<W: Write>(self, sink: &mut W, prev_state: WriterState) -> Result {
        type S = WriterState; // More concise

        match (self, prev_state) {
            (Self::Open(name, _), S::NodeExpected | S::NodeOpen) => {
                // If a node is still open, then we are a child.
                prev_state.close_tag_if_open(sink)?;
                sink.write(b"<")?;
                name.write(sink, prev_state)?;

                Ok(S::NodeOpen)
            }

            (Self::Close(None, _), S::NodeOpen) => {
                sink.write(b"/>")?;

                Ok(S::NodeExpected)
            }

            (Self::Close(Some(name), _), S::NodeExpected | S::NodeOpen) => {
                // If open, we're going to produce an element of the form
                // `<foo></foo>`.
                prev_state.close_tag_if_open(sink)?;

                sink.write(b"</")?;
                name.write(sink, prev_state)?;
                sink.write(b">")?;

                Ok(S::NodeExpected)
            }

            (Self::AttrName(name, _), S::NodeOpen) => {
                sink.write(b" ")?;
                name.write(sink, prev_state)?;

                Ok(S::AttrNameAdjacent)
            }

            (Self::AttrValue(AttrValue(value), _), S::AttrNameAdjacent) => {
                sink.write(b"=\"")?;
                sink.write(value.into_escaped().lookup_str().as_bytes())?;
                sink.write(b"\"")?;

                Ok(S::NodeOpen)
            }

            (Self::AttrValue(AttrValue(value), _), S::AttrFragmentAdjacent) => {
                sink.write(value.into_escaped().lookup_str().as_bytes())?;
                sink.write(b"\"")?;

                Ok(S::NodeOpen)
            }

            (
                Self::AttrValueFragment(AttrValue(value), _),
                S::AttrNameAdjacent,
            ) => {
                sink.write(b"=\"")?;
                sink.write(value.into_escaped().lookup_str().as_bytes())?;

                Ok(S::AttrFragmentAdjacent)
            }

            (
                Self::AttrValueFragment(AttrValue(value), _),
                S::AttrFragmentAdjacent,
            ) => {
                sink.write(value.into_escaped().lookup_str().as_bytes())?;

                Ok(S::AttrFragmentAdjacent)
            }

            // AttrEnd is ignored by the writer (and is optional).
            (Self::AttrEnd, x) => Ok(x),

            // Unescaped not yet supported, but you could use CData.
            (
                Self::Text(Text::Escaped(text), _),
                S::NodeExpected | S::NodeOpen,
            ) => {
                prev_state.close_tag_if_open(sink)?;
                sink.write(text.lookup_str().as_bytes())?;

                Ok(S::NodeExpected)
            }

            // Escaped not yet supported, but you could use Text.
            (
                Self::CData(Text::Unescaped(text), _),
                S::NodeExpected | S::NodeOpen,
            ) => {
                prev_state.close_tag_if_open(sink)?;
                sink.write(b"<![CDATA[")?;
                sink.write(text.lookup_str().as_bytes())?;
                sink.write(b"]]>")?;

                Ok(S::NodeExpected)
            }

            // Unescaped not yet supported, since we do not have a use case.
            (
                Self::Comment(Text::Escaped(comment), _),
                S::NodeExpected | S::NodeOpen,
            ) => {
                prev_state.close_tag_if_open(sink)?;
                sink.write(b"<!--")?;
                sink.write(comment.lookup_str().as_bytes())?;
                sink.write(b"-->")?;

                Ok(S::NodeExpected)
            }

            (Self::Whitespace(ws, _), S::NodeOpen) => {
                sink.write(ws.lookup_str().as_bytes())?;

                Ok(S::NodeOpen)
            }

            // As-of-yet unsupported operations that weren't needed at the
            // time of writing, but were planned for in the design of Xir.
            (invalid @ Self::AttrName(_, _), S::AttrNameAdjacent)
            | (invalid @ Self::Text(Text::Unescaped(_), _), S::NodeExpected)
            | (invalid @ Self::CData(Text::Escaped(_), _), S::NodeExpected) => {
                Err(Error::Todo(format!("{:?}", invalid), prev_state))
            }

            // Everything else represents either an invalid state transition
            // that would produce invalid XML, or something that we forgot
            // to account for in the above error.
            (invalid, _) => Err(Error::UnexpectedToken(
                format!("{:?}", invalid),
                prev_state,
            )),
        }
    }
}

impl<I: TokenStream> XmlWriter for I {
    fn write<W: Write>(
        mut self,
        sink: &mut W,
        initial_state: WriterState,
    ) -> Result {
        self.try_fold(initial_state, |prev_state, tok| {
            tok.write(sink, prev_state)
        })
    }
}

#[cfg(test)]
mod test {
    use std::convert::{TryFrom, TryInto};

    use super::*;
    use crate::{
        span::Span,
        sym::GlobalSymbolIntern,
        xir::{AttrValue, QName, Text, Whitespace},
    };

    type TestResult = std::result::Result<(), Error>;

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case".intern());
    }

    #[test]
    fn writes_beginning_node_tag_without_prefix() -> TestResult {
        let name = QName::new_local("no-prefix".try_into()?);
        let result = Token::Open(name, *S).write_new(Default::default())?;

        assert_eq!(result.0, b"<no-prefix");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_beginning_node_tag_with_prefix() -> TestResult {
        let name = QName::try_from(("prefix", "element-name"))?;
        let result = Token::Open(name, *S).write_new(Default::default())?;

        assert_eq!(result.0, b"<prefix:element-name");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn closes_open_node_when_opening_another() -> TestResult {
        let name = QName::try_from(("p", "another-element"))?;
        let result = Token::Open(name, *S).write_new(WriterState::NodeOpen)?;

        assert_eq!(result.0, b"><p:another-element");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn closes_open_node_as_empty_element() -> TestResult {
        let result = Token::Close(None, *S).write_new(WriterState::NodeOpen)?;

        assert_eq!(result.0, b"/>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn closing_tag_when_node_expected() -> TestResult {
        let name = QName::try_from(("a", "closed-element"))?;

        let result = Token::Close(Some(name), *S)
            .write_new(WriterState::NodeExpected)?;

        assert_eq!(result.0, b"</a:closed-element>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    // Does _not_ check that it's balanced.  Not our job.  In fact, we want
    // to explicitly support outputting malformed XML.
    #[test]
    fn closes_open_node_with_closing_tag() -> TestResult {
        let name = QName::try_from(("b", "closed-element"))?;

        let result =
            Token::Close(Some(name), *S).write_new(WriterState::NodeOpen)?;

        assert_eq!(result.0, b"></b:closed-element>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    // Intended for alignment of attributes, primarily.
    #[test]
    fn whitespace_within_open_node() -> TestResult {
        let result = Token::Whitespace(Whitespace::try_from(" \t ")?, *S)
            .write_new(WriterState::NodeOpen)?;

        assert_eq!(result.0, b" \t ");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_attr_name_to_open_node() -> TestResult {
        let name_ns = QName::try_from(("some", "attr"))?;
        let name_local = QName::new_local("nons".try_into()?);

        // Namespace prefix
        let result =
            Token::AttrName(name_ns, *S).write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b" some:attr");
        assert_eq!(result.1, WriterState::AttrNameAdjacent);

        // No namespace prefix
        let result =
            Token::AttrName(name_local, *S).write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b" nons");
        assert_eq!(result.1, WriterState::AttrNameAdjacent);

        Ok(())
    }

    #[test]
    fn writes_attr_value_when_adjacent_to_attr() -> TestResult {
        let value = AttrValue::from("test str".intern());

        let result = Token::AttrValue(value, *S)
            .write_new(WriterState::AttrNameAdjacent)?;

        assert_eq!(result.0, br#"="test str""#);
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_attr_value_consisting_of_fragments() -> TestResult {
        let value_left = AttrValue::from("left ".intern());
        let value_right = AttrValue::from("right".intern());

        let result = vec![
            Token::AttrValueFragment(value_left, *S),
            Token::AttrValue(value_right, *S),
        ]
        .into_iter()
        .write_new(WriterState::AttrNameAdjacent)?;

        assert_eq!(result.0, br#"="left right""#);
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    // AttrEnd does not even need to be in a semantically valid position; we
    // just ignore it entirely.
    #[test]
    fn ignores_attr_end() -> TestResult {
        let result = Token::AttrEnd.write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b"");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_escaped_text() -> TestResult {
        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let text = Text::Escaped("test > escaped".intern());

        // When a node is expected.
        let result =
            Token::Text(text, *S).write_new(WriterState::NodeExpected)?;
        assert_eq!(result.0, b"test > escaped");
        assert_eq!(result.1, WriterState::NodeExpected);

        // When a node is still open.
        let result = Token::Text(text, *S).write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b">test > escaped");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn writes_unescaped_data() -> TestResult {
        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let text = Text::Unescaped("test > unescaped".intern());

        // When a node is expected.
        let result =
            Token::CData(text, *S).write_new(WriterState::NodeExpected)?;
        assert_eq!(result.0, b"<![CDATA[test > unescaped]]>");
        assert_eq!(result.1, WriterState::NodeExpected);

        // When a node is still open.
        let result = Token::CData(text, *S).write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b"><![CDATA[test > unescaped]]>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn writes_escaped_comment() -> TestResult {
        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let comment = Text::Escaped("comment > escaped".intern());

        // When a node is expected.
        let result =
            Token::Comment(comment, *S).write_new(WriterState::NodeExpected)?;
        assert_eq!(result.0, b"<!--comment > escaped-->");
        assert_eq!(result.1, WriterState::NodeExpected);

        // When a node is still open.
        let result =
            Token::Comment(comment, *S).write_new(WriterState::NodeOpen)?;
        assert_eq!(result.0, b"><!--comment > escaped-->");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn unsupported_transition_results_in_error() -> TestResult {
        assert!(matches!(
            Token::AttrValue(AttrValue::from("".intern()), *S)
                .write(&mut vec![], WriterState::NodeExpected),
            Err(Error::UnexpectedToken(_, WriterState::NodeExpected)),
        ));

        Ok(())
    }

    // The cherry on top, just to demonstrate how this will be used in
    // practice.
    #[test]
    fn test_valid_sequence_of_tokens() -> TestResult {
        let root: QName = ("r", "root").try_into()?;

        let result = vec![
            Token::Open(root, *S),
            Token::AttrName(("an", "attr").try_into()?, *S),
            Token::AttrValue(AttrValue::from("value".intern()), *S),
            Token::AttrEnd,
            Token::Text(Text::Escaped("text".intern()), *S),
            Token::Open(("c", "child").try_into()?, *S),
            Token::Whitespace(" ".try_into()?, *S),
            Token::Close(None, *S),
            Token::Close(Some(root), *S),
        ]
        .into_iter()
        .write_new(Default::default())?;

        assert_eq!(
            result.0,
            br#"<r:root an:attr="value">text<c:child /></r:root>"#
        );
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }
}
