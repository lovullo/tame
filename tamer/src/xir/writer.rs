// XIR writer
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

//! Lower XIR stream into an XML byte stream via [`Write`].

use super::{DefaultEscaper, Escaper};
use super::{Error as XirError, QName, Token, TokenStream};
use crate::sym::GlobalSymbolResolve;
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
            Self::NodeOpen => sink.write_all(b">")?,
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
pub trait XmlWriter<S = DefaultEscaper>
where
    S: Escaper,
    Self: Sized,
{
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
    fn write<W: Write>(
        self,
        sink: &mut W,
        prev_state: WriterState,
        escaper: &S,
    ) -> Result;

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
        escaper: &S,
    ) -> Result<(Vec<u8>, WriterState)> {
        let mut buf = Vec::<u8>::new();
        let state = self.write(&mut buf, prev_state, escaper)?;

        Ok((buf, state))
    }
}

impl<S: Escaper> XmlWriter<S> for QName {
    #[inline]
    fn write<W: Write>(
        self,
        sink: &mut W,
        prev_state: WriterState,
        _escaper: &S,
    ) -> Result {
        if let Some(prefix) = self.prefix() {
            sink.write_all(prefix.lookup_str().as_bytes())?;
            sink.write_all(b":")?;
        }

        sink.write_all(self.local_name().lookup_str().as_bytes())?;
        Ok(prev_state)
    }
}

impl<S: Escaper> XmlWriter<S> for &Token {
    fn write<W: Write>(
        self,
        sink: &mut W,
        prev_state: WriterState,
        escaper: &S,
    ) -> Result {
        type W = WriterState; // More concise

        use Token::*;
        match (self, prev_state) {
            (Open(name, _), W::NodeExpected | W::NodeOpen) => {
                // If a node is still open, then we are a child.
                prev_state.close_tag_if_open(sink)?;
                sink.write_all(b"<")?;
                name.write(sink, prev_state, escaper)?;

                Ok(W::NodeOpen)
            }

            (Close(None, _), W::NodeOpen) => {
                sink.write_all(b"/>")?;

                Ok(W::NodeExpected)
            }

            (Close(Some(name), _), W::NodeExpected | W::NodeOpen) => {
                // If open, we're going to produce an element of the form
                // `<foo></foo>`.
                prev_state.close_tag_if_open(sink)?;

                sink.write_all(b"</")?;
                name.write(sink, prev_state, escaper)?;
                sink.write_all(b">")?;

                Ok(W::NodeExpected)
            }

            (AttrName(name, _), W::NodeOpen) => {
                sink.write_all(b" ")?;
                name.write(sink, prev_state, escaper)?;

                Ok(W::AttrNameAdjacent)
            }

            (AttrValue(value, _), W::AttrNameAdjacent) => {
                sink.write_all(b"=\"")?;
                sink.write_all(escaper.escape(*value).lookup_str().as_bytes())?;
                sink.write_all(b"\"")?;

                Ok(W::NodeOpen)
            }

            (AttrValue(value, _), W::AttrFragmentAdjacent) => {
                sink.write_all(escaper.escape(*value).lookup_str().as_bytes())?;
                sink.write_all(b"\"")?;

                Ok(W::NodeOpen)
            }

            (AttrValueFragment(value, _), W::AttrNameAdjacent) => {
                sink.write_all(b"=\"")?;
                sink.write_all(escaper.escape(*value).lookup_str().as_bytes())?;

                Ok(W::AttrFragmentAdjacent)
            }

            (AttrValueFragment(value, _), W::AttrFragmentAdjacent) => {
                sink.write_all(escaper.escape(*value).lookup_str().as_bytes())?;

                Ok(W::AttrFragmentAdjacent)
            }

            // TODO: We have no way of knowing if text should be formatted
            //   as CData,
            //     which may also be beneficial to avoid escaping if we
            //     haven't yet encountered the unescaped representation.
            (Text(text, _), W::NodeExpected | W::NodeOpen) => {
                prev_state.close_tag_if_open(sink)?;
                sink.write_all(escaper.escape(*text).lookup_str().as_bytes())?;

                Ok(W::NodeExpected)
            }

            // XXX: While we currently only output comments that have been
            //   _read_ as comments,
            //     that will not always be the case and we must escape `--`!
            (Comment(comment, _), W::NodeExpected | W::NodeOpen) => {
                prev_state.close_tag_if_open(sink)?;
                sink.write_all(b"<!--")?;
                sink.write_all(comment.lookup_str().as_bytes())?;
                sink.write_all(b"-->")?;

                Ok(W::NodeExpected)
            }

            // As-of-yet unsupported operations that weren't needed at the
            // time of writing, but were planned for in the design of Xir.
            (invalid @ AttrName(_, _), W::AttrNameAdjacent) => {
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

impl<I: TokenStream, S: Escaper> XmlWriter<S> for I {
    fn write<W: Write>(
        mut self,
        sink: &mut W,
        initial_state: WriterState,
        escaper: &S,
    ) -> Result {
        self.try_fold(initial_state, |prev_state, tok| {
            tok.write(sink, prev_state, escaper)
        })
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use super::*;
    use crate::{
        convert::ExpectInto,
        span::{dummy::DUMMY_SPAN, Span},
        sym::GlobalSymbolIntern,
        xir::{
            error::SpanlessError,
            test::{close, close_empty, open},
            QName,
        },
    };

    type TestResult = std::result::Result<(), Error>;

    #[derive(Debug, Default)]
    struct MockEscaper {}

    // Simply adds ":ESC" as a suffix to the provided byte slice.
    impl Escaper for MockEscaper {
        fn escape_bytes(value: &[u8]) -> Cow<[u8]> {
            let mut esc = value.to_owned();
            esc.extend_from_slice(b":ESC");

            Cow::Owned(esc)
        }

        fn unescape_bytes(
            _: &[u8],
        ) -> result::Result<Cow<[u8]>, SpanlessError> {
            unreachable!("Writer should not be unescaping!")
        }
    }

    const S: Span = DUMMY_SPAN;

    #[test]
    fn writes_beginning_node_tag_without_prefix() -> TestResult {
        let name = QName::new_local("no-prefix".unwrap_into());
        let result = open(name, S)
            .write_new(Default::default(), &MockEscaper::default())?;

        assert_eq!(result.0, b"<no-prefix");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_beginning_node_tag_with_prefix() -> TestResult {
        let name = ("prefix", "element-name");
        let result = open(name, S)
            .write_new(Default::default(), &MockEscaper::default())?;

        assert_eq!(result.0, b"<prefix:element-name");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn closes_open_node_when_opening_another() -> TestResult {
        let name = ("p", "another-element");
        let result = open(name, S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;

        assert_eq!(result.0, b"><p:another-element");
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn closes_open_node_as_empty_element() -> TestResult {
        let result = close_empty(S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;

        assert_eq!(result.0, b"/>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn closing_tag_when_node_expected() -> TestResult {
        let name = ("a", "closed-element");

        let result = close(Some(name), S)
            .write_new(WriterState::NodeExpected, &MockEscaper::default())?;

        assert_eq!(result.0, b"</a:closed-element>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    // Does _not_ check that it's balanced.  Not our job.  In fact, we want
    // to explicitly support outputting malformed XML.
    #[test]
    fn closes_open_node_with_closing_tag() -> TestResult {
        let name = ("b", "closed-element");

        let result = close(Some(name), S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;

        assert_eq!(result.0, b"></b:closed-element>");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn writes_attr_name_to_open_node() -> TestResult {
        let name_ns = ("some", "attr").unwrap_into();
        let name_local = "nons".unwrap_into();

        // Namespace prefix
        let result = Token::AttrName(name_ns, S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;
        assert_eq!(result.0, b" some:attr");
        assert_eq!(result.1, WriterState::AttrNameAdjacent);

        // No namespace prefix
        let result = Token::AttrName(name_local, S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;
        assert_eq!(result.0, b" nons");
        assert_eq!(result.1, WriterState::AttrNameAdjacent);

        Ok(())
    }

    #[test]
    fn writes_attr_value_when_adjacent_to_attr() -> TestResult {
        let value = "test str".intern();

        let result = Token::AttrValue(value, S).write_new(
            WriterState::AttrNameAdjacent,
            &MockEscaper::default(),
        )?;

        assert_eq!(result.0, br#"="test str:ESC""#);
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_attr_value_consisting_of_fragments() -> TestResult {
        let value_left = "left".intern();
        let value_mid = " mid".intern();
        let value_right = " right".intern();

        let result = vec![
            Token::AttrValueFragment(value_left, S),
            Token::AttrValueFragment(value_mid, S),
            Token::AttrValue(value_right, S),
        ]
        .into_iter()
        .write_new(WriterState::AttrNameAdjacent, &MockEscaper::default())?;

        assert_eq!(result.0, br#"="left:ESC mid:ESC right:ESC""#);
        assert_eq!(result.1, WriterState::NodeOpen);

        Ok(())
    }

    #[test]
    fn writes_text() -> TestResult {
        let text = "test unescaped".intern();

        // When a node is expected.
        let result = Token::Text(text, S)
            .write_new(WriterState::NodeExpected, &MockEscaper::default())?;
        assert_eq!(result.0, b"test unescaped:ESC");
        assert_eq!(result.1, WriterState::NodeExpected);

        // When a node is still open.
        let result = Token::Text(text, S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;
        assert_eq!(result.0, b">test unescaped:ESC");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn writes_comment() -> TestResult {
        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let comment = "comment > escaped".intern();

        // When a node is expected.
        let result = Token::Comment(comment, S)
            .write_new(WriterState::NodeExpected, &MockEscaper::default())?;
        assert_eq!(result.0, b"<!--comment > escaped-->");
        assert_eq!(result.1, WriterState::NodeExpected);

        // When a node is still open.
        let result = Token::Comment(comment, S)
            .write_new(WriterState::NodeOpen, &MockEscaper::default())?;
        assert_eq!(result.0, b"><!--comment > escaped-->");
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }

    #[test]
    fn unsupported_transition_results_in_error() -> TestResult {
        assert!(matches!(
            Token::AttrValue("".intern(), S).write(
                &mut vec![],
                WriterState::NodeExpected,
                &MockEscaper::default()
            ),
            Err(Error::UnexpectedToken(_, WriterState::NodeExpected)),
        ));

        Ok(())
    }

    // The cherry on top, just to demonstrate how this will be used in
    // practice.
    #[test]
    fn test_valid_sequence_of_tokens() -> TestResult {
        let root: QName = ("r", "root").unwrap_into();

        let result = vec![
            open(root, S),
            Token::AttrName(("an", "attr").unwrap_into(), S),
            Token::AttrValue("value".intern(), S),
            Token::Text("text".intern(), S),
            open(("c", "child"), S),
            close_empty(S),
            close(Some(root), S),
        ]
        .into_iter()
        .write_new(Default::default(), &MockEscaper::default())?;

        assert_eq!(
            result.0,
            br#"<r:root an:attr="value:ESC">text:ESC<c:child/></r:root>"#
        );
        assert_eq!(result.1, WriterState::NodeExpected);

        Ok(())
    }
}
