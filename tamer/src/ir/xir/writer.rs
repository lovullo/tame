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

use super::{Error as XirError, NodeStream, QName};
use crate::ir::xir::{AttrValue, Text};
use crate::sym::GlobalSymbolResolve;
use crate::sym::SymbolIndexSize;
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
    /// Cursor is position adjacent to an attribute name within an element.
    AttrNameAdjacent,
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

pub trait XmlWriter {
    #[must_use = "Write operation may fail"]
    fn write<W: Write>(self, sink: &mut W, prev_state: WriterState) -> Result;
}

impl<Ix: SymbolIndexSize> XmlWriter for QName<Ix> {
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

impl<Ix: SymbolIndexSize> XmlWriter for NodeStream<Ix> {
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

            // TODO: Remove whitespace from here, add to stream, then it can
            // be used with attrs
            (Self::SelfClose(_), S::NodeOpen) => {
                sink.write(b"/>")?;

                Ok(S::NodeExpected)
            }

            (Self::Close(name, _), S::NodeExpected | S::NodeOpen) => {
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

            (
                Self::AttrValue(AttrValue::Escaped(value), _),
                S::AttrNameAdjacent,
            ) => {
                sink.write(b"=\"")?;
                sink.write(value.lookup_str().as_bytes())?;
                sink.write(b"\"")?;

                Ok(S::NodeOpen)
            }

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
            (
                invalid
                @
                (Self::AttrName(_, _)
                | Self::AttrValue(AttrValue::Unescaped(_), _)),
                S::AttrNameAdjacent,
            )
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

impl<Ix: SymbolIndexSize, I: Iterator<Item = NodeStream<Ix>>> XmlWriter for I {
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
        ir::xir::{AttrValue, QName, Text, Whitespace},
        span::Span,
        sym::GlobalSymbolIntern,
    };

    type TestResult = std::result::Result<(), Error>;

    type Ix = u16;

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case".intern());
    }

    #[test]
    fn writes_beginning_node_tag_without_prefix() -> TestResult {
        let mut buf = vec![];
        let name = QName::<Ix>::new_local("no-prefix".try_into()?);

        assert_eq!(
            NodeStream::Open(name, *S).write(&mut buf, Default::default())?,
            WriterState::NodeOpen
        );

        assert_eq!(buf, b"<no-prefix");

        Ok(())
    }

    #[test]
    fn writes_beginning_node_tag_with_prefix() -> TestResult {
        let mut buf = vec![];
        let name = QName::<Ix>::try_from(("prefix", "element-name"))?;

        assert_eq!(
            NodeStream::Open(name, *S).write(&mut buf, Default::default())?,
            WriterState::NodeOpen
        );

        assert_eq!(buf, b"<prefix:element-name");

        Ok(())
    }

    #[test]
    fn closes_open_node_when_opening_another() -> TestResult {
        let mut buf = vec![];
        let name = QName::<Ix>::try_from(("p", "another-element"))?;

        assert_eq!(
            NodeStream::Open(name, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeOpen
        );

        assert_eq!(buf, b"><p:another-element");

        Ok(())
    }

    #[test]
    fn closes_open_node_as_empty_element() -> TestResult {
        let mut buf = vec![];

        assert_eq!(
            NodeStream::<Ix>::SelfClose(*S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"/>");

        Ok(())
    }

    #[test]
    fn closing_tag_when_node_expected() -> TestResult {
        let mut buf = vec![];
        let name = QName::<Ix>::try_from(("a", "closed-element"))?;

        assert_eq!(
            NodeStream::Close(name, *S)
                .write(&mut buf, WriterState::NodeExpected)?,
            WriterState::NodeExpected
        );

        assert_eq!(buf, b"</a:closed-element>");

        Ok(())
    }

    // Does _not_ check that it's balanced.  Not our job.  In fact, we want
    // to explicitly support outputting malformed XML.
    #[test]
    fn closes_open_node_with_closing_tag() -> TestResult {
        let mut buf = vec![];
        let name = QName::<Ix>::try_from(("b", "closed-element"))?;

        assert_eq!(
            NodeStream::Close(name, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeExpected
        );

        assert_eq!(buf, b"></b:closed-element>");

        Ok(())
    }

    // Intended for alignment of attributes, primarily.
    #[test]
    fn whitespace_within_open_node() -> TestResult {
        let mut buf = vec![];

        assert_eq!(
            NodeStream::<Ix>::Whitespace(Whitespace::try_from(" \t ")?, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeOpen
        );

        assert_eq!(buf, b" \t ");

        Ok(())
    }

    #[test]
    fn writes_attr_name_to_open_node() -> TestResult {
        let mut buf = vec![];
        let name_ns = QName::<Ix>::try_from(("some", "attr"))?;
        let name_local = QName::<Ix>::new_local("nons".try_into()?);

        // Namespace prefix
        assert_eq!(
            NodeStream::AttrName(name_ns, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::AttrNameAdjacent
        );
        assert_eq!(buf, b" some:attr");

        buf.clear();

        // No namespace prefix
        assert_eq!(
            NodeStream::AttrName(name_local, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::AttrNameAdjacent
        );
        assert_eq!(buf, b" nons");

        Ok(())
    }

    #[test]
    fn writes_escaped_attr_value_when_adjacent_to_attr() -> TestResult {
        let mut buf = vec![];

        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let value = AttrValue::<Ix>::Escaped("test \" escaped".intern());

        assert_eq!(
            NodeStream::AttrValue(value, *S)
                .write(&mut buf, WriterState::AttrNameAdjacent)?,
            WriterState::NodeOpen
        );

        assert_eq!(buf, br#"="test " escaped""#);

        Ok(())
    }

    #[test]
    fn writes_escaped_text() -> TestResult {
        let mut buf = vec![];

        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let text = Text::<Ix>::Escaped("test > escaped".intern());

        // When a node is expected.
        assert_eq!(
            NodeStream::Text(text, *S)
                .write(&mut buf, WriterState::NodeExpected)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"test > escaped");

        buf.clear();

        // When a node is still open.
        assert_eq!(
            NodeStream::Text(text, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b">test > escaped");

        Ok(())
    }

    #[test]
    fn writes_unescaped_data() -> TestResult {
        let mut buf = vec![];

        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let text = Text::<Ix>::Unescaped("test > unescaped".intern());

        // When a node is expected.
        assert_eq!(
            NodeStream::CData(text, *S)
                .write(&mut buf, WriterState::NodeExpected)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"<![CDATA[test > unescaped]]>");

        buf.clear();

        // When a node is still open.
        assert_eq!(
            NodeStream::CData(text, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"><![CDATA[test > unescaped]]>");

        Ok(())
    }

    #[test]
    fn writes_escaped_comment() -> TestResult {
        let mut buf = vec![];

        // Just to be sure it's not trying to escape when we say it
        // shouldn't, we include a character that must otherwise be escaped.
        let comment = Text::<Ix>::Escaped("comment > escaped".intern());

        // When a node is expected.
        assert_eq!(
            NodeStream::Comment(comment, *S)
                .write(&mut buf, WriterState::NodeExpected)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"<!--comment > escaped-->");

        buf.clear();

        // When a node is still open.
        assert_eq!(
            NodeStream::Comment(comment, *S)
                .write(&mut buf, WriterState::NodeOpen)?,
            WriterState::NodeExpected
        );
        assert_eq!(buf, b"><!--comment > escaped-->");

        Ok(())
    }

    #[test]
    fn unsupported_transition_results_in_error() -> TestResult {
        assert!(matches!(
            NodeStream::AttrValue(AttrValue::<Ix>::Escaped("".into()), *S)
                .write(&mut vec![], WriterState::NodeExpected),
            Err(Error::UnexpectedToken(_, WriterState::NodeExpected)),
        ));

        Ok(())
    }

    // The cherry on top, just to demonstrate how this will be used in
    // practice.
    #[test]
    fn test_valid_sequence_of_tokens() -> TestResult {
        let mut buf = vec![];
        let root: QName<Ix> = ("r", "root").try_into()?;

        vec![
            NodeStream::Open(root, *S),
            NodeStream::AttrName(("an", "attr").try_into()?, *S),
            NodeStream::AttrValue(AttrValue::Escaped("value".intern()), *S),
            NodeStream::Text(Text::Escaped("text".intern()), *S),
            NodeStream::Open(("c", "child").try_into()?, *S),
            NodeStream::Whitespace(" ".try_into()?, *S),
            NodeStream::SelfClose(*S),
            NodeStream::Close(root, *S),
        ]
        .into_iter()
        .write(&mut buf, Default::default())?;

        assert_eq!(buf, br#"<r:root an:attr="value">text<c:child /></r:root>"#);

        Ok(())
    }
}
