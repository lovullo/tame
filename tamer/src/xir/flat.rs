// XIR flat (XIRF)
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

//! Lightly-parsed XIR as a flat stream (XIRF).
//!
//! XIRF lightly parses a raw XIR [`TokenStream`] into a stream of
//!   [`Object`]s that are,
//!     like a [`TokenStream`],
//!     flat in structure.
//! It provides the following features over raw XIR:
//!
//!   1. All closing tags must correspond to a matching opening tag at the
//!        same depth;
//!   2. [`Object`] exposes the [`Depth`] of each opening/closing tag;
//!   3. Attribute tokens are parsed into [`Attr`] objects;
//!   4. Documents must begin with an element and end with the closing of
//!        that element;
//!   5. Parsing will fail if input ends before all elements have been
//!        closed.
//!
//! XIRF lowering does not perform any dynamic memory allocation;
//!   maximum element nesting depth is set statically depending on the needs
//!   of the caller.

use super::{
    attr::{Attr, AttrParseError, AttrParseState},
    QName, Token as XirToken, TokenStream, Whitespace,
};
use crate::{
    parse::{
        ParseState, ParseStatus, ParsedResult, Token, Transition,
        TransitionResult,
    },
    span::Span,
    sym::SymbolId,
};
use arrayvec::ArrayVec;
use std::{error::Error, fmt::Display};

/// Tag nesting depth
///   (`0` represents the root).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Depth(usize);

impl Display for Depth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

/// A lightly-parsed XIRF object.
///
/// Certain XIR [`Token`]s are formed into a single object,
///   such as an [`Attr`].
/// Other objects retain the same format as their underlying token,
///   but are still validated to ensure that they are well-formed and that
///   the XML is well-structured.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    /// Opening tag of an element.
    Open(QName, Span, Depth),

    /// Closing tag of an element.
    ///
    /// If the name is [`None`],
    ///   then the tag is self-closing.
    /// If the name is [`Some`],
    ///   then the tag is guaranteed to be balanced
    ///     (matching the depth of its opening tag).
    Close(Option<QName>, Span, Depth),

    /// An attribute and its value.
    ///
    /// The associated [`Span`]s can be found on the enclosed [`Attr`]
    ///   object.
    Attr(Attr),

    /// Comment node.
    Comment(SymbolId, Span),

    /// Character data as part of an element.
    ///
    /// See also [`CData`](Object::CData) variant.
    Text(SymbolId, Span),

    /// CData node (`<![CDATA[...]]>`).
    ///
    /// _Warning: It is up to the caller to ensure that the string `]]>` is
    ///   not present in the text!_
    /// This is intended for reading existing XML data where CData is
    ///   already present,
    ///     not for producing new CData safely!
    CData(SymbolId, Span),

    /// Similar to `Text`,
    ///   but intended for use where only whitespace is allowed,
    ///     such as alignment of attributes.
    Whitespace(Whitespace, Span),
}

impl Token for Object {
    fn span(&self) -> Span {
        use Object::*;

        match self {
            Open(_, span, _)
            | Close(_, span, _)
            | Comment(_, span)
            | Text(_, span)
            | CData(_, span)
            | Whitespace(_, span) => *span,

            Attr(attr) => attr.span(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;

        match self {
            Open(qname, span, _) => {
                Display::fmt(&XirToken::Open(*qname, *span), f)
            }
            Close(oqname, span, _) => {
                Display::fmt(&XirToken::Close(*oqname, *span), f)
            }
            Attr(attr) => Display::fmt(&attr, f),
            Comment(sym, span) => {
                Display::fmt(&XirToken::Comment(*sym, *span), f)
            }
            Text(sym, span) => Display::fmt(&XirToken::Text(*sym, *span), f),
            CData(sym, span) => Display::fmt(&XirToken::CData(*sym, *span), f),
            Whitespace(ws, span) => {
                Display::fmt(&XirToken::Whitespace(*ws, *span), f)
            }
        }
    }
}

/// XIRF-compatible attribute parser.
pub trait FlatAttrParseState = ParseState<Token = XirToken, Object = Attr>
where
    <Self as ParseState>::Error: Into<StateError>;

/// Stack of element [`QName`] and [`Span`] pairs,
///   representing the current level of nesting.
///
/// This storage is statically allocated,
///   allowing XIRF's parser to avoid memory allocation entirely.
type ElementStack<const MAX_DEPTH: usize> = ArrayVec<(QName, Span), MAX_DEPTH>;

/// XIRF document parser state.
///
/// This parser is a pushdown automaton that parses a single XML document.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum State<const MAX_DEPTH: usize, SA = AttrParseState>
where
    SA: FlatAttrParseState,
{
    /// Document parsing has not yet begun.
    #[default]
    PreRoot,

    /// Parsing nodes.
    NodeExpected(ElementStack<MAX_DEPTH>),

    /// Delegating to attribute parser.
    AttrExpected(ElementStack<MAX_DEPTH>, SA),

    /// End of document has been reached.
    Done,
}

impl<const MAX_DEPTH: usize, SA> ParseState for State<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState,
{
    type Token = XirToken;
    type Object = Object;
    type Error = StateError;

    fn parse_token(self, tok: Self::Token) -> TransitionResult<Self> {
        use ParseStatus::{Dead, Incomplete, Object as Obj};
        use State::{AttrExpected, Done, NodeExpected, PreRoot};

        match (self, tok) {
            // Comments are permitted before and after the first root element.
            (st @ (PreRoot | Done), XirToken::Comment(sym, span)) => {
                Transition(st).with(Object::Comment(sym, span))
            }

            (PreRoot, tok @ XirToken::Open(..)) => {
                Self::parse_node(Default::default(), tok)
            }

            (PreRoot, tok) => {
                Transition(PreRoot).err(StateError::RootOpenExpected(tok))
            }

            (NodeExpected(stack), tok) => Self::parse_node(stack, tok),

            (AttrExpected(stack, sa), tok) => match sa.parse_token(tok) {
                (Transition(sa), Ok(Incomplete)) => {
                    Transition(AttrExpected(stack, sa)).incomplete()
                }
                (Transition(sa), Ok(Obj(attr))) => {
                    Transition(AttrExpected(stack, sa)).with(Object::Attr(attr))
                }
                (_, Ok(Dead(lookahead))) => Self::parse_node(stack, lookahead),
                (Transition(sa), Err(x)) => {
                    Transition(AttrExpected(stack, sa)).err(x)
                }
            },

            (Done, tok) => Transition(Done).dead(tok),
        }
    }

    /// Whether all elements have been closed.
    ///
    /// Parsing will fail if there are any open elements.
    /// Intuitively,
    ///   this means that the parser must have encountered the closing tag
    ///   for the root element.
    fn is_accepting(&self) -> bool {
        // TODO: It'd be nice if we could also return additional context to
        //   aid the user in diagnosing the problem,
        //     e.g. what element(s) still need closing.
        *self == State::Done
    }
}

impl<const MAX_DEPTH: usize, SA> State<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState,
{
    /// Parse a token while in a state expecting a node.
    fn parse_node(
        mut stack: ElementStack<MAX_DEPTH>,
        tok: <Self as ParseState>::Token,
    ) -> TransitionResult<Self> {
        use Object::*;
        use State::{AttrExpected, Done, NodeExpected};

        match tok {
            XirToken::Open(qname, span) if stack.len() == MAX_DEPTH => {
                Transition(NodeExpected(stack)).err(
                    StateError::MaxDepthExceeded {
                        open: (qname, span),
                        max: Depth(MAX_DEPTH),
                    },
                )
            }

            XirToken::Open(qname, span) => {
                let depth = stack.len();
                stack.push((qname, span));

                // Delegate to the attribute parser until it is complete.
                Transition(AttrExpected(stack, SA::default())).with(Open(
                    qname,
                    span,
                    Depth(depth),
                ))
            }

            XirToken::Close(close_oqname, close_span) => {
                match (close_oqname, stack.pop()) {
                    (_, None) => unreachable!("parser should be in Done state"),

                    (Some(qname), Some((open_qname, open_span)))
                        if qname != open_qname =>
                    {
                        Transition(NodeExpected(stack)).err(
                            StateError::UnbalancedTag {
                                open: (open_qname, open_span),
                                close: (qname, close_span),
                            },
                        )
                    }

                    // Final closing tag (for root node) completes the document.
                    (..) if stack.len() == 0 => Transition(Done).with(Close(
                        close_oqname,
                        close_span,
                        Depth(0),
                    )),

                    (..) => {
                        let depth = stack.len();

                        Transition(NodeExpected(stack)).with(Close(
                            close_oqname,
                            close_span,
                            Depth(depth),
                        ))
                    }
                }
            }

            XirToken::Comment(sym, span) => {
                Transition(NodeExpected(stack)).with(Comment(sym, span))
            }
            XirToken::Text(sym, span) => {
                Transition(NodeExpected(stack)).with(Text(sym, span))
            }
            XirToken::CData(sym, span) => {
                Transition(NodeExpected(stack)).with(CData(sym, span))
            }
            XirToken::Whitespace(ws, span) => {
                Transition(NodeExpected(stack)).with(Whitespace(ws, span))
            }

            // We should transition to `State::Attr` before encountering any
            //   of these tokens.
            XirToken::AttrName(..)
            | XirToken::AttrValue(..)
            | XirToken::AttrValueFragment(..) => {
                unreachable!("attribute token in NodeExpected state: {tok:?}")
            }
        }
    }
}

/// Produce a streaming parser lowering a XIR [`TokenStream`] into a XIRF
///   stream.
pub fn parse<const MAX_DEPTH: usize>(
    toks: impl TokenStream,
) -> impl Iterator<Item = ParsedResult<State<MAX_DEPTH>>> {
    State::<MAX_DEPTH>::parse(toks)
}

/// Parsing error from [`State`].
#[derive(Debug, Eq, PartialEq)]
pub enum StateError {
    /// Opening root element tag was expected.
    RootOpenExpected(XirToken),

    /// Opening tag exceeds the maximum nesting depth for this parser.
    MaxDepthExceeded { open: (QName, Span), max: Depth },

    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName, Span),
        close: (QName, Span),
    },

    /// Error from the attribute parser.
    AttrError(AttrParseError),
}

impl Display for StateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use StateError::*;

        match self {
            RootOpenExpected(tok) => {
                write!(
                    f,
                    "opening root element tag expected, \
                       but found {tok}"
                )
            }

            MaxDepthExceeded {
                open: (name, span),
                max,
            } => {
                write!(
                    f,
                    "maximum element nesting depth of {max} exceeded \
                       by `{name}` at {span}"
                )
            }

            UnbalancedTag {
                open: (open_name, open_span),
                close: (close_name, close_span),
            } => {
                write!(
                    f,
                    "expected closing tag `{open_name}`, \
                       but found `{close_name}` at {close_span} \
                       (opening tag at {open_span})",
                )
            }

            AttrError(e) => Display::fmt(e, f),
        }
    }
}

impl Error for StateError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        todo!()
    }
}

impl From<AttrParseError> for StateError {
    fn from(e: AttrParseError) -> Self {
        Self::AttrError(e)
    }
}

#[cfg(test)]
mod test;
