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
//!   3. Attribute tokens are parsed into [`Attr`] objects; and
//!   4. Parsing will fail if input ends before all elements have been
//!        closed.
//!
//! XIRF lowering does not perform any dynamic memory allocation;
//!   maximum element nesting depth is set statically depending on the needs
//!   of the caller.

use super::{
    attr::{Attr, AttrParseError, AttrParseState},
    parse::{ParseState, ParseStatus, ParsedResult, TransitionResult},
    QName, Token, TokenStream, Whitespace,
};
use crate::{span::Span, sym::SymbolId, xir::parse::Transition};
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

/// XIRF-compatible attribute parser.
pub trait FlatAttrParseState = ParseState<Object = Attr>
where
    <Self as ParseState>::Error: Into<StateError>;

/// Stack of element [`QName`] and [`Span`] pairs,
///   representing the current level of nesting.
///
/// This storage is statically allocated,
///   allowing XIRF's parser to avoid memory allocation entirely.
type ElementStack<const MAX_DEPTH: usize> = ArrayVec<(QName, Span), MAX_DEPTH>;

/// XIRF parser state.
///
/// This parser is a pushdown automaton.
#[derive(Debug, PartialEq, Eq)]
pub enum State<const MAX_DEPTH: usize, SA = AttrParseState>
where
    SA: FlatAttrParseState,
{
    // TODO: Ensure that non-comment nodes are not encountered before the
    //   root,
    //     and that we do not encounter any non-comment nodes after the
    //     root.
    /// Parsing nodes.
    NodeExpected(ElementStack<MAX_DEPTH>),

    /// Delegating to attribute parser.
    AttrExpected(ElementStack<MAX_DEPTH>, SA),
}

impl<const MD: usize, SA: FlatAttrParseState> Default for State<MD, SA> {
    fn default() -> Self {
        Self::NodeExpected(Default::default())
    }
}

impl<const MAX_DEPTH: usize, SA> ParseState for State<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState,
{
    type Object = Object;
    type Error = StateError;

    fn parse_token(self, tok: Token) -> TransitionResult<Self> {
        use ParseStatus::{Dead, Incomplete, Object as Obj};
        use State::{AttrExpected, NodeExpected};

        // This awkward-looking take-reassign forces us to be explicit
        //   about state transitions in every case,
        //     ensuring that we always have documented proof of what state
        //     the system winds up in.
        // The `Invalid` state prevents using `return`.
        match (self, tok) {
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
        matches!(self, Self::NodeExpected(stack) if stack.len() == 0)
    }
}

impl<const MAX_DEPTH: usize, SA> State<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState,
{
    /// Parse a token while in a state expecting a node.
    fn parse_node(
        mut stack: ElementStack<MAX_DEPTH>,
        tok: Token,
    ) -> TransitionResult<Self> {
        use Object::*;
        use State::{AttrExpected, NodeExpected};

        match tok {
            Token::Open(qname, span) if stack.len() == MAX_DEPTH => Transition(
                NodeExpected(stack),
            )
            .err(StateError::MaxDepthExceeded {
                open: (qname, span),
                max: Depth(MAX_DEPTH),
            }),

            Token::Open(qname, span) => {
                let depth = stack.len();
                stack.push((qname, span));

                // Delegate to the attribute parser until it is complete.
                Transition(AttrExpected(stack, SA::default())).with(Open(
                    qname,
                    span,
                    Depth(depth),
                ))
            }

            Token::Close(close_oqname, close_span) => {
                match (close_oqname, stack.pop()) {
                    (_, None) => Transition(NodeExpected(stack)).err(
                        StateError::ExtraClosingTag(close_oqname, close_span),
                    ),

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

            Token::Comment(sym, span) => {
                Transition(NodeExpected(stack)).with(Comment(sym, span))
            }
            Token::Text(sym, span) => {
                Transition(NodeExpected(stack)).with(Text(sym, span))
            }
            Token::CData(sym, span) => {
                Transition(NodeExpected(stack)).with(CData(sym, span))
            }
            Token::Whitespace(ws, span) => {
                Transition(NodeExpected(stack)).with(Whitespace(ws, span))
            }

            // We should transition to `State::Attr` before encountering any
            //   of these tokens.
            Token::AttrName(..)
            | Token::AttrValue(..)
            | Token::AttrValueFragment(..) => {
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
    /// Opening tag exceeds the maximum nesting depth for this parser.
    MaxDepthExceeded { open: (QName, Span), max: Depth },

    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName, Span),
        close: (QName, Span),
    },

    /// Attempt to close a tag with no corresponding opening tag
    ///   (which would result in a negative depth).
    ExtraClosingTag(Option<QName>, Span),

    /// Error from the attribute parser.
    AttrError(AttrParseError),
}

impl Display for StateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use StateError::*;

        match self {
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

            ExtraClosingTag(Some(name), span) => {
                write!(f, "closing tag `{name}` at {span} has no opening tag",)
            }

            // If this occurs, its likely that something generated invalid
            //   XIR;
            //     it should be a parsing error on read and no generator
            //     should ever produce this.
            ExtraClosingTag(None, span) => {
                write!(f, "self-closing tag at {span} has no opening tag")
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
