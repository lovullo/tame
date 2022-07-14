// XIR flat (XIRF)
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

//! Lightly-parsed XIR as a flat stream (XIRF).
//!
//! XIRF lightly parses a raw XIR [`TokenStream`] into a stream of
//!   [`XirfToken`]s that are,
//!     like a [`TokenStream`],
//!     flat in structure.
//! It provides the following features over raw XIR:
//!
//!   1. All closing tags must correspond to a matching opening tag at the
//!        same depth;
//!   2. [`XirfToken`] exposes the [`Depth`] of each opening/closing tag;
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
    CloseSpan, OpenSpan, QName, Token as XirToken, TokenStream, Whitespace,
};
use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    parse::{
        Context, Object, ParseState, ParsedResult, Token, Transition,
        TransitionResult,
    },
    span::Span,
    sym::SymbolId,
    xir::EleSpan,
};
use arrayvec::ArrayVec;
use std::{error::Error, fmt::Display};

/// Tag nesting depth
///   (`0` represents the root).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Depth(pub usize);

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
pub enum XirfToken {
    /// Opening tag of an element.
    Open(QName, OpenSpan, Depth),

    /// Closing tag of an element.
    ///
    /// If the name is [`None`],
    ///   then the tag is self-closing.
    /// If the name is [`Some`],
    ///   then the tag is guaranteed to be balanced
    ///     (matching the depth of its opening tag).
    Close(Option<QName>, CloseSpan, Depth),

    /// An attribute and its value.
    ///
    /// The associated [`Span`]s can be found on the enclosed [`Attr`]
    ///   object.
    Attr(Attr),

    /// Comment node.
    Comment(SymbolId, Span),

    /// Character data as part of an element.
    ///
    /// See also [`CData`](XirfToken::CData) variant.
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

impl Token for XirfToken {
    fn span(&self) -> Span {
        use XirfToken::*;

        match self {
            Open(_, OpenSpan(span, _), _)
            | Close(_, CloseSpan(span, _), _)
            | Comment(_, span)
            | Text(_, span)
            | CData(_, span)
            | Whitespace(_, span) => *span,

            Attr(attr) => attr.span(),
        }
    }
}

impl Object for XirfToken {}

impl Display for XirfToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use XirfToken::*;

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

impl From<Attr> for XirfToken {
    fn from(attr: Attr) -> Self {
        Self::Attr(attr)
    }
}

/// XIRF-compatible attribute parser.
pub trait FlatAttrParseState<const MAX_DEPTH: usize> =
    ParseState<Token = XirToken, Object = Attr>
    where
        Self: Default,
        <Self as ParseState>::Error: Into<XirToXirfError>,
        StateContext<MAX_DEPTH>: AsMut<<Self as ParseState>::Context>;

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
pub enum XirToXirf<const MAX_DEPTH: usize, SA = AttrParseState>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
{
    /// Document parsing has not yet begun.
    #[default]
    PreRoot,
    /// Parsing nodes.
    NodeExpected,
    /// Delegating to attribute parser.
    AttrExpected(SA),
    /// End of document has been reached.
    Done,
}

pub type StateContext<const MAX_DEPTH: usize> =
    Context<ElementStack<MAX_DEPTH>>;

impl<const MAX_DEPTH: usize, SA> ParseState for XirToXirf<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
{
    type Token = XirToken;
    type Object = XirfToken;
    type Error = XirToXirfError;
    type Context = StateContext<MAX_DEPTH>;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self> {
        use XirToXirf::{AttrExpected, Done, NodeExpected, PreRoot};

        match (self, tok) {
            // Comments are permitted before and after the first root element.
            (st @ (PreRoot | Done), XirToken::Comment(sym, span)) => {
                Transition(st).ok(XirfToken::Comment(sym, span))
            }

            (PreRoot, tok @ XirToken::Open(..)) => Self::parse_node(tok, stack),

            (PreRoot, tok) => {
                Transition(PreRoot).err(XirToXirfError::RootOpenExpected(tok))
            }

            (NodeExpected, tok) => Self::parse_node(tok, stack),

            (AttrExpected(sa), tok) => sa.delegate(
                tok,
                stack,
                |sa| Transition(AttrExpected(sa)),
                || Transition(NodeExpected),
            ),

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
        *self == XirToXirf::Done
    }
}

impl<const MAX_DEPTH: usize, SA> Display for XirToXirf<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XirToXirf::*;

        match self {
            PreRoot => write!(f, "expecting document root"),
            NodeExpected => write!(f, "expecting a node"),
            AttrExpected(sa) => Display::fmt(sa, f),
            Done => write!(f, "done parsing"),
        }
    }
}

impl<const MAX_DEPTH: usize, SA> XirToXirf<MAX_DEPTH, SA>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
{
    /// Parse a token while in a state expecting a node.
    fn parse_node(
        tok: <Self as ParseState>::Token,
        stack: &mut ElementStack<MAX_DEPTH>,
    ) -> TransitionResult<Self> {
        use XirToXirf::{AttrExpected, Done, NodeExpected};
        use XirfToken::*;

        match tok {
            XirToken::Open(qname, span) if stack.len() == MAX_DEPTH => {
                Transition(NodeExpected).err(XirToXirfError::MaxDepthExceeded {
                    open: (qname, span.tag_span()),
                    max: Depth(MAX_DEPTH),
                })
            }

            XirToken::Open(qname, span) => {
                let depth = stack.len();
                stack.push((qname, span.tag_span()));

                // Delegate to the attribute parser until it is complete.
                Transition(AttrExpected(SA::default())).ok(Open(
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
                        Transition(NodeExpected).err(
                            XirToXirfError::UnbalancedTag {
                                open: (open_qname, open_span),
                                close: (qname, close_span.tag_span()),
                            },
                        )
                    }

                    // Final closing tag (for root node) completes the document.
                    (..) if stack.len() == 0 => Transition(Done).ok(Close(
                        close_oqname,
                        close_span,
                        Depth(0),
                    )),

                    (..) => {
                        let depth = stack.len();

                        Transition(NodeExpected).ok(Close(
                            close_oqname,
                            close_span,
                            Depth(depth),
                        ))
                    }
                }
            }

            XirToken::Comment(sym, span) => {
                Transition(NodeExpected).ok(Comment(sym, span))
            }
            XirToken::Text(sym, span) => {
                Transition(NodeExpected).ok(Text(sym, span))
            }
            XirToken::CData(sym, span) => {
                Transition(NodeExpected).ok(CData(sym, span))
            }
            XirToken::Whitespace(ws, span) => {
                Transition(NodeExpected).ok(Whitespace(ws, span))
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
) -> impl Iterator<Item = ParsedResult<XirToXirf<MAX_DEPTH>>> {
    XirToXirf::<MAX_DEPTH>::parse(toks)
}

/// Parsing error from [`XirToXirf`].
#[derive(Debug, Eq, PartialEq)]
pub enum XirToXirfError {
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

impl Display for XirToXirfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use XirToXirfError::*;

        match self {
            RootOpenExpected(_tok) => {
                write!(f, "missing opening root element",)
            }

            MaxDepthExceeded {
                open: (_name, _),
                max,
            } => {
                write!(
                    f,
                    "maximum XML element nesting depth of `{max}` exceeded"
                )
            }

            UnbalancedTag {
                open: (open_name, _),
                close: (_close_name, _),
            } => {
                write!(f, "expected closing tag for `{open_name}`")
            }

            AttrError(e) => Display::fmt(e, f),
        }
    }
}

impl Error for XirToXirfError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::AttrError(e) => Some(e),
            _ => None,
        }
    }
}

impl Diagnostic for XirToXirfError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use XirToXirfError::*;

        match self {
            RootOpenExpected(tok) => {
                // TODO: Should the span be the first byte,
                //   or should we delegate that question to an e.g. `SpanLike`?
                tok.span()
                    .error("an opening root node was expected here")
                    .into()
            }

            MaxDepthExceeded {
                open: (_, span),
                max,
            } => span
                .error(format!(
                    "this opening tag increases the level of nesting \
                       past the limit of {max}"
                ))
                .into(),

            UnbalancedTag {
                open: (open_name, open_span),
                close: (_close_name, close_span),
            } => {
                // TODO: hint saying that the nesting could be wrong, etc;
                //   we can't just suggest a replacement,
                //     since that's not necessarily the problem
                vec![
                    open_span
                        .note(format!("element `{open_name}` is opened here")),
                    // No need to state the close name since the source line
                    //   will be highlighted by the diagnostic message.
                    close_span.error(format!("expected `</{open_name}>`")),
                ]
            }

            AttrError(e) => e.describe(),
        }
    }
}

impl From<AttrParseError> for XirToXirfError {
    fn from(e: AttrParseError) -> Self {
        Self::AttrError(e)
    }
}

#[cfg(test)]
pub mod test;
