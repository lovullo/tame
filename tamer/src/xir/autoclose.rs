// Automatically close elements of XIRF streams
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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

//! Automatically close elements of XIRF streams by tracking a virtual
//!   [`Depth`].
//!
//! The [`XirfAutoClose`] lowering operation allows the system to generate a
//!   stream of [`XirfToken`]s without having to worry about balancing tags.
//! This is intended for situations where generating closing tags may be
//!   particularly burdensome relative to problem at hand---where
//!     generating closing tokens takes away significantly from the
//!     expressiveness and concision of the producing implementation.
//!
//! How Does It Work?
//! =================
//! One of the features of XIRF is [`Depth`] tracking,
//!   which is usually produced during
//!   [XIR lowering](crate::xir::flat::XirToXirf).
//!
//! [`XirfAutoClose`] instead uses [`Depth`] to represent a _virtual_
//!   depth [`VDepth`].
//! This is used as a relative depth from which to derive a _physical_
//!   depth [`PDepth`].
//!
//! For example,
//!   if we are provided with [`XirfToken`]s corresponding roughly to this
//!   (invalid) XML[^wisp],
//!     where two spaces of indentation represents one level of [`Depth`]:
//!
//! ```xml
//! <root>
//!   <foo a="1">
//!     <bar>
//!       <baz>
//!     <quux>
//!   <foo a="2">
//!   <foo a="3">
//!     Foo text
//!     <bar>
//!       Bar text
//!     More foo text
//!   <foo a="4">
//! ```
//!
//! [^wisp]: It's almost like Wisp for XML.
//!
//!   then this lowering operation would produce a [`XirfToken`] stream
//!     representing the following XML[^text],
//!       with the indentation also representing its depth as above:
//!
//! ```xml
//! <root>
//!   <foo a="1">
//!     <bar>
//!       <baz />
//!     </bar>
//!   </foo>
//!   <foo a="2" />
//!   <foo a="3">
//!     Foo text
//!     <bar>
//!       Bar text
//!     </bar>
//!     More foo text
//!   </foo>
//!   <foo a="4" />
//! </root>
//! ```
//!
//! [^text]: Note that this example is a bit hand-wavy on the text,
//!            but the rule is simple:
//!              the text symbol is unmodified and so will retain any
//!              whitespace presented as input.
//!
//! If a [`XirfToken::Close`] is provided,
//!   then it will effectively be ignored,
//!   since auto-closing will act the same regardless of whether it is
//!     present.
//! The [`Span`]s of auto-closed [`XirfToken::Close`]
//!   (and explicitly closed ones as well)
//!   will be the span of the respective [`XirfToken::Open`].

use super::{
    flat::{Depth, Text, XirfToken},
    CloseSpan, OpenSpan, QName,
};
use crate::{
    f::Functor,
    parse::prelude::*,
    span::{Span, UNKNOWN_SPAN},
    xir::EleSpan,
};
use std::{convert::Infallible, fmt::Display};

use XirfAutoClose::*;

/// Automatically insert [`XirfToken::Close`] tokens into the input stream
///   and normalize token [`Depth`].
///
/// See the [module-level documentation](super) for more information.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum XirfAutoClose {
    /// Element contains no children and can be self-closing.
    #[default]
    EmptyEle,

    /// Element contains children can must be closed with a separate tag.
    NonEmptyEle,
}

impl Display for XirfAutoClose {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyEle => write!(
                f,
                "automatically closing XIRF tokens (self-closing tag)"
            ),
            Self::NonEmptyEle => {
                write!(f, "automatically closing XIRF tokens (with children)")
            }
        }
    }
}

impl ParseState for XirfAutoClose {
    type Token = XirfToken<Text>;
    type Object = XirfToken<Text>;
    type Error = Infallible;
    type Context = AutoCloseStack;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use XirfToken::*;

        match tok {
            // Perform closing to the necessary depth before processing the
            //   provided `tok`.
            // This will repeat as many times as necessary.
            tok if stack.needs_close(&tok) => {
                // `needs_close` would not match if the stack was empty,
                //   so this is safe to unwrap without panicing.
                let (close_qname, close_span, _, PDepth(close_depth)) =
                    stack.pop().unwrap();

                Transition(NonEmptyEle)
                    .ok(Close(
                        Some(close_qname).filter(|_| self == Self::NonEmptyEle),
                        CloseSpan::without_name_span(close_span),
                        close_depth,
                    ))
                    .with_lookahead(tok)
            }

            Open(qname, ospan, given_depth) => {
                let PDepth(depth) =
                    stack.push(qname, ospan, VDepth(given_depth));

                Transition(EmptyEle).ok(Open(qname, ospan, depth))
            }

            // Reaching this point means that a close was requested but none
            //   is actually needed at this VDepth.
            // Just ignore it.
            // If this is a problem in practice,
            //   we can revisit this decision.
            Close(_, _, _) => Transition(self).incomplete(),

            // Attributes have no depth information that needs adjustment,
            //   and do not affect whether a tag is self-closing,
            //   so they can just be proxied along.
            Attr(_) => Transition(self).ok(tok),

            // Everything else is a child that needs a depth adjustment.
            Comment(..) | Text(..) | CData(..) => Transition(NonEmptyEle)
                .ok(tok.map(|_| stack.child_pdepth().into())),
        }
    }

    fn is_accepting(&self, stack: &Self::Context) -> bool {
        stack.is_empty()
    }

    fn eof_tok(&self, stack: &Self::Context) -> Option<Self::Token> {
        // Once we reach the end of the stream,
        //   any remaining open elements need to be closed.
        (!stack.is_empty()).then_some(XirfToken::Close(
            None,
            CloseSpan::without_name_span(UNKNOWN_SPAN),
            Depth(0),
        ))
    }
}

/// Virtual (provided) [`Depth`].
///
/// The system compares virtual depths _relative_ to the parent:
///
///   - Equal depths are siblings;
///   - Greater depths are children; and
///   - Lesser depths are ancestors.
#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd)]
struct VDepth(Depth);

impl Default for VDepth {
    fn default() -> Self {
        Self(Depth::root())
    }
}

/// Physical (actual) [`Depth`].
///
/// This represents the [`Depth`] that will be emitted in the output
///   [`XirfToken`] stream,
///     derived from the [`VDepth`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct PDepth(Depth);

impl Default for PDepth {
    fn default() -> Self {
        Self(Depth::root())
    }
}

impl From<PDepth> for Depth {
    fn from(value: PDepth) -> Self {
        match value {
            PDepth(depth) => depth,
        }
    }
}

/// Stack of open elements and associated metadata.
#[derive(Debug, PartialEq)]
pub struct AutoCloseStack(Vec<StackItem>);

type StackItem = (QName, Span, VDepth, PDepth);

impl AutoCloseStack {
    /// Pop the most recently opened element off of the stack,
    ///   if any.
    fn pop(&mut self) -> Option<StackItem> {
        match self {
            Self(v) => v.pop(),
        }
    }

    /// The [`VDepth`] of the most recently opened element.
    fn vdepth(&self) -> Option<VDepth> {
        match self {
            Self(stack) => stack[..].last().map(|(_, _, vdepth, _)| *vdepth),
        }
    }

    /// The [`PDepth`] of a child of the most recently opened element.
    fn child_pdepth(&self) -> PDepth {
        match self {
            Self(stack) => stack[..]
                .last()
                .map(|(_, _, _, PDepth(depth))| PDepth(depth.child_depth()))
                .unwrap_or_default(),
        }
    }

    /// Whether the provided [`XirfToken`] requires one or more elements to
    ///   be closed before being emitted.
    ///
    /// Closing is needed if the [`VDepth`] of the element is ≤ the
    ///   [`VDepth`] of the element atop of the stack.
    fn needs_close(&self, tok: &XirfToken<Text>) -> bool {
        self.vdepth()
            .zip(tok.depth().map(VDepth))
            .is_some_and(|(cur, given)| given <= cur)
    }

    /// Push information about an element onto the stack.
    ///
    /// The [`PDepth`] of the element will be computed automatically and
    ///   returned as the result of this operation.
    fn push(
        &mut self,
        qname: QName,
        ospan: OpenSpan,
        vdepth: VDepth,
    ) -> PDepth {
        let Self(stack) = self;
        let pdepth = stack[..]
            .last()
            .map(|(_, _, _, PDepth(depth))| PDepth(depth.child_depth()))
            .unwrap_or(PDepth(Depth(0)));

        stack.push((qname, ospan.tag_span(), vdepth, pdepth));
        pdepth
    }

    fn is_empty(&self) -> bool {
        match self {
            Self(stack) => stack.is_empty(),
        }
    }
}

/// Default size of the stack,
///   intended to accommodate most documents without resizing.
///
/// TODO: Derive a heuristic from real-world inputs.
const DEFAULT_STACK_SIZE: usize = 8;

impl Default for AutoCloseStack {
    fn default() -> Self {
        Self(Vec::with_capacity(DEFAULT_STACK_SIZE))
    }
}

#[cfg(test)]
mod test;
