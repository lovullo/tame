// TAMER parsing framework utilities
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

//! Utilities that make parsing practical and convenient in different
//!   contexts.
//!
//! The utilities presented here do not introduce any new capabilities into
//!   the system;
//!     they provide wrappers around core functionality that make it easier
//!     to use outside of the domain of the parsing system itself.

use crate::{diagnose::Annotate, diagnostic_panic, span::Span, sym::SymbolId};

use super::{
    prelude::*,
    state::{Lookahead, StitchableParseState, TransitionData},
};
use std::{fmt::Display, marker::PhantomData};

pub trait ExpandingParseState<T: Token, O: Object> =
    ParseState<Token = T, Object = Expansion<T, O>>;

/// Represents an expansion operation on some source token of type `T`.
///
/// See variants and [`ExpandingParseState`] for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum Expansion<T, O: Object> {
    /// A token of type `O` has been derived from the source token and
    ///   should be merged into the target token stream.
    Expanded(O),

    /// Expansion is complete and the source token should be replaced with
    ///   the inner `T`.
    DoneExpanding(T),
}

impl<T: Token, O: Object> Object for Expansion<T, O> {}

/// A [`ClosedParseState`] that is able to serve as an expansion parser.
///
/// An expansion parser is a parser yielding [`Expansion`],
///   intended to be integrated into another token stream.
pub trait ExpandableParseState<O: Object> = ClosedParseState
where
    O: Token + Eq,
    Self: ParseState<Object = Expansion<<Self as ParseState>::Token, O>>;

/// An [`ExpandableParseState`] capable of expanding into the token stream
///   of a parent [`ParseState`] `SP`.
///
/// This trait asserts that an [`ExpandableParseState`] is a
///   [`StitchableParseState<SP>`](StitchableParseState) after being wrapped
///   by [`StitchableExpansionState`].
pub trait ExpandableInto<SP: ParseState> =
    ExpandableParseState<<SP as ParseState>::Object>
    where
        StitchableExpansionState<Self, <SP as ParseState>::Object>:
            StitchableParseState<SP>;

/// Convert a [`ClosedParseState`] yielding an [`Expansion<T,O>`](Expansion)
///   object into a parser yielding `O` with a dead state yielding `T`.
///
/// It is more convenient and clear to write parsers using [`Expansion`],
///   since those variants not only state directly what the intent of the
///     operations are,
///   but also avoid having to work with dead states.
/// However,
///   their wrapping in [`Expansion`] makes them difficult to delegate to
///     (compose with)
///   other parsers using [`ParseState`]'s `delegate_*` family of
///   functions.
///
/// This parser handles this translation by stripping away the
///   [`Expansion`] abstraction and producing a [`ParseState`] that looks
///   and acts like what would have been implemented in the absence of such
///   an abstraction.
#[derive(Debug, PartialEq, Eq)]
pub struct StitchableExpansionState<S: ClosedParseState, O: Object> {
    st: S,
    _phantom: PhantomData<O>,
}

// We implement Default if the parser `S` that we're wrapping does.
impl<S: ClosedParseState, O: Object> Default for StitchableExpansionState<S, O>
where
    S: Default,
{
    fn default() -> Self {
        Self {
            st: Default::default(),
            _phantom: Default::default(),
        }
    }
}

impl<S: ClosedParseState, O: Object> ParseState
    for StitchableExpansionState<S, O>
where
    S: ExpandableParseState<O>,
{
    type Token = S::Token;
    type Object = O;
    type Error = S::Error;
    type Context = S::Context;

    #[inline]
    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use Expansion::*;

        match self {
            Self { st, _phantom } => {
                let TransitionResult(Transition(st_new), data) =
                    st.parse_token(tok, ctx);

                let data_new = data.map_when_obj(|obj, la| match (obj, la) {
                    (Expanded(obj), la) => {
                        TransitionData::Result(Ok(ParseStatus::Object(obj)), la)
                    }

                    // A parser must never throw away lookahead tokens.
                    // Since we are converting the `DoneExpanding` variant
                    //   into a lookahead token,
                    //     we would have nothing to do with a token of
                    //     lookahead if one were provided to us.
                    // Ideally this would be prevented using types,
                    //   but such a change is too much effort at the time of
                    //   writing.
                    (DoneExpanding(tok), Some(Lookahead(la_tok))) => {
                        let desc = vec![
                            tok.span().note(
                                "while processing this \
                                    Expansion::DoneExpanding token",
                            ),
                            la_tok.span().internal_error(
                                "encountered this unexpected lookahead token",
                            ),
                        ];

                        diagnostic_panic!(
                            desc,
                            "cannot provide lookahead token with \
                                Expansion::DoneExpanding",
                        )
                    }

                    (DoneExpanding(tok), None) => {
                        TransitionData::Dead(Lookahead(tok))
                    }
                });

                TransitionResult(
                    Transition(Self {
                        st: st_new,
                        _phantom,
                    }),
                    data_new,
                )
            }
        }
    }

    fn is_accepting(&self, ctx: &Self::Context) -> bool {
        self.st.is_accepting(ctx)
    }
}

impl<S: ClosedParseState, O: Object> Display
    for StitchableExpansionState<S, O>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self {
                st: parser,
                _phantom,
            } => {
                write!(f, "{parser}, with Expansion stripped")
            }
        }
    }
}

/// A [`SymbolId`] with a corresponding [`Span`].
///
/// This newtype is required because foreign traits
///   (such as [`Display`])
///   cannot be implemented on tuples at the time of writing.
#[derive(Debug, PartialEq, Eq)]
pub struct SPair(pub SymbolId, pub Span);

impl Token for SPair {
    fn ir_name() -> &'static str {
        "Generic Symbol"
    }

    fn span(&self) -> Span {
        match self {
            Self(_, span) => *span,
        }
    }
}

impl Display for SPair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(sym, _) => Display::fmt(sym, f),
        }
    }
}

impl From<(SymbolId, Span)> for SPair {
    fn from((sym, span): (SymbolId, Span)) -> Self {
        Self(sym, span)
    }
}

impl Into<(SymbolId, Span)> for SPair {
    fn into(self) -> (SymbolId, Span) {
        match self {
            Self(sym, span) => (sym, span),
        }
    }
}

#[cfg(test)]
mod test;
