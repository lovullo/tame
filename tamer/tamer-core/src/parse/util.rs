// TAMER parsing framework utilities
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

//! Utilities that make parsing practical and convenient in different
//!   contexts.
//!
//! The utilities presented here do not introduce any new capabilities into
//!   the system;
//!     they provide wrappers around core functionality that make it easier
//!     to use outside of the domain of the parsing system itself.

use super::{prelude::*, state::TransitionData};
use crate::{f::Map, span::Span, sym::SymbolId};
use std::fmt::Display;

/// A [`SymbolId`] with a corresponding [`Span`].
///
/// This newtype is required because foreign traits
///   (such as [`Display`])
///   cannot be implemented on tuples at the time of writing.
///
/// This type provides some notable benefits:
///
///   - [`Display`]ing an [`SPair`] will render the [`SymbolId`]'s
///       interned string,
///         and the same [`SPair`] when used with
///         [`crate::diagnose::Annotate`] will provide annotated spans;
///           this allows [`SPair`] to be used without destructuring in
///           diagnostic messages.
///   - [`Span`]s are explicitly coupled with their corresponding
///       [`SymbolId`],
///         reducing complexity when many spans are in play.
///   - [`SPair`] implements [`Token`],
///       and so can serve as an ad-hoc IR if appropriate.
///
/// This implements [`Copy`] because each inner type is small
///   (and itself [`Copy`]),
///   and in practice,
///     we'd just destructure it to copy each part anyway.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SPair(pub SymbolId, pub Span);

/// More concisely construct an [`SPair`] from [`SymbolId`]- and
///   [`Span`]-like things.
///
/// This is restricted to `cfg(test)` because it can cause unexpected
///   internment if you're not careful.
/// For example,
///   if a [`str`] is assigned to a variable and then that variable is
///   supplied to multiple calls to this function,
///     each call will invoke the internment system.
/// This isn't much of a concern for short-running tests,
///   but is not acceptable elsewhere.
#[cfg(test)]
pub fn spair(sym: impl Into<SymbolId>, span: impl Into<Span>) -> SPair {
    SPair(sym.into(), span.into())
}

impl SPair {
    /// Retrieve the [`SymbolId`] of this pair.
    ///
    /// This is an alternative to pattern matching.
    pub fn symbol(&self) -> SymbolId {
        match self {
            Self(sym, _) => *sym,
        }
    }
}

impl Map<SymbolId> for SPair {
    /// Map over the [`SymbolId`] of the pair while retaining the original
    ///   associated [`Span`].
    ///
    /// Span retention is the desired behavior when modifying the source
    ///   code the user entered,
    ///     since diagnostic messages will reference the original source
    ///     location that the modification was derived from.
    fn map(self, f: impl FnOnce(SymbolId) -> SymbolId) -> Self {
        match self {
            Self(sym, span) => Self(f(sym), span),
        }
    }
}

impl Map<Span> for SPair {
    /// Map over the [`Span`] of the pair while retaining the associated
    ///   [`SymbolId`].
    ///
    /// This operation is useful,
    ///   for example,
    ///   when resolving or overriding identifiers.
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self {
        match self {
            Self(sym, span) => Self(sym, f(span)),
        }
    }
}

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

impl From<SPair> for (SymbolId, Span) {
    fn from(val: SPair) -> Self {
        match val {
            SPair(sym, span) => (sym, span),
        }
    }
}

impl From<SPair> for SymbolId {
    fn from(spair: SPair) -> Self {
        spair.symbol()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct EchoParseState<S: ClosedParseState>(S);

impl<S: ClosedParseState> ParseState for EchoParseState<S> {
    type Token = S::Token;
    type Object = S::Object;
    type Error = S::Error;
    type Context = S::Context;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        match self {
            Self(st) => st
                .parse_token(tok, ctx)
                .bimap(Self, TransitionData::reflexivity),
        }
    }

    fn is_accepting(&self, ctx: &Self::Context) -> bool {
        match self {
            Self(st) => st.is_accepting(ctx),
        }
    }
}

impl<S: ClosedParseState> Display for EchoParseState<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(st) => Display::fmt(st, f),
        }
    }
}
