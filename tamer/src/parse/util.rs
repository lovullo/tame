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

pub mod expand;

use super::{prelude::*, state::TransitionData};
use crate::{span::Span, sym::SymbolId};
use std::fmt::Display;

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
