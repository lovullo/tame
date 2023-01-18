// Basic streaming parsing framework
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

//! Generic errors and container for State-specific parsing errors.

use super::Token;
use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    span::Span,
};
use std::{error::Error, fmt::Display};

#[cfg(doc)]
use super::{ParseState, ParseStatus, Parser};

/// Common parsing errors produced by [`Parser`].
///
/// These errors are common enough that they are handled in a common way,
///   such that individual parsers needn't check for these situations
///   themselves.
///
/// Having a common type also allows combinators to handle error types in a
///   consistent way when composing parsers.
///
/// Parsers may return their own unique errors via the
///   [`StateError`][ParseError::StateError] variant.
#[derive(Debug, PartialEq)]
pub enum ParseError<T: Token, E: Diagnostic + PartialEq> {
    /// The parser reached an unhandled dead state.
    ///
    /// For more information,
    ///   see [`ParseState::delegate`] and [`Parser::feed_tok`].
    ///
    /// The string is intended to describe what was expected to have been
    ///   available based on the current [`ParseState`].
    /// It is a heap-allocated string so that a copy of [`ParseState`]
    ///   needn't be stored.
    UnexpectedToken(T, String),

    /// A parser-specific error associated with an inner
    ///   [`ParseState`].
    StateError(E),

    /// The parser has no more input,
    ///   but it failed to automatically finalize.
    ///
    /// See [`Parser::finalize`] for more information.
    FinalizeError(FinalizeError),
}

impl<T: Token, EA: Diagnostic + PartialEq> ParseError<T, EA> {
    pub fn inner_into<EB: Diagnostic + PartialEq + Eq>(
        self,
    ) -> ParseError<T, EB>
    where
        EA: Into<EB>,
    {
        use ParseError::*;
        match self {
            UnexpectedToken(x, desc) => UnexpectedToken(x, desc),
            StateError(e) => StateError(e.into()),
            FinalizeError(e) => FinalizeError(e),
        }
    }
}

//impl<T: Token, E: Diagnostic + PartialEq> From<E> for ParseError<T, E> {
//    fn from(e: E) -> Self {
//        Self::StateError(e)
//    }
//}

impl<T: Token, E: Diagnostic + PartialEq> From<FinalizeError>
    for ParseError<T, E>
{
    fn from(e: FinalizeError) -> Self {
        Self::FinalizeError(e)
    }
}

impl<T: Token, E: Diagnostic + PartialEq> Display for ParseError<T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(tok, desc) => {
                write!(f, "unexpected {} while {desc}", TtQuote::wrap(tok))
            }
            Self::StateError(e) => Display::fmt(e, f),
            Self::FinalizeError(e) => Display::fmt(e, f),
        }
    }
}

impl<T: Token, E: Diagnostic + PartialEq + 'static> Error for ParseError<T, E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::UnexpectedToken(_, _) => None,
            Self::StateError(e) => Some(e),
            Self::FinalizeError(e) => Some(e),
        }
    }
}

impl<T: Token, E: Diagnostic + PartialEq + 'static> Diagnostic
    for ParseError<T, E>
{
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use ParseError::*;

        match self {
            UnexpectedToken(tok, desc) => tok.span().error(desc).into(),
            // TODO: Is there any additional useful context we can augment
            //   this with?
            StateError(e) => e.describe(),
            FinalizeError(e) => e.describe(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FinalizeError {
    /// Token stream ended unexpectedly.
    ///
    /// This error means that the parser was expecting more input before
    ///   reaching an accepting state.
    /// This could represent a truncated file,
    ///   a malformed stream,
    ///   or maybe just a user that's not done typing yet
    ///     (e.g. in the case of an LSP implementation).
    ///
    /// If no span is available,
    ///   then parsing has not even had the chance to begin.
    /// If this parser follows another,
    ///   then the combinator ought to substitute a missing span with
    ///   whatever span preceded this invocation.
    ///
    /// The string is intended to describe what was expected to have been
    ///   available based on the current [`ParseState`].
    /// It is a heap-allocated string so that a copy of [`ParseState`]
    ///   needn't be stored.
    UnexpectedEof(Span, String),

    /// The parser contains an outstanding token of lookahead that is no
    ///   longer
    ///     (or possibly never was)
    ///   part of the token stream,
    ///     and would therefore be lost if the parser is finalized.
    ///
    /// The parser must consume the next token,
    ///   which will be the token of lookahead,
    ///   after which it may finalize provided that it is in an accepting
    ///     state.
    ///
    /// See [`Parser::take_lookahead_tok`] for more information.
    Lookahead(Span, String),
}

impl Display for FinalizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof(_, desc) => {
                write!(f, "unexpected end of input while {desc}")
            }
            // This is not really something the user should have to deal
            //   with,
            //     but maybe this will provide enough information that the
            //     user can alter the input in such a way as to avoid this
            //     condition.
            // It likely represents a bug in the parser,
            //   or at the very least poor handling of unexpected input.
            Self::Lookahead(_, desc) => {
                write!(
                    f,
                    "internal error: attempt to finalize parsing with \
                       outstanding token of lookahead while {desc}"
                )
            }
        }
    }
}

impl Error for FinalizeError {}

impl Diagnostic for FinalizeError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use FinalizeError::*;

        match self {
            UnexpectedEof(span, desc) => span.error(desc).into(),
            Lookahead(span, desc) => span.error(desc).into(),
        }
    }
}
