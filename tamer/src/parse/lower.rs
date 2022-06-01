// Lowering operation between parsers
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

//! IR lowering operation between [`Parser`]s.

use super::{ParseError, ParseState, Parsed, ParsedResult, Parser, Token};
use crate::iter::{TripIter, TrippableIterator};
use std::iter;

#[cfg(doc)]
use super::TokenStream;

/// An IR lowering operation that pipes the output of one [`Parser`] to the
///   input of another.
///
/// This is produced by [`Lower`].
pub struct LowerIter<'a, 'b, S, I, LS>
where
    S: ParseState,
    I: Iterator<Item = ParsedResult<S>>,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    /// A push [`Parser`].
    lower: Parser<LS, iter::Empty<LS::Token>>,

    /// Source tokens from higher-level [`Parser`],
    ///   with the outer [`Result`] having been stripped by a [`TripIter`].
    toks: &'a mut TripIter<
        'b,
        I,
        Parsed<S::Object>,
        ParseError<S::Token, S::Error>,
    >,
}

impl<'a, 'b, S, I, LS> LowerIter<'a, 'b, S, I, LS>
where
    S: ParseState,
    I: Iterator<Item = ParsedResult<S>>,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    /// Consume inner parser and yield its context.
    #[inline]
    fn finalize(self) -> Result<LS::Context, ParseError<LS::Token, LS::Error>> {
        self.lower.finalize().map_err(|(_, e)| e)
    }
}

/// Lowering operation from one [`ParseState`] to another.
pub trait Lower<S, LS>
where
    S: ParseState,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    /// Lower the IR produced by this [`Parser`] into another IR by piping
    ///   the output to a new parser defined by the [`ParseState`] `LS`.
    ///
    /// This parser consumes tokens `S::Token` and produces the IR
    ///   `S::Output`.
    /// If there is some other [`ParseState`] `LS` such that
    ///   `LS::Token == S::Output`
    ///     (that is—the output of this parser is the input to another),
    ///     then this method will wire the two together into a new iterator
    ///       that produces `LS::Output`.
    ///
    /// Visually, we have,
    ///   within the provided closure `f`,
    ///   a [`LowerIter`] that acts as this pipeline:
    ///
    /// ```text
    /// (S::Token) -> (S::Output == LS::Token) -> (LS::Output)
    /// ```
    ///
    /// The new iterator is a [`LowerIter`],
    ///   and scoped to the provided closure `f`.
    /// The outer [`Result`] of `Self`'s [`ParsedResult`] is stripped by
    ///   a [`TripIter`] before being provided as input to a new push
    ///   [`Parser`] utilizing `LS`.
    /// A push parser,
    ///   rather than pulling tokens from a [`TokenStream`],
    ///   has tokens pushed into it;
    ///     this parser is created automatically for you.
    ///
    /// _TODO_: There's no way to access the inner parser for error recovery
    ///   after tripping the [`TripIter`].
    /// Consequently,
    ///   this API (likely the return type) will change.
    #[inline]
    fn lower<U, E>(
        &mut self,
        f: impl FnOnce(&mut LowerIter<S, Self, LS>) -> Result<U, E>,
    ) -> Result<U, E>
    where
        Self: Iterator<Item = ParsedResult<S>> + Sized,
        <LS as ParseState>::Context: Default,
        ParseError<S::Token, S::Error>: Into<E>,
        ParseError<LS::Token, LS::Error>: Into<E>,
    {
        self.while_ok(|toks| {
            // TODO: This parser is not accessible after error recovery!
            let lower = LS::parse(iter::empty());
            let mut iter = LowerIter { lower, toks };
            f(&mut iter)
        })
        .map_err(Into::into)
    }

    /// Perform a lowering operation between two parsers where the context
    ///   is both received and returned.
    ///
    /// This allows state to be shared among parsers.
    ///
    /// See [`Lower::lower`] and [`ParseState::parse_with_context`] for more
    ///   information.
    fn lower_with_context<U, E>(
        &mut self,
        ctx: LS::Context,
        f: impl FnOnce(&mut LowerIter<S, Self, LS>) -> Result<U, E>,
    ) -> Result<(U, LS::Context), E>
    where
        Self: Iterator<Item = ParsedResult<S>> + Sized,
        ParseError<S::Token, S::Error>: Into<E>,
        ParseError<LS::Token, LS::Error>: Into<E>,
    {
        self.while_ok(|toks| {
            let lower = LS::parse_with_context(iter::empty(), ctx);
            let mut iter = LowerIter { lower, toks };
            let val = f(&mut iter)?;

            iter.finalize().map_err(Into::into).map(|ctx| (val, ctx))
        })
    }
}

impl<S, LS, I> Lower<S, LS> for I
where
    I: Iterator<Item = ParsedResult<S>> + Sized,
    S: ParseState,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
}

impl<'a, 'b, S, I, LS> Iterator for LowerIter<'a, 'b, S, I, LS>
where
    S: ParseState,
    I: Iterator<Item = ParsedResult<S>>,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    type Item = ParsedResult<LS>;

    /// Pull a token through the higher-level [`Parser`],
    ///   push it to the lowering parser,
    ///   and yield the resulting [`ParsedResult`].
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.toks.next() {
            None => None,
            Some(Parsed::Incomplete) => Some(Ok(Parsed::Incomplete)),
            Some(Parsed::Object(obj)) => Some(self.lower.feed_tok(obj)),
        }
    }
}
