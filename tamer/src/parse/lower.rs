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

use super::{
    NoContext, Object, ParseError, ParseState, Parsed, ParsedResult, Parser,
    Token, TransitionResult, UnknownToken,
};
use crate::{
    diagnose::Diagnostic,
    iter::{TripIter, TrippableIterator},
};
use std::{fmt::Display, iter, marker::PhantomData};

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
///
/// Lowering is intended to be used between standalone [`ParseState`]s that
///   implement [`Default`].
pub trait Lower<S, LS>
where
    S: ParseState,
    LS: ParseState<Token = S::Object> + Default,
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
    LS: ParseState<Token = S::Object> + Default,
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
        let tok = self
            .lower
            .take_lookahead_tok()
            .map(Parsed::Object)
            .or_else(|| self.toks.next());

        match tok {
            None => None,
            Some(Parsed::Incomplete) => Some(Ok(Parsed::Incomplete)),
            Some(Parsed::Object(obj)) => Some(self.lower.feed_tok(obj)),
        }
    }
}

/// Representation of a [`ParseState`] producing some type of [`Object`].
///
/// This is intended to be used not as a value,
///   but as a type for lowering operations.
/// This is useful when a parser does not make use of [`ParseState`] but
///   still wishes to participate in a lowering pipeline.
/// The type of [`Token`] will always be [`UnknownToken`],
///   so this is only useful at the head of such a pipeline.
#[derive(Debug)]
pub struct ParsedObject<O: Object, E: Diagnostic + PartialEq> {
    _phantom: PhantomData<(O, E)>,
}

impl<O: Object, E: Diagnostic + PartialEq> PartialEq for ParsedObject<O, E> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<O: Object, E: Diagnostic + PartialEq> Eq for ParsedObject<O, E> {}

impl<O: Object, E: Diagnostic + PartialEq> Default for ParsedObject<O, E> {
    fn default() -> Self {
        Self {
            _phantom: Default::default(),
        }
    }
}

impl<O: Object, E: Diagnostic + PartialEq> Display for ParsedObject<O, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<generic data>")
    }
}

impl<O: Object, E: Diagnostic + PartialEq> ParseState for ParsedObject<O, E> {
    type Token = UnknownToken;
    type Object = O;
    type Error = E;

    fn parse_token(
        self,
        _tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        unreachable!("ParsedObject must be used for type information only")
    }

    fn is_accepting(&self) -> bool {
        unreachable!("ParsedObject must be used for type information only")
    }
}

// See `super::test` for more information on why there are so few tests
//   here.
#[cfg(test)]
mod test {
    use super::super::{
        parser::test::{StubError, StubObject, StubParseState, StubToken},
        Transition,
    };
    use super::*;

    #[derive(Debug, PartialEq, Eq, Default)]
    struct StubEchoParseState {}

    impl Display for StubEchoParseState {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "StubEchoParseState")
        }
    }

    impl ParseState for StubEchoParseState {
        type Token = StubToken;
        type Object = StubToken;
        type Error = StubError;

        fn parse_token(
            self,
            tok: Self::Token,
            _: &mut Self::Context,
        ) -> TransitionResult<Self> {
            Transition(self).ok(tok)
        }

        fn is_accepting(&self) -> bool {
            true
        }
    }

    // Similar to tests in parse::parser::test.
    #[test]
    fn can_emit_object_with_lookahead_for_lower_iter() {
        let given = 27; // some value
        let toks = vec![StubToken::YieldWithLookahead(given)];

        Lower::<StubEchoParseState, StubParseState>::lower(
            &mut StubEchoParseState::parse(toks.into_iter()),
            |sut| {
                // We have a single token,
                //   and this consumes it,
                //   but it should introduce a lookahead token.
                assert_eq!(
                    sut.next(),
                    Some(Ok(Parsed::Object(StubObject::FromYield(given))))
                );

                // The token of lookahead should still be available to the parser,
                //   and this should consume it.
                assert_eq!(
                    sut.next(),
                    Some(Ok(Parsed::Object(StubObject::FromLookahead(given)))),
                    "lookahead token did not take effect"
                );

                // And now this should be the end,
                //   provided that the lookahead token was actually consumed and not
                //   copied and retained.
                assert_eq!(
                    sut.next(),
                    None,
                    "expected end of both input stream and lookahead"
                );

                Ok::<(), StubError>(())
            },
        )
        .unwrap();
    }
}
