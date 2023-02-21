// Lowering operation between parsers
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

//! IR lowering operation between [`Parser`]s.

use super::{
    state::ClosedParseState, FinalizeError, FinalizedParser, NoContext, Object,
    ParseError, ParseState, Parsed, Parser, Token, TransitionResult,
};
use crate::diagnose::Diagnostic;
use std::{fmt::Display, iter, marker::PhantomData};

#[cfg(doc)]
use super::TokenStream;

/// An IR lowering operation that pipes the output of one [`Parser`] to the
///   input of another while propagating errors via a common
///   [`WidenedError`] type `E`.
///
/// This is produced by [`Lower`] methods.
pub struct LowerIter<'a, S, I, LS, E>
where
    S: ParseState,
    I: Iterator<Item = WidenedParsedResult<S, E>>,
    LS: ClosedParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
    E: WidenedError<S, LS>,
{
    /// A push [`Parser`].
    lower: Parser<LS, iter::Empty<LS::Token>>,

    /// Source tokens from higher-level [`Parser`].
    toks: &'a mut I,

    /// `S` is used for its associated types only.
    _phantom: PhantomData<S>,
}

impl<'a, S, I, LS, E> LowerIter<'a, S, I, LS, E>
where
    S: ParseState,
    I: Iterator<Item = WidenedParsedResult<S, E>>,
    LS: ClosedParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
    E: WidenedError<S, LS>,
{
    /// Consume inner parser and yield its context.
    #[inline]
    fn finalize(self) -> Result<FinalizedParser<LS>, FinalizeError> {
        self.lower.finalize().map_err(|(_, e)| e)
    }
}

/// Lowering operation from one [`ParseState`] to another.
///
/// Lowering is intended to be used between standalone [`ParseState`]s that
///   implement [`Default`].
///
/// It is expected that input tokens have already been widened into `E`
///   (a [`WidenedError`]) by a previous lowering operation,
///   or by an introduction parser.
pub trait Lower<S, LS, EW>
where
    S: ParseState,
    LS: ClosedParseState<Token = S::Object> + Default,
    <S as ParseState>::Object: Token,
    EW: WidenedError<S, LS>,
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
    /// A push parser,
    ///   rather than pulling tokens from a [`TokenStream`],
    ///   has tokens pushed into it;
    ///     this parser is created automatically for you.
    ///
    /// All errors from the parser `LS` are widened to the error type `E`,
    ///   which is expected to be an aggregate error type
    ///     (such as a sum type)
    ///     shared by the already-widened `S`-derived input.
    /// Errors are propagated to the caller without lowering.
    #[inline]
    fn lower<U, E>(
        &mut self,
        f: impl FnOnce(&mut LowerIter<S, Self, LS, EW>) -> Result<U, E>,
    ) -> Result<U, E>
    where
        Self: Iterator<Item = WidenedParsedResult<S, EW>> + Sized,
        <LS as ParseState>::Context: Default,
    {
        let lower = LS::parse(iter::empty());
        let mut iter = LowerIter {
            lower,
            toks: self,
            _phantom: PhantomData::default(),
        };
        f(&mut iter)

        // TODO: Finalize!
    }

    /// Perform a lowering operation between two parsers where the context
    ///   is both received and returned.
    ///
    /// This allows state to be shared among parsers.
    ///
    /// See [`Lower::lower`] and [`ParseState::parse_with_context`] for more
    ///   information.
    #[inline]
    fn lower_with_context<U, E>(
        &mut self,
        ctx: LS::Context,
        f: impl FnOnce(&mut LowerIter<S, Self, LS, EW>) -> Result<U, E>,
    ) -> Result<(U, LS::Context), E>
    where
        Self: Iterator<Item = WidenedParsedResult<S, EW>> + Sized,
        E: Diagnostic + From<FinalizeError>,
    {
        let lower = LS::parse_with_context(iter::empty(), ctx);
        let mut iter = LowerIter {
            lower,
            toks: self,
            _phantom: PhantomData::default(),
        };
        let val = f(&mut iter)?;

        // TODO: Further propagate `FinalizedParser`
        iter.finalize()
            .map(FinalizedParser::into_context)
            .map(|ctx| (val, ctx))
            .map_err(E::from)
    }
}

impl<S, LS, E, I> Lower<S, LS, E> for I
where
    I: Iterator<Item = WidenedParsedResult<S, E>> + Sized,
    S: ParseState,
    LS: ClosedParseState<Token = S::Object> + Default,
    <S as ParseState>::Object: Token,
    E: WidenedError<S, LS>,
{
}

impl<'a, S, I, LS, E> Iterator for LowerIter<'a, S, I, LS, E>
where
    S: ParseState,
    I: Iterator<Item = WidenedParsedResult<S, E>>,
    LS: ClosedParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
    E: WidenedError<S, LS>,
{
    type Item = WidenedParsedResult<LS, E>;

    /// Pull a token through the higher-level [`Parser`],
    ///   push it to the lowering parser,
    ///   and yield the lowered result.
    ///
    /// Errors from `LS` are widened into `E`.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        // TODO: This is a maintenance burden with Parser's Iterator impl;
        //   they can easily get out of sync,
        //     as evidenced by the commit introducing this comment.
        let tok = self
            .lower
            .take_lookahead_tok()
            .map(Parsed::Object)
            .map(Ok)
            .or_else(|| self.toks.next())
            .or_else(|| self.lower.eof_tok().map(Parsed::Object).map(Ok));

        match tok {
            // We are done when no tokens remain.
            None => None,

            // Errors have already been widened by the previous lowering
            //   operation.
            Some(Err(e)) => Some(Err(e)),

            // Incomplete parses are simply propagated,
            //   since we have no work to do.
            Some(Ok(Parsed::Incomplete)) => Some(Ok(Parsed::Incomplete)),

            // If a token was successfully parsed,
            //   then we can do our job and lower it.
            // This utilizes the push parser `self.lower`.
            Some(Ok(Parsed::Object(obj))) => {
                Some(self.lower.feed_tok(obj).map_err(Into::into))
            }
        }
    }
}

/// A [`Diagnostic`] error type common to both `S` and `LS`.
///
/// This error type must be able to accommodate error variants from all
///   associated lowering operations.
/// The most obvious example of such an error type is an enum acting as a
///   sum type,
///     where the errors of each lowering operation are contained within
///     separate variants.
///
/// This creates a common type that can be propagated through the lowering
///   pipeline all the way to the calling terminal parser,
///     which may then decide what to do
///       (e.g. report errors and permit recovery,
///         or terminate at the first sign of trouble).
pub trait WidenedError<S: ParseState, LS: ParseState> = Diagnostic
    + From<ParseError<<S as ParseState>::Token, <S as ParseState>::Error>>
    + From<ParseError<<LS as ParseState>::Token, <LS as ParseState>::Error>>;

/// A [`ParsedResult`](super::ParsedResult) with a [`WidenedError`].
pub type WidenedParsedResult<S, E> =
    Result<Parsed<<S as ParseState>::Object>, E>;

/// Representation of a [`ParseState`] producing some type of [`Object`].
///
/// This is intended to be used not as a value,
///   but as a type for lowering operations.
/// This is useful when a parser does not make use of [`ParseState`] but
///   still wishes to participate in a lowering pipeline.
#[derive(Debug)]
pub struct ParsedObject<T: Token, O: Object, E: Diagnostic + PartialEq> {
    _phantom: PhantomData<(T, O, E)>,
}

impl<T: Token, O: Object, E: Diagnostic + PartialEq> PartialEq
    for ParsedObject<T, O, E>
{
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T: Token, O: Object, E: Diagnostic + PartialEq> Eq
    for ParsedObject<T, O, E>
{
}

impl<T: Token, O: Object, E: Diagnostic + PartialEq> Default
    for ParsedObject<T, O, E>
{
    fn default() -> Self {
        Self {
            _phantom: Default::default(),
        }
    }
}

impl<T: Token, O: Object, E: Diagnostic + PartialEq> Display
    for ParsedObject<T, O, E>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<generic data>")
    }
}

impl<T: Token, O: Object, E: Diagnostic + PartialEq> ParseState
    for ParsedObject<T, O, E>
{
    type Token = T;
    type Object = O;
    type Error = E;

    fn parse_token(
        self,
        _tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        unreachable!("ParsedObject must be used for type information only")
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        unreachable!("ParsedObject must be used for type information only")
    }
}

// See `super::test` for more information on why there are so few tests
//   here.
// The robust types are quite effective at demanding coherency in spite of
//   complexity.
#[cfg(test)]
mod test {
    use super::super::{
        parser::test::{StubError, StubObject, StubParseState, StubToken},
        Transition,
    };
    use super::*;

    #[derive(Debug, PartialEq, Eq, Default)]
    enum StubEchoParseState {
        #[default]
        PreEof,
        PostEof,
    }

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
            match tok {
                StubToken::Foo => Transition(Self::PostEof).ok(tok),
                _ => Transition(self).ok(tok),
            }
        }

        fn is_accepting(&self, _: &Self::Context) -> bool {
            true
        }

        fn eof_tok(&self, _ctx: &Self::Context) -> Option<Self::Token> {
            matches!(self, Self::PreEof).then_some(StubToken::Foo)
        }
    }

    // Similar to tests in parse::parser::test.
    #[test]
    fn can_emit_object_with_lookahead_and_eof_for_lower_iter() {
        let given = 27; // some value
        let toks = vec![StubToken::YieldWithLookahead(given)];

        Lower::<StubEchoParseState, StubParseState, _>::lower::<_, StubError>(
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

                // Prior to end,
                //   we give parsers the opportunity to emit an EOF token.
                assert_eq!(
                    sut.next(),
                    Some(Ok(Parsed::Object(StubObject::FromFoo))),
                    "EOF token was note emitted",
                );

                // And now this should be the end,
                //   provided that the lookahead token was actually consumed and not
                //   copied and retained.
                assert_eq!(
                    sut.next(),
                    None,
                    "expected end of both input stream and lookahead"
                );

                Ok(Ok::<(), StubError>(()))
            },
        )
        .unwrap()
        .unwrap();
    }
}
