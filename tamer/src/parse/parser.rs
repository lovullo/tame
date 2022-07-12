// High-level parser
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

//! High-level parsing abstraction.

use super::{
    ParseError, ParseResult, ParseState, ParseStatus, TokenStream, Transition,
    TransitionResult,
};
use crate::{
    parse::state::{Lookahead, TransitionData},
    span::{Span, UNKNOWN_SPAN},
};

#[cfg(doc)]
use super::Token;

/// Result of applying a [`Token`] to a [`ParseState`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParsedResult<S> = ParseResult<S, Parsed<<S as ParseState>::Object>>;

/// Result of a parsing operation.
///
/// Whereas [`ParseStatus`] is used by [`ParseState`] to influence parser
///   operation,
///     this type is public-facing and used by [`Parser`].
#[derive(Debug, PartialEq, Eq)]
pub enum Parsed<O> {
    /// Additional tokens are needed to complete parsing of the next object.
    Incomplete,

    /// Parsing of an object is complete.
    ///
    /// This does not indicate that the parser is complete,
    ///   as more objects may be able to be emitted.
    Object(O),
}

impl<S: ParseState> From<ParseStatus<S>> for Parsed<S::Object> {
    fn from(status: ParseStatus<S>) -> Self {
        match status {
            ParseStatus::Incomplete => Parsed::Incomplete,
            ParseStatus::Object(x) => Parsed::Object(x),
        }
    }
}

/// A streaming parser defined by a [`ParseState`] with exclusive
///   mutable access to an underlying [`TokenStream`].
///
/// This parser handles operations that are common among all types of
///   parsers,
///     such that specialized parsers need only implement logic that is
///     unique to their operation.
/// This also simplifies combinators,
///   since there is more uniformity among distinct parser types.
///
/// After you have finished with a parser,
///   if you have not consumed the entire iterator,
///   call [`finalize`](Parser::finalize) to ensure that parsing has
///     completed in an accepting state.
#[derive(Debug, PartialEq)]
pub struct Parser<S: ParseState, I: TokenStream<S::Token>> {
    /// Input token stream to be parsed by the [`ParseState`] `S`.
    toks: I,

    /// Token of lookahead to serve as the next input to the parser in place
    ///   of the next token of `toks`.
    ///
    /// See [`take_lookahead_tok`](Parser::take_lookahead_tok) for more
    ///   information.
    lookahead: Option<Lookahead<S>>,

    /// Parsing automaton.
    ///
    /// This [`ParseState`] is stored within an [`Option`] to allow for
    ///   taking ownership for [`ParseState::parse_token`];
    ///     this is where the functional [`ParseState`] is married with
    ///       mutable state.
    ///
    /// The alternative here is to forego [`Option`] and mandate [`Default`]
    ///   on [`ParseState`],
    ///     which used to be the case in an older implementation.
    /// However,
    ///   while that requirement was nice to have here,
    ///   it forces [`ParseState`] to be initialized without context,
    ///     which significantly reduces the quality of diagnostic output
    ///     unless augmented by the calling context,
    ///       which puts a much greater burden on the caller.
    ///
    /// This will only ever contain [`None`] during a the call to
    ///   [`ParseState::parse_token`] in [`Parser::feed_tok`],
    ///     so it is safe to call [`unwrap`](Option::unwrap) without
    ///     worrying about panics.
    /// This is also why Dead states require transitions,
    ///   given that [`ParseState`] does not implement [`Default`].
    ///
    /// For more information,
    ///   see the implementation of [`Parser::feed_tok`].
    state: Option<S>,

    /// The span of the last read [`Token`] from the [`TokenStream`] `I`,
    ///   used to provide context for diagnostic output.
    last_span: Span,

    /// Mutable context provided by mutable reference to each invocation of
    ///   [`ParseState::parse_token`].
    ///
    /// This should be avoided in favor of functional, immutable parsers
    ///   unless you have a reason to make use of it;
    ///     it was originally added for situations where Rust is unable to
    ///     elide the move of [`Parser::state`] in [`Parser::feed_tok`].
    ctx: S::Context,
}

impl<S: ParseState, I: TokenStream<S::Token>> Parser<S, I> {
    /// Create a parser with a pre-initialized [`ParseState`].
    ///
    /// If the provided [`ParseState`] does not require context
    ///   (and so implements [`Default`]),
    ///     [`ParseState::parse`] or `Parser::from` may be preferable to
    ///     this function.
    pub fn with_state(state: S, toks: I) -> Self
    where
        S::Context: Default,
    {
        Self {
            toks,
            lookahead: None,
            state: Some(state),
            last_span: UNKNOWN_SPAN,
            ctx: Default::default(),
        }
    }

    /// Indicate that no further parsing will take place using this parser,
    ///   retrieve any final aggregate state (the context),
    ///   and [`drop`] it.
    ///
    /// Invoking the method is equivalent to stating that the stream has
    ///   ended,
    ///     since the parser will have no later opportunity to continue
    ///     parsing.
    /// Consequently,
    ///   the caller should expect [`ParseError::UnexpectedEof`] if the
    ///   parser is not in an accepting state.
    ///
    /// To re-use the context returned by this method,
    ///   see [`ParseState::parse_with_context`].
    /// Note that whether the context is permitted to be reused,
    ///   or is useful independently to the caller,
    ///   is a decision made by the [`ParseState`].
    pub fn finalize(
        self,
    ) -> Result<S::Context, (Self, ParseError<S::Token, S::Error>)> {
        match self.assert_accepting() {
            Ok(()) => Ok(self.ctx),
            Err(err) => Err((self, err)),
        }
    }

    /// Return [`Ok`] if the parser both has no outstanding lookahead token
    ///   and is in an accepting state,
    ///     otherwise [`Err`] with [`ParseError::UnexpectedEof`].
    ///
    /// See [`finalize`](Self::finalize) for the public-facing method.
    fn assert_accepting(&self) -> Result<(), ParseError<S::Token, S::Error>> {
        let st = self.state.as_ref().unwrap();

        if let Some(Lookahead(lookahead)) = &self.lookahead {
            Err(ParseError::Lookahead(lookahead.span(), st.to_string()))
        } else if st.is_accepting() {
            Ok(())
        } else {
            let endpoints = self.last_span.endpoints();
            Err(ParseError::UnexpectedEof(
                endpoints.1.unwrap_or(endpoints.0),
                st.to_string(),
            ))
        }
    }

    /// Feed an input token to the parser.
    ///
    /// This _pushes_ data into the parser,
    ///   rather than the typical pull system used by [`Parser`]'s
    ///   [`Iterator`] implementation.
    /// The pull system also uses this method to provided data to the
    ///   parser.
    ///
    /// This method is intentionally private,
    ///   since push parsers are currently supported only internally.
    /// The only thing preventing this being public is formalization and a
    ///   commitment to maintain it.
    ///
    /// Recursion Warning
    /// -----------------
    /// If a [`ParseState`] yields an incomplete parse along with a token of
    ///   lookahead,
    ///     this will immediately recurse with that token;
    ///       that situation is common with [`ParseState::delegate`].
    /// This is intended as an optimization to save a wasteful
    ///   [`Parsed::Incomplete`] from being propagated down the entire
    ///   lowering pipeline,
    ///     but it could potentially result in unbounded recursion if a
    ///     misbehaving [`ParseState`] continuously yields the same token of
    ///     lookahead.
    /// Such behavior would be incorrect,
    ///   but would otherwise result in recursion across the entire lowering
    ///   pipeline.
    ///
    /// A [`ParseState`] should never yield a token of lookahead unless
    ///   consuming that same token will result in either a state transition
    ///   or a dead state.
    ///
    /// Panics
    /// ------
    /// This uses a debug assertion to enforce the invariant
    ///   `self.lookahead.is_none()`.
    /// Failure to consume the lookahead token using
    ///   [`take_lookahead_tok`](Parser::take_lookahead_tok) and provide to
    ///   this method would result in the loss of data.
    /// This is something that can only be done by the caller.
    pub(super) fn feed_tok(&mut self, tok: S::Token) -> ParsedResult<S> {
        // Store the most recently encountered Span for error
        //   reporting in case we encounter an EOF.
        self.last_span = tok.span();

        // Lookahead tokens must be consumed before invoking this method,
        //   otherwise they will be overwritten and lost.
        // See doc block above for more information.
        debug_assert!(
            self.lookahead.is_none(),
            "lookahead token is available but was not consumed",
        );

        // Parse a single token and perform the requested state transition.
        //
        // This is where the functional `ParseState` is married with a
        //   mutable abstraction.
        // Rust will optimize away this take+move+replace under most
        //   circumstances;
        //     if it does not,
        //       then you should utilize `ctx` as necessary.
        //
        // Note that this used to use `mem::take`,
        //   and the generated assembly was identical in both cases.
        //
        // Note also that this is what Dead states require transitions.
        let TransitionResult(Transition(state), data) =
            self.state.take().unwrap().parse_token(tok, &mut self.ctx);
        self.state.replace(state);

        use ParseStatus::{Incomplete, Object};
        match data {
            // Nothing handled this dead state,
            //   and we cannot discard a lookahead token,
            //   so we have no choice but to produce an error.
            TransitionData::Dead(Lookahead(invalid)) => {
                Err(ParseError::UnexpectedToken(
                    invalid,
                    self.state.as_ref().unwrap().to_string(),
                ))
            }

            // If provided a token of lookahead and an incomplete parse,
            //   then just try again right away and avoid propagating this
            //   delay throughout the entire lowering pipeline.
            // This is likely to happen on a dead state transition during
            //   parser delegation
            //     (see [`ParseState::delegate`]).
            // This will only result in unbounded recursion if the parser
            //   continues to yield the same token of lookahead
            //   continuously,
            //     which represents an implementation flaw in the parser.
            TransitionData::Result(
                Ok(Incomplete),
                Some(Lookahead(lookahead)),
            ) => self.feed_tok(lookahead),

            TransitionData::Result(result, lookahead) => {
                self.lookahead = lookahead;

                match result {
                    Ok(parsed @ (Incomplete | Object(..))) => Ok(parsed.into()),
                    Err(e) => Err(e.into()),
                }
            }
        }
    }

    /// Retrieve a single token of lookahead,
    ///   if any,
    ///   to be used in place of the next token from the input stream.
    ///
    /// The term "lookahead" here means that a token has been read from an
    ///   input stream,
    ///     has been used to make a state determination,
    ///     but parsing the token is incomplete.
    /// For example,
    ///   the token may have been used to recognize that an aggregating
    ///   parser has finished its work,
    ///     but now the token must be provided to the parent parser;
    ///       in this sense,
    ///         the token was used for context but not substance.
    /// Such an operation is a byproduct of parser composition,
    ///   where inner parsers determine when they are complete and,
    ///     in doing so,
    ///     have an extra token of input that they have not consumed.
    /// This is like a "rest" argument to parser combinators,
    ///   except "rest" here is the single token of lookahead + the
    ///   remainder of the token stream that has not yet been read.
    ///
    /// Not all parsers produce lookahead tokens,
    ///   but all parsers have that option;
    ///     Rust will optimize away unneeded functionality appropriately.
    /// It is essential that the parser use this token if it is available,
    ///   since it will be replaced
    ///     (possibly with [`None`])
    ///     after the next parsing step.
    /// [`feed_tok`](Parser::feed_tok) will panic if a lookahead token is
    ///   available and is not consumed by this method.
    /// This calling order could be enforced via Rust's type system,
    ///   but doing so would require a bit of work that may not be worth it
    ///   for how few systems have to worry about this.
    ///
    /// Note that there is no protection against infinitely looping on the
    ///   same token---it
    ///     is the responsibility of the parser to ensure that such a thing
    ///     does not occur by ensuring that the token is properly consumed
    ///     and not re-emitted as a lookahead without also changing state.
    /// Parser generator macros may have their own checks in place that
    ///   prevent this type of thing from happening,
    ///     but proving correctness is better left to proof systems than
    ///     Rust's type system.
    pub(super) fn take_lookahead_tok(&mut self) -> Option<S::Token> {
        self.lookahead.take().map(|Lookahead(tok)| tok)
    }
}

impl<S: ParseState, I: TokenStream<S::Token>> Iterator for Parser<S, I> {
    type Item = ParsedResult<S>;

    /// Parse a single [`Token`] according to the current
    ///   [`ParseState`],
    ///     if available.
    ///
    /// If the underlying [`TokenStream`] yields [`None`],
    ///   then the [`ParseState`] must be in an accepting state;
    ///     otherwise, [`ParseError::UnexpectedEof`] will occur.
    ///
    /// This is intended to be invoked by [`Iterator::next`].
    /// Accepting a token rather than the [`TokenStream`] allows the caller
    ///   to inspect the token first
    ///     (e.g. to store a copy of the [`Span`][crate::span::Span]).
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let otok = self.take_lookahead_tok().or_else(|| self.toks.next());

        match otok {
            None => match self.assert_accepting() {
                Ok(()) => None,
                Err(e) => Some(Err(e)),
            },

            Some(tok) => Some(self.feed_tok(tok)),
        }
    }
}

impl<S, I> From<I> for Parser<S, I>
where
    S: ParseState + Default,
    I: TokenStream<S::Token>,
    <S as ParseState>::Context: Default,
{
    /// Create a new parser with a default context.
    ///
    /// This can only be used if the associated [`ParseState::Context`] does
    ///   not implement [`Default`];
    ///     otherwise,
    ///       consider instantiating from a `(TokenStream, Context)` pair.
    /// See also [`ParseState::parse`] and
    ///   [`ParseState::parse_with_context`].
    ///
    /// If [`ParseState`] does not implement [`Default`],
    ///   see [`Parser::with_state`].
    fn from(toks: I) -> Self {
        Self {
            toks,
            lookahead: None,
            state: Some(Default::default()),
            last_span: UNKNOWN_SPAN,
            ctx: Default::default(),
        }
    }
}

impl<S, I, C> From<(I, C)> for Parser<S, I>
where
    S: ParseState<Context = C> + Default,
    I: TokenStream<S::Token>,
{
    /// Create a new parser with a provided context.
    ///
    /// For more information,
    ///   see [`ParseState::parse_with_context`].
    ///
    /// See also [`ParseState::parse`].
    fn from((toks, ctx): (I, C)) -> Self {
        Self {
            toks,
            lookahead: None,
            state: Some(Default::default()),
            last_span: UNKNOWN_SPAN,
            ctx,
        }
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::{
        diagnose::Diagnostic,
        parse::{Object, Token},
        span::DUMMY_SPAN,
    };
    use std::{assert_matches::assert_matches, error::Error, fmt::Display};

    ///! [`Parser`] unit tests.
    ///!
    ///! Note that this system is comprehensively tested via integration
    ///!   tests by concrete parser implementations.
    ///! The reason for this is historical:
    ///!   the implementation was quite volatile and the refactoring cost
    ///!   was too high with unit tests when the system had complete test
    ///!   coverage via integration tests.
    ///! The upside of this approach is that this system is complex to test
    ///!   in isolation and so integration tests save a lot of development
    ///!   time and frustration,
    ///!     but the downside is that any regressions will potentially
    ///!     manifest as errors in every parser that uses this framework.
    ///! Eventually,
    ///!   parser generator macros may be derived (as an abstraction) that
    ///!   will make testing this framework significantly less effort,
    ///!     and perhaps then it will be worth implementing more unit
    ///!     tests now that the implementation has settled.
    ///!
    ///! Generally speaking,
    ///!   since this framework is written exclusively for TAMER,
    ///!   features that are no longer used ought to be pruned,
    ///!     and so there should be no features that are not tested by
    ///!     parsers' tests.
    ///! Consequently,
    ///!   features ought to be developed alongside the parsers that require
    ///!   those features.

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum StubToken {
        YieldWithLookahead(usize),
        Lookahead(usize),
        Foo,
    }

    impl Token for StubToken {
        fn span(&self) -> Span {
            DUMMY_SPAN
        }
    }

    impl Object for StubToken {}

    impl Display for StubToken {
        fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
            unimplemented!()
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum StubError {}

    impl Error for StubError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }

    impl Display for StubError {
        fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
            unimplemented!()
        }
    }

    impl Diagnostic for StubError {
        fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
            unimplemented!()
        }
    }

    impl From<ParseError<StubToken, StubError>> for StubError {
        fn from(_: ParseError<StubToken, StubError>) -> Self {
            unimplemented!()
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum StubObject {
        FromYield(usize),
        FromLookahead(usize),
    }

    impl Object for StubObject {}

    #[derive(Debug, PartialEq, Eq, Default)]
    pub struct StubParseState {}

    impl Display for StubParseState {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "StubParseState")
        }
    }

    impl ParseState for StubParseState {
        type Token = StubToken;
        type Object = StubObject;
        type Error = StubError;

        fn parse_token(
            self,
            tok: Self::Token,
            _: &mut Self::Context,
        ) -> TransitionResult<Self> {
            match tok {
                StubToken::YieldWithLookahead(val) => Transition(self)
                    .ok(StubObject::FromYield(val))
                    .with_lookahead(StubToken::Lookahead(val)),

                StubToken::Lookahead(val) => {
                    Transition(self).ok(StubObject::FromLookahead(val))
                }
                _ => Transition(self).incomplete(),
            }
        }

        fn is_accepting(&self) -> bool {
            true
        }
    }

    // This technically only fails when debug assertions are enabled,
    //   which happens to be the case for tests.
    //
    // The intent is to ensure that implementations working with Parser
    //   actually consider lookahead tokens.
    #[test]
    #[should_panic]
    fn fails_if_lookahead_not_consumed() {
        let given = 64; // a fine value, but not an important one

        let mut sut = StubParseState::parse(std::iter::empty());

        // Feed a token that will force a lookahead token to be stored.
        assert_eq!(
            sut.feed_tok(StubToken::YieldWithLookahead(given)),
            Ok(Parsed::Object(StubObject::FromYield(given)))
        );

        // If we now attempt to feed another token without consuming
        //   the lookahead,
        //     it should panic.
        let _ = sut.feed_tok(StubToken::Foo);
    }

    // This is the API that will be used by implementations utilizing
    //   Parser to utilize lookahead tokens.
    #[test]
    fn succeeds_if_lookahead_is_consumed() {
        let given = 63; // one less than a fine value, but not important

        let mut sut = StubParseState::parse(std::iter::empty());

        // Given that this is a fresh parser,
        //   we shouldn't have any lookahead,
        //   but let's make sure that we don't cause any problems by
        //     checking.
        assert!(sut.take_lookahead_tok().is_none());

        // Feed a token that will force a lookahead token to be stored.
        assert_eq!(
            sut.feed_tok(StubToken::YieldWithLookahead(given)),
            Ok(Parsed::Object(StubObject::FromYield(given)))
        );

        // Consume the lookahead token that was generated above.
        let la_tok = sut.take_lookahead_tok();
        assert_eq!(la_tok, Some(StubToken::Lookahead(given)));

        // The _proper_ thing to do here would be to push the lookahead
        //   token we retrieved above.
        // However,
        //   all we care about for this test is that we're able to push some
        //   sort of token without an error occurring.
        let _ = sut.feed_tok(StubToken::Foo);
    }

    #[test]
    fn cannot_finalize_with_outstanding_lookahead() {
        let given = 32; // half of a fine number that is pretty fine itself

        let mut sut = StubParseState::parse(std::iter::empty());

        // Feed a token that will force a lookahead token to be stored.
        assert_eq!(
            sut.feed_tok(StubToken::YieldWithLookahead(given)),
            Ok(Parsed::Object(StubObject::FromYield(given)))
        );

        // Even though our token stream is empty,
        //   and even though we are in an accepting state,
        //   we should _not_ be able to finalize,
        //     since doing so would discard a token.
        // The parser is forced to consume it and,
        //   if that results in an error,
        //   that's fine---at
        //     least it was not ignored.
        let (_, err) = sut
            .finalize()
            .expect_err("must not finalize with token of lookahead");

        assert_matches!(err, ParseError::Lookahead(span, _) if span == DUMMY_SPAN);
    }

    // Tests the above,
    //   but using the Iterator API.
    #[test]
    fn can_emit_object_with_lookahead_for_iter_parser() {
        let given = 27; // some value
        let toks = vec![StubToken::YieldWithLookahead(given)];

        let mut sut = StubParseState::parse(toks.into_iter());

        // We have a single token,
        //   and this consumes it,
        //   but it should introduce a lookahead token.
        assert_eq!(
            sut.next(),
            Some(Ok(Parsed::Object(StubObject::FromYield(given))))
        );

        // Normally this would be the end of the token stream,
        //   but we should have a token of lookahead.
        let (mut sut, err) = sut
            .finalize()
            .expect_err("must not finalize with token of lookahead");

        assert_matches!(err, ParseError::Lookahead(span, _) if span == DUMMY_SPAN);

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
    }
}
