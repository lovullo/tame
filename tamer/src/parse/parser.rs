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
use crate::span::{Span, UNKNOWN_SPAN};

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
            ParseStatus::Dead(_) => {
                unreachable!("Dead status must be filtered by Parser")
            }
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
#[derive(Debug, PartialEq, Eq)]
pub struct Parser<S: ParseState, I: TokenStream<S::Token>> {
    /// Input token stream to be parsed by the [`ParseState`] `S`.
    toks: I,

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
    ) -> Result<S::Context, (Self, ParseError<S::DeadToken, S::Error>)> {
        match self.assert_accepting() {
            Ok(()) => Ok(self.ctx),
            Err(err) => Err((self, err)),
        }
    }

    /// Return [`Ok`] if the parser is in an accepting state,
    ///   otherwise [`Err`] with [`ParseError::UnexpectedEof`].
    ///
    /// See [`finalize`](Self::finalize) for the public-facing method.
    fn assert_accepting(
        &self,
    ) -> Result<(), ParseError<S::DeadToken, S::Error>> {
        if self.state.as_ref().unwrap().is_accepting() {
            Ok(())
        } else {
            let endpoints = self.last_span.endpoints();
            Err(ParseError::UnexpectedEof(
                endpoints.1.unwrap_or(endpoints.0),
                self.state.as_ref().unwrap().to_string(),
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
    pub(super) fn feed_tok(&mut self, tok: S::Token) -> ParsedResult<S> {
        // Store the most recently encountered Span for error
        //   reporting in case we encounter an EOF.
        self.last_span = tok.span();

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
        let TransitionResult(Transition(state), result) =
            self.state.take().unwrap().parse_token(tok, &mut self.ctx);
        self.state.replace(state);

        use ParseStatus::*;
        match result {
            // Nothing handled this dead state,
            //   and we cannot discard a lookahead token,
            //   so we have no choice but to produce an error.
            Ok(Dead(invalid)) => Err(ParseError::UnexpectedToken(
                invalid,
                self.state.as_ref().unwrap().to_string(),
            )),

            Ok(parsed @ (Incomplete | Object(..))) => Ok(parsed.into()),
            Err(e) => Err(e.into()),
        }
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
        let otok = self.toks.next();

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
            state: Some(Default::default()),
            last_span: UNKNOWN_SPAN,
            ctx,
        }
    }
}