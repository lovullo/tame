// Parsing automaton
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

//! State transitions for parser automata.

use super::{ParseState, ParseStateResult, ParseStatus, Token};
use std::{
    convert::Infallible,
    hint::unreachable_unchecked,
    ops::{ControlFlow, FromResidual},
};

#[cfg(doc)]
use super::Parser;

/// A state transition with associated data.
///
/// Conceptually,
///   imagine the act of a state transition producing data.
/// See [`Transition`] for convenience methods for producing this tuple.
///
/// Sometimes a parser is not able to complete the operation requested
///   based on the provided input token.
/// Since TAMER uses a streaming parsing framework that places strict
///   limits on control flow,
///     a single token can be returned as lookahead to indicate that the
///     token could not be parsed yet and should be provided once again
///     in place of the next token from the input stream.
/// This allows,
///   for example,
///   for multiple data to be emitted in response to a single token.
///
/// This struct is opaque to ensure that critical invariants involving
///   transitions and lookahead are properly upheld;
///     callers must use the appropriate parsing APIs.
#[derive(Debug, PartialEq)]
pub struct TransitionResult<S: ParseState>(
    /// New parser state.
    pub(in super::super) Transition<S>,
    /// Result of the parsing operation.
    pub(in super::super) TransitionData<S>,
);

impl<S: ParseState> TransitionResult<S> {
    /// Indicate that this transition include a single token of lookahead,
    ///   which should be provided back to the parser in place of the
    ///   next token from the input stream.
    pub fn with_lookahead(self, lookahead: S::Token) -> Self {
        match self {
            Self(transition, TransitionData::Result(result, None)) => Self(
                transition,
                TransitionData::Result(result, Some(Lookahead(lookahead))),
            ),

            // This represents a problem with the parser;
            //   we should never specify a lookahead token more than once.
            // This could be enforced statically with the type system if
            //   ever such a thing is deemed to be worth doing.
            Self(
                ..,
                TransitionData::Result(_, Some(prev))
                | TransitionData::Dead(prev),
            ) => {
                panic!("internal error: lookahead token overwrite: {prev:?}")
            }
        }
    }

    /// Possibly indicate that this transition includes a single token of
    ///   lookahead.
    ///
    /// If the argument is [`None`],
    ///   this returns `self` unchanged.
    ///
    /// This is useful when working with the output of other parsers.
    /// See [`with_lookahead`](TransitionResult::with_lookahead) for more
    ///   information.
    pub(in super::super) fn maybe_with_lookahead(
        self,
        lookahead: Option<Lookahead<S::Token>>,
    ) -> Self {
        match lookahead {
            Some(Lookahead(lookahead)) => self.with_lookahead(lookahead),
            None => self,
        }
    }
}

/// Token to use as a lookahead token in place of the next token from the
///   input stream.
#[derive(Debug, PartialEq)]
pub struct Lookahead<T: Token>(pub(in super::super) T);

/// Information about the state transition.
///
/// Note: Ideally a state wouldn't even be required for
///   [`Dead`](TransitionData::Dead),
///     but [`ParseState`] does not implement [`Default`] and [`Parser`]
///     requires _some_ state exist.
#[derive(Debug, PartialEq)]
pub(in super::super) enum TransitionData<S: ParseState> {
    /// State transition was successful or not attempted,
    ///   with an optional token of [`Lookahead`].
    ///
    /// Note that a successful state transition _does not_ imply a
    ///   successful [`ParseStateResult`]---the
    ///     parser may choose to successfully transition into an error
    ///     recovery state to accommodate future tokens.
    Result(ParseStateResult<S>, Option<Lookahead<S::Token>>),

    /// No valid state transition exists from the current state for the
    ///   given input token,
    ///     which is returned as a token of [`Lookahead`].
    ///
    /// A dead state is an accepting state that has no state transition for
    ///   the given token.
    /// This could simply mean that the parser has completed its job and
    ///   that control must be returned to a parent context.
    /// Note that this differs from an error state,
    ///   where a parser is unable to reach an accepting state because it
    ///   received unexpected input.
    ///
    /// Note that the parser may still choose to perform a state transition
    ///   for the sake of error recovery,
    ///     but note that the dead state is generally interpreted to mean
    ///       "I have no further work that I am able to perform"
    ///       and may lead to finalization of the parser.
    /// If a parser intends to do additional work,
    ///   it should return an error instead via [`TransitionData::Result`].
    Dead(Lookahead<S::Token>),
}

/// A verb denoting a state transition.
///
/// This is typically instantiated directly by a [`ParseState`] to perform a
///   state transition in [`ParseState::parse_token`].
///
/// This newtype was created to produce clear, self-documenting code;
///   parsers can get confusing to read with all of the types involved,
///     so this provides a mental synchronization point.
///
/// This also provides some convenience methods to help remove boilerplate
///   and further improve code clarity.
#[derive(Debug, PartialEq, Eq)]
pub struct Transition<S: ParseState>(pub S);

impl<S: ParseState> Transition<S> {
    /// A state transition with corresponding data.
    ///
    /// This allows [`ParseState::parse_token`] to emit a parsed object and
    ///   corresponds to [`ParseStatus::Object`].
    pub fn ok<T>(self, obj: T) -> TransitionResult<S>
    where
        T: Into<ParseStatus<S>>,
    {
        TransitionResult(self, TransitionData::Result(Ok(obj.into()), None))
    }

    /// A transition with corresponding error.
    ///
    /// This indicates a parsing failure.
    /// The state ought to be suitable for error recovery.
    pub fn err<E: Into<S::Error>>(self, err: E) -> TransitionResult<S> {
        TransitionResult(self, TransitionData::Result(Err(err.into()), None))
    }

    /// A state transition with corresponding [`Result`].
    ///
    /// This translates the provided [`Result`] in a manner equivalent to
    ///   [`Transition::ok`] and [`Transition::err`].
    pub fn result<T, E>(self, result: Result<T, E>) -> TransitionResult<S>
    where
        T: Into<ParseStatus<S>>,
        E: Into<S::Error>,
    {
        TransitionResult(
            self,
            TransitionData::Result(
                result.map(Into::into).map_err(Into::into),
                None,
            ),
        )
    }

    /// A state transition indicating that more data is needed before an
    ///   object can be emitted.
    ///
    /// This corresponds to [`ParseStatus::Incomplete`].
    pub fn incomplete(self) -> TransitionResult<S> {
        TransitionResult(
            self,
            TransitionData::Result(Ok(ParseStatus::Incomplete), None),
        )
    }

    /// A state transition could not be performed and parsing will not
    ///   continue.
    ///
    /// A dead state represents an _accepting state_ that has no edge to
    ///   another state for the given `tok`.
    /// Rather than throw an error,
    ///   a parser uses this status to indicate that it has completed
    ///   parsing and that the token should be utilized elsewhere;
    ///     the provided token will be used as a token of [`Lookahead`].
    ///
    /// If a parser is not prepared to be finalized and needs to yield an
    ///   object first,
    ///     use [`Transition::result`] or other methods along with a token
    ///     of [`Lookahead`].
    pub fn dead(self, tok: S::Token) -> TransitionResult<S> {
        TransitionResult(self, TransitionData::Dead(Lookahead(tok)))
    }
}

impl<S: ParseState> FromResidual<(Transition<S>, ParseStateResult<S>)>
    for TransitionResult<S>
{
    fn from_residual(residual: (Transition<S>, ParseStateResult<S>)) -> Self {
        match residual {
            (st, result) => Self(st, TransitionData::Result(result, None)),
        }
    }
}

impl<S: ParseState> FromResidual<Result<Infallible, TransitionResult<S>>>
    for TransitionResult<S>
{
    fn from_residual(
        residual: Result<Infallible, TransitionResult<S>>,
    ) -> Self {
        match residual {
            Err(e) => e,
            // SAFETY: This match arm doesn't seem to be required in
            //   core::result::Result's FromResidual implementation,
            //     but as of 1.61 nightly it is here.
            // Since this is Infallable,
            //   it cannot occur.
            Ok(_) => unsafe { unreachable_unchecked() },
        }
    }
}

impl<S: ParseState> FromResidual<ControlFlow<TransitionResult<S>, Infallible>>
    for TransitionResult<S>
{
    fn from_residual(
        residual: ControlFlow<TransitionResult<S>, Infallible>,
    ) -> Self {
        match residual {
            ControlFlow::Break(result) => result,
            // SAFETY: Infallible, so cannot hit.
            ControlFlow::Continue(_) => unsafe { unreachable_unchecked() },
        }
    }
}

/// An object able to be used as data for a state [`Transition`].
///
/// This flips the usual order of things:
///   rather than using a method of [`Transition`] to provide data,
///     this starts with the data and produces a transition from it.
/// This is sometimes necessary to satisfy ownership/borrowing rules.
///
/// This trait simply removes boilerplate associated with storing
///   intermediate values and translating into the resulting type.
pub trait Transitionable<S: ParseState> {
    /// Perform a state transition to `S` using [`Self`] as the associated
    ///   data.
    ///
    /// This may be necessary to satisfy ownership/borrowing rules when
    ///   state data from `S` is used to compute [`Self`].
    fn transition(self, to: S) -> TransitionResult<S>;
}

impl<S, E> Transitionable<S> for Result<ParseStatus<S>, E>
where
    S: ParseState,
    <S as ParseState>::Error: From<E>,
{
    fn transition(self, to: S) -> TransitionResult<S> {
        Transition(to).result(self)
    }
}

impl<S, E> Transitionable<S> for Result<(), E>
where
    S: ParseState,
    <S as ParseState>::Error: From<E>,
{
    fn transition(self, to: S) -> TransitionResult<S> {
        Transition(to).result(self.map(|_| ParseStatus::Incomplete))
    }
}

impl<S: ParseState> Transitionable<S> for ParseStatus<S> {
    fn transition(self, to: S) -> TransitionResult<S> {
        Transition(to).ok(self)
    }
}
