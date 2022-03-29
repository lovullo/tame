// Basic streaming parsing framework
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

//! Basic streaming parser framework for lowering operations.
//!
//! _TODO: Some proper docs and examples!_

use crate::iter::{TripIter, TrippableIterator};
use crate::span::Span;
use std::fmt::Debug;
use std::hint::unreachable_unchecked;
use std::iter::{self, Empty};
use std::mem::take;
use std::ops::{ControlFlow, FromResidual, Try};
use std::{convert::Infallible, error::Error, fmt::Display};

/// Result of applying a [`Token`] to a [`ParseState`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParsedResult<S> = ParseResult<S, Parsed<<S as ParseState>::Object>>;

/// Result of some non-parsing operation on a [`Parser`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParseResult<S, T> =
    Result<T, ParseError<<S as ParseState>::Token, <S as ParseState>::Error>>;

/// A single datum from a streaming IR with an associated [`Span`].
///
/// A token may be a lexeme with associated data,
///   or a more structured object having been lowered from other IRs.
pub trait Token: Display + Debug + PartialEq + Eq {
    /// Retrieve the [`Span`] representing the source location of the token.
    fn span(&self) -> Span;
}

impl<T: Token> From<T> for Span {
    fn from(tok: T) -> Self {
        tok.span()
    }
}

/// An IR object produced by a lowering operation on one or more [`Token`]s.
///
/// Note that an [`Object`] may also be a [`Token`] if it will be in turn
///   fed to another [`Parser`] for lowering.
///
/// This trait exists to disambiguate an otherwise unbounded type for
///   [`From`] conversions,
///     used in the [`Transition`] API to provide greater flexibility.
pub trait Object: Debug + PartialEq + Eq {}

/// An infallible [`Token`] stream.
///
/// If the token stream originates from an operation that could potentially
///   fail and ought to be propagated,
///     use [`TokenResultStream`].
///
/// The name "stream" in place of "iterator" is intended to convey that this
///   type is expected to be processed in real-time as a stream,
///     not read into memory.
pub trait TokenStream<T: Token> = Iterator<Item = T>;

/// A [`Token`] stream that may encounter errors during parsing.
///
/// If the stream cannot fail,
///   consider using [`TokenStream`].
pub trait TokenResultStream<T: Token, E: Error> = Iterator<Item = Result<T, E>>;

/// A deterministic parsing automaton.
///
/// These states are utilized by a [`Parser`].
///
/// A [`ParseState`] is also responsible for storing data about the
///   accepted input,
///     and handling appropriate type conversions into the final type.
/// That is---an
///   automaton may store metadata that is subsequently emitted once an
///   accepting state has been reached.
/// Whatever the underlying automaton,
///   a `(state, token)` pair must uniquely determine the next parser
///   action.
///
/// Intuitively,
///   since only one [`Parser`] may hold a mutable reference to
///   an underlying [`TokenStream`] at any given point,
///   this does in fact represent the current state of the entire
///     [`TokenStream`] at the current position for a given parser
///     composition.
pub trait ParseState: Default + PartialEq + Eq + Debug {
    /// Input tokens to the parser.
    type Token: Token;

    /// Objects produced by a parser utilizing these states.
    type Object: Object;

    /// Errors specific to this set of states.
    type Error: Debug + Error + PartialEq + Eq;

    /// Construct a parser.
    ///
    /// Whether this method is helpful or provides any clarity depends on
    ///   the context and the types that are able to be inferred.
    fn parse<I: TokenStream<Self::Token>>(toks: I) -> Parser<Self, I> {
        Parser::from(toks)
    }

    /// Parse a single [`Token`] and optionally perform a state transition.
    ///
    /// The current state is represented by `self`.
    /// The result of a parsing operation is a state transition with
    ///   associated [`ParseStatus`] data.
    ///
    /// Note that `self` is owned,
    ///   for a couple primary reasons:
    ///
    ///   1. This forces the parser to explicitly consider and document all
    ///        state transitions,
    ///          rather than potentially missing unintended behavior through
    ///          implicit behavior; and
    ///   2. It allows for more natural functional composition of state,
    ///        which in turn makes it easier to compose parsers
    ///          (which conceptually involves stitching together state
    ///            machines).
    fn parse_token(self, tok: Self::Token) -> TransitionResult<Self>;

    /// Whether the current state represents an accepting state.
    ///
    /// An accepting state represents a valid state to stop parsing.
    /// If parsing stops at a state that is _not_ accepting,
    ///   then the [`TokenStream`] has ended unexpectedly and should produce
    ///   a [`ParseError::UnexpectedEof`].
    ///
    /// It makes sense for there to be exist multiple accepting states for a
    ///   parser.
    /// For example:
    ///   A parser that parses a list of attributes may be used to parse one
    ///   or more attributes,
    ///     or the entire list of attributes.
    ///   It is acceptable to attempt to parse just one of those attributes,
    ///     or it is acceptable to parse all the way until the end.
    fn is_accepting(&self) -> bool;

    /// Delegate parsing from a compatible, stitched [`ParseState`]~`SP`.
    ///
    /// This helps to combine two state machines that speak the same input
    ///   language
    ///   (share the same [`Self::Token`]),
    ///     handling the boilerplate of delegating [`Self::Token`] from a
    ///     parent state~`SP` to `Self`.
    ///
    /// Token delegation happens after [`Self`] has been entered from a
    ///   parent [`ParseState`] context~`SP`,
    ///     so stitching the start and accepting states must happen elsewhere
    ///     (for now).
    ///
    /// This assumes that no lookahead token from [`ParseStatus::Dead`] will
    ///   need to be handled by the parent state~`SP`.
    /// To handle a token of lookahead,
    ///   use [`Self::delegate_lookahead`] instead.
    ///
    /// _TODO: More documentation once this is finalized._
    fn delegate<C, SP>(
        self,
        context: C,
        tok: Self::Token,
        into: impl FnOnce(C, Self) -> SP,
    ) -> TransitionResult<SP>
    where
        SP: ParseState<Token = Self::Token>,
        Self::Object: Into<<SP as ParseState>::Object>,
        Self::Error: Into<<SP as ParseState>::Error>,
    {
        use ParseStatus::{Dead, Incomplete, Object as Obj};

        let (Transition(newst), result) = self.parse_token(tok).into();

        // This does not use `delegate_lookahead` so that we can have
        //   `into: impl FnOnce` instead of `Fn`.
        Transition(into(context, newst)).result(match result {
            Ok(Incomplete) => Ok(Incomplete),
            Ok(Obj(obj)) => Ok(Obj(obj.into())),
            Ok(Dead(tok)) => Ok(Dead(tok)),
            Err(e) => Err(e.into()),
        })
    }

    /// Delegate parsing from a compatible, stitched [`ParseState`]~`SP` with
    ///   support for a lookahead token.
    ///
    /// This does the same thing as [`Self::delegate`],
    ///   but allows for the handling of a lookahead token from [`Self`]
    ///   rather than simply proxying [`ParseStatus::Dead`].
    ///
    /// _TODO: More documentation once this is finalized._
    fn delegate_lookahead<C, SP>(
        self,
        context: C,
        tok: Self::Token,
        into: impl FnOnce(C, Self) -> SP,
        lookahead: impl FnOnce(C, Self, Self::Token) -> TransitionResult<SP>,
    ) -> TransitionResult<SP>
    where
        SP: ParseState<Token = Self::Token>,
        Self::Object: Into<<SP as ParseState>::Object>,
        Self::Error: Into<<SP as ParseState>::Error>,
    {
        use ParseStatus::{Dead, Incomplete, Object as Obj};

        let (Transition(newst), result) = self.parse_token(tok).into();

        match result {
            Ok(Incomplete) => Transition(into(context, newst)).incomplete(),
            Ok(Obj(obj)) => Transition(into(context, newst)).ok(obj.into()),
            Ok(Dead(tok)) => lookahead(context, newst, tok),
            Err(e) => Transition(into(context, newst)).err(e),
        }
    }
}

/// Result of applying a [`Token`] to a [`ParseState`].
///
/// This is used by [`ParseState::parse_token`];
///   see that function for rationale.
pub type ParseStateResult<S> = Result<ParseStatus<S>, <S as ParseState>::Error>;

/// A state transition with associated data.
///
/// Conceptually,
///   imagine the act of a state transition producing data.
/// See [`Transition`] for convenience methods for producing this tuple.
#[derive(Debug, PartialEq, Eq)]
pub struct TransitionResult<S: ParseState>(
    pub Transition<S>,
    pub ParseStateResult<S>,
);

/// Denotes a state transition.
///
/// This newtype was created to produce clear, self-documenting code;
///   parsers can get confusing to read with all of the types involved,
///     so this provides a mental synchronization point.
///
/// This also provides some convenience methods to help remote boilerplate
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
        TransitionResult(self, Ok(obj.into()))
    }

    /// A transition with corresponding error.
    ///
    /// This indicates a parsing failure.
    /// The state ought to be suitable for error recovery.
    pub fn err<E: Into<S::Error>>(self, err: E) -> TransitionResult<S> {
        TransitionResult(self, Err(err.into()))
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
        TransitionResult(self, result.map(Into::into).map_err(Into::into))
    }

    /// A state transition indicating that more data is needed before an
    ///   object can be emitted.
    ///
    /// This corresponds to [`ParseStatus::Incomplete`].
    pub fn incomplete(self) -> TransitionResult<S> {
        TransitionResult(self, Ok(ParseStatus::Incomplete))
    }

    /// A dead state transition.
    ///
    /// This corresponds to [`ParseStatus::Dead`],
    ///   and a calling parser should use the provided [`Token`] as
    ///   lookahead.
    pub fn dead(self, tok: S::Token) -> TransitionResult<S> {
        TransitionResult(self, Ok(ParseStatus::Dead(tok)))
    }
}

impl<S: ParseState> Into<(Transition<S>, ParseStateResult<S>)>
    for TransitionResult<S>
{
    fn into(self) -> (Transition<S>, ParseStateResult<S>) {
        (self.0, self.1)
    }
}

impl<S: ParseState> Try for TransitionResult<S> {
    type Output = (Transition<S>, ParseStateResult<S>);
    type Residual = (Transition<S>, ParseStateResult<S>);

    fn from_output(output: Self::Output) -> Self {
        match output {
            (st, result) => Self(st, result),
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self.into() {
            (st, Ok(x)) => ControlFlow::Continue((st, Ok(x))),
            (st, Err(e)) => ControlFlow::Break((st, Err(e))),
        }
    }
}

impl<S: ParseState> FromResidual<(Transition<S>, ParseStateResult<S>)>
    for TransitionResult<S>
{
    fn from_residual(residual: (Transition<S>, ParseStateResult<S>)) -> Self {
        match residual {
            (st, result) => Self(st, result),
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
    toks: I,
    state: S,
    last_span: Option<Span>,
}

impl<S: ParseState, I: TokenStream<S::Token>> Parser<S, I> {
    /// Indicate that no further parsing will take place using this parser,
    ///   and [`drop`] it.
    ///
    /// Invoking the method is equivalent to stating that the stream has
    ///   ended,
    ///     since the parser will have no later opportunity to continue
    ///     parsing.
    /// Consequently,
    ///   the caller should expect [`ParseError::UnexpectedEof`] if the
    ///   parser is not in an accepting state.
    pub fn finalize(
        self,
    ) -> Result<(), (Self, ParseError<S::Token, S::Error>)> {
        self.assert_accepting().map_err(|err| (self, err))
    }

    /// Return [`Ok`] if the parser is in an accepting state,
    ///   otherwise [`Err`] with [`ParseError::UnexpectedEof`].
    ///
    /// See [`finalize`](Self::finalize) for the public-facing method.
    fn assert_accepting(&self) -> Result<(), ParseError<S::Token, S::Error>> {
        if self.state.is_accepting() {
            Ok(())
        } else {
            let span = self.last_span.and_then(|s| s.endpoints().1);
            Err(ParseError::UnexpectedEof(span))
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
    fn feed_tok(&mut self, tok: S::Token) -> ParsedResult<S> {
        // Store the most recently encountered Span for error
        //   reporting in case we encounter an EOF.
        self.last_span = Some(tok.span());

        let result;
        TransitionResult(Transition(self.state), result) =
            take(&mut self.state).parse_token(tok);

        use ParseStatus::*;
        match result {
            // Nothing handled this dead state,
            //   and we cannot discard a lookahead token,
            //   so we have no choice but to produce an error.
            Ok(Dead(invalid)) => Err(ParseError::UnexpectedToken(invalid)),

            Ok(parsed @ (Incomplete | Object(..))) => Ok(parsed.into()),
            Err(e) => Err(e.into()),
        }
    }

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
    pub fn lower_while_ok<LS, U>(
        &mut self,
        f: impl FnOnce(&mut LowerIter<S, I, LS>) -> U,
    ) -> Result<U, ParseError<S::Token, S::Error>>
    where
        LS: ParseState<Token = S::Object>,
        <S as ParseState>::Object: Token,
    {
        self.while_ok(|toks| {
            // TODO: This parser is not accessible after error recovery!
            let lower = LS::parse(iter::empty());
            f(&mut LowerIter { lower, toks })
        })
    }
}

/// An IR lowering operation that pipes the output of one [`Parser`] to the
///   input of another.
///
/// This is produced by [`Parser::lower_while_ok`].
pub struct LowerIter<'a, 'b, S, I, LS>
where
    S: ParseState,
    I: TokenStream<S::Token>,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    /// A push [`Parser`].
    lower: Parser<LS, Empty<LS::Token>>,

    /// Source tokens from higher-level [`Parser`],
    ///   with the outer [`Result`] having been stripped by a [`TripIter`].
    toks: &'a mut TripIter<
        'b,
        Parser<S, I>,
        Parsed<S::Object>,
        ParseError<S::Token, S::Error>,
    >,
}

impl<'a, 'b, S, I, LS> Iterator for LowerIter<'a, 'b, S, I, LS>
where
    S: ParseState,
    I: TokenStream<S::Token>,
    LS: ParseState<Token = S::Object>,
    <S as ParseState>::Object: Token,
{
    type Item = ParsedResult<LS>;

    /// Pull a token through the higher-level [`Parser`],
    ///   push it to the lowering parser,
    ///   and yield the resulting [`ParseResult`].
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.toks.next() {
            None => None,
            Some(Parsed::Incomplete) => Some(Ok(Parsed::Incomplete)),
            Some(Parsed::Object(obj)) => Some(self.lower.feed_tok(obj)),
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
#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<T: Token, E: Error + PartialEq + Eq> {
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
    UnexpectedEof(Option<Span>),

    /// The parser reached an unhandled dead state.
    ///
    /// Once a parser returns [`ParseStatus::Dead`],
    ///   a parent context must use that provided token as a lookahead.
    /// If that does not occur,
    ///   [`Parser`] produces this error.
    ///
    /// In the future,
    ///   it may be desirable to be able to query [`ParseState`] for what
    ///   tokens are acceptable at this point,
    ///     to provide better error messages.
    UnexpectedToken(T),

    /// A parser-specific error associated with an inner
    ///   [`ParseState`].
    StateError(E),
}

impl<T: Token, EA: Error + PartialEq + Eq> ParseError<T, EA> {
    pub fn inner_into<EB: Error + PartialEq + Eq>(self) -> ParseError<T, EB>
    where
        EA: Into<EB>,
    {
        use ParseError::*;
        match self {
            UnexpectedEof(x) => UnexpectedEof(x),
            UnexpectedToken(x) => UnexpectedToken(x),
            StateError(e) => StateError(e.into()),
        }
    }
}

impl<T: Token, E: Error + PartialEq + Eq> From<E> for ParseError<T, E> {
    fn from(e: E) -> Self {
        Self::StateError(e)
    }
}

impl<T: Token, E: Error + PartialEq + Eq> Display for ParseError<T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof(ospan) => {
                write!(f, "unexpected end of input at ")?;

                match ospan {
                    None => write!(f, "<unknown location>"),
                    Some(span) => write!(f, "{}", span),
                }
            }
            Self::UnexpectedToken(tok) => {
                write!(f, "unexpected {}", tok)
            }
            Self::StateError(e) => Display::fmt(e, f),
        }
    }
}

impl<T: Token, E: Error + PartialEq + Eq + 'static> Error for ParseError<T, E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::StateError(e) => Some(e),
            _ => None,
        }
    }
}

impl<S: ParseState, I: TokenStream<S::Token>> From<I> for Parser<S, I> {
    fn from(toks: I) -> Self {
        Self {
            toks,
            state: Default::default(),
            last_span: None,
        }
    }
}

/// Result of a parsing operation.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseStatus<S: ParseState> {
    /// Additional tokens are needed to complete parsing of the next object.
    Incomplete,

    /// Parsing of an object is complete.
    ///
    /// This does not indicate that the parser is complete,
    ///   as more objects may be able to be emitted.
    Object(S::Object),

    /// Parser encountered a dead state relative to the given token.
    ///
    /// A dead state is an empty accepting state that has no state
    ///   transition for the given token.
    /// A state is empty if a [`ParseStatus::Object`] will not be lost if
    ///   parsing ends at this point
    ///     (that is---there is no partially-built object).
    /// This could simply mean that the parser has completed its job and
    ///   that control must be returned to a parent context.
    ///
    /// If a parser is _not_ in an accepting state,
    ///   then an error ought to occur rather than a dead state;
    ///     the difference between the two is that the token associated with
    ///       a dead state can be used as a lookahead token in order to
    ///       produce a state transition at a higher level,
    ///     whereas an error indicates that parsing has failed.
    /// Intuitively,
    ///   this means that a [`ParseStatus::Object`] had just been emitted
    ///   and that the token following it isn't something that can be
    ///   parsed.
    ///
    /// If there is no parent context to handle the token,
    ///   [`Parser`] must yield an error.
    Dead(S::Token),
}

impl<S: ParseState<Object = T>, T: Object> From<T> for ParseStatus<S> {
    fn from(obj: T) -> Self {
        Self::Object(obj)
    }
}

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

#[cfg(test)]
pub mod test {
    use std::{assert_matches::assert_matches, iter::once};

    use super::*;
    use crate::{span::DUMMY_SPAN as DS, sym::GlobalSymbolIntern};

    #[derive(Debug, PartialEq, Eq, Clone)]
    enum TestToken {
        Close(Span),
        Comment(Span),
        Text(Span),
    }

    impl Display for TestToken {
        fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unimplemented!("fmt::Display")
        }
    }

    impl Token for TestToken {
        fn span(&self) -> Span {
            use TestToken::*;
            match self {
                Close(span) | Comment(span) | Text(span) => *span,
            }
        }
    }

    impl Object for TestToken {}

    #[derive(Debug, PartialEq, Eq)]
    enum EchoState {
        Empty,
        Done,
    }

    impl Default for EchoState {
        fn default() -> Self {
            Self::Empty
        }
    }

    impl ParseState for EchoState {
        type Token = TestToken;
        type Object = TestToken;
        type Error = EchoStateError;

        fn parse_token(self, tok: TestToken) -> TransitionResult<Self> {
            match tok {
                TestToken::Comment(..) => Transition(Self::Done).ok(tok),
                TestToken::Close(..) => {
                    Transition(self).err(EchoStateError::InnerError(tok))
                }
                TestToken::Text(..) => Transition(self).dead(tok),
            }
        }

        fn is_accepting(&self) -> bool {
            *self == Self::Done
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    enum EchoStateError {
        InnerError(TestToken),
    }

    impl Display for EchoStateError {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unimplemented!()
        }
    }

    impl Error for EchoStateError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }

    type Sut<I> = Parser<EchoState, I>;

    #[test]
    fn successful_parse_in_accepting_state_with_spans() {
        // EchoState is placed into a Done state given Comment.
        let tok = TestToken::Comment(DS);
        let mut toks = once(tok.clone());

        let mut sut = Sut::from(&mut toks);

        // The first token should be processed normally.
        // EchoState proxies the token back.
        assert_eq!(Some(Ok(Parsed::Object(tok))), sut.next());

        // This is now the end of the token stream,
        //   which should be okay provided that the first token put us into
        //   a proper accepting state.
        assert_eq!(None, sut.next());

        // Further, finalizing should work in this state.
        assert!(sut.finalize().is_ok());
    }

    #[test]
    fn fails_on_end_of_stream_when_not_in_accepting_state() {
        let span = Span::new(10, 20, "ctx".intern());
        let mut toks = [TestToken::Close(span)].into_iter();

        let mut sut = Sut::from(&mut toks);

        // The first token is fine,
        //   and allows us to acquire our most recent span.
        sut.next();

        // Given that we have no tokens,
        //   and that EchoState::default does not start in an accepting
        //     state,
        //   we must fail when we encounter the end of the stream.
        assert_eq!(
            Some(Err(ParseError::UnexpectedEof(span.endpoints().1))),
            sut.next()
        );
    }

    #[test]
    fn returns_state_specific_error() {
        // TestToken::Close causes EchoState to produce an error.
        let errtok = TestToken::Close(DS);
        let mut toks = [errtok.clone()].into_iter();

        let mut sut = Sut::from(&mut toks);

        assert_eq!(
            Some(Err(ParseError::StateError(EchoStateError::InnerError(
                errtok
            )))),
            sut.next()
        );

        // The token must have been consumed.
        // It is up to a recovery process to either bail out or provide
        //   recovery tokens;
        //     continuing without recovery is unlikely to make sense.
        assert_eq!(0, toks.len());
    }

    #[test]
    fn fails_when_parser_is_finalized_in_non_accepting_state() {
        let span = Span::new(10, 10, "ctx".intern());

        // Set up so that we have a single token that we can use for
        //   recovery as part of the same iterator.
        let recovery = TestToken::Comment(DS);
        let mut toks = [
            // Used purely to populate a Span.
            TestToken::Close(span),
            // Recovery token here:
            recovery.clone(),
        ]
        .into_iter();

        let mut sut = Sut::from(&mut toks);

        // Populate our most recently seen token's span.
        sut.next();

        // Attempting to finalize now in a non-accepting state should fail
        //   in the same way that encountering an end-of-stream does,
        //     since we're effectively saying "we're done with the stream"
        //     and the parser will have no further opportunity to reach an
        //     accepting state.
        let result = sut.finalize();
        assert_matches!(
            result,
            Err((_, ParseError::UnexpectedEof(s))) if s == span.endpoints().1
        );

        // The sut should have been re-returned,
        //   allowing for attempted error recovery if the caller can manage
        //   to produce a sequence of tokens that will be considered valid.
        // `toks` above is set up already for this,
        //   which allows us to assert that we received back the same `sut`.
        let mut sut = result.unwrap_err().0;
        assert_eq!(Some(Ok(Parsed::Object(recovery))), sut.next());

        // And so we should now be in an accepting state,
        //   able to finalize.
        assert!(sut.finalize().is_ok());
    }

    #[test]
    fn unhandled_dead_state_results_in_error() {
        // A Text will cause our parser to return Dead.
        let tok = TestToken::Text(DS);
        let mut toks = once(tok.clone());

        let mut sut = Sut::from(&mut toks);

        // Our parser returns a Dead status,
        //   which is unhandled by any parent context
        //     (since we're not composing parsers),
        //     which causes an error due to an unhandled Dead state.
        assert_eq!(sut.next(), Some(Err(ParseError::UnexpectedToken(tok))),);
    }
}
