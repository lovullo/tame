// Basic parsing framework for XIR into XIRT
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

//! Basic streaming parsing framework for XIR lowering operations.

use super::{Token, TokenStream};
use crate::span::Span;
use std::fmt::Debug;
use std::{error::Error, fmt::Display};

/// Result of applying a [`Token`] to a [`ParseState`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParsedResult<S> = ParseResult<S, Parsed<<S as ParseState>::Object>>;

/// Result of some non-parsing operation on a [`Parser`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParseResult<S, T> = Result<T, ParseError<<S as ParseState>::Error>>;

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
    /// Objects produced by a parser utilizing these states.
    type Object;

    /// Errors specific to this set of states.
    type Error: Error + PartialEq + Eq;

    /// Construct a parser.
    ///
    /// Whether this method is helpful or provides any clarity depends on
    ///   the context and the types that are able to be inferred.
    fn parse<I: TokenStream>(toks: I) -> Parser<Self, I> {
        Parser::from(toks)
    }

    /// Parse a single [`Token`] and optionally perform a state transition.
    ///
    /// The current state is represented by `self`,
    ///   which is mutable to allow for a state transition.
    /// The result of a parsing operation is either an object or an
    ///   indication that additional tokens of input are needed;
    ///     see [`Parsed`] for more information.
    fn parse_token(&mut self, tok: Token) -> ParseStateResult<Self>;

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
}

/// Result of applying a [`Token`] to a [`ParseState`].
pub type ParseStateResult<S> =
    Result<ParseStatus<<S as ParseState>::Object>, <S as ParseState>::Error>;

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
pub struct Parser<S: ParseState, I: TokenStream> {
    toks: I,
    state: S,
    last_span: Option<Span>,
}

impl<S: ParseState, I: TokenStream> Parser<S, I> {
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
    pub fn finalize(self) -> Result<(), (Self, ParseError<S::Error>)> {
        if self.state.is_accepting() {
            Ok(())
        } else {
            let span = self.last_span;
            Err((self, ParseError::UnexpectedEof(span)))
        }
    }
}

impl<S: ParseState, I: TokenStream> Iterator for Parser<S, I> {
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
            None if self.state.is_accepting() => None,
            None => Some(Err(ParseError::UnexpectedEof(self.last_span))),
            Some(tok) => {
                // Store the most recently encountered Span for error
                //   reporting in case we encounter an EOF.
                self.last_span = Some(tok.span());

                use ParseStatus::*;
                match self.state.parse_token(tok) {
                    // Nothing handled this dead state,
                    //   and we cannot discard a lookahead token,
                    //   so we have no choice but to produce an error.
                    Ok(Dead(invalid)) => {
                        Some(Err(ParseError::UnexpectedToken(invalid)))
                    }

                    Ok(parsed @ (Incomplete | Object(..))) => {
                        Some(Ok(parsed.into()))
                    }

                    Err(e) => Some(Err(e.into())),
                }
            }
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
pub enum ParseError<E: Error + PartialEq + Eq> {
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
    UnexpectedToken(Token),

    /// A parser-specific error associated with an inner
    ///   [`ParseState`].
    StateError(E),
}

impl<EA: Error + PartialEq + Eq> ParseError<EA> {
    pub fn inner_into<EB: Error + PartialEq + Eq>(self) -> ParseError<EB>
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

impl<E: Error + PartialEq + Eq> From<E> for ParseError<E> {
    fn from(e: E) -> Self {
        Self::StateError(e)
    }
}

impl<E: Error + PartialEq + Eq> Display for ParseError<E> {
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

impl<E: Error + PartialEq + Eq + 'static> Error for ParseError<E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::StateError(e) => Some(e),
            _ => None,
        }
    }
}

impl<S: ParseState, I: TokenStream> From<I> for Parser<S, I> {
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
pub enum ParseStatus<T> {
    /// Additional tokens are needed to complete parsing of the next object.
    Incomplete,

    /// Parsing of an object is complete.
    ///
    /// This does not indicate that the parser is complete,
    ///   as more objects may be able to be emitted.
    Object(T),

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
    Dead(Token),
}

/// Result of a parsing operation.
///
/// Whereas [`ParseStatus`] is used by [`ParseState`] to influence parser
///   operation,
///     this type is public-facing and used by [`Parser`].
#[derive(Debug, PartialEq, Eq)]
pub enum Parsed<T> {
    /// Additional tokens are needed to complete parsing of the next object.
    Incomplete,

    /// Parsing of an object is complete.
    ///
    /// This does not indicate that the parser is complete,
    ///   as more objects may be able to be emitted.
    Object(T),
}

impl<T> From<ParseStatus<T>> for Parsed<T> {
    fn from(status: ParseStatus<T>) -> Self {
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
    use crate::span::DUMMY_SPAN as DS;

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
        type Object = Token;
        type Error = EchoStateError;

        fn parse_token(&mut self, tok: Token) -> ParseStateResult<Self> {
            match tok {
                Token::Comment(..) => {
                    *self = Self::Done;
                }
                Token::Close(..) => {
                    return Err(EchoStateError::InnerError(tok))
                }
                Token::Text(..) => return Ok(ParseStatus::Dead(tok)),
                _ => {}
            }

            Ok(ParseStatus::Object(tok))
        }

        fn is_accepting(&self) -> bool {
            *self == Self::Done
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    enum EchoStateError {
        InnerError(Token),
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
        let tok = Token::Comment("foo".into(), DS);
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
        let mut toks = [Token::Close(None, DS)].into_iter();

        let mut sut = Sut::from(&mut toks);

        // The first token is fine,
        //   and allows us to acquire our most recent span.
        sut.next();

        // Given that we have no tokens,
        //   and that EchoState::default does not start in an accepting
        //     state,
        //   we must fail when we encounter the end of the stream.
        assert_eq!(Some(Err(ParseError::UnexpectedEof(Some(DS)))), sut.next());
    }

    #[test]
    fn returns_state_specific_error() {
        // Token::Close causes EchoState to produce an error.
        let errtok = Token::Close(None, DS);
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
        // Set up so that we have a single token that we can use for
        //   recovery as part of the same iterator.
        let recovery = Token::Comment("recov".into(), DS);
        let mut toks = [
            // Used purely to populate a Span.
            Token::Close(None, DS),
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
            Err((_, ParseError::UnexpectedEof(Some(span)))) if span == DS
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
        let tok = Token::Text("dead".into(), DS);
        let mut toks = once(tok.clone());

        let mut sut = Sut::from(&mut toks);

        // Our parser returns a Dead status,
        //   which is unhandled by any parent context
        //     (since we're not composing parsers),
        //     which causes an error due to an unhandled Dead state.
        assert_eq!(sut.next(), Some(Err(ParseError::UnexpectedToken(tok))),);
    }
}