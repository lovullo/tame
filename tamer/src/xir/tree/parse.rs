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

//! Basic streaming parsing framework to lower XIR into XIRT.

use super::super::{Token, TokenStream};
use std::{error::Error, fmt::Display};

/// Lower a [`TokenStream`] into XIRT.
///
/// Parsers are wrappers around a ([`TokenStreamState`], [`TokenStream`])
///   pair,
///     where only one parser may have mutable access to the stream at any
///     given time.
///
/// After you have finished with a parser,
///   you should call [`finalize`](TokenStreamParser::finalize) to ensure
///   that parsing has completed in an accepting state.
pub trait TokenStreamParser<I: TokenStream>:
    Iterator<Item = TokenStreamParsedResult<Self::State>> + Sized
{
    /// Parsing automaton.
    type State: TokenStreamState;

    /// Parse a single [`Token`] according to the current
    ///   [`TokenStreamState`],
    ///     if available.
    ///
    /// If the underlying [`TokenStream`] yields [`None`],
    ///   then the [`TokenStreamState`] must be in an accepting state;
    ///     otherwise, [`ParseError::UnexpectedEof`] will occur.
    ///
    /// This is intended to be invoked by [`Iterator::next`].
    /// Accepting a token rather than the [`TokenStream`] allows the caller
    ///   to inspect the token first
    ///     (e.g. to store a copy of the [`Span`][crate::span::Span]).
    #[inline]
    fn parse_next(
        state: &mut Self::State,
        otok: Option<Token>,
    ) -> Option<Self::Item> {
        match otok {
            None if state.is_accepting() => None,
            None => Some(Err(ParseError::UnexpectedEof)),
            Some(tok) => Some(state.parse_token(tok).map_err(ParseError::from)),
        }
    }

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
    fn finalize(
        self,
    ) -> Result<(), (Self, ParseError<<Self::State as TokenStreamState>::Error>)>;
}

/// Result of applying a [`Token`] to a [`TokenStreamState`],
///   with any error having been wrapped in a [`ParseError`].
pub type TokenStreamParsedResult<S> =
    TokenStreamParserResult<S, Parsed<<S as TokenStreamState>::Object>>;

/// Result of some non-parsing operation on a [`TokenStreamParser`],
///   with any error having been wrapped in a [`ParseError`].
pub type TokenStreamParserResult<S, T> =
    Result<T, ParseError<<S as TokenStreamState>::Error>>;

/// A deterministic parsing automaton.
///
/// These states are utilized by a [`TokenStreamParser`].
///
/// A [`TokenStreamState`] is also responsible for storing data about the
///   accepted input,
///     and handling appropriate type conversions into the final type.
/// That is---an
///   automaton may store metadata that is subsequently emitted once an
///   accepting state has been reached.
/// Whatever the underlying automaton,
///   a `(state, token)` pair must uniquely determine the next parser
///   action via [`TokenStreamParser::parse_next`].
///
/// Intuitively,
///   since only one [`TokenStreamParser`] may hold a mutable reference to
///   an underlying [`TokenStream`] at any given point,
///   this does in fact represent the current state of the entire
///     [`TokenStream`] at the current position for a given parser
///     composition.
pub trait TokenStreamState: Default {
    /// Objects produced by a parser utilizing these states.
    type Object;

    /// Errors specific to this set of states.
    type Error: Error + PartialEq;

    /// Construct a parser.
    ///
    /// Whether this method is helpful or provides any clarity depends on
    ///   the context and the types that are able to be inferred.
    /// This is completely generic,
    ///   able to construct any compatible type of [`TokenStreamParser`],
    ///   and so does not in itself do anything to help with type inference
    ///     (compared to `P::from`,
    ///        you trade an unknown `P::State` for an unknown `P`).
    fn parser<P, I>(toks: &mut I) -> P
    where
        I: TokenStream,
        P: TokenStreamParser<I> + for<'a> From<&'a mut I>,
    {
        P::from(toks)
    }

    /// Parse a single [`Token`] and optionally perform a state transition.
    ///
    /// The current state is represented by `self`,
    ///   which is mutable to allow for a state transition.
    /// The result of a parsing operation is either an object or an
    ///   indication that additional tokens of input are needed;
    ///     see [`Parsed`] for more information.
    fn parse_token(&mut self, tok: Token) -> TokenStreamStateResult<Self>;

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

/// Result of applying a [`Token`] to a [`TokenStreamState`].
///
/// See [`TokenStreamState::parse_token`] and
///   [`TokenStreamParser::parse_next`] for more information.
pub type TokenStreamStateResult<S> = Result<
    Parsed<<S as TokenStreamState>::Object>,
    <S as TokenStreamState>::Error,
>;

/// A streaming parser defined by a [`TokenStreamState`] with exclusive
///   mutable access to an underlying [`TokenStream`].
///
/// This parser handles operations that are common among all types of
///   parsers,
///     such that specialized parsers need only implement logic that is
///     unique to their operation.
/// This also simplifies combinators,
///   since there is more uniformity among distinct parser types.
#[derive(Debug, PartialEq, Eq)]
pub struct Parser<'a, S: TokenStreamState, I: TokenStream> {
    toks: &'a mut I,
    state: S,
}

impl<'a, S: TokenStreamState, I: TokenStream> TokenStreamParser<I>
    for Parser<'a, S, I>
{
    type State = S;

    fn finalize(
        self,
    ) -> Result<(), (Self, ParseError<<Self::State as TokenStreamState>::Error>)>
    {
        if self.state.is_accepting() {
            Ok(())
        } else {
            Err((self, ParseError::UnexpectedEof))
        }
    }
}

impl<'a, S: TokenStreamState, I: TokenStream> Iterator for Parser<'a, S, I> {
    type Item = TokenStreamParsedResult<S>;

    /// Consume a single token from the underlying [`TokenStream`] and parse
    ///   it according to the current [`TokenStreamState`].
    ///
    /// See [`TokenStreamParser::parse_next`] for more information.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        <Self as TokenStreamParser<I>>::parse_next(
            &mut self.state,
            self.toks.next(),
        )
    }
}

/// Common parsing errors produced by [`TokenStreamParser`].
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
pub enum ParseError<E: Error + PartialEq> {
    // TODO: Last span encountered, maybe?
    UnexpectedEof,

    /// A parser-specific error associated with an inner
    ///   [`TokenStreamState`].
    StateError(E),
}

impl<E: Error + PartialEq> From<E> for ParseError<E> {
    fn from(e: E) -> Self {
        Self::StateError(e)
    }
}

impl<E: Error + PartialEq> Display for ParseError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::StateError(e) => Display::fmt(e, f),
        }
    }
}

impl<E: Error + PartialEq + 'static> Error for ParseError<E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::StateError(e) => Some(e),
            _ => None,
        }
    }
}

impl<'a, S: TokenStreamState, I: TokenStream> From<&'a mut I>
    for Parser<'a, S, I>
{
    fn from(toks: &'a mut I) -> Self {
        Self {
            toks,
            state: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Parsed<T> {
    Incomplete,
    Object(T),
}

#[cfg(test)]
pub mod test {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::span::DUMMY_SPAN as DS;

    /// Preferred [`TokenStreamParser`].
    ///
    /// TODO: Move into parent module once used outside of tests.
    pub type DefaultParser<'a, S, I> = Parser<'a, S, I>;

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

    impl TokenStreamState for EchoState {
        type Object = Token;
        type Error = EchoStateError;

        fn parse_token(&mut self, tok: Token) -> TokenStreamStateResult<Self> {
            match tok {
                Token::AttrEnd(..) => {
                    *self = Self::Done;
                }
                Token::Close(..) => {
                    return Err(EchoStateError::InnerError(tok))
                }
                _ => {}
            }

            Ok(Parsed::Object(tok))
        }

        fn is_accepting(&self) -> bool {
            *self == Self::Done
        }
    }

    #[derive(Debug, PartialEq)]
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

    type Sut<'a, I> = DefaultParser<'a, EchoState, I>;

    #[test]
    fn permits_end_of_stream_in_accepting_state() {
        // EchoState is placed into a Done state given AttrEnd.
        let mut toks = [Token::AttrEnd(DS)].into_iter();

        let mut sut = Sut::from(&mut toks);

        // The first token should be processed normally.
        // EchoState proxies the token back.
        assert_eq!(Some(Ok(Parsed::Object(Token::AttrEnd(DS)))), sut.next());

        // This is now the end of the token stream,
        //   which should be okay provided that the first token put us into
        //   a proper accepting state.
        assert_eq!(None, sut.next());

        // Further, finalizing should work in this state.
        assert!(sut.finalize().is_ok());
    }

    #[test]
    fn fails_on_end_of_stream_when_not_in_accepting_state() {
        // No tokens, so EchoState starts in a non-accepting state.
        let mut toks = [].into_iter();

        let mut sut = Sut::from(&mut toks);

        // Given that we have no tokens,
        //   and that EchoState::default does not start in an accepting
        //     state,
        //   we must fail when we encounter the end of the stream.
        assert_eq!(Some(Err(ParseError::UnexpectedEof)), sut.next());
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
        let mut toks = [Token::AttrEnd(DS)].into_iter();

        let sut = Sut::from(&mut toks);

        // Attempting to finalize now in a non-accepting state should fail
        //   in the same way that encountering an end-of-stream does,
        //     since we're effectively saying "we're done with the stream"
        //     and the parser will have no further opportunity to reach an
        //     accepting state.
        let result = sut.finalize();
        assert_matches!(result, Err((_, ParseError::UnexpectedEof)));

        // The sut should have been re-returned,
        //   allowing for attempted error recovery if the caller can manage
        //   to produce a sequence of tokens that will be considered valid.
        // `toks` above is set up already for this,
        //   which allows us to assert that we received back the same `sut`.
        let mut sut = result.unwrap_err().0;
        assert_eq!(Some(Ok(Parsed::Object(Token::AttrEnd(DS)))), sut.next());

        // And so we should now be in an accepting state,
        //   able to finalize.
        assert!(sut.finalize().is_ok());
    }
}
