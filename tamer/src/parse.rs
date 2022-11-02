// Basic streaming parsing framework
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

//! Basic streaming parser framework for lowering operations.
//!
//! _TODO: Some proper docs and examples!_

mod error;
mod lower;
mod parser;
mod state;
mod trace;

pub use error::{FinalizeError, ParseError};
pub use lower::{Lower, LowerIter, ParsedObject};
pub use parser::{FinalizedParser, Parsed, ParsedResult, Parser};
pub use state::{
    context::{Context, Empty as EmptyContext, NoContext},
    ClosedParseState, ParseResult, ParseState, ParseStatus, Transition,
    TransitionResult, Transitionable,
};

use crate::span::{Span, UNKNOWN_SPAN};
use std::{
    error::Error,
    fmt::{Debug, Display},
};

/// Prelude for TAME's parsing framework.
///
/// This contains the boilerplate types necessary for virtually every
///   parser.
pub mod prelude {
    pub use super::{
        Context, Object, ParseState, Token, Transition, TransitionResult,
    };
}

/// A single datum from a streaming IR with an associated [`Span`].
///
/// A token may be a lexeme with associated data,
///   or a more structured object having been lowered from other IRs.
pub trait Token: Display + Debug + PartialEq {
    /// Name of the intermediate representation (IR) this token represents.
    ///
    /// This is used for diagnostic information,
    ///   primarily for debugging TAMER itself.
    fn ir_name() -> &'static str;

    /// Retrieve the [`Span`] representing the source location of the token.
    fn span(&self) -> Span;
}

impl<T: Token> From<T> for Span {
    fn from(tok: T) -> Self {
        tok.span()
    }
}

/// A type of [`Token`] that is not relevant.
///
/// This may be used when a [`Token`] type is required but only incidental,
///   such as for use with a [`ParseState`] in the context of a source of a
///   lowering operation.
#[derive(Debug, PartialEq)]
pub struct UnknownToken;

impl Token for UnknownToken {
    fn span(&self) -> Span {
        UNKNOWN_SPAN
    }

    fn ir_name() -> &'static str {
        "<UNKNOWN IR>"
    }
}

impl Display for UnknownToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<unknown token>")
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
pub trait Object: Debug + PartialEq {}

impl Object for () {}

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

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::{
        diagnose::{AnnotatedSpan, Diagnostic},
        span::{dummy::DUMMY_SPAN as DS, UNKNOWN_SPAN},
        sym::GlobalSymbolIntern,
    };
    use std::{assert_matches::assert_matches, iter::once};

    #[derive(Debug, PartialEq, Eq, Clone)]
    enum TestToken {
        Close(Span),
        MarkDone(Span),
        Text(Span),
        SetCtxVal(u8),
    }

    impl Display for TestToken {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "(test token)")
        }
    }

    impl Token for TestToken {
        fn ir_name() -> &'static str {
            "<PARSE TEST IR>"
        }

        fn span(&self) -> Span {
            use TestToken::*;
            match self {
                Close(span) | MarkDone(span) | Text(span) => *span,
                _ => UNKNOWN_SPAN,
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

    #[derive(Debug, PartialEq, Default)]
    struct StubContext {
        val: u8,
    }

    impl ParseState for EchoState {
        type Token = TestToken;
        type Object = TestToken;
        type Error = EchoStateError;

        type Context = StubContext;

        fn parse_token(
            self,
            tok: TestToken,
            ctx: &mut StubContext,
        ) -> TransitionResult<Self> {
            match tok {
                TestToken::MarkDone(..) => Transition(Self::Done).ok(tok),
                TestToken::Close(..) => {
                    Transition(self).err(EchoStateError::InnerError(tok))
                }
                TestToken::Text(..) => Transition(self).dead(tok),
                TestToken::SetCtxVal(val) => {
                    ctx.val = val;
                    Transition(Self::Done).incomplete()
                }
            }
        }

        fn is_accepting(&self, _: &Self::Context) -> bool {
            *self == Self::Done
        }
    }

    impl Display for EchoState {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "<EchoState as Display>::fmt")
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    enum EchoStateError {
        InnerError(TestToken),
    }

    impl Display for EchoStateError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "test EchoStateError")
        }
    }

    impl Error for EchoStateError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }

    impl Diagnostic for EchoStateError {
        fn describe(&self) -> Vec<AnnotatedSpan> {
            vec![]
        }
    }

    type Sut<I> = Parser<EchoState, I>;

    #[test]
    fn successful_parse_in_accepting_state_with_spans() {
        // EchoState is placed into a Done state given Comment.
        let tok = TestToken::MarkDone(DS);
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
            Some(Err(ParseError::FinalizeError(
                FinalizeError::UnexpectedEof(
                    span.endpoints().1.unwrap(),
                    // All the states have the same string
                    //   (at time of writing).
                    EchoState::default().to_string(),
                )
            ))),
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
        let recovery = TestToken::MarkDone(DS);
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
            Err((_, FinalizeError::UnexpectedEof(s, _)))
                if s == span.endpoints().1.unwrap()
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
        assert_eq!(
            sut.next(),
            Some(Err(ParseError::UnexpectedToken(
                tok,
                EchoState::default().to_string()
            ))),
        );
    }

    // A context can be both retrieved from a finished parser and provided
    //   to a new one.
    #[test]
    fn provide_and_retrieve_context() {
        // First, verify that it's initialized to a default context.
        let mut toks = vec![TestToken::MarkDone(DS)].into_iter();
        let mut sut = Sut::from(&mut toks);
        sut.next().unwrap().unwrap();
        let ctx = sut.finalize().unwrap().into_context();
        assert_eq!(ctx, Default::default());

        // Next, verify that the context that is manipulated is the context
        //   that is returned to us.
        let val = 5;
        let mut toks = vec![TestToken::SetCtxVal(5)].into_iter();
        let mut sut = Sut::from(&mut toks);
        sut.next().unwrap().unwrap();
        let ctx = sut.finalize().unwrap().into_context();
        assert_eq!(ctx, StubContext { val });

        // Finally, verify that the context provided is the context that is
        //   used.
        let val = 10;
        let given_ctx = StubContext { val };
        let mut toks = vec![TestToken::MarkDone(DS)].into_iter();
        let mut sut = EchoState::parse_with_context(&mut toks, given_ctx);
        sut.next().unwrap().unwrap();
        let ctx = sut.finalize().unwrap().into_context();
        assert_eq!(ctx, StubContext { val });
    }

    // This healthy block of mostly-boilerplate verifies that the practical
    //   use case of the trampoline system actually type-checks,
    //     and was used during development as a simpler example than having
    //     to content with the mammoth `ele_parse!`.
    // There is no runtime test;
    //   it will fail to compile if there's a problem.
    mod superst {
        use crate::span::dummy::S1;

        use super::*;

        #[derive(Debug, PartialEq, Eq)]
        enum Sup {
            SubA(SubA),
            SubB(SubB),
        }

        #[derive(Debug, PartialEq, Eq)]
        enum SubA {
            A,
        }

        #[derive(Debug, PartialEq, Eq)]
        enum SubB {
            B,
        }

        impl Display for Sup {
            fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
                unimplemented!()
            }
        }

        impl Display for SubA {
            fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
                unimplemented!()
            }
        }

        impl Display for SubB {
            fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
                unimplemented!()
            }
        }

        impl From<SubA> for Sup {
            fn from(sub: SubA) -> Self {
                Self::SubA(sub)
            }
        }

        impl From<SubB> for Sup {
            fn from(sub: SubB) -> Self {
                Self::SubB(sub)
            }
        }

        #[derive(Debug, PartialEq)]
        enum SupError {
            SubA(SubAError),
            SubB(SubBError),
        }
        #[derive(Debug, PartialEq)]
        enum SubAError {}
        #[derive(Debug, PartialEq)]
        enum SubBError {}

        impl Error for SupError {
            fn source(&self) -> Option<&(dyn Error + 'static)> {
                None
            }
        }

        impl Display for SupError {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "SupError")
            }
        }

        impl Diagnostic for SupError {
            fn describe(&self) -> Vec<AnnotatedSpan> {
                vec![]
            }
        }

        impl Error for SubAError {
            fn source(&self) -> Option<&(dyn Error + 'static)> {
                None
            }
        }

        impl Display for SubAError {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "SubAError")
            }
        }

        impl Diagnostic for SubAError {
            fn describe(&self) -> Vec<AnnotatedSpan> {
                vec![]
            }
        }

        impl Error for SubBError {
            fn source(&self) -> Option<&(dyn Error + 'static)> {
                None
            }
        }

        impl Display for SubBError {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "SubBError")
            }
        }

        impl Diagnostic for SubBError {
            fn describe(&self) -> Vec<AnnotatedSpan> {
                vec![]
            }
        }

        impl From<SubAError> for SupError {
            fn from(sub: SubAError) -> Self {
                Self::SubA(sub)
            }
        }

        impl From<SubBError> for SupError {
            fn from(sub: SubBError) -> Self {
                Self::SubB(sub)
            }
        }

        #[allow(dead_code)] // Used only for type checking atm.
        #[derive(Debug, PartialEq, Eq)]
        enum SupToken {
            ToA,
            ToB,
        }

        impl Token for SupToken {
            fn ir_name() -> &'static str {
                "SupTest"
            }

            fn span(&self) -> Span {
                S1
            }
        }

        impl Display for SupToken {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "SupToken")
            }
        }

        #[derive(Debug, PartialEq, Eq)]
        enum SupObject {
            FromA(SupToken),
            FromB(SupToken),
        }

        impl Object for SupObject {}

        impl ParseState for Sup {
            type Token = SupToken;
            type Object = SupObject;
            type Error = SupError;

            fn parse_token(
                self,
                tok: Self::Token,
                ctx: &mut Self::Context,
            ) -> TransitionResult<Self> {
                match self {
                    Self::SubA(st) => st.parse_token(tok, ctx),
                    Self::SubB(st) => st.parse_token(tok, ctx),
                }
            }

            fn is_accepting(&self, _: &Self::Context) -> bool {
                true
            }
        }

        impl ParseState for SubA {
            type Token = SupToken;
            type Object = SupObject;
            type Error = SubAError;
            type Super = Sup;

            fn parse_token(
                self,
                tok: Self::Token,
                _ctx: &mut Self::Context,
            ) -> TransitionResult<Self::Super> {
                match tok {
                    SupToken::ToA => Transition(self).ok(SupObject::FromA(tok)),
                    SupToken::ToB => {
                        Transition(SubB::B).ok(SupObject::FromA(tok))
                    }
                }
            }

            fn is_accepting(&self, _: &Self::Context) -> bool {
                true
            }
        }

        impl ParseState for SubB {
            type Token = SupToken;
            type Object = SupObject;
            type Error = SubBError;
            type Super = Sup;

            fn parse_token(
                self,
                tok: Self::Token,
                _ctx: &mut Self::Context,
            ) -> TransitionResult<Self::Super> {
                match tok {
                    SupToken::ToA => Transition(self).ok(SupObject::FromB(tok)),
                    SupToken::ToB => {
                        Transition(SubA::A).ok(SupObject::FromB(tok))
                    }
                }
            }

            fn is_accepting(&self, _: &Self::Context) -> bool {
                true
            }
        }
    }
}
