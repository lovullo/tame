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

//! Parsing automaton.

use super::{Object, ParseError, Parser, Token, TokenStream};
use crate::diagnose::Diagnostic;
use std::{
    fmt::{Debug, Display},
    ops::ControlFlow,
};
pub use transition::*;

#[cfg(doc)]
use context::{Context, NoContext};

/// Result of some non-parsing operation on a [`Parser`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParseResult<S, T> = Result<
    T,
    ParseError<<S as ParseState>::DeadToken, <S as ParseState>::Error>,
>;

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
    /// A dead state is an accepting state that has no state transition for
    ///   the given token.
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
    /// Certain parsers may aggregate data until reaching a dead state,
    ///   in which case [`Aggregate`] may be of use to yield both a
    ///   lookahead token and an aggregate [`ParseStatus::Object`].
    ///
    /// If there is no parent context to handle the token,
    ///   [`Parser`] must yield an error.
    Dead(S::DeadToken),
}

impl<S: ParseState<Object = T>, T: Object> From<T> for ParseStatus<S> {
    fn from(obj: T) -> Self {
        Self::Object(obj)
    }
}

/// A parsing automaton.
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
///   a `(state, token, context)` triple must uniquely determine the next
///   parser action.
///
/// A [`ParseState`] is not required to implement [`Default`],
///   but it is afforded a number of API conveniences if it does not require
///   context for initialization.
/// This is generally true for standalone parsers,
///   but is not necessarily true for smaller, specialized parsers intended
///   for use as components of a larger parser
///     (in a spirit similar to parser combinators).
pub trait ParseState: PartialEq + Eq + Display + Debug + Sized {
    /// Input tokens to the parser.
    type Token: Token;

    /// Objects produced by a parser utilizing these states.
    type Object: Object;

    /// Errors specific to this set of states.
    type Error: Debug + Diagnostic + PartialEq;

    /// Object provided to parser alongside each token.
    ///
    /// This may be used in situations where Rust/LLVM are unable to
    ///   optimize away moves of interior data associated with the
    ///   otherwise-immutable [`ParseState`].
    type Context: Debug = context::Empty;

    /// Token returned when the parser cannot perform a state transition.
    ///
    /// This is generally the type of the input token itself
    ///   (and so the same as [`ParseState::Token`]),
    ///     which can be used as a token of lookahead.
    /// Parsers may change this type to provide additional data.
    /// For more information and a practical use case of this,
    ///   see [`Aggregate`].
    type DeadToken: Token = Self::Token;

    /// Construct a parser with a [`Default`] state.
    ///
    /// Whether this method is helpful or provides any clarity depends on
    ///   the context and the types that are able to be inferred.
    fn parse<I: TokenStream<Self::Token>>(toks: I) -> Parser<Self, I>
    where
        Self: Default,
        Self::Context: Default,
    {
        Parser::from(toks)
    }

    /// Construct a parser with a [`Default`] state but a non-default
    ///   [`ParseState::Context`].
    ///
    /// This is useful in two ways:
    ///
    ///   1. To allow for parsing using a context that does not implement
    ///        [`Default`],
    ///          or whose default is not sufficient; and
    ///   2. To re-use a context from a previous [`Parser`].
    ///
    /// If neither of these apply to your situation,
    ///   consider [`ParseState::parse`] instead.
    ///
    /// To retrieve a context from a parser for re-use,
    ///   see [`Parser::finalize`].
    fn parse_with_context<I: TokenStream<Self::Token>>(
        toks: I,
        ctx: Self::Context,
    ) -> Parser<Self, I>
    where
        Self: Default,
    {
        Parser::from((toks, ctx))
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
    ///
    /// Since a [`ParseState`] produces a new version of itself with each
    ///   invocation,
    ///     it is functionally pure.
    /// Generally,
    ///   Rust/LLVM are able to optimize moves into direct assignments.
    /// However,
    ///   there are circumstances where this is _not_ the case,
    ///   in which case [`Context`] can be used to provide a mutable context
    ///     owned by the caller (e.g. [`Parser`]) to store additional
    ///     information that is not subject to Rust's move semantics.
    /// If this is not necessary,
    ///   see [`NoContext`].
    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self>;

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
    fn delegate<SP, C>(
        self,
        mut context: C,
        tok: <Self as ParseState>::Token,
        into: impl FnOnce(Self) -> SP,
    ) -> TransitionResult<SP>
    where
        Self: StitchableParseState<SP>
            + ParseState<DeadToken = <SP as ParseState>::DeadToken>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use ParseStatus::{Dead, Incomplete, Object as Obj};

        let (Transition(newst), result) =
            self.parse_token(tok, context.as_mut()).into();

        // This does not use `delegate_lookahead` so that we can have
        //   `into: impl FnOnce` instead of `Fn`.
        Transition(into(newst)).result(match result {
            Ok(Incomplete) => Ok(Incomplete),
            Ok(Obj(obj)) => Ok(Obj(obj.into())),
            Ok(Dead(tok)) => Ok(Dead(tok.into())),
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
    fn delegate_lookahead<SP, C>(
        self,
        mut context: C,
        tok: <Self as ParseState>::Token,
        into: impl FnOnce(Self) -> SP,
    ) -> ControlFlow<
        TransitionResult<SP>,
        (Self, <Self as ParseState>::DeadToken, C),
    >
    where
        Self: StitchableParseState<SP>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use ControlFlow::*;
        use ParseStatus::{Dead, Incomplete, Object as Obj};

        // NB: Rust/LLVM are generally able to elide these moves into direct
        //   assignments,
        //     but sometimes this does not work
        //       (e.g. XIRF's use of `ArrayVec`).
        // If your [`ParseState`] has a lot of `memcpy`s or other
        //   performance issues,
        //     move heavy objects into `context`.
        let (Transition(newst), result) =
            self.parse_token(tok, context.as_mut()).into();

        match result {
            Ok(Incomplete) => Break(Transition(into(newst)).incomplete()),
            Ok(Obj(obj)) => Break(Transition(into(newst)).ok(obj.into())),
            Ok(Dead(tok)) => Continue((newst, tok, context)),
            Err(e) => Break(Transition(into(newst)).err(e)),
        }
    }
}

/// Result of applying a [`Token`] to a [`ParseState`].
///
/// This is used by [`ParseState::parse_token`];
///   see that function for rationale.
pub type ParseStateResult<S> = Result<ParseStatus<S>, <S as ParseState>::Error>;

/// A [`ParseState`] capable of being automatically stitched together with
///   a parent [`ParseState`] `SP` to create a composite parser.
///
/// Conceptually,
///   this can be visualized as combining the state machines of multiple
///   parsers into one larger state machine.
///
/// The term _state stitching_ refers to a particular pattern able to be
///   performed automatically by this parsing framework;
///     it is not necessary for parser composition,
///       provided that you perform the necessary wiring yourself in absence
///       of state stitching.
pub trait StitchableParseState<SP: ParseState> = ParseState
where
    SP: ParseState<Token = <Self as ParseState>::Token>,
    <Self as ParseState>::Object: Into<<SP as ParseState>::Object>,
    <Self as ParseState>::Error: Into<<SP as ParseState>::Error>;

/// Indicates that a parser has completed an aggregate operation,
///   marked by having reached a [dead state](ParseStatus::Dead).
///
/// This struct is compatible with [`ParseState::DeadToken`] and is intended
///   to be used with parsers that continue to aggregate data until they no
///   longer can.
/// For example,
///   an attribute parser may continue to parse element attributes until it
///   reaches the end of the attribute list,
///     which cannot be determined until reading a [`ParseState::Token`]
///     that must result in a [`ParseStatus::Dead`].
#[derive(Debug, PartialEq, Eq)]
pub struct Aggregate<O: Object, T: Token>(pub O, pub T);

impl<O: Object, T: Token> Token for Aggregate<O, T> {
    fn span(&self) -> crate::span::Span {
        let Aggregate(_, tok) = self;
        tok.span()
    }
}

impl<O: Object, T: Token> Object for Aggregate<O, T> {}

impl<O: Object, T: Token> Display for Aggregate<O, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Aggregate(_obj, tok) => write!(f, "{tok} with associated object"),
        }
    }
}

mod transition {
    use super::{ParseState, ParseStateResult, ParseStatus};
    use std::{
        convert::Infallible,
        hint::unreachable_unchecked,
        ops::{ControlFlow, FromResidual, Try},
    };

    #[cfg(doc)]
    use super::Token;

    /// A state transition with associated data.
    ///
    /// Conceptually,
    ///   imagine the act of a state transition producing data.
    /// See [`Transition`] for convenience methods for producing this tuple.
    #[derive(Debug, PartialEq)]
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
        pub fn dead(self, tok: S::DeadToken) -> TransitionResult<S> {
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
        fn from_residual(
            residual: (Transition<S>, ParseStateResult<S>),
        ) -> Self {
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

    impl<S: ParseState>
        FromResidual<ControlFlow<TransitionResult<S>, Infallible>>
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
}

pub mod context {
    use super::Debug;
    use std::ops::{Deref, DerefMut};

    #[cfg(doc)]
    use super::{ParseState, Parser, StitchableParseState};

    /// Mutable context for [`ParseState`].
    ///
    /// [`ParseState`]s are immutable and pure---they
    ///   are invoked via [`ParseState::parse_token`] and return a new version
    ///   of themselves representing their new state.
    /// Rust/LLVM are generally able to elide intermediate values and moves,
    ///   optimizing these parsers away into assignments.
    ///
    /// However,
    ///   there are circumstances where moves may not be elided and may retain
    ///   their `memcpy` equivalents.
    /// To work around this,
    ///   [`ParseState::parse_token`] accepts a mutable [`Context`] reference
    ///   which is held by the parent [`Parser`],
    ///     which can be mutated in-place without worrying about Rust's move
    ///     semantics.
    ///
    /// Plainly: you should only use this if you have to.
    /// This was added because certain parsers may be invoked millions of times
    ///   for each individual token in systems with many source packages,
    ///     which may otherwise result in millions of `memcpy`s.
    ///
    /// When composing two [`ParseState`]s `A<B, C>`,
    ///   a [`Context<B, C>`](Context) must be contravariant over `B` and~`C`.
    /// Concretely,
    ///   this means that [`AsMut<B::Context>`](AsMut) and
    ///   [`AsMut<C::Context>`](AsMut) must be implemented for `A::Context`.
    /// This almost certainly means that `A::Context` is a product type.
    /// Consequently,
    ///   a single [`Parser`] is able to hold a composite [`Context`] in a
    ///   single memory location.
    ///
    /// [`Context<T>`](Context) implements [`Deref<T>`](Deref) for convenience.
    ///
    /// If your [`ParseState`] does not require a mutable [`Context`],
    ///   see [`NoContext`].
    #[derive(Debug, Default)]
    pub struct Context<T: Debug + Default>(T, Empty);

    /// Empty [`Context`] for [`ParseState`]s with pure functional
    ///   implementations with no mutable state.
    ///
    /// Using this value means that a [`ParseState`] does not require a
    ///   context.
    /// All [`Context`]s implement [`AsMut<Empty>`](AsMut),
    ///   and so all pure [`ParseState`]s have contexts compatible with every
    ///   other parser for composition
    ///     (provided that the other invariants in [`StitchableParseState`] are
    ///       met).
    ///
    /// This can be clearly represented in function signatures using
    ///   [`NoContext`].
    #[derive(Debug, PartialEq, Eq, Default)]
    pub struct Empty;

    impl AsMut<Empty> for Empty {
        fn as_mut(&mut self) -> &mut Empty {
            self
        }
    }

    /// A [`ParseState`] does not require any mutable [`Context`].
    ///
    /// A [`ParseState`] using this context is pure
    ///   (has no mutable state),
    ///     returning a new version of itself on each state change.
    ///
    /// This type is intended to be self-documenting:
    ///   `_: NoContext` is nicer to readers than `_: &mut NoContext`.
    ///
    /// See [`Empty`] for more information.
    pub type NoContext<'a> = &'a mut Empty;

    impl<T: Debug + Default> AsMut<Empty> for Context<T> {
        fn as_mut(&mut self) -> &mut Empty {
            &mut self.1
        }
    }

    impl<T: Debug + Default> Deref for Context<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Debug + Default> DerefMut for Context<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    impl<T: Debug + Default> From<T> for Context<T> {
        fn from(x: T) -> Self {
            Context(x, Empty)
        }
    }
}
