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

mod transition;

use super::{Object, ParseError, Parser, Token, TokenStream};
use crate::diagnose::Diagnostic;
use std::fmt::{Debug, Display};
pub use transition::*;

#[cfg(doc)]
use context::{Context, NoContext};

/// Result of some non-parsing operation on a [`Parser`],
///   with any error having been wrapped in a [`ParseError`].
pub type ParseResult<S, T> =
    Result<T, ParseError<<S as ParseState>::Token, <S as ParseState>::Error>>;

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
}

impl<S: ParseState<Object = T>, T: Object> From<T> for ParseStatus<S> {
    fn from(obj: T) -> Self {
        Self::Object(obj)
    }
}

/// A [`ParseState`] that transitions only to itself
///   (is closed under transition).
///
/// These are the only [`ParseState`]s that can be used directly by
///   [`Parser`],
///     since [`Parser`] must be able to both handle every provided
///     [`Transition`] and know how to delegate to inner [`ParseState`]s.
pub trait ClosedParseState = ParseState<Super = Self>;

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
pub trait ParseState: PartialEq + Eq + Display + Debug + Sized
where
    Self: Into<Self::Super>,
{
    /// Input tokens to the parser.
    type Token: Token;

    /// Objects produced by a parser utilizing these states.
    type Object: Object;

    /// Errors specific to this set of states.
    type Error: Debug
        + Diagnostic
        + PartialEq
        + Into<<Self::Super as ParseState>::Error>;

    /// Superstate (parent state).
    ///
    /// This is applicable only if the [`ParseState`] is capable of
    ///   transitioning to a state outside of its own.
    /// It was initially introduced for implementing trampolines in place of
    ///   composition-based delegation,
    ///     the latter of which would otherwise require boxing on
    ///     (extremely) hot code paths for otherwise-recursive data
    ///     structures.
    ///
    /// Intuitively,
    ///   the superstate represents a sum type of the pool of all possible
    ///   [`ParseState`]s that we can request transfer of control to.
    /// This is the same concept as [`StitchableParseState`],
    ///   but operating in reverse
    ///     (delegation via trampoline instead of direct function call).
    type Super: ClosedParseState<
        Token = Self::Token,
        Object = Self::Object,
        Context = Self::Context,
    > = Self;

    /// Object provided to parser alongside each token.
    ///
    /// This may be used in situations where Rust/LLVM are unable to
    ///   optimize away moves of interior data associated with the
    ///   otherwise-immutable [`ParseState`].
    type Context: Debug = context::Empty;

    /// Construct a parser with a [`Default`] state.
    ///
    /// Whether this method is helpful or provides any clarity depends on
    ///   the context and the types that are able to be inferred.
    fn parse<I: TokenStream<Self::Token>>(toks: I) -> Parser<Self, I>
    where
        Self: ClosedParseState + Default,
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
        Self: ClosedParseState + Default,
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
    ///
    /// This method must not produce a token of [`Lookahead`] _unless_
    ///   consuming that token again will result in either a state
    ///   transition or a dead state indication.
    /// Otherwise,
    ///   the system may recurse indefinitely.
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

    /// Delegate parsing from a compatible, stitched [`ParseState`] `SP`.
    ///
    /// This helps to combine two state machines that speak the same input
    ///   language
    ///   (share the same [`Self::Token`]),
    ///     handling the boilerplate of delegating [`Self::Token`] from a
    ///     parent state `SP` to `Self`.
    ///
    /// Token delegation happens after [`Self`] has been entered from a
    ///   parent [`ParseState`] context `SP`,
    ///     so stitching the start and accepting states must happen elsewhere
    ///     (for now).
    ///
    /// If the parser indicates a dead state,
    ///   the token of lookahead will be delegated to the parent `SP` and
    ///   result in an incomplete parse to the state indicated by the `dead`
    ///   callback.
    /// This will cause a [`Parser`] to yield that token of lookahead back
    ///   to `SP`
    ///     (or whatever ancestor exists at the root)
    ///     for re-processing.
    /// It is expected that the `dead` callback will cause~`SP` to
    ///   transition into a state that will avoid invoking this parser again
    ///   with the same token,
    ///     which may otherwise result in unbounded recursion
    ///       (see Recursion Warning in [`Parser::feed_tok`]).
    fn delegate<SP, C>(
        self,
        tok: <Self as ParseState>::Token,
        mut context: C,
        into: impl FnOnce(
            <Self as ParseState>::Super,
        ) -> Transition<<SP as ParseState>::Super>,
        dead: impl FnOnce() -> Transition<SP>,
    ) -> TransitionResult<SP>
    where
        Self: StitchableParseState<SP>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use ParseStatus::{Incomplete, Object as Obj};

        let TransitionResult(Transition(newst), data) =
            self.parse_token(tok, context.as_mut());

        match data {
            // The token of lookahead must bubble up to the ancestor
            //   [`Parser`] so that it knows to provide that token in place
            //   of the next from the token stream,
            //     otherwise the token will be lost.
            // Since we have stitched together states,
            //   the dead state simply means that we should transition back
            //   out of this parser back to `SP` so that it can use the
            //   token of lookahead.
            TransitionData::Dead(Lookahead(lookahead)) => {
                dead().incomplete().with_lookahead(lookahead)
            }
            TransitionData::Result(result, lookahead) => TransitionResult(
                into(newst),
                TransitionData::Result(
                    match result {
                        Ok(Incomplete) => Ok(Incomplete),
                        Ok(Obj(obj)) => Ok(Obj(obj.into())),
                        Err(e) => Err(e.into()),
                    },
                    lookahead,
                ),
            ),
        }
    }

    /// Delegate parsing from a compatible, stitched [`ParseState`] `SP`
    ///   until this parser yields an [`Object`].
    ///
    /// This method is appropriate for [`ParseState`]s that yield an object
    ///   after they have completed parsing.
    /// It is not suitable for [`ParseState`]s that yield multiple objects
    ///   during parsing,
    ///     which typically indicate completion with a dead state
    ///     (see [`ParseState::delegate`]).
    ///
    /// _This method is still under development and has outstanding TODOs._
    fn delegate_until_obj<SP, C>(
        self,
        tok: <Self as ParseState>::Token,
        mut context: C,
        into: impl FnOnce(
            <Self as ParseState>::Super,
        ) -> Transition<<SP as ParseState>::Super>,
        _dead: impl FnOnce() -> Transition<SP>,
        objf: impl FnOnce(
            <Self as ParseState>::Super,
            <Self as ParseState>::Object,
        ) -> TransitionResult<SP>,
    ) -> TransitionResult<SP>
    where
        Self: PartiallyStitchableParseState<SP>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use ParseStatus::{Incomplete, Object as Obj};

        let TransitionResult(Transition(newst), data) =
            self.parse_token(tok, context.as_mut());

        match data {
            TransitionData::Dead(Lookahead(_lookahead)) => {
                // Or restrict this to certain types of ParseState
                todo!("expecting object, so what should we do on Dead?")
            }

            TransitionData::Result(Ok(Obj(obj)), lookahead) => {
                // TODO: check accepting
                objf(newst, obj).maybe_with_lookahead(lookahead)
            }

            TransitionData::Result(result, lookahead) => TransitionResult(
                into(newst),
                TransitionData::Result(
                    match result {
                        Ok(_) => Ok(Incomplete),
                        Err(e) => Err(e.into()),
                    },
                    lookahead,
                ),
            ),
        }
    }

    /// Delegate parsing from a compatible, stitched [`ParseState`] `SP`
    ///   while consuming objects during `SP` state transition.
    ///
    /// See [`ParseState::delegate`] for more information.
    /// This method exists for a XIRT and ought to be removed when it is no
    ///   longer needed.
    fn delegate_with_obj<SP, C, X>(
        self,
        tok: <Self as ParseState>::Token,
        mut context: C,
        env: X,
        into: impl FnOnce(
            <Self as ParseState>::Super,
            Option<<Self as ParseState>::Object>,
            X,
        ) -> Transition<<SP as ParseState>::Super>,
        dead: impl FnOnce(X) -> Transition<SP>,
    ) -> TransitionResult<SP>
    where
        Self: PartiallyStitchableParseState<SP>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use ParseStatus::{Incomplete, Object as Obj};

        let TransitionResult(Transition(newst), data) =
            self.parse_token(tok, context.as_mut());

        match data {
            TransitionData::Dead(Lookahead(lookahead)) => {
                dead(env).incomplete().with_lookahead(lookahead)
            }

            // Consume object and allow processing as part of state
            //   transition.
            TransitionData::Result(Ok(Obj(obj)), lookahead) => {
                TransitionResult(
                    into(newst, Some(obj), env),
                    TransitionData::Result(Ok(Incomplete), lookahead),
                )
            }

            TransitionData::Result(result, lookahead) => TransitionResult(
                into(newst, None, env),
                TransitionData::Result(
                    match result {
                        Ok(_) => Ok(Incomplete),
                        Err(e) => Err(e.into()),
                    },
                    lookahead,
                ),
            ),
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
pub trait StitchableParseState<SP: ParseState> =
    PartiallyStitchableParseState<SP>
    where <Self as ParseState>::Object: Into<<SP as ParseState>::Object>;

pub trait PartiallyStitchableParseState<SP: ParseState> = ParseState
where
    SP: ParseState<Token = <Self as ParseState>::Token>,
    <Self as ParseState>::Error: Into<<SP as ParseState>::Error>;

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

    impl<T: Debug + Default> AsMut<Context<T>> for Context<T> {
        fn as_mut(&mut self) -> &mut Context<T> {
            self
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
