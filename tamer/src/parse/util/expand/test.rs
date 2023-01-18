// Tests for TAMER parsing framework utilities
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

use super::super::SPair;
use super::*;
use crate::{
    span::{dummy::*, Span},
    sym::{st::raw, SymbolId},
};
use std::{
    assert_matches::assert_matches, convert::Infallible, fmt::Display,
    marker::PhantomData,
};

#[derive(Debug, PartialEq, Eq)]
pub struct StitchableExpansionState<S: ClosedParseState, O: Object> {
    st: S,
    _phantom: PhantomData<O>,
}

impl<S: ClosedParseState, O: Object> Default for StitchableExpansionState<S, O>
where
    S: Default,
{
    fn default() -> Self {
        Self {
            st: Default::default(),
            _phantom: Default::default(),
        }
    }
}

impl<S: ClosedParseState, O: Object> ParseState
    for StitchableExpansionState<S, O>
where
    S: ExpandableParseState<O> + StitchExpansion,
    <S as ParseState>::Context: AsMut<<S as ParseState>::Context>,
{
    type Token = S::Token;
    type Object = O;
    type Error = S::Error;
    type Context = S::Context;

    #[inline]
    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        let Self { st, _phantom } = self;

        st.stitch_expansion(
            tok,
            ctx,
            Transition::fmap(|st| Self { st, _phantom }),
            |Transition(st), tok| Transition(Self { st, _phantom }).dead(tok),
        )
    }

    fn is_accepting(&self, ctx: &Self::Context) -> bool {
        self.st.is_accepting(ctx)
    }
}

impl<S: ClosedParseState, O: Object> Display
    for StitchableExpansionState<S, O>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self {
                st: parser,
                _phantom,
            } => {
                write!(f, "{parser}, with Expansion stripped")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct TestObject(SPair);

impl Token for TestObject {
    fn ir_name() -> &'static str {
        "TestObject"
    }

    fn span(&self) -> Span {
        match self {
            Self(SPair(_, span)) => *span,
        }
    }
}

impl Display for TestObject {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(spair) => Display::fmt(spair, f),
        }
    }
}

impl Object for TestObject {}

/// Just some parser to wrap for our tests.
///
/// Eventually we'll be able to more easily create these on-demand without so
///   so much boilerplate,
///     but that hasn't evolved yet.
#[derive(Debug, PartialEq, Eq, Default)]
struct TestParseState;

impl ParseState for TestParseState {
    type Token = SPair;
    type Object = Expansion<Self::Token, TestObject>;
    type Error = Infallible;

    fn parse_token(
        self,
        tok: Self::Token,
        _ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        match tok {
            tok @ SPair(sym @ (STOP | DEAD_SYM), span) => {
                let st = Transition(self).ok(Expansion::DoneExpanding(tok));

                st.maybe_with_lookahead(if sym == DEAD_SYM {
                    // It doesn't matter what this token is for our tests.
                    Some(Lookahead(SPair(sym, span)))
                } else {
                    None
                })
            }
            _ => Transition(self).ok(Expansion::Expanded(TestObject(tok))),
        }
    }

    fn is_accepting(&self, _ctx: &Self::Context) -> bool {
        true
    }
}

impl Display for TestParseState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "doing its thing") // well, it is
    }
}

impl StitchExpansion for TestParseState {}

const STOP: SymbolId = raw::L_YIELD;
const DEAD_SYM: SymbolId = raw::L_WARNING;

type ExpansionSut = StitchableExpansionState<TestParseState, TestObject>;

#[test]
fn expansion_can_be_stripped_for_stitching() {
    let syma = "foo".into();
    let symb = "bar".into();

    let toks = vec![SPair(syma, S1), SPair(symb, S2), SPair(STOP, S3)];

    // The wraps the above TestParseState to strip Expansion.
    let mut sut = ExpansionSut::parse(toks.into_iter());

    // Our test parser echoes back the tokens wrapped in an "expanded"
    //   `TestObject` until we reach `STOP`.
    // The first two are expanded,
    //   and our SUT strips the expansion.
    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(TestObject(SPair(syma, S1))))),
    );
    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(TestObject(SPair(symb, S2))))),
    );

    // The final `Expansion::DoneExpanding` is converted into a dead state
    //   transition.
    // That manifests here as an `UnexpectedToken` error because nothing
    //   handled it within our parser,
    //     but this is expected to stitched via delegation,
    //       which _would_ handle this case.
    assert_matches!(
        sut.next(),
        Some(Err(ParseError::UnexpectedToken(dead_tok, _)))
            if dead_tok == SPair(STOP, S3)
    );
}

// We must not lose lookahead tokens;
//   see SUT for more information.
#[should_panic]
#[test]
fn expansion_stripping_panics_if_lookahead() {
    let toks = vec![SPair(DEAD_SYM, S1)];

    // The above token will trigger the panic on the first call.
    let _ = ExpansionSut::parse(toks.into_iter()).next();
}

// This test would fail at compile-time.
#[test]
fn expandable_into_is_stitchable_with_target() {
    // This is utilized only for its types in the below assertions.
    #[derive(Debug, PartialEq, Eq)]
    struct TargetParseState;

    impl ParseState for TargetParseState {
        type Token = SPair;
        type Object = TestObject;
        type Error = Infallible;

        fn parse_token(
            self,
            _tok: Self::Token,
            _ctx: &mut Self::Context,
        ) -> TransitionResult<Self::Super> {
            unimplemented!()
        }

        fn is_accepting(&self, _ctx: &Self::Context) -> bool {
            unimplemented!()
        }
    }

    impl Display for TargetParseState {
        fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
            unimplemented!()
        }
    }

    // The `ExpandableInto` trait alias is responsible for asserting that a
    //   given parser is an expansion parser that is able to be converted
    //   into a parser stitchable with the target.
    //
    // If this fails but the above assertion succeeds,
    //   then the compatibility is working but something is wrong with the
    //   definition of `ExpandableInto`.
    assert_impl_all!(TestParseState: ExpandableInto<TargetParseState>);
}
