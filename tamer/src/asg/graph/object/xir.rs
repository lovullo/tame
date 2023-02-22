// XML representation of graph objects
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

//! XML representation of graph objects via [XIR](crate::xir).
//!
//! Attempts to produce a representation of graph [`Object`]s that is
//!   familiar to those writing TAME's XML-based source language.
//!
//! _This representation will change over time_ as TAME's source language
//!   evolves.
//! There is no guarantee that this representation will stay over time;
//!   it was written for transitional purposes,
//!     but may be useful in the future for concrete code suggestions/fixes,
//!       or observing template expansions.

use super::ObjectRelTy;
use crate::{
    asg::{visit::TreeWalkRel, Asg},
    diagnose::Annotate,
    diagnostic_unreachable,
    parse::prelude::*,
    sym::st::raw::URI_LV_RATER,
    xir::{
        attr::Attr,
        flat::{Text, XirfToken},
        st::qname::{QN_PACKAGE, QN_XMLNS},
        OpenSpan,
    },
};
use arrayvec::ArrayVec;
use std::{convert::Infallible, fmt::Display, marker::PhantomData};

#[derive(Debug, PartialEq, Eq)]
pub enum AsgTreeToXirf<'a> {
    Ready(PhantomData<&'a ()>),
}

impl<'a> Default for AsgTreeToXirf<'a> {
    fn default() -> Self {
        Self::Ready(PhantomData::default())
    }
}

impl<'a> Display for AsgTreeToXirf<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "generating XIRF sources from ASG tree")
    }
}

impl<'a> ParseState for AsgTreeToXirf<'a> {
    type Token = TreeWalkRel;
    type Object = XirfToken<Text>;
    type Error = Infallible;
    type Context = TreeContext<'a>;

    fn parse_token(
        self,
        tok: Self::Token,
        TreeContext(tok_stack, asg): &mut TreeContext,
    ) -> TransitionResult<Self::Super> {
        use ObjectRelTy as Ty;

        if let Some(emit) = tok_stack.pop() {
            return Transition(self).ok(emit).with_lookahead(tok);
        }

        let tok_span = tok.span();
        let TreeWalkRel(dyn_rel, depth) = tok;

        let obj = dyn_rel.target().resolve(asg);
        let obj_span = obj.span();

        match dyn_rel.target_ty() {
            Ty::Pkg => {
                tok_stack.push(XirfToken::Attr(Attr::new(
                    QN_XMLNS,
                    URI_LV_RATER,
                    (obj_span, obj_span),
                )));

                Transition(self).ok(XirfToken::Open(
                    QN_PACKAGE,
                    OpenSpan::without_name_span(obj_span),
                    depth,
                ))
            }

            Ty::Ident | Ty::Expr => Transition(self).incomplete(),

            Ty::Root => diagnostic_unreachable!(
                vec![tok_span.error("unexpected Root")],
                "tree walk is not expected to emit Root",
            ),
        }
    }

    fn is_accepting(&self, _ctx: &Self::Context) -> bool {
        true
    }
}

/// Size of the token stack.
///
/// See [`TokenStack`] for more information.
const TOK_STACK_SIZE: usize = 8;

/// Token stack to hold generated tokens between [`AsgTreeToXirf`]
///   iterations.
///
/// The token stack is used to avoid having to create separate states for
///   emitting each individual token.
/// It is populated by [`AsgTreeToXirf`] if more than a single [`XirfToken`]
///   needs to be emitted,
///     and tokens are removed on each subsequent iteration until empty.
///
/// This need only be big enough to accommodate [`AsgTreeToXirf`]'s
///   implementation;
///     the size is independent of user input.
type TokenStack<'a> =
    ArrayVec<<AsgTreeToXirf<'a> as ParseState>::Object, TOK_STACK_SIZE>;

#[derive(Debug)]
pub struct TreeContext<'a>(TokenStack<'a>, &'a Asg);

impl<'a> From<&'a Asg> for TreeContext<'a> {
    fn from(asg: &'a Asg) -> Self {
        Self(Default::default(), asg)
    }
}
