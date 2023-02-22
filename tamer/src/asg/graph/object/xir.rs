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

use std::{convert::Infallible, fmt::Display, marker::PhantomData};

use crate::{
    asg::{visit::TreeWalkRel, Asg},
    diagnose::Annotate,
    diagnostic_unreachable,
    parse::prelude::*,
    xir::{
        flat::{Text, XirfToken},
        st::qname::QN_PACKAGE,
        OpenSpan,
    },
};

use super::ObjectRelTy;

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
        TreeContext(_, asg): &mut TreeContext,
    ) -> TransitionResult<Self::Super> {
        use ObjectRelTy as Ty;

        let tok_span = tok.span();
        let TreeWalkRel(dyn_rel, depth) = tok;

        // POC: Read-only access to the ASG for resolving edge refs.
        let obj = dyn_rel.target().resolve(asg);
        let obj_span = obj.span();

        match dyn_rel.target_ty() {
            Ty::Pkg => Transition(self).ok(XirfToken::Open(
                QN_PACKAGE,
                OpenSpan::without_name_span(obj_span),
                depth,
            )),

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

#[derive(Debug)]
pub struct TreeContext<'a>(StackPlaceholder, &'a Asg);

type StackPlaceholder = ();

impl<'a> From<&'a Asg> for TreeContext<'a> {
    fn from(asg: &'a Asg) -> Self {
        Self(Default::default(), asg)
    }
}
