// ASG IR opaque object parsing
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

//! AIR opaque object parser.
//!
//! This parser exists primarily to ensure that the parent frame can be held
//!   on the stack and considered in lexical scoping operations.
//!
//! See the [parent module](super) for more information.

use super::{super::AsgError, ir::AirIdent, AirAggregate, AirAggregateCtx};
use crate::parse::prelude::*;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum AirOpaqueAggregate {
    Ready,
}

impl Display for AirOpaqueAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready => {
                write!(f, "ready for opaque object")
            }
        }
    }
}

impl ParseState for AirOpaqueAggregate {
    type Token = AirIdent;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;
    type Super = AirAggregate;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self::Super> {
        use super::ir::AirIdent::*;
        use AirOpaqueAggregate::*;

        match (self, tok) {
            (Ready, IdentDecl(name, kind, src)) => ctx
                .lookup_lexical_or_missing(name)
                .declare(ctx.asg_mut(), name, kind, src)
                .map(|_| ())
                .transition(Ready),

            (Ready, IdentExternDecl(name, kind, src)) => ctx
                .lookup_lexical_or_missing(name)
                .declare_extern(ctx.asg_mut(), name, kind, src)
                .map(|_| ())
                .transition(Ready),

            (Ready, IdentDep(name, dep)) => {
                let oi_from = ctx.lookup_lexical_or_missing(name);
                let oi_to = ctx.lookup_lexical_or_missing(dep);
                oi_from.add_opaque_dep(ctx.asg_mut(), oi_to);

                Transition(Ready).incomplete()
            }

            (Ready, IdentFragment(name, text)) => ctx
                .lookup_lexical_or_missing(name)
                .set_fragment(ctx.asg_mut(), text)
                .map(|_| ())
                .transition(Ready),

            (Ready, IdentRoot(name)) => {
                ctx.lookup_lexical_or_missing(name).root(ctx.asg_mut());

                Transition(Ready).incomplete()
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready)
    }
}

impl AirOpaqueAggregate {
    pub(super) fn new() -> Self {
        Self::Ready
    }
}
