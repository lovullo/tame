// ASG IR documentation parsing
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

//! AIR doc parser.
//!
//! See the [parent module](super) for more information.

use super::{super::AsgError, AirAggregate, AirAggregateCtx, ir::AirDoc};
use crate::{asg::graph::object::ObjectIndexTreeRelTo, parse::prelude::*};

/// Package parsing with support for loaded identifiers.
///
/// This supports non-nested package definitions of source files,
///   as well as declaring opaque identifiers loaded from object files via
///   [`AirIdent`](super::ir::AirIdent).
#[derive(Debug, PartialEq)]
pub enum AirDocAggregate {
    /// Ready to document active object.
    Ready,
}

impl Display for AirDocAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirDocAggregate::*;

        match self {
            Ready => {
                write!(f, "expecting documentation for object")
            }
        }
    }
}

impl ParseState for AirDocAggregate {
    type Token = AirDoc;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;
    type Super = AirAggregate;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use AirDoc::*;
        use AirDocAggregate::*;

        match (self, tok) {
            (Ready, DocIndepClause(clause)) => ctx
                .require_active_doc_oi(clause.span())
                .and_then(|oi| oi.add_desc_short(ctx.asg_mut(), clause))
                .map(|_| ())
                .transition(Ready),

            (Ready, DocIndepClauseRef(name)) => {
                let oi_ident = ctx.lookup_lexical_or_missing(name);

                ctx.require_active_doc_oi(name.span())
                    .and_then(|oi| {
                        oi.add_desc_short_ref(
                            ctx.asg_mut(),
                            oi_ident,
                            name.span(),
                        )
                    })
                    .map(|_| ())
                    .transition(Ready)
            }

            (Ready, DocText(text)) => ctx
                .require_active_doc_oi(text.span())
                .and_then(|oi| oi.append_doc_text(ctx.asg_mut(), text))
                .map(|_| ())
                .transition(Ready),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        true
    }
}

impl AirDocAggregate {
    pub fn new() -> Self {
        Self::Ready
    }
}
