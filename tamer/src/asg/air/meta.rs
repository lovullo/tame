// ASG IR metavariable parsing
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

//! AIR metavariable parser.
//!
//! See the [parent module](super) for more information.

use super::{
    super::{AsgError, ObjectIndex},
    ir::AirBindableMeta,
    AirAggregate, AirAggregateCtx,
};
use crate::{asg::graph::object::Meta, diagnostic_todo, parse::prelude::*};

/// Metalinguistic variable (metavariable) parser.
#[derive(Debug, PartialEq)]
pub enum AirMetaAggregate {
    /// Ready for the start of a metavariable.
    Ready,

    /// Defining a metavariable.
    TplMeta(ObjectIndex<Meta>),
}

impl Display for AirMetaAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirMetaAggregate::*;

        match self {
            Ready => write!(f, "ready for metavariable"),
            TplMeta(_) => write!(f, "defining metavariable"),
        }
    }
}

impl ParseState for AirMetaAggregate {
    type Token = AirBindableMeta;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;
    type Super = AirAggregate;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirDoc::*, AirMeta::*};
        use AirBindableMeta::*;
        use AirMetaAggregate::*;

        match (self, tok) {
            (Ready, AirMeta(MetaStart(span))) => {
                let oi_meta = ctx.asg_mut().create(Meta::new_required(span));
                Transition(TplMeta(oi_meta)).incomplete()
            }
            (TplMeta(oi_meta), AirMeta(MetaEnd(cspan))) => {
                oi_meta.close(ctx.asg_mut(), cspan);
                Transition(Ready).incomplete()
            }

            (TplMeta(oi_meta), AirMeta(MetaLexeme(lexeme))) => Transition(
                TplMeta(oi_meta.append_lexeme(ctx.asg_mut(), lexeme)),
            )
            .incomplete(),

            (TplMeta(oi_meta), AirBind(BindIdent(name))) => ctx
                .defines_concrete(name)
                .and_then(|oi_ident| {
                    oi_ident.bind_definition(ctx.asg_mut(), name, oi_meta)
                })
                .map(|_| ())
                .transition(TplMeta(oi_meta)),

            (TplMeta(oi_meta), AirBind(BindIdentAbstract(meta_name))) => {
                diagnostic_todo!(
                    vec![
                        oi_meta.note("for this metavariable"),
                        meta_name.note(
                            "attempting to bind an abstract identifier with \
                                this metavariable"
                        ),
                    ],
                    "attempt to bind abstract identifier to metavariable",
                )
            }

            (TplMeta(oi_meta), AirDoc(DocIndepClause(clause))) => {
                oi_meta.desc_short(ctx.asg_mut(), clause);
                Transition(TplMeta(oi_meta)).incomplete()
            }

            // TODO: The user _probably_ meant to use `<text>` in XML NIR,
            //   so maybe we should have an error to that effect.
            (TplMeta(..), tok @ AirDoc(DocText(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "AirDoc in metavar context \
                        (is this something we want to support?)"
                )
            }

            // Reference to another metavariable,
            //   e.g. using `<param-value>` in XML NIR.
            (TplMeta(oi_meta), AirBind(RefIdent(name))) => {
                let oi_ref = ctx.lookup_lexical_or_missing(name);

                Transition(TplMeta(oi_meta.concat_ref(ctx.asg_mut(), oi_ref)))
                    .incomplete()
            }

            (TplMeta(..), tok @ AirMeta(MetaStart(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "AirMeta variant"
                )
            }

            (Ready, tok @ AirMeta(MetaEnd(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "unbalanced meta"
                )
            }

            (Ready, tok @ AirMeta(MetaLexeme(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "unexpected lexeme"
                )
            }

            // Maybe the token can be handled by the parent frame.
            (Ready, tok @ (AirBind(..) | AirDoc(..))) => {
                Transition(Ready).dead(tok)
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready)
    }
}

impl AirMetaAggregate {
    pub(super) fn new() -> Self {
        Self::Ready
    }
}

#[cfg(test)]
mod test;
