// Lower NIR into AIR
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

//! Lower [NIR](super) into [AIR](crate::asg::air).

use std::{error::Error, fmt::Display};

use crate::{
    asg::air::Air, diagnose::Diagnostic, parse::prelude::*, span::UNKNOWN_SPAN,
};

// These are also used by the `test` module which imports `super`.
#[cfg(feature = "wip-nir-to-air")]
use crate::{
    asg::ExprOp,
    nir::NirEntity,
    sym::{GlobalSymbolIntern, GlobalSymbolResolve},
};

use super::Nir;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum NirToAir {
    #[default]
    Ready,
}

impl Display for NirToAir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAir::*;

        match self {
            Ready => write!(f, "ready to lower NIR to AIR"),
        }
    }
}

type QueuedObj = Option<Air>;

impl ParseState for NirToAir {
    type Token = Nir;
    type Object = Air;
    type Error = NirToAirError;
    type Context = QueuedObj;

    #[cfg(not(feature = "wip-nir-to-air"))]
    fn parse_token(
        self,
        tok: Self::Token,
        _queue: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;

        let _ = tok; // prevent `unused_variables` warning
        Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN))
    }

    #[cfg(feature = "wip-nir-to-air")]
    fn parse_token(
        self,
        tok: Self::Token,
        queue: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;

        // Single-item "queue".
        if let Some(obj) = queue.take() {
            return Transition(Ready).ok(obj).with_lookahead(tok);
        }

        match (self, tok) {
            (Ready, Nir::Open(NirEntity::Package, span)) => {
                Transition(Ready).ok(Air::PkgStart(span))
            }

            (Ready, Nir::Close(NirEntity::Package, span)) => {
                Transition(Ready).ok(Air::PkgEnd(span))
            }

            (Ready, Nir::Open(NirEntity::Rate | NirEntity::Sum, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Sum, span))
            }
            (Ready, Nir::Open(NirEntity::Product, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Product, span))
            }
            (Ready, Nir::Open(NirEntity::Ceil, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Ceil, span))
            }
            (Ready, Nir::Open(NirEntity::Floor, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Floor, span))
            }
            (Ready, Nir::Open(NirEntity::Classify | NirEntity::All, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Conj, span))
            }
            (Ready, Nir::Open(NirEntity::Any, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Disj, span))
            }

            (Ready, Nir::Open(NirEntity::Tpl, span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }
            (Ready, Nir::Close(NirEntity::Tpl, span)) => {
                Transition(Ready).ok(Air::TplEnd(span))
            }

            (Ready, Nir::Open(NirEntity::TplApply(None), span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }

            // Short-hand template application contains the name of the
            //   template _without_ the underscore padding as the local part
            //   of the QName.
            //
            // Template application will create an anonymous template,
            //   apply it,
            //   and then expand it.
            (Ready, Nir::Open(NirEntity::TplApply(Some(qname)), span)) => {
                // TODO: Determine whether caching these has any notable
                //   benefit over repeated heap allocations,
                //     comparing packages with very few applications and
                //     packages with thousands
                //       (we'd still have to hit the heap for the cache).
                let tpl_name =
                    format!("_{}_", qname.local_name().lookup_str()).intern();

                queue.replace(Air::RefIdent(SPair(tpl_name, span)));
                Transition(Ready).ok(Air::TplStart(span))
            }
            (Ready, Nir::Close(NirEntity::TplApply(_), span)) => {
                Transition(Ready).ok(Air::TplEndRef(span))
            }

            (
                Ready,
                Nir::Close(
                    NirEntity::Rate
                    | NirEntity::Sum
                    | NirEntity::Product
                    | NirEntity::Ceil
                    | NirEntity::Floor
                    | NirEntity::Classify
                    | NirEntity::All
                    | NirEntity::Any,
                    span,
                ),
            ) => Transition(Ready).ok(Air::ExprEnd(span)),

            (Ready, Nir::BindIdent(spair)) => {
                Transition(Ready).ok(Air::BindIdent(spair))
            }
            (Ready, Nir::Ref(spair)) => {
                Transition(Ready).ok(Air::RefIdent(spair))
            }

            (
                Ready,
                Nir::Todo
                | Nir::TodoAttr(..)
                | Nir::Desc(..)
                | Nir::Text(_)
                | Nir::Open(NirEntity::TplParam, _)
                | Nir::Close(NirEntity::TplParam, _),
            ) => Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN)),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        true
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NirToAirError {
    Todo,
}

impl Display for NirToAirError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAirError::*;

        match self {
            Todo => write!(f, "TODO"),
        }
    }
}

impl Error for NirToAirError {}

impl Diagnostic for NirToAirError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        // TODO
        vec![]
    }
}

#[cfg(all(test, feature = "wip-nir-to-air"))]
mod test;
