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

use super::Nir;
use crate::{
    asg::air::Air, diagnose::Diagnostic, parse::prelude::*, span::UNKNOWN_SPAN,
};
use std::{error::Error, fmt::Display};

// These are also used by the `test` module which imports `super`.
#[cfg(feature = "wip-nir-to-air")]
use crate::{
    asg::ExprOp,
    nir::{Nir::*, NirEntity::*},
};

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

        use crate::{diagnose::Annotate, diagnostic_panic};

        // Single-item "queue".
        if let Some(obj) = queue.take() {
            return Transition(Ready).ok(obj).with_lookahead(tok);
        }

        match (self, tok) {
            (Ready, Open(Package, span)) => {
                Transition(Ready).ok(Air::PkgStart(span))
            }

            (Ready, Close(Package, span)) => {
                Transition(Ready).ok(Air::PkgEnd(span))
            }

            (Ready, Open(Rate | Sum, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Sum, span))
            }
            (Ready, Open(Product, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Product, span))
            }
            (Ready, Open(Ceil, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Ceil, span))
            }
            (Ready, Open(Floor, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Floor, span))
            }
            (Ready, Open(Classify | All, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Conj, span))
            }
            (Ready, Open(Any, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Disj, span))
            }

            (Ready, Open(Tpl, span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }
            (Ready, Close(Tpl, span)) => {
                Transition(Ready).ok(Air::TplEnd(span))
            }

            (Ready, Open(TplApply, span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }

            // Short-hand template application must be handled through
            //   desugaring as part of the lowering pipeline,
            //     so that it is converted to long form before getting here.
            (
                Ready,
                Open(TplApplyShort(..) | TplParamShort(..), span)
                | Close(TplApplyShort(..) | TplParamShort(..), span),
            ) => {
                // TODO: In the future maybe TAMER will have evolved its
                //   abstractions enough that there's an ROI for prohibiting
                //   this at the type level.
                diagnostic_panic!(
                    vec![
                        span.internal_error(
                            "attempted shorthand template application"
                        ),
                        span.help(
                            "TAMER must be compiled with support for \
                               shorthand template application by utilizing the \
                               nir::tplshort module in the lowering pipeline."
                        )
                    ],
                    "shortand template application is unsupported in this \
                         build of TAMER"
                )
            }
            (Ready, Close(TplApply, span)) => {
                Transition(Ready).ok(Air::TplEndRef(span))
            }

            (Ready, Open(TplParam, span)) => {
                Transition(Ready).ok(Air::TplMetaStart(span))
            }
            (Ready, Close(TplParam, span)) => {
                Transition(Ready).ok(Air::TplMetaEnd(span))
            }
            (Ready, Text(lexeme)) => {
                Transition(Ready).ok(Air::TplLexeme(lexeme))
            }

            (
                Ready,
                Close(
                    Rate | Sum | Product | Ceil | Floor | Classify | All | Any,
                    span,
                ),
            ) => Transition(Ready).ok(Air::ExprEnd(span)),

            (Ready, BindIdent(spair)) => {
                Transition(Ready).ok(Air::BindIdent(spair))
            }
            (Ready, Ref(spair)) => Transition(Ready).ok(Air::RefIdent(spair)),

            (Ready, Todo | TodoAttr(..) | Desc(..)) => {
                Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN))
            }
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
