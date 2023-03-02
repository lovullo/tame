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
    asg::{air::Air, ExprOp},
    diagnose::Diagnostic,
    nir::NirEntity,
    parse::prelude::*,
    span::UNKNOWN_SPAN,
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

impl ParseState for NirToAir {
    type Token = Nir;
    type Object = Air;
    type Error = NirToAirError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;

        #[cfg(not(feature = "wip-nir-to-air"))]
        {
            let _ = tok; // prevent `unused_variables` warning
            return Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN));
        }

        #[allow(unreachable_code)] // due to wip-nir-to-air
        match (self, tok) {
            (Ready, Nir::Open(NirEntity::Package, span)) => {
                Transition(Ready).ok(Air::PkgOpen(span))
            }

            (Ready, Nir::Close(NirEntity::Package, span)) => {
                Transition(Ready).ok(Air::PkgClose(span))
            }

            (Ready, Nir::Open(NirEntity::Rate | NirEntity::Sum, span)) => {
                Transition(Ready).ok(Air::ExprOpen(ExprOp::Sum, span))
            }
            (Ready, Nir::Open(NirEntity::Product, span)) => {
                Transition(Ready).ok(Air::ExprOpen(ExprOp::Product, span))
            }
            (Ready, Nir::Open(NirEntity::Classify | NirEntity::All, span)) => {
                Transition(Ready).ok(Air::ExprOpen(ExprOp::Conj, span))
            }
            (Ready, Nir::Open(NirEntity::Any, span)) => {
                Transition(Ready).ok(Air::ExprOpen(ExprOp::Disj, span))
            }

            (Ready, Nir::Open(NirEntity::Tpl, span)) => {
                Transition(Ready).ok(Air::TplOpen(span))
            }

            (Ready, Nir::Close(NirEntity::Tpl, span)) => {
                Transition(Ready).ok(Air::TplClose(span))
            }

            (
                Ready,
                Nir::Close(
                    NirEntity::Rate
                    | NirEntity::Sum
                    | NirEntity::Product
                    | NirEntity::Classify
                    | NirEntity::All
                    | NirEntity::Any,
                    span,
                ),
            ) => Transition(Ready).ok(Air::ExprClose(span)),

            (Ready, Nir::BindIdent(spair)) => {
                Transition(Ready).ok(Air::BindIdent(spair))
            }

            (
                Ready,
                Nir::Todo
                | Nir::TodoAttr(..)
                | Nir::Ref(..)
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
