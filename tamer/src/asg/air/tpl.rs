// ASG IR template parsing
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

//! AIR template parser.
//!
//! See the [parent module](super) for more information.

use super::{
    super::{
        graph::object::{Pkg, Tpl},
        Asg, AsgError, ObjectIndex,
    },
    expr::AirExprAggregateStoreDangling,
    Air, AirExprAggregate,
};
use crate::{
    fmt::{DisplayWrapper, TtQuote},
    parse::prelude::*,
};

/// Template parser and token aggregator.
///
/// A template consists of
///
///   - Metadata about the template,
///       including its parameters; and
///   - A collection of [`Air`] tokens representing the body of the
///       template that will be expanded into the application site when the
///       template is applied.
///
/// This contains an embedded [`AirExprAggregate`] parser for handling
///   expressions just the same as [`super::AirAggregate`] does with
///   packages.
#[derive(Debug, PartialEq)]
pub enum AirTplAggregate {
    /// Ready for a template,
    ///   defined as part of the given package.
    ///
    /// This state also includes the template header;
    ///   unlike NIR,
    ///     AIR has no restrictions on when template header tokens are
    ///     provided,
    ///       which simplifies AIR generation.
    Ready(ObjectIndex<Pkg>),

    /// Aggregating tokens into a template.
    BuildingTpl(
        ObjectIndex<Pkg>,
        ObjectIndex<Tpl>,
        AirExprAggregateStoreDangling<Tpl>,
        Option<SPair>,
    ),
}

impl Display for AirTplAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready(_) => write!(f, "ready for template definition"),
            Self::BuildingTpl(_, expr, _, None) => {
                write!(f, "building anonymous template with {expr}")
            }
            Self::BuildingTpl(_, expr, _, Some(name)) => {
                write!(
                    f,
                    "building named template {} with {expr}",
                    TtQuote::wrap(name)
                )
            }
        }
    }
}

impl ParseState for AirTplAggregate {
    type Token = Air;
    type Object = ();
    type Error = AsgError;
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirSubsets::*, AirTodo::*, AirTpl::*};
        use AirTplAggregate::*;

        match (self, tok.into()) {
            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            (Ready(oi_pkg), AirTpl(TplOpen(span))) => {
                let oi_tpl = asg.create(Tpl::new(span));

                Transition(BuildingTpl(
                    oi_pkg,
                    oi_tpl,
                    AirExprAggregate::new_in(oi_tpl),
                    None,
                ))
                .incomplete()
            }

            (
                BuildingTpl(oi_pkg, oi_tpl, expr, _),
                AirBind(BindIdent(name)),
            ) => asg
                .lookup_or_missing(name)
                .bind_definition(asg, name, oi_tpl)
                .map(|oi_ident| oi_pkg.defines(asg, oi_ident))
                .map(|_| ())
                .transition(BuildingTpl(oi_pkg, oi_tpl, expr, Some(name))),

            (
                BuildingTpl(oi_pkg, oi_tpl, _expr_done, _),
                AirTpl(TplClose(span)),
            ) => {
                oi_tpl.close(asg, span);
                Transition(Ready(oi_pkg)).incomplete()
            }

            (BuildingTpl(..), AirPkg(_)) => {
                todo!("template cannot define packages")
            }

            (BuildingTpl(..), tok) => todo!("BuildingTpl body: {tok:?}"),

            (st @ Ready(..), AirTpl(TplClose(span))) => {
                Transition(st).err(AsgError::UnbalancedTpl(span))
            }

            (
                st @ Ready(..),
                tok @ (AirPkg(..) | AirExpr(..) | AirBind(..) | AirIdent(..)),
            ) => Transition(st).dead(tok.into()),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready(..))
    }
}

impl AirTplAggregate {
    pub(super) fn new_in_pkg(oi_pkg: ObjectIndex<Pkg>) -> Self {
        Self::Ready(oi_pkg)
    }
}

#[cfg(test)]
mod test;
