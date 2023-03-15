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
    ir::AirTemplatable,
    AirExprAggregate,
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
///   - A collection of [`AirTemplatable`] tokens representing the body of
///       the template that will be expanded into the application site when
///       the template is applied.
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

    Toplevel(
        ObjectIndex<Pkg>,
        ObjectIndex<Tpl>,
        AirExprAggregateStoreDangling<Tpl>,
        Option<SPair>,
    ),

    /// Aggregating tokens into a template.
    TplExpr(
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

            Self::Toplevel(_, expr, _, None)
            | Self::TplExpr(_, expr, _, None) => {
                write!(f, "building anonymous template with {expr}")
            }

            Self::Toplevel(_, expr, _, Some(name))
            | Self::TplExpr(_, expr, _, Some(name)) => {
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
    type Token = AirTemplatable;
    type Object = ();
    type Error = AsgError;
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirTpl::*};
        use AirTemplatable::*;
        use AirTplAggregate::*;

        match (self, tok) {
            (Ready(oi_pkg), AirTpl(TplStart(span))) => {
                let oi_tpl = asg.create(Tpl::new(span));

                Transition(Toplevel(
                    oi_pkg,
                    oi_tpl,
                    AirExprAggregate::new_in(oi_tpl),
                    None,
                ))
                .incomplete()
            }

            (Toplevel(..), AirTpl(TplStart(_span))) => todo!("nested tpl open"),

            (Toplevel(oi_pkg, oi_tpl, expr, _), AirBind(BindIdent(name))) => {
                asg.lookup_or_missing(name)
                    .bind_definition(asg, name, oi_tpl)
                    .map(|oi_ident| oi_pkg.defines(asg, oi_ident))
                    .map(|_| ())
                    .transition(Toplevel(oi_pkg, oi_tpl, expr, Some(name)))
            }

            (Toplevel(..), AirBind(RefIdent(_))) => {
                todo!("tpl Toplevel RefIdent")
            }

            (
                Toplevel(..),
                tok @ AirTpl(TplMetaStart(..) | TplMetaEnd(..) | TplApply(..)),
            ) => {
                todo!("Toplevel meta {tok:?}")
            }

            (Toplevel(..), tok @ AirTpl(TplLexeme(..))) => {
                todo!("err: Toplevel lexeme {tok:?} (must be within metavar)")
            }

            (Toplevel(oi_pkg, oi_tpl, _expr_done, _), AirTpl(TplEnd(span))) => {
                oi_tpl.close(asg, span);
                Transition(Ready(oi_pkg)).incomplete()
            }

            (TplExpr(oi_pkg, oi_tpl, expr, name), AirTpl(TplEnd(span))) => {
                // TODO: duplicated with AirAggregate
                match expr.is_accepting(asg) {
                    true => {
                        // TODO: this is duplicated with the above
                        oi_tpl.close(asg, span);
                        Transition(Ready(oi_pkg)).incomplete()
                    }
                    false => Transition(TplExpr(oi_pkg, oi_tpl, expr, name))
                        .err(AsgError::InvalidTplEndContext(span)),
                }
            }

            (Toplevel(..) | TplExpr(..), AirTpl(TplEndRef(..))) => {
                todo!("TplEndRef")
            }

            (
                Toplevel(oi_pkg, oi_tpl, expr, name)
                | TplExpr(oi_pkg, oi_tpl, expr, name),
                AirExpr(etok),
            ) => Self::delegate_expr(asg, oi_pkg, oi_tpl, expr, name, etok),

            (TplExpr(oi_pkg, oi_tpl, expr, name), AirBind(etok)) => {
                Self::delegate_expr(asg, oi_pkg, oi_tpl, expr, name, etok)
            }

            (TplExpr(..), AirTpl(TplStart(_))) => {
                todo!("nested template (template-generated template)")
            }

            (
                Ready(..) | TplExpr(..),
                tok @ AirTpl(
                    TplMetaStart(..) | TplLexeme(..) | TplMetaEnd(..)
                    | TplApply(..),
                ),
            ) => {
                todo!(
                    "metasyntactic token in non-tpl-toplevel context: {tok:?}"
                )
            }

            (st @ Ready(..), AirTpl(TplEnd(span) | TplEndRef(span))) => {
                Transition(st).err(AsgError::UnbalancedTpl(span))
            }

            (st @ Ready(..), tok @ (AirExpr(..) | AirBind(..))) => {
                Transition(st).dead(tok)
            }
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

    /// Delegate to the expression parser [`AirExprAggregate`].
    // TODO: Sir, this argument count is out of control.
    fn delegate_expr(
        asg: &mut <Self as ParseState>::Context,
        oi_pkg: ObjectIndex<Pkg>,
        oi_tpl: ObjectIndex<Tpl>,
        expr: AirExprAggregateStoreDangling<Tpl>,
        name: Option<SPair>,
        etok: impl Into<<AirExprAggregateStoreDangling<Tpl> as ParseState>::Token>,
    ) -> TransitionResult<Self> {
        let tok = etok.into();

        expr.parse_token(tok, asg).branch_dead::<Self, _>(
            |expr, ()| {
                Transition(Self::Toplevel(oi_pkg, oi_tpl, expr, name))
                    .incomplete()
            },
            |expr, result, ()| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::TplExpr(oi_pkg, oi_tpl, expr, name))
            },
            (),
        )
    }
}

#[cfg(test)]
mod test;
