// ASG IR
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

//! Intermediate representation for construction of the
//!   [abstract semantic graph (ASG)](super) (AIR).
//!
//! AIR serves as an abstraction layer between higher-level parsers and the
//!   aggregate ASG.
//! It allows parsers to operate as a raw stream of data without having to
//!   worry about ownership of or references to the ASG,
//!     and allows for multiple such parsers to be joined.
//!
//! AIR is _not_ intended to replace the API of the ASG---it
//!   is intended as a termination point for the parsing pipeline,
//!     and as such implements a subset of the ASG's API that is suitable
//!     for aggregating raw data from source and object files.
//! Given that it does so little and is so close to the [`Asg`] API,
//!   one might say that the abstraction is as light as air,
//!   but that would surely result in face-palming and so we're not going
//!     air such cringeworthy dad jokes here.

use self::expr::AirExprAggregateReachable;

use super::{graph::object::Pkg, Asg, AsgError, ObjectIndex};
use crate::{
    diagnose::Annotate, diagnostic_unreachable, parse::prelude::*,
    sym::SymbolId,
};
use std::fmt::{Debug, Display};

#[macro_use]
mod ir;
pub use ir::Air;

mod expr;
mod tpl;
use expr::AirExprAggregate;
use tpl::AirTplAggregate;

pub type IdentSym = SymbolId;
pub type DepSym = SymbolId;

/// AIR parser state.
#[derive(Debug, PartialEq, Default)]
pub enum AirAggregate {
    /// Parser is not currently performing any work.
    #[default]
    Empty,

    /// Expecting a package-level token.
    PkgHead(ObjectIndex<Pkg>, AirExprAggregateReachable<Pkg>),

    /// Parsing an expression.
    ///
    /// This expects to inherit an [`AirExprAggregate`] from the prior state
    ///   so that we are not continuously re-allocating its stack for each
    ///   new expression root.
    PkgExpr(ObjectIndex<Pkg>, AirExprAggregateReachable<Pkg>),

    /// Parser is in template parsing mode.
    ///
    /// All objects encountered until the closing [`Air::TplClose`] will be
    ///   parented to this template rather than the parent [`Pkg`].
    /// See [`Air::TplOpen`] for more information.
    PkgTpl(
        ObjectIndex<Pkg>,
        AirExprAggregateReachable<Pkg>,
        AirTplAggregate,
    ),
}

impl Display for AirAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirAggregate::*;

        match self {
            Empty => write!(f, "awaiting AIR input for ASG"),
            PkgHead(_, _) => {
                write!(f, "expecting package header or an expression")
            }
            PkgExpr(_, expr) => {
                write!(f, "defining a package expression: {expr}")
            }
            PkgTpl(_, _, tpl) => {
                write!(f, "building a template: {tpl}",)
            }
        }
    }
}

impl ParseState for AirAggregate {
    type Token = Air;
    type Object = ();
    type Error = AsgError;

    /// Destination [`Asg`] that this parser lowers into.
    ///
    /// This ASG will be yielded by [`crate::parse::Parser::finalize`].
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use ir::{
            AirIdent::*, AirPkg::*, AirSubsets::*, AirTodo::*, AirTpl::*,
        };
        use AirAggregate::*;

        // TODO: Seems to be about time for refactoring this...
        match (self, tok.into()) {
            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            (Empty, AirPkg(PkgOpen(span))) => {
                let oi_pkg = asg.create(Pkg::new(span)).root(asg);
                Transition(PkgHead(oi_pkg, AirExprAggregate::new_in(oi_pkg)))
                    .incomplete()
            }

            (PkgHead(oi_pkg, expr), AirPkg(PkgOpen(span))) => {
                Transition(PkgHead(oi_pkg, expr))
                    .err(AsgError::NestedPkgOpen(span, oi_pkg.span()))
            }

            (PkgExpr(oi_pkg, expr), AirPkg(PkgOpen(span))) => {
                Transition(PkgExpr(oi_pkg, expr))
                    .err(AsgError::NestedPkgOpen(span, oi_pkg.span()))
            }

            // No expression was started.
            (PkgHead(oi_pkg, _expr), AirPkg(PkgClose(span))) => {
                oi_pkg.close(asg, span);
                Transition(Empty).incomplete()
            }

            (PkgHead(..), AirBind(ident)) => {
                todo!("PkgBody AirBind {ident:?}")
            }

            (PkgHead(oi_pkg, expr), tok @ AirExpr(..)) => {
                Transition(PkgExpr(oi_pkg, expr))
                    .incomplete()
                    .with_lookahead(tok)
            }

            // Note that templates may preempt expressions at any point,
            //   unlike in NIR at the time of writing.
            (
                PkgHead(oi_pkg, expr) | PkgExpr(oi_pkg, expr),
                tok @ AirTpl(..),
            ) => Transition(PkgTpl(
                oi_pkg,
                expr,
                AirTplAggregate::new_in_pkg(oi_pkg),
            ))
            .incomplete()
            .with_lookahead(tok),

            // Note: We unfortunately can't match on `AirExpr | AirBind`
            //   and delegate in the same block
            //     (without having to duplicate type checks and then handle
            //       unreachable paths)
            //     because of the different inner types.
            (PkgExpr(oi_pkg, expr), AirExpr(etok)) => {
                Self::delegate_expr(asg, oi_pkg, expr, etok)
            }
            (PkgExpr(oi_pkg, expr), AirBind(etok)) => {
                Self::delegate_expr(asg, oi_pkg, expr, etok)
            }

            // Templates can contain just about anything,
            //   so completely hand over parsing when we're in template mode.
            (PkgTpl(oi_pkg, stored_expr, tplst), tok) => {
                Self::delegate_tpl(asg, oi_pkg, stored_expr, tplst, tok.into())
            }

            (Empty, AirTpl(TplClose(..))) => {
                todo!("Empty AirTpl::TplClose")
            }

            (Empty, AirPkg(PkgClose(span))) => {
                Transition(Empty).err(AsgError::InvalidPkgCloseContext(span))
            }

            (PkgExpr(oi_pkg, expr), AirPkg(PkgClose(span))) => {
                match expr.is_accepting(asg) {
                    true => {
                        // TODO: this is duplicated with the above
                        oi_pkg.close(asg, span);
                        Transition(Empty).incomplete()
                    }
                    false => Transition(PkgExpr(oi_pkg, expr))
                        .err(AsgError::InvalidPkgCloseContext(span)),
                }
            }

            (Empty, tok @ (AirExpr(..) | AirBind(..) | AirTpl(TplOpen(_)))) => {
                Transition(Empty).err(AsgError::PkgExpected(tok.span()))
            }

            (Empty, AirIdent(IdentDecl(name, kind, src))) => {
                asg.declare(name, kind, src).map(|_| ()).transition(Empty)
            }

            (Empty, AirIdent(IdentExternDecl(name, kind, src))) => asg
                .declare_extern(name, kind, src)
                .map(|_| ())
                .transition(Empty),

            (Empty, AirIdent(IdentDep(sym, dep))) => {
                asg.add_dep_lookup(sym, dep);
                Transition(Empty).incomplete()
            }

            (Empty, AirIdent(IdentFragment(sym, text))) => {
                asg.set_fragment(sym, text).map(|_| ()).transition(Empty)
            }

            (Empty, AirIdent(IdentRoot(sym))) => {
                let obj = asg.lookup_or_missing(sym);
                asg.add_root(obj);

                Transition(Empty).incomplete()
            }

            (st, tok @ AirIdent(..)) => todo!("{st:?}, {tok:?}"),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Empty)
    }
}

impl AirAggregate {
    /// Delegate to the expression parser [`AirExprAggregate`].
    ///
    /// TODO: This ought to be further reduced into primitives in the core
    ///   [`crate::parse`] framework.
    fn delegate_expr(
        asg: &mut <Self as ParseState>::Context,
        oi_pkg: ObjectIndex<Pkg>,
        expr: AirExprAggregateReachable<Pkg>,
        etok: impl Into<<AirExprAggregateReachable<Pkg> as ParseState>::Token>,
    ) -> TransitionResult<Self> {
        let tok = etok.into();
        let tokspan = tok.span();

        expr.parse_token(tok, asg).branch_dead::<Self, _>(
            // TODO: Enforce using type system to avoid need for this
            //   runtime check and prove that it is indeed impossible
            //     (which otherwise could fail to be the case due to changes
            //       since this was written).
            |_, ()| {
                diagnostic_unreachable!(
                    vec![tokspan.internal_error(
                        "unexpected dead state transition at this token"
                    )],
                    "AirExprAggregate should not have dead states"
                )
            },
            |expr, result, ()| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::PkgExpr(oi_pkg, expr))
            },
            (),
        )
    }

    /// Delegate to the expression parser [`AirTplAggregate`].
    ///
    /// After template parsing is complete
    ///   (when reaching a dead state),
    ///   the stored expression [`AirExprAggregate`] is reinstated,
    ///     allowing parsing to continue where it left off before being
    ///     preempted by template parsing.
    fn delegate_tpl(
        asg: &mut <Self as ParseState>::Context,
        oi_pkg: ObjectIndex<Pkg>,
        stored_expr: AirExprAggregateReachable<Pkg>,
        tplst: AirTplAggregate,
        tok: Air,
    ) -> TransitionResult<Self> {
        tplst.parse_token(tok, asg).branch_dead::<Self, _>(
            |_, stored_expr| {
                Transition(Self::PkgExpr(oi_pkg, stored_expr)).incomplete()
            },
            |tplst, result, stored_expr| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::PkgTpl(oi_pkg, stored_expr, tplst))
            },
            stored_expr,
        )
    }
}

#[cfg(test)]
mod test;
