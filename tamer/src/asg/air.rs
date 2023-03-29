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

use super::{
    graph::object::{ObjectIndexTo, Pkg, Tpl},
    Asg, AsgError, Ident, ObjectIndex,
};
use crate::{
    diagnose::Annotate, diagnostic_todo, parse::prelude::*, sym::SymbolId,
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
    Toplevel(ObjectIndex<Pkg>),

    /// Parsing an expression.
    ///
    /// This expects to inherit an [`AirExprAggregate`] from the prior state
    ///   so that we are not continuously re-allocating its stack for each
    ///   new expression root.
    PkgExpr(AirExprAggregateReachable<Pkg>),

    /// Parser is in template parsing mode.
    ///
    /// All objects encountered until the closing [`Air::TplEnd`] will be
    ///   parented to this template rather than the parent [`Pkg`].
    /// See [`Air::TplStart`] for more information.
    PkgTpl(AirTplAggregate),
}

impl Display for AirAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirAggregate::*;

        match self {
            Empty => write!(f, "awaiting AIR input for ASG"),
            Toplevel(_) => {
                write!(f, "expecting package header or an expression")
            }
            PkgExpr(expr) => {
                write!(f, "defining a package expression: {expr}")
            }
            PkgTpl(tpl) => {
                write!(f, "building a template: {tpl}",)
            }
        }
    }
}

impl ParseState for AirAggregate {
    type Token = Air;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;

    /// Destination [`Asg`] that this parser lowers into.
    ///
    /// This ASG will be yielded by [`crate::parse::Parser::finalize`].
    type PubContext = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use ir::{
            AirBind::*, AirIdent::*, AirPkg::*, AirSubsets::*, AirTodo::*,
        };
        use AirAggregate::*;

        // TODO: Seems to be about time for refactoring this...
        match (self, tok.into()) {
            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            (Empty, AirPkg(PkgStart(span))) => {
                let oi_pkg =
                    ctx.asg_mut().create(Pkg::new(span)).root(ctx.asg_mut());
                Transition(Toplevel(oi_pkg)).incomplete()
            }

            (Toplevel(oi_pkg), AirPkg(PkgStart(span))) => {
                Transition(Toplevel(oi_pkg))
                    .err(AsgError::NestedPkgStart(span, oi_pkg.span()))
            }

            (PkgExpr(expr), AirPkg(PkgStart(span))) => {
                let oi_pkg = ctx.stack_mut().rooting_oi().expect("TODO");
                Transition(PkgExpr(expr))
                    .err(AsgError::NestedPkgStart(span, oi_pkg.span()))
            }

            // No expression was started.
            (Toplevel(oi_pkg), AirPkg(PkgEnd(span))) => {
                oi_pkg.close(ctx.asg_mut(), span);
                Transition(Empty).incomplete()
            }

            // TODO: We don't support package ids yet
            (st @ Toplevel(..), AirBind(BindIdent(id))) => {
                Transition(st).err(AsgError::InvalidExprBindContext(id))
            }
            (st @ Toplevel(..), AirBind(RefIdent(id))) => {
                Transition(st).err(AsgError::InvalidExprRefContext(id))
            }

            (Toplevel(oi_pkg), tok @ AirExpr(..)) => {
                ctx.stack_mut().push(Toplevel(oi_pkg));
                let expr = AirExprAggregate::new_in(oi_pkg);
                Transition(PkgExpr(expr)).incomplete().with_lookahead(tok)
            }

            // TODO: This is temporary during refactoring
            //   (creating an AirExprAggregate just to pass to this).
            (Toplevel(oi_pkg), tok @ AirTpl(..)) => {
                ctx.stack_mut().push(Toplevel(oi_pkg));

                Transition(PkgTpl(AirTplAggregate::new()))
                    .incomplete()
                    .with_lookahead(tok)
            }

            // Note that templates may preempt expressions at any point,
            //   unlike in NIR at the time of writing.
            (PkgExpr(expr), tok @ AirTpl(..)) => {
                ctx.stack_mut().push(PkgExpr(expr));

                Transition(PkgTpl(AirTplAggregate::new()))
                    .incomplete()
                    .with_lookahead(tok)
            }

            // Note: We unfortunately can't match on `AirExpr | AirBind`
            //   and delegate in the same block
            //     (without having to duplicate type checks and then handle
            //       unreachable paths)
            //     because of the different inner types.
            (PkgExpr(expr), AirExpr(etok)) => {
                Self::delegate_expr(ctx, expr, etok)
            }
            (PkgExpr(expr), AirBind(etok)) => {
                Self::delegate_expr(ctx, expr, etok)
            }

            // Template parsing.
            (PkgTpl(tplst), AirExpr(ttok)) => {
                Self::delegate_tpl(ctx, tplst, ttok)
            }
            (PkgTpl(tplst), AirBind(ttok)) => {
                Self::delegate_tpl(ctx, tplst, ttok)
            }
            (PkgTpl(tplst), AirTpl(ttok)) => {
                Self::delegate_tpl(ctx, tplst, ttok)
            }

            (PkgTpl(..), tok @ AirPkg(PkgStart(..))) => {
                diagnostic_todo!(
                    vec![tok.note("for this token")],
                    "templates cannot contain packages"
                )
            }

            (Empty, AirPkg(PkgEnd(span))) => {
                Transition(Empty).err(AsgError::InvalidPkgEndContext(span))
            }

            (PkgExpr(expr), AirPkg(PkgEnd(span))) => {
                match expr.is_accepting(ctx) {
                    true => {
                        // TODO: this is duplicated
                        Transition(ctx.stack_mut().pop().expect("TODO"))
                            .incomplete()
                            .with_lookahead(AirPkg(PkgEnd(span)))
                    }
                    false => Transition(PkgExpr(expr))
                        .err(AsgError::InvalidPkgEndContext(span)),
                }
            }

            (PkgTpl(tplst), AirPkg(PkgEnd(span))) => {
                match tplst.is_accepting(ctx) {
                    // TODO
                    true => Transition(ctx.stack_mut().pop().expect("TODO"))
                        .incomplete()
                        .with_lookahead(AirPkg(PkgEnd(span))),
                    false => Transition(PkgTpl(tplst))
                        .err(AsgError::InvalidPkgEndContext(span)),
                }
            }

            (Empty, tok @ (AirExpr(..) | AirBind(..) | AirTpl(..))) => {
                Transition(Empty).err(AsgError::PkgExpected(tok.span()))
            }

            (Empty, AirIdent(IdentDecl(name, kind, src))) => ctx
                .asg_mut()
                .declare(name, kind, src)
                .map(|_| ())
                .transition(Empty),

            (Empty, AirIdent(IdentExternDecl(name, kind, src))) => ctx
                .asg_mut()
                .declare_extern(name, kind, src)
                .map(|_| ())
                .transition(Empty),

            (Empty, AirIdent(IdentDep(sym, dep))) => {
                ctx.asg_mut().add_dep_lookup_global(sym, dep);
                Transition(Empty).incomplete()
            }

            (Empty, AirIdent(IdentFragment(sym, text))) => ctx
                .asg_mut()
                .set_fragment(sym, text)
                .map(|_| ())
                .transition(Empty),

            (Empty, AirIdent(IdentRoot(sym))) => {
                let asg = ctx.asg_mut();
                let obj = asg.lookup_global_or_missing(sym);
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
        ctx: &mut <Self as ParseState>::Context,
        expr: AirExprAggregateReachable<Pkg>,
        etok: impl Into<<AirExprAggregateReachable<Pkg> as ParseState>::Token>,
    ) -> TransitionResult<Self> {
        let tok = etok.into();

        expr.parse_token(tok, ctx).branch_dead::<Self, _>(
            |_, ()| {
                Transition(ctx.stack_mut().pop().expect("TODO")).incomplete()
            },
            |expr, result, ()| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::PkgExpr(expr))
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
        ctx: &mut <Self as ParseState>::Context,
        tplst: AirTplAggregate,
        ttok: impl Into<<AirTplAggregate as ParseState>::Token>,
    ) -> TransitionResult<Self> {
        tplst.parse_token(ttok.into(), ctx).branch_dead::<Self, _>(
            |_, ()| {
                Transition(ctx.stack_mut().pop().expect("TODO")).incomplete()
            },
            |tplst, result, ()| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::PkgTpl(tplst))
            },
            (),
        )
    }
}

/// Additional parser context.
///
/// TODO: This was introduced to hold additional context;
///   see future commit.
#[derive(Debug, Default)]
pub struct AirAggregateCtx(Asg, AirStack);

impl AirAggregateCtx {
    fn asg_mut(&mut self) -> &mut Asg {
        self.as_mut()
    }

    fn stack_mut(&mut self) -> &mut AirStack {
        self.as_mut()
    }
}

impl AsRef<Asg> for AirAggregateCtx {
    fn as_ref(&self) -> &Asg {
        match self {
            Self(asg, _) => asg,
        }
    }
}

impl AsMut<Asg> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut Asg {
        match self {
            Self(asg, _) => asg,
        }
    }
}

impl AsMut<AirStack> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut AirStack {
        match self {
            Self(_, stack) => stack,
        }
    }
}

impl From<AirAggregateCtx> for Asg {
    fn from(ctx: AirAggregateCtx) -> Self {
        match ctx {
            AirAggregateCtx(asg, _) => asg,
        }
    }
}

impl From<Asg> for AirAggregateCtx {
    fn from(asg: Asg) -> Self {
        Self(asg, Default::default())
    }
}

/// Held parser stack frames.
///
/// [`ObjectIndex`] lookups perform reverse linear searches beginning from
///   the last stack frame until a non-[`None`] value is found;
///     this creates an environment whereby inner contexts shadow outer.
/// Missing values create holes,
///   much like a prototype chain.
/// In practice,
///   this should only have to search the last two frames.
///
/// TODO: This is still under development.
#[derive(Debug, Default)]
pub struct AirStack(Vec<AirAggregate>);

impl AirStack {
    fn push(&mut self, st: AirAggregate) {
        let Self(stack) = self;
        stack.push(st);
    }

    fn pop(&mut self) -> Option<AirAggregate> {
        let Self(stack) = self;
        stack.pop()
    }

    /// The active container (binding context) for [`Ident`]s.
    ///
    /// A value of [`None`] indicates that no bindings are permitted in the
    ///   current context.
    fn rooting_oi(&self) -> Option<ObjectIndexTo<Ident>> {
        let Self(stack) = self;

        stack.iter().rev().find_map(|st| match *st {
            AirAggregate::Empty => None,
            AirAggregate::Toplevel(pkg_oi) => Some(pkg_oi.into()),

            // Expressions never serve as roots for identifiers;
            //   this will always fall through to the parent context.
            // Since the parent context is a package or a template,
            //   the next frame should succeed.
            AirAggregate::PkgExpr(_) => None,

            AirAggregate::PkgTpl(_) => {
                diagnostic_todo!(vec![], "PkgTpl rooting_oi")
            }
        })
    }

    /// The active expansion target (splicing context) for [`Tpl`]s.
    ///
    /// A value of [`None`] indicates that template expansion is not
    ///   permitted in this current context.
    fn expansion_oi(&self) -> Option<ObjectIndexTo<Tpl>> {
        let Self(stack) = self;

        stack.iter().rev().find_map(|st| match *st {
            AirAggregate::Empty => None,
            AirAggregate::Toplevel(pkg_oi) => Some(pkg_oi.into()),
            AirAggregate::PkgExpr(_) => {
                diagnostic_todo!(vec![], "PkgExpr expansion_oi")
            }
            AirAggregate::PkgTpl(_) => {
                diagnostic_todo!(vec![], "PkgTpl expansion_oi")
            }
        })
    }
}

#[cfg(test)]
mod test;
