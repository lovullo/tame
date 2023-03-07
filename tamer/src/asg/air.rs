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

use self::ir::AirBindableExpr;

use super::{
    graph::object::{Expr, Pkg},
    Asg, AsgError, ObjectIndex,
};
use crate::{
    asg::graph::object::Tpl,
    diagnose::Annotate,
    diagnostic_unreachable,
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{prelude::*, util::SPair},
    span::Span,
    sym::SymbolId,
};
use std::fmt::{Debug, Display};

#[macro_use]
mod ir;
pub use ir::Air;

pub type IdentSym = SymbolId;
pub type DepSym = SymbolId;

/// Stack of held expressions,
///   with the root expression at the bottom of the stack.
///
/// Expression [`ObjectIndex`]es are pushed onto this stack when
///   parsing a subexpression,
///     and are popped when the subexpression terminates.
/// The active expression is _not_ stored on this stack to avoid unnecessary
///   indirection.
///
/// Despite the immutable interface,
///   this does modify the inner [`Vec`] in-place;
///     it does not reallocate unless its capacity has been reached.
///
/// Unlike other parts of the system,
///   this is heap-allocated,
///   but should be very cache-friendly.
/// This reason for heap allocation is that this is explicitly
///   _unbounded_—systems like code generators ought to be able to output
///   expressions in a tacit style without worrying about arbitrary limits.
/// It is worth noting that the other parts of the system using
///   stack-allocated data structures is less about performance and more
///   about the simplicity afforded by keeping allocators out of the picture.
/// We'll address performance issues if they appear during profiling.
///
/// Another benefit of using [`Vec`] here is that Rust is able to properly
///   optimize away `memcpy`s for it,
///     rather than having to utilize the parser's mutable context.
/// Further,
///   the ASG is heap-allocated,
///   so we're not avoiding the heap anyway.
///
/// The interface is modeled after [Haskell's `Stack`][haskell-stack],
///   with a slight variation for [`Self::pop`] so that we can avoid
///   reallocation after a stack is used up,
///     which is frequent.
///
/// [haskell-stack]: https://hackage.haskell.org/package/Stack/docs/Data-Stack.html
///
/// The stack states [`Dormant`] and [`Active`] selectively provide
///   different APIs to enforce certain invariants,
///     as an alternative to re-allocating an inner [`Vec`] each time a new
///     root expression is encountered.
#[derive(Debug, PartialEq, Eq)]
pub struct ExprStack<S>(Vec<ObjectIndex<Expr>>, S);

/// Expression stack is not in use and must be empty;
///   no ongoing expression parsing.
#[derive(Debug, PartialEq, Eq)]
pub struct Dormant;
/// Expression stack is in use as part of an expression parse.
#[derive(Debug, PartialEq, Eq)]
pub struct Active(StackEdge);

#[derive(Debug, PartialEq, Eq)]
pub enum StackEdge {
    /// Root expression is yet not reachable from any other object.
    ///
    /// Dangling expressions are expected to transition into
    ///   [`Self::Reachable`] after being bound to an identifier.
    /// Closing a dangling expression will result in a
    ///   [`AsgError::DanglingExpr`].
    ///
    /// Binding a sub-expression does not bind the root of the stack,
    ///   since sub-expressions cannot reference their parent;
    ///     a stack is dangling until its root expression has been bound to
    ///     an identifier.
    Dangling,

    /// Root expression is reachable from another object.
    ///
    /// The associated [`SPair`] serves as _evidence_ of this assertion.
    Reachable(SPair),
}

impl Display for StackEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Dangling => write!(f, "dangling"),
            Self::Reachable(ident) => {
                write!(f, "reachable (by {})", TtQuote::wrap(ident))
            }
        }
    }
}

impl ExprStack<Dormant> {
    /// Mark the stack as active,
    ///   exposing its stack API for use.
    ///
    /// [`ExprStack::done`] will return the stack to a dormant state.
    fn activate(self) -> ExprStack<Active> {
        let Self(stack, _) = self;
        ExprStack(stack, Active(StackEdge::Dangling))
    }
}

impl ExprStack<Active> {
    fn push(self, item: ObjectIndex<Expr>) -> Self {
        let Self(mut stack, s) = self;
        stack.push(item);
        Self(stack, s)
    }

    /// Attempt to remove an item from the stack,
    ///   returning a new stack and the item,
    ///   if any.
    ///
    /// This returns a new [`Self`] even if it is empty so that it can be
    ///   reused without having to reallocate.
    fn pop(self) -> (Self, Option<ObjectIndex<Expr>>) {
        let Self(mut stack, s) = self;
        let oi = stack.pop();

        (Self(stack, s), oi)
    }

    /// Whether the stack is dangling.
    fn is_dangling(&self) -> bool {
        matches!(self, Self(_, Active(StackEdge::Dangling)))
    }

    /// Mark stack as reachable if processing the root expression.
    ///
    /// `ident` is admitted as evidence of reachability,
    ///   both for debugging and for making it more difficult to
    ///   misuse this API.
    /// If the stack is already reachable,
    ///   the previous identifier takes precedence.
    ///
    /// If not parsing the root expression
    ///   (if the stack is non-empty),
    ///   this returns `self` unchanged.
    fn reachable_by(self, ident: SPair) -> Self {
        match self {
            Self(stack, Active(StackEdge::Dangling)) if stack.is_empty() => {
                Self(stack, Active(StackEdge::Reachable(ident)))
            }
            _ => self,
        }
    }

    /// Mark the stack as dormant,
    ///   hiding its stack API and ensuring that its state is properly reset
    ///   for the next root expression.
    ///
    /// [`ExprStack::activate`] will re-activate the stack for use.
    fn done(self) -> ExprStack<Dormant> {
        let Self(stack, _) = self;

        // TODO: error if non-empty stack (unclosed expr)
        if !stack.is_empty() {
            todo!("ExprStack::done(): error on non-empty stack")
        }

        ExprStack(stack, Dormant)
    }
}

impl Default for ExprStack<Dormant> {
    fn default() -> Self {
        // TODO: 16 is a generous guess that is very unlikely to be exceeded
        //   in practice at the time of writing,
        //     even with template expansion,
        //     but let's develop an informed heuristic.
        //  Note that this is very unlikely to make a difference;
        //    I just don't like using numbers without data to back them up.
        Self(Vec::with_capacity(16), Dormant)
    }
}

impl Display for ExprStack<Dormant> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(stack, _) = self;
        write!(f, "dormant expression stack of size {}", stack.capacity())
    }
}

impl Display for ExprStack<Active> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(stack, Active(edge_st)) = self;
        write!(
            f,
            "active {edge_st} expression stack of length {} and size {}",
            stack.len(),
            stack.capacity()
        )
    }
}

/// AIR parser state.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum AirAggregate {
    /// Parser is not currently performing any work.
    #[default]
    Empty,

    /// Expecting a package-level token.
    PkgHead(ObjectIndex<Pkg>, AirExprAggregate),

    /// Parsing an expression.
    ///
    /// This expects to inherit an [`AirExprAggregate`] from the prior state
    ///   so that we are not continuously re-allocating its stack for each
    ///   new expression root.
    PkgExpr(ObjectIndex<Pkg>, AirExprAggregate),

    /// Parser is in template parsing mode.
    ///
    /// All objects encountered until the closing [`Air::TplClose`] will be
    ///   parented to this template rather than the parent [`Pkg`].
    /// See [`Air::TplOpen`] for more information.
    BuildingTpl(
        (ObjectIndex<Pkg>, AirExprAggregate),
        ObjectIndex<Tpl>,
        Option<SPair>,
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
            BuildingTpl((_, expr), _, None) => {
                write!(f, "building anonymous template with {expr}")
            }
            BuildingTpl((_, expr), _, Some(name)) => {
                write!(
                    f,
                    "building named template {} with {expr}",
                    TtQuote::wrap(name)
                )
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
            AirBind::*, AirIdent::*, AirPkg::*, AirSubsets::*, AirTodo::*,
            AirTpl::*,
        };
        use AirAggregate::*;

        // TODO: Seems to be about time for refactoring this...
        match (self, tok.into()) {
            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            (Empty, AirPkg(PkgOpen(span))) => {
                let oi_pkg = asg.create(Pkg::new(span)).root(asg);
                Transition(PkgHead(oi_pkg, AirExprAggregate::parse_pkg(oi_pkg)))
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

            (
                PkgExpr(oi_pkg, expr) | PkgHead(oi_pkg, expr),
                AirTpl(TplOpen(span)),
            ) => {
                let oi_tpl = asg.create(Tpl::new(span));

                Transition(BuildingTpl((oi_pkg, expr), oi_tpl, None))
                    .incomplete()
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

            (PkgExpr(..), AirTpl(TplClose(..))) => {
                todo!("PkgExpr AirTpl::TplClose")
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

            (
                BuildingTpl((oi_pkg, expr), oi_tpl, None),
                AirBind(BindIdent(name)),
            ) => asg
                .lookup_or_missing(name)
                .bind_definition(asg, name, oi_tpl)
                .map(|oi_ident| oi_pkg.defines(asg, oi_ident))
                .map(|_| ())
                .transition(BuildingTpl((oi_pkg, expr), oi_tpl, Some(name))),

            (
                BuildingTpl((oi_pkg, expr), oi_tpl, _),
                AirTpl(TplClose(span)),
            ) => {
                oi_tpl.close(asg, span);
                Transition(PkgHead(oi_pkg, expr)).incomplete()
            }

            (BuildingTpl(..), tok) => todo!("BuildingTpl body: {tok:?}"),

            (st @ (Empty | PkgHead(..)), AirTpl(TplClose(span))) => {
                Transition(st).err(AsgError::UnbalancedTpl(span))
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
        expr: AirExprAggregate,
        etok: impl Into<<AirExprAggregate as ParseState>::Token>,
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
}

/// Parse an AIR expression with binding support.
///
/// Expressions are composable,
///   so this parser need only care about whether it has any active
///   expression being parsed.
///
/// This parser has no dead states---it
///   handles each of its tokens and performs error recovery on invalid
///   state transitions.
#[derive(Debug, PartialEq, Eq)]
pub enum AirExprAggregate {
    /// Ready for an expression;
    ///   expression stack is empty.
    Ready(ObjectIndex<Pkg>, ExprStack<Dormant>),

    /// Building an expression.
    BuildingExpr(ObjectIndex<Pkg>, ExprStack<Active>, ObjectIndex<Expr>),
}

impl Display for AirExprAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready(_, es) => write!(f, "ready for expression with {es}"),
            Self::BuildingExpr(_, es, _) => {
                write!(f, "building expression with {es}")
            }
        }
    }
}

impl ParseState for AirExprAggregate {
    type Token = AirBindableExpr;
    type Object = ();
    type Error = AsgError;
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self::Super> {
        use ir::{AirBind::*, AirExpr::*};
        use AirBindableExpr::*;
        use AirExprAggregate::*;

        match (self, tok) {
            (Ready(oi_pkg, es), AirExpr(ExprOpen(op, span))) => {
                let oi = asg.create(Expr::new(op, span));
                Transition(BuildingExpr(oi_pkg, es.activate(), oi)).incomplete()
            }

            (BuildingExpr(oi_pkg, es, poi), AirExpr(ExprOpen(op, span))) => {
                let oi = poi.create_subexpr(asg, Expr::new(op, span));
                Transition(BuildingExpr(oi_pkg, es.push(poi), oi)).incomplete()
            }

            (BuildingExpr(oi_pkg, es, oi), AirExpr(ExprClose(end))) => {
                let start: Span = oi.into();

                let _ = oi.map_obj(asg, |expr| {
                    expr.map(|span| span.merge(end).unwrap_or(span))
                });

                match es.pop() {
                    (es, Some(poi)) => {
                        Transition(BuildingExpr(oi_pkg, es, poi)).incomplete()
                    }
                    (es, None) => {
                        let dangling = es.is_dangling();
                        let st = Ready(oi_pkg, es.done());

                        if dangling {
                            Transition(st).err(AsgError::DanglingExpr(
                                start.merge(end).unwrap_or(start),
                            ))
                        } else {
                            Transition(st).incomplete()
                        }
                    }
                }
            }

            (BuildingExpr(oi_pkg, es, oi), AirBind(BindIdent(id))) => {
                let oi_ident = asg.lookup_or_missing(id);
                oi_pkg.defines(asg, oi_ident);

                // It is important that we do not mark this expression as
                //   reachable unless we successfully bind the identifier.
                match oi_ident.bind_definition(asg, id, oi) {
                    Ok(_) => Transition(BuildingExpr(
                        oi_pkg,
                        es.reachable_by(id),
                        oi,
                    ))
                    .incomplete(),
                    Err(e) => Transition(BuildingExpr(oi_pkg, es, oi)).err(e),
                }
            }

            (BuildingExpr(oi_pkg, es, oi), AirBind(RefIdent(ident))) => {
                Transition(BuildingExpr(oi_pkg, es, oi.ref_expr(asg, ident)))
                    .incomplete()
            }

            (st @ Ready(..), AirBind(BindIdent(id))) => {
                Transition(st).err(AsgError::InvalidExprBindContext(id))
            }

            (st @ Ready(..), AirBind(RefIdent(id))) => {
                Transition(st).err(AsgError::InvalidExprRefContext(id))
            }

            (st @ Ready(..), AirExpr(ExprClose(span))) => {
                Transition(st).err(AsgError::UnbalancedExpr(span))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready(..))
    }
}

impl AirExprAggregate {
    fn parse_pkg(oi: ObjectIndex<Pkg>) -> Self {
        Self::Ready(oi, ExprStack::default())
    }
}

#[cfg(test)]
mod test;
