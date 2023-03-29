// ASG IR expression parsing
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

//! AIR expression parser.
//!
//! See the [parent module](super) for more information.

use super::{
    super::{
        graph::object::{Expr, Object},
        Asg, AsgError, ObjectIndex,
    },
    ir::AirBindableExpr,
    AirAggregateCtx,
};
use crate::{
    asg::{
        graph::object::{ObjectIndexRelTo, ObjectIndexTo},
        Ident, ObjectKind,
    },
    f::Functor,
    parse::prelude::*,
};

#[cfg(doc)]
use StackEdge::{Dangling, Reachable};

/// Parse and aggregate [`Reachable`] [`Expr`]s into the graph,
///   with expression roots bound to their associated [`Ident`]s.
///
/// See [`ReachableOnly`] for more information.
pub type AirExprAggregateReachable = AirExprAggregate<ReachableOnly>;

/// Parse and aggregate both [`Reachable`] and [`Dangling`] [`Expr`]s into
///   the graph.
///
/// See [`StoreDangling`] for more information.
pub type AirExprAggregateStoreDangling = AirExprAggregate<StoreDangling>;

/// Parse an AIR expression with binding support.
///
/// Expressions are composable,
///   so this parser need only care about whether it has any active
///   expression being parsed.
///
/// This parser has no dead states---it
///   handles each of its tokens and performs error recovery on invalid
///   state transitions.
#[derive(Debug, PartialEq)]
pub enum AirExprAggregate<S: RootStrategy> {
    /// Ready for an expression;
    ///   expression stack is empty.
    Ready(S, ExprStack<Dormant>),

    /// Building an expression.
    BuildingExpr(S, ExprStack<Active>, ObjectIndex<Expr>),
}

impl<S: RootStrategy> Display for AirExprAggregate<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready(_, es) => {
                write!(f, "ready for expression with {es}")
            }
            Self::BuildingExpr(_, es, _) => {
                write!(f, "building expression with {es}")
            }
        }
    }
}

impl<S: RootStrategy> ParseState for AirExprAggregate<S> {
    type Token = AirBindableExpr;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirExpr::*};
        use AirBindableExpr::*;
        use AirExprAggregate::*;

        match (self, tok) {
            (Ready(root, es), AirExpr(ExprStart(op, span))) => {
                let oi = ctx.asg_mut().create(Expr::new(op, span));
                Transition(BuildingExpr(root, es.activate(), oi)).incomplete()
            }

            (BuildingExpr(root, es, poi), AirExpr(ExprStart(op, span))) => {
                let oi = poi.create_subexpr(ctx.asg_mut(), Expr::new(op, span));
                Transition(BuildingExpr(root, es.push(poi), oi)).incomplete()
            }

            (BuildingExpr(root, es, oi), AirExpr(ExprEnd(end))) => {
                let _ = oi.map_obj(ctx.asg_mut(), |expr| {
                    expr.map(|span| span.merge(end).unwrap_or(span))
                });

                let dangling = es.is_dangling();
                let oi_root = ctx.dangling_expr_oi();

                match (es.pop(), dangling) {
                    ((es, Some(poi)), _) => {
                        Transition(BuildingExpr(root, es, poi)).incomplete()
                    }
                    ((es, None), true) => root
                        .hold_dangling(ctx.asg_mut(), oi_root, oi)
                        .transition(Ready(root, es.done())),
                    ((es, None), false) => {
                        Transition(Ready(root, es.done())).incomplete()
                    }
                }
            }

            (BuildingExpr(root, es, oi), AirBind(BindIdent(id))) => {
                let oi_root = ctx.rooting_oi().expect("TODO");
                let oi_ident = root.defines(ctx.asg_mut(), oi_root, id);

                // It is important that we do not mark this expression as
                //   reachable unless we successfully bind the identifier.
                match oi_ident.bind_definition(ctx.asg_mut(), id, oi) {
                    Ok(_) => Transition(BuildingExpr(
                        root,
                        es.reachable_by(oi_ident),
                        oi,
                    ))
                    .incomplete(),
                    Err(e) => Transition(BuildingExpr(root, es, oi)).err(e),
                }
            }

            (BuildingExpr(root, es, oi), AirBind(RefIdent(ident))) => {
                Transition(BuildingExpr(
                    root,
                    es,
                    oi.ref_expr(ctx.asg_mut(), ident),
                ))
                .incomplete()
            }

            (st @ Ready(..), AirExpr(ExprEnd(span))) => {
                Transition(st).err(AsgError::UnbalancedExpr(span))
            }

            // The binding may refer to a parent context.
            (st @ Ready(..), tok @ AirBind(..)) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready(..))
    }
}

impl<S: RootStrategy> AirExprAggregate<S> {
    pub(super) fn new() -> Self {
        Self::Ready(S::new(), ExprStack::default())
    }
}

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
    /// The associated [`ObjectIndex`] serves as _evidence_ of this
    ///   assertion.
    Reachable(ObjectIndex<Object>),
}

impl Display for StackEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Dangling => write!(f, "dangling"),
            Self::Reachable(oi) => {
                write!(f, "reachable (by {oi})")
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
    fn reachable_by<O: ObjectKind>(self, oi: ObjectIndex<O>) -> Self {
        match self {
            Self(stack, Active(StackEdge::Dangling)) if stack.is_empty() => {
                Self(stack, Active(StackEdge::Reachable(oi.widen())))
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

pub use root::*;
mod root {
    use super::*;
    use std::fmt::Debug;

    /// The rooting strategy to employ after an [`Expr`] construction.
    ///
    /// The method [`Self::defines`] roots an identifier,
    ///   stating that the object associated with [`Self`] is responsible
    ///   for the definition associated with this identifier.
    /// An identified expression will be rooted in [`Self`] even if it is a
    ///   sub-expression.
    pub trait RootStrategy: Debug + PartialEq {
        /// Declare `oi` as the root of all accepted [`Expr`]s produced by
        ///   the parser.
        fn new() -> Self;

        /// Look up the provided identifier `id` on the [`Asg`] and indicate
        ///   that its definition is associated with [`Self`]'s root.
        ///
        /// This is invoked for _all_ identifiers,
        ///   including sub-expressions.
        fn defines(
            &self,
            asg: &mut Asg,
            oi_root: ObjectIndexTo<Ident>,
            id: SPair,
        ) -> ObjectIndex<Ident>;

        /// Hold or reject a [`Dangling`] root [`Expr`].
        ///
        /// A [`Dangling`] expression is not reachable by any other object,
        ///   so this strategy must decide whether to root it in [`Self`] or
        ///   reject it.
        fn hold_dangling(
            &self,
            asg: &mut Asg,
            oi_root: Option<ObjectIndexTo<Expr>>,
            oi_expr: ObjectIndex<Expr>,
        ) -> Result<(), AsgError>;
    }

    /// Accept and root only [`Reachable`] root expressions.
    ///
    /// Note that a root expresion is still [`Dangling`]
    ///   (and therefore not [`Reachable`])
    ///   even if one of its sub-expressions has been bound to an
    ///   identifier.
    /// In that case,
    ///   the sub-expression will be rooted in [`Self`],
    ///     but the [`Dangling`] root expression will still be rejected.
    ///
    /// This expects identifiers to be rooted in the global environment,
    ///   which is the package representing the active compilation unit.
    /// This may be relaxed once identifier caching is generalized;
    ///   at the time of writing it is too coupled to the graph root.
    ///
    /// See [`RootStrategy`] for more information.
    #[derive(Debug, PartialEq)]
    pub struct ReachableOnly;

    impl RootStrategy for ReachableOnly {
        fn new() -> Self {
            Self
        }

        fn defines(
            &self,
            asg: &mut Asg,
            oi_root: ObjectIndexTo<Ident>,
            id: SPair,
        ) -> ObjectIndex<Ident> {
            asg.lookup_global_or_missing(id)
                .add_edge_from(asg, oi_root, None)
        }

        fn hold_dangling(
            &self,
            asg: &mut Asg,
            _oi_root: Option<ObjectIndexTo<Expr>>,
            oi_expr: ObjectIndex<Expr>,
        ) -> Result<(), AsgError> {
            Err(AsgError::DanglingExpr(oi_expr.resolve(asg).span()))
        }
    }

    /// Accept both [`Reachable`] and [`Dangling`] expressions.
    ///
    /// A [`Dangling`] expression will have the [`Expr`] rooted instead of
    ///   an [`Ident`].
    ///
    /// Sub-expressions can be thought of as utilizing this strategy with an
    ///   implicit parent [`ObjectIndex<Expr>`](ObjectIndex).
    ///
    /// Unlike [`ReachableOnly`],
    ///   this does _not_ cache identifiers in the global environment.
    /// See there for more information.
    ///
    /// See [`RootStrategy`] for more information.
    #[derive(Debug, PartialEq)]
    pub struct StoreDangling;

    impl RootStrategy for StoreDangling {
        fn new() -> Self {
            Self
        }

        fn defines(
            &self,
            asg: &mut Asg,
            oi_root: ObjectIndexTo<Ident>,
            name: SPair,
        ) -> ObjectIndex<Ident> {
            // This cannot simply call [`ReachableOnly`]'s `defines` because
            //   we cannot cache in the global environment.
            // This can be realized once caching is generalized;
            //   see the commit that introduced this comment.
            oi_root.declare_local(asg, name)
        }

        fn hold_dangling(
            &self,
            asg: &mut Asg,
            oi_root: Option<ObjectIndexTo<Expr>>,
            oi_expr: ObjectIndex<Expr>,
        ) -> Result<(), AsgError> {
            oi_root.expect("TODO").add_edge_to(asg, oi_expr, None);
            Ok(())
        }
    }
}

#[cfg(test)]
pub mod test;
