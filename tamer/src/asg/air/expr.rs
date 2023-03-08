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
};
use crate::{
    asg::{graph::object::ObjectRelTo, Ident, ObjectKind},
    f::Functor,
    parse::prelude::*,
    span::Span,
};
use std::marker::PhantomData;

#[cfg(doc)]
use StackEdge::{Dangling, Reachable};

/// Parse and aggregate [`Reachable`] [`Expr`]s into the graph,
///   with expression roots bound to their associated [`Ident`]s.
///
/// See [`ReachableOnly`] for more information.
pub type AirExprAggregateReachable<O> = AirExprAggregate<O, ReachableOnly<O>>;

/// Parse and aggregate both [`Reachable`] and [`Dangling`] [`Expr`]s into
///   the graph.
///
/// See [`StoreDangling`] for more information.
pub type AirExprAggregateStoreDangling<O> =
    AirExprAggregate<O, StoreDangling<O>>;

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
pub enum AirExprAggregate<O: ObjectKind, S: RootStrategy<O>> {
    /// Ready for an expression;
    ///   expression stack is empty.
    Ready(S, ExprStack<Dormant>, PhantomData<O>),

    /// Building an expression.
    BuildingExpr(S, ExprStack<Active>, ObjectIndex<Expr>),
}

impl<O: ObjectKind, S: RootStrategy<O>> Display for AirExprAggregate<O, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready(_, es, _) => {
                write!(f, "ready for expression with {es}")
            }
            Self::BuildingExpr(_, es, _) => {
                write!(f, "building expression with {es}")
            }
        }
    }
}

impl<O: ObjectKind, S: RootStrategy<O>> ParseState for AirExprAggregate<O, S> {
    type Token = AirBindableExpr;
    type Object = ();
    type Error = AsgError;
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirExpr::*};
        use AirBindableExpr::*;
        use AirExprAggregate::*;

        match (self, tok) {
            (Ready(root, es, _), AirExpr(ExprOpen(op, span))) => {
                let oi = asg.create(Expr::new(op, span));
                Transition(BuildingExpr(root, es.activate(), oi)).incomplete()
            }

            (BuildingExpr(root, es, poi), AirExpr(ExprOpen(op, span))) => {
                let oi = poi.create_subexpr(asg, Expr::new(op, span));
                Transition(BuildingExpr(root, es.push(poi), oi)).incomplete()
            }

            (BuildingExpr(root, es, oi), AirExpr(ExprClose(end))) => {
                let start: Span = oi.into();

                let _ = oi.map_obj(asg, |expr| {
                    expr.map(|span| span.merge(end).unwrap_or(span))
                });

                match es.pop() {
                    (es, Some(poi)) => {
                        Transition(BuildingExpr(root, es, poi)).incomplete()
                    }
                    (es, None) => {
                        let dangling = es.is_dangling();
                        let st = Ready(root, es.done(), PhantomData::default());

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

            (BuildingExpr(root, es, oi), AirBind(BindIdent(id))) => {
                let oi_ident = root.defines(asg, id);

                // It is important that we do not mark this expression as
                //   reachable unless we successfully bind the identifier.
                match oi_ident.bind_definition(asg, id, oi) {
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
                Transition(BuildingExpr(root, es, oi.ref_expr(asg, ident)))
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

impl<O: ObjectKind, S: RootStrategy<O>> AirExprAggregate<O, S> {
    pub(super) fn new_in(oi: ObjectIndex<O>) -> Self {
        Self::Ready(
            S::new_root(oi),
            ExprStack::default(),
            PhantomData::default(),
        )
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
    pub trait RootStrategy<O: ObjectKind>: Debug + PartialEq {
        /// Declare `oi` as the root of all accepted [`Expr`]s produced by
        ///   the parser.
        fn new_root(oi: ObjectIndex<O>) -> Self;

        /// Look up the provided identifier `id` on the [`Asg`] and indicate
        ///   that its definition is associated with [`Self`]'s root.
        ///
        /// This is invoked for _all_ identifiers,
        ///   including sub-expressions.
        fn defines(&self, asg: &mut Asg, id: SPair) -> ObjectIndex<Ident>;
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
    /// See [`RootStrategy`] for more information.
    #[derive(Debug, PartialEq)]
    pub struct ReachableOnly<O: ObjectKind>(ObjectIndex<O>)
    where
        O: ObjectRelTo<Ident>;

    impl<O: ObjectKind> RootStrategy<O> for ReachableOnly<O>
    where
        O: ObjectRelTo<Ident>,
    {
        fn new_root(oi: ObjectIndex<O>) -> Self {
            Self(oi)
        }

        fn defines(&self, asg: &mut Asg, id: SPair) -> ObjectIndex<Ident> {
            match self {
                Self(oi_root) => {
                    asg.lookup_or_missing(id).add_edge_from(asg, *oi_root, None)
                }
            }
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
    /// See [`RootStrategy`] for more information.
    #[derive(Debug, PartialEq)]
    pub struct StoreDangling<O: ObjectKind>(ObjectIndex<O>)
    where
        O: ObjectRelTo<Ident> + ObjectRelTo<Expr>;

    impl<O: ObjectKind> RootStrategy<O> for StoreDangling<O>
    where
        O: ObjectRelTo<Ident> + ObjectRelTo<Expr>,
    {
        fn new_root(oi: ObjectIndex<O>) -> Self {
            Self(oi)
        }

        fn defines(&self, asg: &mut Asg, id: SPair) -> ObjectIndex<Ident> {
            // We are a superset of `ReachableOnly`'s behavior,
            //   so delegate to avoid duplication.
            match self {
                Self(oi_root) => ReachableOnly(*oi_root).defines(asg, id),
            }
        }
    }
}

#[cfg(test)]
pub mod test;
