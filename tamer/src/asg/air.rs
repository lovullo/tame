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

use super::{
    graph::object::{Expr, Pkg},
    Asg, AsgError, ExprOp, FragmentText, IdentKind, ObjectIndex, Source,
};
use crate::{
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{self, util::SPair, ParseState, Token, Transition, Transitionable},
    span::{Span, UNKNOWN_SPAN},
    sym::SymbolId,
};
use std::fmt::{Debug, Display};

///! Intermediate representation for construction of the
///!   [abstract semantic graph (ASG)](super) (AIR).
///!
///! AIR serves as an abstraction layer between higher-level parsers and the
///!   aggregate ASG.
///! It allows parsers to operate as a raw stream of data without having to
///!   worry about ownership of or references to the ASG,
///!     and allows for multiple such parsers to be joined.
///!
///! AIR is _not_ intended to replace the API of the ASG---it
///!   is intended as a termination point for the parsing pipeline,
///!     and as such implements a subset of the ASG's API that is suitable
///!     for aggregating raw data from source and object files.
///! Given that it does so little and is so close to the [`Asg`] API,
///!   one might say that the abstraction is as light as air,
///!   but that would surely result in face-palming and so we're not going
///!     air such cringeworthy dad jokes here.

pub type IdentSym = SymbolId;
pub type DepSym = SymbolId;

/// AIR token.
///
/// These tokens mimic a public API for the ASG,
///   and allow parsers to be completely decoupled from the ASG object that
///   they will eventually aggregate data into.
///
/// This IR is not intended to perform sophisticated manipulation of the
///   ASG---it
///     is intended to perform initial aggregation as part of a parsing
///     phase,
///       populating the ASG with the raw data that that will be
///       subsequently analyzed and rewritten.
#[derive(Debug, PartialEq)]
pub enum Air {
    /// Placeholder token for objects that do not yet have a proper place on
    ///   the ASG.
    Todo,

    /// Begin a new package of identifiers.
    ///
    /// Packages are responsible for bundling together identifiers
    ///   representing subsystems that can be composed with other packages.
    ///
    /// A source language may place limits on the objects that may appear
    ///   within a given package,
    ///     but we have no such restriction.
    ///
    /// TODO: The package needs a name,
    ///   and we'll need to determine how to best represent that relative to
    ///   the project root and be considerate of symlinks.
    PkgOpen(Span),

    /// Complete processing of the current package.
    PkgClose(Span),

    /// Create a new [`Expr`] on the graph and place it atop of the
    ///   expression stack.
    ///
    /// If there was previously an expression ρ atop of the stack before
    ///   this operation,
    ///     a reference to this new expression will be automatically added
    ///     to ρ,
    ///       treating it as a child expression.
    /// Otherwise,
    ///   the expression will be dangling unless bound to an identifier,
    ///     which will produce an error.
    ///
    /// All expressions have an associated [`ExprOp`] that determines how
    ///   the expression will be evaluated.
    /// An expression is associated with a source location,
    ///   but is anonymous unless assigned an identifier using
    ///   [`Air::BindIdent`].
    ///
    /// Expressions are composed of references to other expressions.
    ExprOpen(ExprOp, Span),

    /// Complete the expression atop of the expression stack and pop it from
    ///   the stack.
    ExprClose(Span),

    /// Reference another expression identified by the given [`SPair`].
    ///
    /// Values can be referenced before they are declared or defined,
    ///   so the provided identifier need not yet exist.
    /// However,
    ///   the identifier must eventually be bound to an [`Expr`].
    ///
    /// Since all values in TAME are referentially tansparent,
    ///   the system has flexibility in determining what it should do with a
    ///   reference.
    ExprRef(SPair),

    /// Assign an identifier to the active object.
    ///
    /// The "active" object depends on the current parsing state.
    BindIdent(SPair),

    /// Declare a resolved identifier.
    IdentDecl(SPair, IdentKind, Source),

    /// Declare an external identifier that must be resolved before linking.
    IdentExternDecl(SPair, IdentKind, Source),

    /// Declare that an identifier depends on another for its definition.
    ///
    /// The first identifier will depend on the second
    ///   (`0 -> 1`).
    /// The spans associated with each [`SPair`] will be used
    ///   if the respective identifier has not yet been defined.
    IdentDep(SPair, SPair),

    /// Associate a code fragment with an identifier.
    ///
    /// A fragment does not have an associated span because it is
    ///   conceptually associated with all the spans from which it is
    ///   derived;
    ///     the format of the object file will change in the future to
    ///     retain this information.
    IdentFragment(SPair, FragmentText),

    /// Root an identifier at the request of some entity at the associated
    ///   span of the [`SPair`].
    ///
    /// Rooting is caused by _something_,
    ///   and the span is intended to aid in tracking down why rooting
    ///   occurred.
    IdentRoot(SPair),
}

impl Token for Air {
    fn ir_name() -> &'static str {
        "AIR"
    }

    fn span(&self) -> crate::span::Span {
        use Air::*;

        match self {
            Todo => UNKNOWN_SPAN,

            PkgOpen(span)
            | PkgClose(span)
            | ExprOpen(_, span)
            | ExprClose(span) => *span,

            BindIdent(spair)
            | ExprRef(spair)
            | IdentDecl(spair, _, _)
            | IdentExternDecl(spair, _, _)
            | IdentDep(spair, _)
            | IdentFragment(spair, _)
            | IdentRoot(spair) => spair.span(),
        }
    }
}

impl parse::Object for Air {}

impl Display for Air {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Air::*;

        match self {
            Todo => write!(f, "TODO"),

            PkgOpen(_) => write!(f, "open package"),
            PkgClose(_) => write!(f, "close package"),

            ExprOpen(op, _) => write!(f, "open {op} expression"),

            ExprClose(_) => write!(f, "close expression"),

            BindIdent(id) => {
                write!(f, "identify active object as {}", TtQuote::wrap(id))
            }

            ExprRef(id) => {
                write!(
                    f,
                    "reference to the expression identified by {}",
                    TtQuote::wrap(id)
                )
            }

            IdentDecl(spair, _, _) => {
                write!(f, "declaration of identifier {}", TtQuote::wrap(spair))
            }
            IdentExternDecl(spair, _, _) => {
                write!(
                    f,
                    "declaration of external identifier {}",
                    TtQuote::wrap(spair)
                )
            }
            IdentDep(isym, dsym) => write!(
                f,
                // TODO: Use list wrapper
                "declaration of identifier dependency `{isym} -> {dsym}`"
            ),
            IdentFragment(depsym, _text) => {
                write!(f, "identifier {}` fragment text", TtQuote::wrap(depsym))
            }
            IdentRoot(sym) => {
                write!(f, "rooting of identifier {}", TtQuote::wrap(sym))
            }
        }
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
#[derive(Debug, PartialEq, Eq)]
pub enum AirAggregate {
    /// Parser is not currently performing any work.
    Empty(ExprStack<Dormant>),

    /// A package is being defined.
    PkgDfn(ObjectIndex<Pkg>, ExprStack<Dormant>),

    /// Building an expression.
    ///
    /// Expressions may be nested arbitrarily deeply.
    BuildingExpr(ObjectIndex<Pkg>, ExprStack<Active>, ObjectIndex<Expr>),
}

impl Default for AirAggregate {
    fn default() -> Self {
        Self::Empty(ExprStack::default())
    }
}

impl Display for AirAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirAggregate::*;

        match self {
            Empty(es) => write!(f, "awaiting AIR input for ASG with {es}"),
            PkgDfn(_, es) => write!(f, "defining package with {es}"),
            BuildingExpr(_, es, _) => {
                write!(f, "building expression with {es}")
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
    /// This ASG will be yielded by [`parse::Parser::finalize`].
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use Air::*;
        use AirAggregate::*;

        match (self, tok) {
            (st, Todo) => Transition(st).incomplete(),

            (Empty(es), PkgOpen(span)) => {
                let oi_pkg = asg.create(Pkg::new(span)).root(asg);
                Transition(PkgDfn(oi_pkg, es)).incomplete()
            }

            (PkgDfn(oi_pkg, es), PkgOpen(span)) => {
                Transition(PkgDfn(oi_pkg, es))
                    .err(AsgError::NestedPkgOpen(span, oi_pkg.span()))
            }
            (BuildingExpr(oi_pkg, es, oi), PkgOpen(span)) => {
                Transition(BuildingExpr(oi_pkg, es, oi))
                    .err(AsgError::NestedPkgOpen(span, oi_pkg.span()))
            }

            (PkgDfn(oi_pkg, es), PkgClose(span)) => {
                oi_pkg.close(asg, span);
                Transition(Empty(es)).incomplete()
            }

            (st @ (Empty(..) | BuildingExpr(..)), PkgClose(span)) => {
                Transition(st).err(AsgError::InvalidPkgCloseContext(span))
            }

            (PkgDfn(oi_pkg, es), ExprOpen(op, span)) => {
                let oi = asg.create(Expr::new(op, span));
                Transition(BuildingExpr(oi_pkg, es.activate(), oi)).incomplete()
            }

            (BuildingExpr(oi_pkg, es, poi), ExprOpen(op, span)) => {
                let oi = poi.create_subexpr(asg, Expr::new(op, span));
                Transition(BuildingExpr(oi_pkg, es.push(poi), oi)).incomplete()
            }

            (st @ Empty(..), ExprOpen(_, span)) => {
                Transition(st).err(AsgError::InvalidExprContext(span))
            }

            (st @ (Empty(..) | PkgDfn(..)), ExprClose(span)) => {
                Transition(st).err(AsgError::UnbalancedExpr(span))
            }

            (BuildingExpr(oi_pkg, es, oi), ExprClose(end)) => {
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
                        let st = PkgDfn(oi_pkg, es.done());

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

            (BuildingExpr(oi_pkg, es, oi), BindIdent(id)) => {
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

            (BuildingExpr(oi_pkg, es, oi), ExprRef(ident)) => {
                Transition(BuildingExpr(oi_pkg, es, oi.ref_expr(asg, ident)))
                    .incomplete()
            }

            (st @ (Empty(_) | PkgDfn(_, _)), BindIdent(ident)) => {
                Transition(st).err(AsgError::InvalidExprBindContext(ident))
            }

            (st @ (Empty(_) | PkgDfn(_, _)), ExprRef(ident)) => {
                Transition(st).err(AsgError::InvalidExprRefContext(ident))
            }

            (st @ Empty(_), IdentDecl(name, kind, src)) => {
                asg.declare(name, kind, src).map(|_| ()).transition(st)
            }

            (st @ Empty(_), IdentExternDecl(name, kind, src)) => asg
                .declare_extern(name, kind, src)
                .map(|_| ())
                .transition(st),

            (st @ Empty(_), IdentDep(sym, dep)) => {
                asg.add_dep_lookup(sym, dep);
                Transition(st).incomplete()
            }

            (st @ Empty(_), IdentFragment(sym, text)) => {
                asg.set_fragment(sym, text).map(|_| ()).transition(st)
            }

            (st @ Empty(_), IdentRoot(sym)) => {
                let obj = asg.lookup_or_missing(sym);
                asg.add_root(obj);

                Transition(st).incomplete()
            }

            (
                st,
                tok @ (IdentDecl(..) | IdentExternDecl(..) | IdentDep(..)
                | IdentFragment(..) | IdentRoot(..)),
            ) => todo!("{st:?}, {tok:?}"),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Empty(_))
    }
}

#[cfg(test)]
mod test;
