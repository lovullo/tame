// ASG IR
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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
    Asg, AsgError, ExprOp, FragmentText, IdentKind, ObjectRef, Source,
};
use crate::{
    asg::Expr,
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
    ///   [`Air::IdentExpr`].
    ///
    /// Expressions are composed of references to other expressions.
    OpenExpr(ExprOp, Span),

    /// Complete the expression atop of the expression stack and pop it from
    ///   the stack.
    CloseExpr(Span),

    /// Assign an identifier to the expression atop of the expression stack.
    ///
    /// An expression may be bound to multiple identifiers,
    ///   but an identifier can only be bound to a single expression.
    /// Binding an identifier will declare it.
    IdentExpr(SPair),

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
        // TODO: This can be provided once the xmlo files store source
        //   locations for symbols.
        UNKNOWN_SPAN
    }
}

impl parse::Object for Air {}

impl Display for Air {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Air::*;

        match self {
            Todo => write!(f, "TODO"),

            OpenExpr(op, _) => write!(f, "open {op} expression"),

            CloseExpr(_) => write!(f, "close expression"),

            IdentExpr(id) => {
                write!(f, "identify expression as {}", TtQuote::wrap(id))
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

/// AIR parser state.
///
/// This currently has no parsing state;
///   all state is stored on the ASG itself,
///     which is the parsing context.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum AirAggregate {
    #[default]
    Empty,

    /// Building an expression that is yet not reachable from any other
    ///   object.
    ///
    /// Dangling expressions are expected to transition into
    ///   [`Self::ReachableExpr`] after being bound to an identifier.
    /// Closing a dangling expression will result in a
    ///   [`AsgError::DanglingExpr`].
    DanglingExpr(ObjectRef),

    /// Building an expression that is reachable from another object.
    ///
    /// See also [`Self::DanglingExpr`].
    ReachableExpr(ObjectRef),
}

impl Display for AirAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirAggregate::*;

        match self {
            Empty => write!(f, "awaiting AIR input for ASG"),
            DanglingExpr(_) => write!(f, "building dangling expression"),
            ReachableExpr(_) => write!(f, "building reachable expression"),
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
            (_, Todo) => Transition(Empty).incomplete(),

            (Empty, OpenExpr(op, span)) => {
                let oi = asg.create(Expr::new(op, span));
                Transition(DanglingExpr(oi)).incomplete()
            }

            (Empty, CloseExpr(_)) => todo!("no matching expr to end"),

            (DanglingExpr(oi), CloseExpr(end)) => {
                let start: Span = oi.into();
                Transition(Empty).err(AsgError::DanglingExpr(
                    start.merge(end).unwrap_or(start),
                ))
            }

            (ReachableExpr(oi), CloseExpr(end)) => {
                let _ = asg.mut_map_obj::<Expr>(oi, |expr| {
                    expr.map_span(|span| span.merge(end).unwrap_or(span))
                });

                Transition(Empty).incomplete()
            }

            (Empty, IdentExpr(_)) => todo!("cannot bind ident to nothing"),

            (DanglingExpr(oi), IdentExpr(id)) => {
                // TODO: error on existing ident
                let identi = asg.lookup_or_missing(id);
                asg.add_dep(identi, oi);

                Transition(ReachableExpr(oi)).incomplete()
            }

            (st, tok @ (OpenExpr(..) | IdentExpr(..))) => {
                todo!("{st:?} -> {tok:?}")
            }

            (Empty, IdentDecl(name, kind, src)) => {
                asg.declare(name, kind, src).map(|_| ()).transition(Empty)
            }

            (Empty, IdentExternDecl(name, kind, src)) => asg
                .declare_extern(name, kind, src)
                .map(|_| ())
                .transition(Empty),

            (Empty, IdentDep(sym, dep)) => {
                asg.add_dep_lookup(sym, dep);
                Transition(Empty).incomplete()
            }

            (Empty, IdentFragment(sym, text)) => {
                asg.set_fragment(sym, text).map(|_| ()).transition(Empty)
            }

            (Empty, IdentRoot(sym)) => {
                let obj = asg.lookup_or_missing(sym);
                asg.add_root(obj);

                Transition(Empty).incomplete()
            }

            (
                st,
                tok @ (IdentDecl(..) | IdentExternDecl(..) | IdentDep(..)
                | IdentFragment(..) | IdentRoot(..)),
            ) => todo!("{st:?}, {tok:?}"),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        *self == Self::Empty
    }
}

#[cfg(test)]
mod test;
