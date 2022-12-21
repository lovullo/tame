// Objects represented on ASG
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

//! Objects represented by the ASG.
//!
//! Dynamic Object Types and Narrowing
//! ==================================
//! Unlike the functional lowering pipeline that precedes it,
//!   the ASG is a mutable, ever-evolving graph of dynamic data.
//! The ASG does not benefit from the same type-level guarantees that the
//!   rest of the system does at compile-time.
//!
//! Any node on the graph can represent any type of [`Object`].
//! An [`ObjectRef`] contains an index into the graph,
//!   _not_ a reference;
//!     consequently,
//!       it is possible (though avoidable) for objects to be modified out
//!       from underneath references.
//! Consequently,
//!   we cannot trust that an [`ObjectRef`] is what we expect it to be when
//!   performing an operation on the graph using that index.
//!
//! To perform an operation on a particular type of object,
//!   we must first _narrow_ it.
//! Narrowing converts from the [`Object`] sum type into a more specific
//!   inner type,
//!     such as [`Ident`] or [`Expr`].
//! This operation _should_,
//!   if the compiler is operating correctly,
//!   always succeed,
//!     because the type of object should always match our expectations;
//!       the explicit narrowing is to ensure memory safety in case that
//!       assumption does not hold.
//!
//! Since a type mismatch represents a bug in the compiler,
//!   the API favors [`Result`]-free narrowing rather than burdening every
//!   caller with additional complexity---we
//!     will attempt to narrow and panic in the event of a failure,
//!       including a diagnostic message that helps to track down the issue
//!       using whatever [`Span`]s we have available.
//! [`ObjectRef`] is associated with a span derived from the point of its
//!   creation to handle this diagnostic situation automatically.

use super::{Expr, Ident};
use crate::{
    diagnose::Annotate,
    diagnostic_panic,
    span::{Span, UNKNOWN_SPAN},
};
use petgraph::graph::NodeIndex;
use std::fmt::Display;

/// An object on the ASG.
///
/// See the [module-level documentation](super) for more information.
#[derive(Debug, PartialEq)]
pub enum Object {
    /// Represents the root of all reachable identifiers.
    ///
    /// Any identifier not reachable from the root will not be linked into
    ///   the final executable.
    ///
    /// There should be only one object of this kind.
    Root,

    /// Identifier (a named object).
    Ident(Ident),

    /// Expression.
    ///
    /// An expression may optionally be named by one or more [`Ident`]s.
    Expr(Expr),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "root ASG node"),
            Self::Ident(ident) => Display::fmt(ident, f),
            Self::Expr(expr) => Display::fmt(expr, f),
        }
    }
}

impl Object {
    pub fn span(&self) -> Span {
        match self {
            Self::Root => UNKNOWN_SPAN,
            Self::Ident(ident) => ident.span(),
            Self::Expr(expr) => expr.span(),
        }
    }

    /// Retrieve an [`Ident`] reference,
    ///   or [`None`] if the object is not an identifier.
    pub fn as_ident_ref(&self) -> Option<&Ident> {
        match self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    /// Unwraps an object as an [`Ident`],
    ///   panicing if the object is of a different type.
    ///
    /// This should be used only when a panic would represent an internal
    ///   error resulting from state inconsistency on the graph.
    /// Ideally,
    ///   the graph would be typed in such a way to prevent this type of
    ///   thing from occurring in the future.
    pub fn unwrap_ident(self) -> Ident {
        match self {
            Self::Ident(ident) => ident,
            x => panic!("internal error: expected Ident, found {x:?}"),
        }
    }

    /// Unwraps an object as an [`&Ident`](Ident),
    ///   panicing if the object is of a different type.
    ///
    /// This should be used only when a panic would represent an internal
    ///   error resulting from state inconsistency on the graph.
    /// Ideally,
    ///   the graph would be typed in such a way to prevent this type of
    ///   thing from occurring in the future.
    pub fn unwrap_ident_ref(&self) -> &Ident {
        match self {
            Self::Ident(ident) => ident,
            x => panic!("internal error: expected Ident, found {x:?}"),
        }
    }
}

impl From<Ident> for Object {
    fn from(ident: Ident) -> Self {
        Self::Ident(ident)
    }
}

impl From<Expr> for Object {
    fn from(expr: Expr) -> Self {
        Self::Expr(expr)
    }
}

impl Into<Expr> for Object {
    /// Narrow an object into an [`Expr`],
    ///   panicing if the object is not of that type.
    fn into(self) -> Expr {
        match self {
            Self::Expr(expr) => expr,
            _ => diagnostic_panic!(
                self.span()
                    .internal_error("expected this object to be an expression")
                    .into(),
                "expected expression, found {self}",
            ),
        }
    }
}

/// Index representing an [`Object`] stored on the [`Asg`](super::Asg).
///
/// Ident references are integer offsets,
///   not pointers.
/// See the [module-level documentation][self] for more information.
///
/// This object is associated with a [`Span`] that identifies the source
///   location from which this object was derived;
///     this is intended to be used to provide diagnostic information in the
///     event that the object somehow becomes unavailable for later
///       operations.
///
/// _The span is not accounted for in [`PartialEq`]_,
///   since it represents the context in which the [`ObjectRef`] was
///   retrieved,
///     and the span associated with the underlying [`Object`] may evolve
///     over time.
#[derive(Debug, Clone, Copy)]
pub struct ObjectRef(NodeIndex, Span);

impl ObjectRef {
    pub fn new(index: NodeIndex, span: Span) -> Self {
        Self(index, span)
    }

    pub fn map_span(self, f: impl FnOnce(Span) -> Span) -> Self {
        match self {
            Self(index, span) => Self(index, f(span)),
        }
    }
}

impl PartialEq for ObjectRef {
    /// Compare two [`ObjectRef`]s' indicies,
    ///   without concern for its associated [`Span`].
    ///
    /// See [`ObjectRef`] for more information on why the span is not
    ///   accounted for in this comparison.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self(index_a, _), Self(index_b, _)) => index_a == index_b,
        }
    }
}

impl Eq for ObjectRef {}

// TODO: Remove this,
//   since we need thought to be put into providing spans.
impl From<NodeIndex> for ObjectRef {
    fn from(index: NodeIndex) -> Self {
        Self(index, UNKNOWN_SPAN)
    }
}

impl From<ObjectRef> for NodeIndex {
    fn from(objref: ObjectRef) -> Self {
        match objref {
            ObjectRef(index, _) => index,
        }
    }
}

impl From<ObjectRef> for Span {
    fn from(value: ObjectRef) -> Self {
        match value {
            ObjectRef(_, span) => span,
        }
    }
}
