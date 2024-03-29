// Expressions represented on the ASG
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

//! Expressions on the ASG.

use super::{prelude::*, Doc, Ident, ObjectIndexTo, Tpl};
use crate::{f::Map, num::Dim, span::Span};
use std::fmt::Display;

#[cfg(doc)]
use super::ObjectKind;

/// Expression.
///
/// The [`Span`] of an expression should be expanded to encompass not only
///   all child expressions,
///     but also any applicable closing span.
#[derive(Debug, PartialEq, Eq)]
pub struct Expr(pub ExprOp, ExprDim, Span);

impl Expr {
    pub fn new(op: ExprOp, span: Span) -> Self {
        Self(op, ExprDim::default(), span)
    }

    pub fn span(&self) -> Span {
        match self {
            Expr(_, _, span) => *span,
        }
    }

    pub fn op(&self) -> ExprOp {
        match self {
            Expr(op, _, _) => *op,
        }
    }
}

impl Map<Span> for Expr {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self {
        match self {
            Self(op, dim, span) => Self(op, dim, f(span)),
        }
    }
}

impl From<&Expr> for Span {
    fn from(val: &Expr) -> Self {
        val.span()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(op, dim, _) => write!(f, "{op} expression with {dim}"),
        }
    }
}

/// Expression operation type.
///
/// TODO: Ideally this will be replaced with arbitrary binary (dyadic)
///   functions defined within the language of TAME itself,
///     as was the original plan with TAMER.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExprOp {
    /// Summation (+)
    Sum,
    /// Product (×)
    Product,
    // Ceiling (⌈)
    Ceil,
    // Floor (⌊)
    Floor,
    /// Logical conjunction (∧)
    Conj,
    /// Logical disjunction (∨)
    Disj,
    /// Equality predicate (=)
    Eq,
}

impl Display for ExprOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ExprOp::*;

        // "{self} expression"
        match self {
            Sum => write!(f, "sum (+)"),
            Product => write!(f, "product (×)"),
            Ceil => write!(f, "ceiling (⌈)"),
            Floor => write!(f, "floor (⌊)"),
            Conj => write!(f, "conjunctive (∧)"),
            Disj => write!(f, "disjunctive (∨)"),
            Eq => write!(f, "equality (=)"),
        }
    }
}

/// The dimensionality of the expression itself and the target
///   dimensionality required by its context.
///
/// If the target dimensionality is greater,
///   then the expression's value will be broadcast;
///   if it is less,
///     than it will be folded according to the expression's [`ExprOp`].
///
/// A value of [`None`] means that the dimensionality has not yet been
///   constrained either through inference or explicit specification by the
///   user.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct ExprDim(InnerDim, OuterDim);

impl Display for ExprDim {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(inner, outer) => write!(f, "{inner} and {outer}"),
        }
    }
}

/// Dimensionality of the inner expression.
///
/// This represents the dimensionality after any necessary broadcasting of
///   all values referenced by this expression.
/// This does not necessarily correspond to the dimensionality of the
///   final value of this expression;
///     see [`OuterDim`].
#[derive(Debug, PartialEq, Eq, Default)]
pub struct InnerDim(DimState);

impl Display for InnerDim {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(dim_st) => write!(f, "inner dimensionality {dim_st}"),
        }
    }
}

/// Final dimensionality of the expression as observed by others.
///
/// This represents what other expressions will see as the dimensionality of
///   this expression.
/// For example,
///   [`InnerDim`] may require broadcasting,
///   but this outer dimensionality may require subsequent folding.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct OuterDim(DimState);

impl Display for OuterDim {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(dim_st) => write!(f, "outer dimensionality {dim_st}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum DimState {
    /// Dimensionality has been explicitly constrained via user
    ///   specification and cannot be changed.
    ///
    /// The provided [`Span`] references the location of the user-specified
    ///   constraint.
    /// Unification must honor this value.
    _Constraint(Dim, Span),

    /// Dimensionality is still being inferred,
    ///   but it is known to be at least this size.
    ///
    /// The provide [`Span`] must serve as evidence for this inference by
    ///   referencing a value of this dimensionality.
    _AtLeast(Dim, Span),

    /// Dimensionality is not constrained and has not yet been inferred.
    ///
    /// This also serves as a TODO until we infer dimensions.
    #[default]
    Unknown,
}

impl Display for DimState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use DimState::*;

        match self {
            _Constraint(dim, _) => write!(f, "constrained to {dim}"),
            _AtLeast(dim, _) => write!(f, "of at least {dim}"),
            Unknown => write!(f, "unknown"),
        }
    }
}

object_rel! {
    /// An expression is inherently a tree,
    ///   however it may contain references to other identifiers which
    ///   represent their own trees.
    /// Any [`Ident`] reference is a cross edge.
    Expr -> {
        cross Ident,
        tree  Expr,
        tree  Doc,

        // Template application
        tree Tpl,
    }
}

impl ObjectIndex<Expr> {
    /// Create a new subexpression as the next child of this expression and
    ///   return the [`ObjectIndex`] of the new subexpression.
    ///
    /// Sub-expressions maintain relative order to accommodate
    ///   non-associative and non-commutative expressions.
    pub fn create_subexpr(
        self,
        asg: &mut Asg,
        expr: Expr,
    ) -> Result<ObjectIndex<Expr>, AsgError> {
        let oi_subexpr = asg.create(expr);
        oi_subexpr.add_edge_from(asg, self, None)
    }

    /// Reference the value of the expression identified by `oi_ident` as if
    ///   it were a subexpression.
    pub fn ref_expr(
        self,
        asg: &mut Asg,
        oi_ident: ObjectIndex<Ident>,
    ) -> Result<Self, AsgError> {
        self.add_edge_to(asg, oi_ident, Some(oi_ident.span()))
    }

    /// The expression is held by the container `oi_container`.
    ///
    /// This is intended to convey that an expression would otherwise be
    ///   dangling (unreachable) were it not for the properties
    ///   of `oi_container`.
    /// If this is not true,
    ///   consider using:
    ///
    ///  1. [`Self::create_subexpr`] to create and assign ownership of
    ///       expressions contained within other expressions; or
    ///  2. [`ObjectIndex<Ident>::bind_definition`] if this expression is to
    ///       be assigned to an identifier.
    pub fn held_by(
        &self,
        asg: &mut Asg,
        oi_container: ObjectIndexTo<Expr>,
    ) -> Result<Self, AsgError> {
        self.add_edge_from(asg, oi_container, None)
    }
}
