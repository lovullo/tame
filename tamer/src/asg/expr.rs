// Expressions represented on the ASG
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

//! Expressions on the ASG.

use std::fmt::Display;

use super::{Asg, ObjectIndex};
use crate::{f::Functor, num::Dim, span::Span};

/// Expression.
///
/// The [`Span`] of an expression should be expanded to encompass not only
///   all child expressions,
///     but also any applicable closing span.
#[derive(Debug, PartialEq, Eq)]
pub struct Expr(ExprOp, ExprDim, Span);

impl Expr {
    pub fn new(op: ExprOp, span: Span) -> Self {
        Self(op, ExprDim::default(), span)
    }

    pub fn span(&self) -> Span {
        match self {
            Expr(_, _, span) => *span,
        }
    }
}

impl Functor<Span> for Expr {
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
#[derive(Debug, PartialEq, Eq)]
pub enum ExprOp {
    Sum,
    Product,
}

impl Display for ExprOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ExprOp::*;

        match self {
            Sum => write!(f, "sum"),
            Product => write!(f, "product"),
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
    ) -> ObjectIndex<Expr> {
        let oi_subexpr = asg.create(expr);
        self.add_edge_to(asg, oi_subexpr);

        oi_subexpr
    }
}
