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
//!
//! An _expression_ in TAME is an object that yields a numeric value
//!   (a mathematical expression).
//! Expressions are referentially transparent,
//!   and values are expressions,
//!   so expressions both naturally compose and are able to be replaced with
//!     the value that they represent without affecting the meaning of the
//!     program.

use super::{
    ident::IdentDefinition, prelude::*, Doc, Ident, ObjectIndexToTree, Tpl,
};
use crate::{
    asg::graph::ProposedRel, diagnose::panic::DiagnosticPanic, num::Dim,
    parse::prelude::Annotate, span::Span,
};
use std::{fmt::Display, num::NonZeroU16};

#[cfg(doc)]
use super::ObjectKind;

/// Expression.
///
/// The [`Span`] of an expression should be expanded to encompass not only
///   all child expressions,
///     but also any applicable closing span.
#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    op: ExprOp,
    dim: ExprDim,
    meta: MetaState,
    span: Span,
}

impl Expr {
    pub fn new(op: ExprOp, span: Span) -> Self {
        Self {
            op,
            dim: ExprDim::default(),
            meta: MetaState::default(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr { span, .. } => *span,
        }
    }

    pub fn op(&self) -> ExprOp {
        match self {
            Expr { op, .. } => *op,
        }
    }

    /// Whether an expression is concrete, abstract, or not yet known.
    ///
    /// Note that,
    ///   since [`Ident`]s reference expressions,
    ///   an abstract identifier is able to reference a concrete
    ///   expression.
    /// This may not be intuitive when looking at the source XML notation,
    ///   or when looking at AIR,
    ///   since both are structured to appear as though the expression
    ///   parents the identifier;
    ///     this is not the case.
    pub fn meta_state(&self) -> MetaState {
        match self {
            Expr { meta, .. } => *meta,
        }
    }
}

impl_mono_map! {
    Span => Expr { span, .. },
    MetaState => Expr { meta, .. },
}

impl From<&Expr> for Span {
    fn from(val: &Expr) -> Self {
        val.span()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self {
                op,
                dim,
                meta,
                // intentional: exhaustiveness check to bring attention to
                //   this when fields change
                span: _span,
            } => write!(f, "{meta} {op} expression with {dim}"),
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

/// The state of an [`Expr`] in a metalanguage context.
///
/// Intuitively,
///   an expression is [`MetaState::Concrete`] if and only if template
///   expansion would act as an identity function.
///
/// This does not cache edges that contributed to these decisions,
///   since all such edges are direct children and can be quickly and easily
///   discovered by iterating over those edges.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum MetaState {
    /// Neither the expression nor its children references any
    ///   metavariables.
    #[default]
    Concrete,

    /// Either the expression or one of its children references some
    ///   metavariable.
    ///
    /// This does not store information about the location of those
    ///   references;
    ///     it is expected that they will be located during a walk of the
    ///     graph during e.g. template expansion.
    ///
    /// Note that a metavariable reference is behind an [`Ident`].
    Abstract,

    /// There are a number of references to [`Ident`]s that are missing
    ///   definitions,
    ///     but no abstract references have yet been found.
    ///
    /// As soon as a single abstract reference is encountered,
    ///   [`Self::Abstract`] is able to be inferred and this distinction no
    ///   longer matters.
    ///
    /// The choice of [`u16`] for this count is a compromise:
    ///   [`u8`] is too small for aggressive out-of-order code generation,
    ///     and [`u32`] is a lot of space to waste for every [`Expr`] for
    ///     something that is very unlikely to ever occur.
    /// [`u16`] is plenty large enough to put the burden on a code
    ///   generation tool to either break up expressions,
    ///     or to order dependencies
    ///       (the former is relatively trivial for any tool).
    MaybeConcrete(NonZeroU16),
}

impl MetaState {
    /// Cache the existence of an abstract identifier.
    ///
    /// This will always result in [`Self::Abstract`],
    ///   no matter what the current state of `self`.
    fn found_abstract(self) -> Self {
        // At the time of writing,
        //   abstract takes precedence over all other states.
        // However,
        //   please keep the exhaustive check here,
        //   as it will draw our attention to this for new variants just in
        //   case that assumption changes.
        match self {
            Self::Concrete | Self::Abstract | Self::MaybeConcrete(_) => {
                Self::Abstract
            }
        }
    }

    /// Cache the existence of an identifier that is not known to be either
    ///   concrete or abstract.
    ///
    /// The [`Span`] `at` is used only for diagnostics if storage limits are
    ///   exceeded
    ///     (see [`Self::MaybeConcrete`]).
    /// The system does not cache information about edges;
    ///   it maintains only a count that can be decremented as edges are
    ///   resolved in the future.
    fn found_missing(self, at: Span) -> Self {
        match self {
            Self::Concrete => Self::MaybeConcrete(NonZeroU16::MIN),
            Self::Abstract => Self::Abstract,
            Self::MaybeConcrete(x) => {
                Self::MaybeConcrete(x.checked_add(1).diagnostic_unwrap(|| {
                    vec![
                        at.internal_error("missing identifier limit exceeded"),
                        at.help(
                            "either move this reference into another \
                               expression or move dependencies before this \
                               expression",
                        ),
                        at.help(
                            "it is not expected that this limit be reached \
                               except by code generation",
                        ),
                    ]
                }))
            }
        }
    }
}

impl Display for MetaState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MetaState::Concrete => write!(f, "concrete"),
            MetaState::Abstract => write!(f, "abstract"),
            MetaState::MaybeConcrete(n) => {
                write!(f, "possibly-concrete ({n}-Missing)")
            }
        }
    }
}

object_rel! {
    /// An expression is inherently a tree,
    ///   however it may contain references to other identifiers which
    ///   represent their own trees.
    /// Any [`Ident`] reference is a cross edge.
    Expr -> {
        cross Ident {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Ident>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                match rel.to_oi.definition(asg) {
                    // Metavariable references mean that the source
                    //   expression will require expansion.
                    Some(IdentDefinition::Meta(_)) => {
                        rel.from_oi.map_obj_inner(
                            asg,
                            |meta: MetaState| meta.found_abstract()
                        );
                    },

                    // Non-meta identifiers are just references.
                    // We don't care what they are as long as they're not
                    //   metavariables.
                    Some(IdentDefinition::Expr(_) | IdentDefinition::Tpl(_)) => (),

                    None => {
                        rel.from_oi.map_obj_inner(asg, |meta: MetaState| {
                            // This is a cross edge and so this span must be
                            //   available, but the types provided don't
                            //   guarantee that.
                            let span = rel.ref_span.unwrap_or(rel.to_oi.span());
                            meta.found_missing(span)
                        });
                    }
                };

                Ok(commit(asg))
            }
        },

        tree  Expr,
        tree  Doc,

        // Template application
        tree Tpl,
    }
}

impl ObjectIndex<Expr> {
    /// Finalize an expression's definition by updating its span to
    ///   encompass the entire (lexical) definition.
    pub fn close(self, asg: &mut Asg, end: Span) -> Self {
        self.map_obj_inner(asg, |span: Span| span.merge(end).unwrap_or(span))
    }

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
        oi_subexpr.add_tree_edge_from(asg, self)
    }

    /// Reference the value of the expression identified by `oi_ident` as if
    ///   it were a subexpression.
    pub fn ref_expr(
        self,
        asg: &mut Asg,
        oi_ident: ObjectIndex<Ident>,
    ) -> Result<Self, AsgError> {
        self.add_cross_edge_to(asg, oi_ident, oi_ident.span())
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
        oi_container: ObjectIndexToTree<Expr>,
    ) -> Result<Self, AsgError> {
        self.add_tree_edge_from(asg, oi_container)
    }
}
