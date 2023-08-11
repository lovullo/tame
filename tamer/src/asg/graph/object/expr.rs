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
//!
//! An expression is [_concrete_](`MetaState::Concrete`) if it requires no
//!   expansion by the template system.
//! If an expression or any of its children reference any
//!   [metavariables](super::Meta)
//!     (template parameters),
//!       then the expression will be [_abstract_](MetaState::Abstract).
//! Expressions' static bindings together with their referential
//!   transparency means that a concrete expression is able to be moved and
//!   copied to any other point in the program without changing its
//!   meaning;
//!     this includes the act of copying via template expansion.
//!
//! A _reference_ to another expression does not have any influence over
//!   whether an expression is abstract or not.
//! In graph terms:
//!   tree edges influence an expression's [`MetaState`],
//!     but not cross edges.
//! Consider the following expression in XML notation to help with intuition
//!   on this.
//! Assume that this is the body of some template with a single template
//!   parameter identified as `@foo@`:
//!
//! ```xml
//!   <!-- there are no metavariable references, so this is concrete -->
//!   <c:sum id="conc">
//!     <c:value-of name="#5" />
//!   </c:sum>
//!
//!   <!-- this is abstract because it requires expansion -->
//!   <c:sum id="abstract">
//!     <c:value-of name="@foo@" />
//!   </c:sum>
//!
//!   <!-- this is concrete... -->
//!   <c:sum id="combine">
//!     <c:value-of name="conc" />
//!     <!-- ...even though `abstract` is abstract, because moving or
//!          copying `combine` would have no effect on the meaning of the
//!          the expression, and there is nothing to expand via the template
//!          system -->
//!     <c:value-of name="abstract" />
//!   </c:sum>
//! ```

use super::{
    ident::IdentDefinition, prelude::*, Doc, Ident, ObjectIndexToTree, Tpl,
};
use crate::{
    asg::graph::ProposedRel,
    diagnose::panic::DiagnosticPanic,
    num::Dim,
    parse::{prelude::Annotate, Token},
    span::Span,
};
use std::{fmt::Display, num::NonZeroU16};

#[cfg(doc)]
use super::ObjectKind;

/// Expression.
///
/// The [`Span`] of an expression should be expanded to encompass not only
///   all child expressions,
///     but also any applicable closing span.
///
/// See the [parent module](self) for more information.
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

    /// Determine how a child expression should impact whether this
    ///   expression is abstract.
    fn observe_child(self, state: MetaState, span: Span) -> Self {
        match state {
            Self::Concrete => self,
            Self::Abstract => self.found_abstract(),

            // Since we track the number of missing edges to direct children,
            //   we treat the child subgraph as if it were a single node on
            //   the graph.
            Self::MaybeConcrete(_) => self.found_missing(span),
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

                    // This is a _reference_ to another expression tree.
                    // Only tree edges influence our abstract status.
                    Some(IdentDefinition::Expr(_)) => (),

                    // Unlike the XSLT-based TAME,
                    //   this reference can act as a template application,
                    //   just as the `tree Tpl` edge below.
                    // TODO: We can expand closed expr templates here,
                    //   since it's no different than referencing the inner
                    //   expression.
                    Some(IdentDefinition::Tpl(_)) => diagnostic_todo!(
                        vec![
                            rel.to_oi.error("this references a template"),
                            rel.to_oi.help(
                                "only closed expression templates will be \
                                   supported in this context"
                            )
                        ],
                        "template references in an expression context are
                           not yet supported"
                    ),

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

        tree Expr {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Self>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                let to = rel.to_oi.resolve(asg);

                let child_state = to.meta_state();
                let span = to.span();

                rel.from_oi.map_obj_inner(asg, |state: MetaState| {
                    state.observe_child(child_state, span)
                });

                Ok(commit(asg))
            }
        },

        tree Doc {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Doc>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                let doc = rel.to_oi.resolve(asg);

                match doc {
                    Doc::IndepClause(_) => Ok(commit(asg)),

                    // This maintains compatibility with the XLST-based
                    //   system.
                    // TODO: Inline documentation was prohibited within
                    //   expressions because of questions of how it would
                    //   render within the Summary Page;
                    //     this restriction should just be lifted and dealt
                    //     with separately.
                    Doc::Text(spair) => Err(AsgError::InvalidDocContextExpr(
                        spair.span(),
                        rel.from_oi.resolve(asg).span(),
                    ))
                }
            }
        },

        // Deferred template application
        tree Tpl,
    }
}

impl ObjectIndex<Expr> {
    /// Finalize an expression's definition by updating its span to
    ///   encompass the entire (lexical) definition.
    pub fn close(self, asg: &mut Asg, end: Span) -> Self {
        self.map_obj_inner(asg, |span: Span| span.merge(end).unwrap_or(span))
    }

    /// Add a completed subexpression as a child of a parent expression.
    ///
    /// It is important that the subexpression has _completed parsing_ so
    ///   that edge hooks are able to conduct inference on the entirety of
    ///   the subexpression.
    ///
    /// Sub-expressions maintain relative order to accommodate
    ///   non-associative and non-commutative expressions.
    pub fn add_completed_subexpr(
        self,
        asg: &mut Asg,
        oi_sub: ObjectIndex<Expr>,
    ) -> Result<ObjectIndex<Expr>, AsgError> {
        self.add_tree_edge_to(asg, oi_sub)
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
    ///  1. [`Self::add_completed_subexpr`] to create and assign ownership
    ///       of expressions contained within other expressions; or
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
