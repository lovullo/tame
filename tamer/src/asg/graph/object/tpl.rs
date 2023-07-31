// Templates represented on the ASG
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

//! Templates on the ASG.

use std::fmt::Display;

use super::{ident::IdentDefinition, prelude::*, Doc, Expr, Ident};
use crate::{asg::graph::ProposedRel, f::Map, parse::util::SPair, span::Span};

/// Template with associated name.
#[derive(Debug, PartialEq, Eq)]
pub struct Tpl(Span, TplShape);

impl Tpl {
    pub fn new(span: Span) -> Self {
        Self(span, TplShape::default())
    }

    pub fn span(&self) -> Span {
        match self {
            Self(span, _) => *span,
        }
    }

    pub fn shape(&self) -> TplShape {
        match self {
            Self(_, shape) => *shape,
        }
    }
}

impl_mono_map! {
    Span => Tpl(@, shape),
    TplShape => Tpl(span, @),
}

impl Display for Tpl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(_, shape) = self;
        write!(f, "template with {shape}")
    }
}

/// The "shape" of a template when expanded into an expression context.
///
/// The shape of a template can be thought of like a puzzle piece.
/// Each application context permits a particular type of puzzle piece,
///   and a compatible template must be expanded into it,
///   or otherwise be made to be compatible.
///
/// Template shapes must be known statically by the time the definition has
///   completed.
/// A definition is not complete until all missing identifier references
///   have been defined.
/// A corollary of this is that templates applied _within_ templates will
///   be able to determine their shape because the shape of the applied
///   template will be known,
///     allowing them to compose without compromising this property.
///
/// Objects that would typically be hoisted out of an expression context do
///   not contribute to the shape of a template.
/// That is---​
///   if an object would not typically be parented to the expansion context
///   if manually written at that source location,
///     then it will not be parented by a template expansion,
///     and so will not contribute to its shape.
///
/// Dynamic Inner Template Application
/// ==================================
/// Sometimes the shape of inner applications cannot be known because their
///   application depends on values of metavariables that are provided by
///   the caller.
/// One such example is that the body of the template is conditional
///   depending on what values are provided to the template.
///
/// In this case,
///   it may be necessary for the body of the template to _coerce_ into a
///   statically known shape by wrapping the dynamic application in a known
///   object.
/// For example,
///   if a template's body can conditionally expand into one of a set of
///   [`TplShape::Expr`] templates,
///     then that condition can be wrapped in an [`Expr`] object so that,
///       no matter what the expansion,
///     we'll always have a shape of [`TplShape::Expr`].
///
/// Expansion Ordering
/// ==================
/// By requiring a shape to be available by the time the definition of a
///   template is completed,
///     a system like [`AIR`](crate::asg::air) is able to pre-allocate an
///     [`Object`] at the application site.
/// This ensures that we are able to generate a graph with the proper edge
///   ordering,
///     which is important for non-commutative objects.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum TplShape {
    /// The template will not inline any objects.
    #[default]
    Empty,

    /// The template is non-[`Empty`](Self::Empty),
    ///   but its shape cannot yet be determined.
    ///
    /// A template's shape must be known by the time its definition has been
    ///   completed.
    /// Note that a definition is not complete until all missing identifiers
    ///   have been defined.
    ///
    /// The associated span represents the location that resulted in
    ///   uncertainty.
    Unknown(Span),

    /// The template can be expanded inline into a single [`Expr`].
    ///
    /// This allows a template to be expanded into an expression context and
    ///   provides assurances that it will not take the place of more than a
    ///   single expression.
    ///
    /// The associated span provides rationale for this shape assertion.
    /// The [`ObjectIndex`] is not cached here to avoid having to keep them
    ///   in sync if the graph changes,
    ///     in which case this rationale may represent the _original_
    ///     rationale before any graph rewriting.
    Expr(Span),
}

impl TplShape {
    /// Attempt to adapt a template shape to that of another.
    ///
    /// If the shape of `other` is a refinement of the shape of `self`,
    ///   then `other` will be chosen.
    /// If the shape of `other` conflicts with `self`,
    ///   an appropriate [`AsgError`] will describe the problem.
    fn try_adapt_to(
        self,
        other: TplShape,
        tpl_name: Option<SPair>,
    ) -> Result<Self, (Self, AsgError)> {
        match (self, other) {
            (TplShape::Expr(first_span), TplShape::Expr(bad_span)) => Err((
                self,
                AsgError::TplShapeExprMulti(tpl_name, bad_span, first_span),
            )),

            // Higher levels of specificity take precedence.
            // This pattern is designed to be very clear in what shape takes
            //   precedence over another.
            // It should be clear enough that there is no value in writing
            //   unit test against this method since those tests' examples
            //   would simply reiterate this table
            //     (but tests for AIR should still be written to test more
            //        complex interactions).
            #[rustfmt::skip]
            (
                TplShape::Empty,
                give_precedence_to @ (
                    TplShape::Empty
                    | TplShape::Unknown(_)
                    | TplShape::Expr(_)
                ),
            )
            | (
                TplShape::Unknown(_),
                give_precedence_to @ TplShape::Expr(_),
            )
            | (
                give_precedence_to @ TplShape::Unknown(_),
                TplShape::Empty | TplShape::Unknown(_),
            )
            | (
                give_precedence_to @ TplShape::Expr(_),
                TplShape::Empty | TplShape::Unknown(_),
            )
            => Ok(give_precedence_to),
        }
    }

    /// If the shape stores [`Span`] information as evidence of inference,
    ///   overwrite it with the provided `span`.
    ///
    /// This is most commonly used to encapsulate a previous shape
    ///   inference.
    /// For example,
    ///   a template application's span may overwrite the inferred shape of
    ///   its own body.
    fn overwrite_span_if_any(self, span: Span) -> Self {
        match self {
            TplShape::Empty => self,
            TplShape::Unknown(_) => TplShape::Unknown(span),
            TplShape::Expr(_) => TplShape::Expr(span),
        }
    }
}

/// Attempt to adapt a template shape to that of another.
///
/// This returns a partially applied [`TplShape::try_adapt_to`],
///   where the remaining argument is `self`.
fn try_adapt_to(
    other: TplShape,
    tpl_name: Option<SPair>,
) -> impl FnOnce(TplShape) -> Result<TplShape, (TplShape, AsgError)> {
    move |s| s.try_adapt_to(other, tpl_name)
}

impl Display for TplShape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // phrase as "template with ..."
        match self {
            TplShape::Empty => write!(f, "empty shape"),
            TplShape::Unknown(_) => write!(f, "unknown shape"),
            TplShape::Expr(_) => write!(f, "shape of a single expression"),
        }
    }
}

object_rel! {
    /// Templates may expand into nearly any context,
    ///   and must therefore be able to contain just about anything.
    Tpl -> {
        // Expressions must be able to be anonymous to allow templates in
        //   any `Expr` context.
        tree Expr {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Expr>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                let tpl_name = rel.from_oi.name(asg);
                let span = rel.to_oi.resolve(asg).span();

                rel.from_oi.try_map_obj_inner(
                    asg,
                    try_adapt_to(TplShape::Expr(span), tpl_name),
                )?;

                Ok(commit(asg))
            }
        },

        // Identifiers are used for both references and identifiers that
        //   will expand into an application site.
        dyn Ident {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Ident>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                let tpl_name = rel.from_oi.name(asg);

                match (rel.ctx_span, rel.to_oi.definition(asg)) {
                    // Missing definition results in shape uncertainty that
                    //   will have to be resolved when (if) a definition
                    //   becomes available.
                    (Some(ref_span), None) => {
                        rel.from_oi.try_map_obj_inner(
                            asg,
                            try_adapt_to(TplShape::Unknown(ref_span), tpl_name),
                        ).map(|_| ())
                    }

                    // TAME is referentally transparent,
                    //   so a reference to an Expr is no different than
                    //   inlining that Expr.
                    (Some(ref_span), Some(IdentDefinition::Expr(_))) => {
                        rel.from_oi.try_map_obj_inner(
                            asg,
                            try_adapt_to(TplShape::Expr(ref_span), tpl_name),
                        ).map(|_| ())
                    }

                    // This is the same as the `Tpl` tree edge below,
                    //   but a named template instead of an anonymous one.
                    (Some(ref_span), Some(IdentDefinition::Tpl(to_oi))) => {
                        apply_tpl_shape(asg, rel.from_oi, to_oi, Some(ref_span))
                    }

                    // The mere _existence_ of metavariables (template
                    //   params) do not influence the expansion shape.
                    (Some(_), Some(IdentDefinition::Meta(_))) => Ok(()),

                    // Lack of span means that this is not a cross edge,
                    //   and so not a reference;
                    //     this means that the object is identified and will
                    //     be hoisted into the rooting context of the
                    //     application site,
                    //       which does not impact template shape.
                    // TODO: Let's make that span assumption explicit in the
                    //   `ProposeRel` abstraction.
                    (None, _) => Ok(()),
                }?;

                Ok(commit(asg))
            }
        },

        // Template application.
        tree Tpl {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Tpl>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                apply_tpl_shape(asg, rel.from_oi, rel.to_oi, None)?;
                Ok(commit(asg))
            }
        },

        // Short template description and arbitrary documentation to be
        //   expanded into the application site.
        tree Doc,
    }
}

/// Apply the shape of a template to a parent template.
///
/// Phrased another way:
///   given a template application `oi_apply` in the body of a parent
///   template `oi_parent`,
///     infer the appropriate shape for `oi_parent`.
///
/// If `oi_apply` is behind a reference,
///   `ref_span` may be used to override the diagnostic span stored within
///   the shape.
/// Since this is used to report shape errors to the user,
///   this span should _always_ be within the context of `oi_parent`.
fn apply_tpl_shape(
    asg: &mut Asg,
    oi_parent: ObjectIndex<Tpl>,
    oi_apply: ObjectIndex<Tpl>,
    ref_span: Option<Span>,
) -> Result<(), AsgError> {
    let tpl_name = oi_parent.name(asg);
    let apply = oi_apply.resolve(asg);
    let apply_shape = apply
        .shape()
        .overwrite_span_if_any(ref_span.unwrap_or(apply.span()));

    oi_parent.try_map_obj_inner(asg, try_adapt_to(apply_shape, tpl_name))?;

    Ok(())
}

impl ObjectIndex<Tpl> {
    /// Name of template,
    ///   if any.
    ///
    /// A template may either be anonymous,
    ///   or it may not yet have a name because it is still under
    ///   construction.
    pub fn name(&self, asg: &Asg) -> Option<SPair> {
        self.ident(asg).and_then(|oi| oi.resolve(asg).name())
    }

    /// Attempt to complete a template definition.
    ///
    /// This updates the span of the template to encompass the entire
    ///   definition.
    pub fn close(self, asg: &mut Asg, close_span: Span) -> Self {
        self.map_obj(asg, |tpl| {
            tpl.map(|open_span: Span| {
                open_span.merge(close_span).unwrap_or(open_span)
            })
        })
    }

    /// Apply a named template `id` to the context of `self`.
    ///
    /// During evaluation,
    ///   this application will expand the template in place,
    ///   re-binding metavariables to the context of `self`.
    pub fn apply_named_tpl(
        self,
        asg: &mut Asg,
        oi_apply: ObjectIndex<Ident>,
        ref_span: Span,
    ) -> Result<Self, AsgError> {
        self.add_edge_to(asg, oi_apply, Some(ref_span))
    }

    /// Directly reference this template from another object
    ///   `oi_target_parent`,
    ///     indicating the intent to expand the template in place.
    ///
    /// This direct reference allows applying anonymous templates.
    ///
    /// The term "expansion" is equivalent to the application of a closed
    ///   template.
    /// If this template is _not_ closed,
    ///   it will result in an error during evaluation.
    pub fn expand_into<OP: ObjectIndexRelTo<Tpl>>(
        self,
        asg: &mut Asg,
        oi_target_parent: OP,
    ) -> Result<Self, AsgError> {
        self.add_edge_from(asg, oi_target_parent, None)
    }

    /// Arbitrary text serving as documentation in a literate style,
    ///   to be expanded into the application site.
    pub fn append_doc_text(
        &self,
        asg: &mut Asg,
        text: SPair,
    ) -> Result<Self, AsgError> {
        let oi_doc = asg.create(Doc::new_text(text));
        self.add_edge_to(asg, oi_doc, None)
    }
}
