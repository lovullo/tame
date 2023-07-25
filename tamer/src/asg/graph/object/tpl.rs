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

use super::{prelude::*, Doc, Expr, Ident};
use crate::{f::Functor, parse::util::SPair, span::Span};

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

impl Functor<Span> for Tpl {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self(span, shape) => Self(f(span), shape),
        }
    }
}

impl Functor<TplShape> for Tpl {
    fn map(self, f: impl FnOnce(TplShape) -> TplShape) -> Self::Target {
        match self {
            Self(span, shape) => Self(span, f(shape)),
        }
    }
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
    Unknown,

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

impl Display for TplShape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // phrase as "template with ..."
        match self {
            TplShape::Unknown => write!(f, "unknown shape"),
            TplShape::Empty => write!(f, "empty shape"),
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
        tree Expr,

        // Identifiers are used for both references and identifiers that
        //   will expand into an application site.
        dyn Ident,

        // Template application.
        tree Tpl,

        // Short template description and arbitrary documentation to be
        //   expanded into the application site.
        tree Doc,
    }
}

impl ObjectIndex<Tpl> {
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

impl AsgRelMut<Expr> for Tpl {
    fn pre_add_edge(
        asg: &mut Asg,
        from_oi: ObjectIndex<Self>,
        to_oi: ObjectIndex<Expr>,
        _ctx_span: Option<Span>,
        commit: impl FnOnce(&mut Asg),
    ) -> Result<(), AsgError> {
        let span = to_oi.resolve(asg).span();
        from_oi.map_obj(asg, |tpl| tpl.overwrite(TplShape::Expr(span)));

        Ok(commit(asg))
    }
}

// TODO: Merge this into the macro above
impl AsgRelMut<Ident> for Tpl {}
impl AsgRelMut<Tpl> for Tpl {}
impl AsgRelMut<Doc> for Tpl {}

// This uses `min_specialization` to satisfy trait bounds for
//   `<ObjectIndexTo as ObjectIndexRelTo>::add_edge`.
// This will be better integrated in future commits.
// See message of the commit that introduced this comment for more
//   information.
impl<OB: ObjectRelatable> AsgRelMut<OB> for Tpl {
    default fn pre_add_edge(
        asg: &mut Asg,
        _from_oi: ObjectIndex<Self>,
        _to_oi: ObjectIndex<OB>,
        _ctx_span: Option<Span>,
        commit: impl FnOnce(&mut Asg),
    ) -> Result<(), AsgError> {
        Ok(commit(asg))
    }
}
