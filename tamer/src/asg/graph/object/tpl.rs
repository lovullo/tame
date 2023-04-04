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

use super::{
    Expr, Ident, Object, ObjectIndex, ObjectIndexRelTo, ObjectRel,
    ObjectRelFrom, ObjectRelTy, ObjectRelatable, ObjectTreeRelTo,
};
use crate::{
    asg::Asg,
    f::Functor,
    parse::{util::SPair, Token},
    span::Span,
};

/// Template with associated name.
#[derive(Debug, PartialEq, Eq)]
pub struct Tpl(Span);

impl Tpl {
    pub fn span(&self) -> Span {
        match self {
            Self(span) => *span,
        }
    }

    pub fn new(span: Span) -> Self {
        Self(span)
    }
}

impl Functor<Span> for Tpl {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self(span) => Self(f(span)),
        }
    }
}

impl Display for Tpl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "template")
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
    }
}

impl ObjectIndex<Tpl> {
    /// Attempt to complete a template definition.
    ///
    /// This updates the span of the template to encompass the entire
    ///   definition.
    pub fn close(self, asg: &mut Asg, close_span: Span) -> Self {
        self.map_obj(asg, |tpl| {
            tpl.map(|open_span| {
                open_span.merge(close_span).unwrap_or(open_span)
            })
        })
    }

    /// Apply a named template `id` to the context of `self`.
    ///
    /// During evaluation,
    ///   this application will expand the template in place,
    ///   re-binding metavariables to the context of `self`.
    pub fn apply_named_tpl(self, asg: &mut Asg, id: SPair) -> Self {
        let oi_apply = asg.lookup_global_or_missing(id);
        self.add_edge_to(asg, oi_apply, Some(id.span()))
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
    ) -> Self {
        self.add_edge_from(asg, oi_target_parent, None)
    }
}
