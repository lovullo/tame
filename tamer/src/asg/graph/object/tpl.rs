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
    Object, ObjectIndex, ObjectRel, ObjectRelFrom, ObjectRelTy, ObjectRelatable,
};
use crate::span::Span;

/// Template.
#[derive(Debug, PartialEq, Eq)]
pub struct Tpl;

impl Tpl {
    pub fn span(&self) -> Span {
        todo!("Tpl::span")
    }
}

impl Display for Tpl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "template")
    }
}

/// Subset of [`ObjectKind`]s that are valid targets for edges from
///   [`Tpl`].
///
/// See [`ObjectRel`] for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum TplRel {
    // TODO
}

impl ObjectRel<Tpl> for TplRel {
    fn narrow<OB: ObjectRelFrom<Tpl> + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>> {
        None
    }

    /// Whether this is a cross edge to another tree.
    ///
    /// An expression is inherently a tree,
    ///   however it may contain references to other identifiers which
    ///   represent their own trees.
    /// Any [`Ident`] reference is a cross edge.
    fn is_cross_edge(&self) -> bool {
        false
    }
}

impl ObjectRelatable for Tpl {
    type Rel = TplRel;

    fn rel_ty() -> ObjectRelTy {
        ObjectRelTy::Tpl
    }

    fn new_rel_dyn(
        ty: ObjectRelTy,
        _oi: ObjectIndex<Object>,
    ) -> Option<TplRel> {
        match ty {
            ObjectRelTy::Root => None,
            ObjectRelTy::Pkg => None,
            ObjectRelTy::Ident => None,
            ObjectRelTy::Expr => None,
            ObjectRelTy::Tpl => None,
        }
    }
}

impl ObjectIndex<Tpl> {
    // TODO
}
