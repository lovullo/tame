// Packages represented on ASG
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

//! Package object on the ASG.

use super::{
    Ident, Object, ObjectIndex, ObjectRel, ObjectRelFrom, ObjectRelTy,
    ObjectRelatable, ObjectTreeRelTo, Tpl,
};
use crate::{asg::Asg, f::Functor, span::Span};
use std::fmt::Display;

#[cfg(doc)]
use super::ObjectKind;

#[derive(Debug, PartialEq, Eq)]
pub struct Pkg(Span);

impl Pkg {
    pub fn new<S: Into<Span>>(span: S) -> Self {
        Self(span.into())
    }

    pub fn span(&self) -> Span {
        match self {
            Self(span) => *span,
        }
    }
}

impl Display for Pkg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "package")
    }
}

impl Functor<Span> for Pkg {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self(span) => Self(f(span)),
        }
    }
}

object_rel! {
    /// Packages serve as a root for all identifiers defined therein,
    ///   and so an edge to [`Ident`] will never be a cross edge.
    ///
    /// Imported [`Ident`]s do not have edges from this package.
    Pkg -> {
        tree Ident,

        // Anonymous templates are used for expansion.
        tree Tpl,
    }
}

impl ObjectIndex<Pkg> {
    /// Complete the definition of a package.
    pub fn close(self, asg: &mut Asg, span: Span) -> Self {
        self.map_obj(asg, Pkg::fmap(|open| open.merge(span).unwrap_or(open)))
    }
}
