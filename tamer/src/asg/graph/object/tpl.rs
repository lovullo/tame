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
use crate::{asg::Asg, f::Functor, span::Span};

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
    /// TODO
    Tpl -> {
        // ...
    }
}

impl ObjectIndex<Tpl> {
    /// Complete a template definition.
    ///
    /// This simply updates the span of the template to encompass the entire
    ///   definition.
    pub fn close(self, asg: &mut Asg, close_span: Span) -> Self {
        self.map_obj(asg, |tpl| {
            tpl.map(|open_span| {
                open_span.merge(close_span).unwrap_or(open_span)
            })
        })
    }
}
