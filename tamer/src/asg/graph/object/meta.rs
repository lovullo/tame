// Metasyntactic variables represented on the ASG
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

//! Metasyntactic variables on the ASG.
//!
//! Metasyntactic variables
//!   (sometimes called "metavariables" herein for short)
//!   have historically been a feature of the template system.
//! The canonical metavariable is the template parameter.

use super::{
    Ident, Object, ObjectIndex, ObjectIndexRelTo, ObjectRel, ObjectRelFrom,
    ObjectRelTy, ObjectRelatable, Tpl,
};
use crate::{
    asg::Asg,
    diagnose::Annotate,
    diagnostic_todo,
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::util::SPair,
    span::Span,
};
use std::fmt::Display;

/// Metasyntactic variable (metavariable).
///
/// A metavariable is a lexical construct.
/// Its value is a lexeme that represents an [`Ident`],
///   whose meaning depends on the context in which the metavariable is
///   referenced.
/// Its lexeme may be composed of multiple [`Self::Lexeme`]s,
///   and may even be constructed dynamically based on the values of other
///   [`Meta`]s.
///
/// Metavariables are identified by being bound by an [`Ident`];
///   the symbol representing that identifier then acts as a metavariable.
#[derive(Debug, PartialEq, Eq)]
pub enum Meta {
    Required(Span),
    ConcatList(Span),
    Lexeme(Span, SPair),
}

impl Meta {
    /// Create a new metavariable without a value.
    ///
    /// Metavariables with no value cannot be used in an expansion context.
    /// Intuitively,
    ///   they act as required parameters.
    pub fn new_required(span: Span) -> Self {
        Self::Required(span)
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Required(span)
            | Self::ConcatList(span)
            | Self::Lexeme(span, _) => *span,
        }
    }

    /// Assign a lexeme to a metavariable.
    ///
    /// In a template definition context,
    ///   this acts as a default value for this metavariable.
    /// In an application context,
    ///   this has the effect of binding a value to this metavariable.
    pub fn assign_lexeme(self, lexeme: SPair) -> Self {
        match self {
            Self::Required(span) => Self::Lexeme(span, lexeme),

            Self::ConcatList(_) => diagnostic_todo!(
                vec![lexeme.note("while parsing this lexeme")],
                "append to ConcatList",
            ),

            Self::Lexeme(_, _) => diagnostic_todo!(
                vec![lexeme.note("while parsing this lexeme")],
                "Lexeme => ConcatList",
            ),
        }
    }
}

impl From<&Meta> for Span {
    fn from(meta: &Meta) -> Self {
        meta.span()
    }
}

impl Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Required(_) => {
                write!(f, "metasyntactic parameter with required value")
            }
            Self::ConcatList(_) => {
                write!(f, "metasyntactic concatenation list")
            }
            Self::Lexeme(_, spair) => {
                write!(f, "lexeme {}", TtQuote::wrap(spair))
            }
        }
    }
}

impl Functor<Span> for Meta {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self::Required(span) => Self::Required(f(span)),
            Self::ConcatList(span) => Self::ConcatList(f(span)),
            Self::Lexeme(span, spair) => Self::Lexeme(f(span), spair),
        }
    }
}

object_rel! {
    /// Templates may expand into nearly any context,
    ///   and must therefore be able to contain just about anything.
    Meta -> {
        tree  Meta,
        cross Ident,
    }
}

impl ObjectIndex<Meta> {
    pub fn identify_as_tpl_param(
        &self,
        asg: &mut Asg,
        oi_tpl: ObjectIndex<Tpl>,
        name: SPair,
    ) -> ObjectIndex<Ident> {
        oi_tpl
            .declare_local(asg, name)
            .add_edge_to(asg, *self, None)
    }

    pub fn assign_lexeme(self, asg: &mut Asg, lexeme: SPair) -> Self {
        self.map_obj(asg, |meta| meta.assign_lexeme(lexeme))
    }

    pub fn close(self, asg: &mut Asg, close_span: Span) -> Self {
        self.map_obj(asg, |meta| {
            meta.map(|open_span| {
                open_span.merge(close_span).unwrap_or(open_span)
            })
        })
    }
}
