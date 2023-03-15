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
    Ident, Object, ObjectIndex, ObjectRel, ObjectRelFrom, ObjectRelTy,
    ObjectRelatable,
};
use crate::{
    fmt::{DisplayWrapper, TtQuote},
    parse::{util::SPair, Token},
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
    Lexeme(SPair),
}

impl Meta {
    pub fn span(&self) -> Span {
        match self {
            Self::Required(span) | Self::ConcatList(span) => *span,
            Self::Lexeme(spair) => spair.span(),
        }
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
            Self::Lexeme(spair) => write!(f, "lexeme {}", TtQuote::wrap(spair)),
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
