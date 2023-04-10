// Documentation represented on the ASG
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

//! Documentation on the ASG.
//!
//! TODO: Document TAME's stance on documentation and literate programming,
//!   much of which hasn't been able to be realized over the years.

use super::prelude::*;
use crate::{
    parse::{util::SPair, Token},
    span::Span,
};
use std::fmt::Display;

/// Documentation string.
///
/// TODO: This presently serves as a subject line,
///   e.g. a description or label,
///   but will evolve in the future.
#[derive(Debug, PartialEq, Eq)]
pub enum Doc {
    /// An (ideally) concise independent clause describing an object.
    IndepClause(SPair),
}

impl Display for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "documentation string")
    }
}

impl Doc {
    /// Document an object using what is ideally a concise independent
    ///   clause.
    pub fn new_indep_clause(clause: SPair) -> Self {
        Self::IndepClause(clause)
    }

    pub fn indep_clause(&self) -> Option<SPair> {
        match self {
            Self::IndepClause(spair) => Some(*spair),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::IndepClause(spair) => spair.span(),
        }
    }
}

object_rel! {
    /// Templates may expand into nearly any context,
    ///   and must therefore be able to contain just about anything.
    Doc -> {
        // empty
    }
}
