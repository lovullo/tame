// Abstract semantic graph (ASG) errors
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

//! Errors resulting from operations on the ASG.

use std::{
    error::Error,
    fmt::{self, Display},
};

use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    span::Span,
};

use super::TransitionError;

/// An error from an ASG operation.
#[derive(Debug, PartialEq)]
pub enum AsgError {
    /// An object could not change state in the manner requested.
    IdentTransition(TransitionError),

    /// An expresion is not reachable by any other expression or
    ///   identifier.
    ///
    /// A dangling expression has no incoming edge from any other object and
    ///   can therefore not be referenced.
    ///
    /// Since the expression is dangling,
    ///   it must be anonymous,
    ///   and can therefore only be identified meaningfully to the user by
    ///   its span.
    /// The span should encompass the entirety of the expression.
    DanglingExpr(Span),
}

impl Display for AsgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AsgError::*;

        match self {
            IdentTransition(err) => Display::fmt(&err, f),
            DanglingExpr(_) => write!(f, "dangling expression"),
        }
    }
}

impl Error for AsgError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use AsgError::*;

        match self {
            IdentTransition(err) => err.source(),
            DanglingExpr(_) => None,
        }
    }
}

impl From<TransitionError> for AsgError {
    fn from(err: TransitionError) -> Self {
        Self::IdentTransition(err)
    }
}

impl Diagnostic for AsgError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use AsgError::*;

        match self {
            // TODO: need spans
            IdentTransition(_) => vec![],

            DanglingExpr(span) => vec![
                span.error("expression has no parent or identifier"),
                span.help("an expression must either be the child of another "),
                span.help(
                    "  expression or be assigned an identifier, otherwise ",
                ),
                span.help("  its value cannot referenced."),
            ],
        }
    }
}
