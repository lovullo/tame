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
    fmt::{DisplayWrapper, TtQuote},
    parse::util::SPair,
    span::Span,
};

use super::TransitionError;

/// An error from an ASG operation.
#[derive(Debug, PartialEq)]
pub enum AsgError {
    /// An object could not change state in the manner requested.
    IdentTransition(TransitionError),

    /// An identifier was already bound to some object,
    ///   and an attempt was made to bind it to a different one.
    ///
    /// This includes an [`SPair`] representing the _original_ definition
    ///   that was already accepted by the system and a [`Span`]
    ///   representing the _duplicate_ definition that triggered this error.
    ///
    /// Note that this is different than a _redeclaration_;
    ///   _defining_ an identifier associates it with an object,
    ///     whereas _declaring_ an identifier provides metadata about it.
    IdentRedefine(SPair, Span),

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

    /// Attempted to close an expression with no corresponding opening
    ///   delimiter.
    ///
    /// Note that the user may encounter an equivalent error in the source
    ///   document format
    ///     (e.g. XML via [XIR->NIR lowering](crate::nir))
    ///     and therefore may never see this error.
    /// However,
    ///   a source IR _may_ choose to allow improperly nested expressions
    ///   through to this IR,
    ///     or may utilize this IR directly.
    UnbalancedExpr(Span),

    /// Attempted to bind the an identifier to an expression while not in an
    ///   expression context.
    ///
    /// Note that the user may encounter an error from a higher-level IR
    ///   instead of this one.
    InvalidExprBindContext(SPair),
}

impl Display for AsgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AsgError::*;

        match self {
            IdentTransition(err) => Display::fmt(&err, f),
            IdentRedefine(spair, _) => {
                write!(f, "cannot redefine {}", TtQuote::wrap(spair))
            }
            DanglingExpr(_) => write!(f, "dangling expression"),
            UnbalancedExpr(_) => write!(f, "unbalanced expression"),
            InvalidExprBindContext(_) => {
                write!(f, "invalid expression identifier binding context")
            }
        }
    }
}

impl Error for AsgError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use AsgError::*;

        match self {
            IdentTransition(err) => err.source(),
            IdentRedefine(_, _)
            | DanglingExpr(_)
            | UnbalancedExpr(_)
            | InvalidExprBindContext(_) => None,
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

        // Before improving the diagnostic messages below,
        //   be sure that you have a use case in mind and that higher-level
        //   IRs do not preempt them in practice;
        //     your efforts may be better focused in those higher IRs.
        match self {
            // TODO: need spans
            IdentTransition(_) => vec![],

            IdentRedefine(first, span_redecl) => vec![
                first.note(format!(
                    "first definition of {} is here",
                    TtQuote::wrap(first)
                )),
                span_redecl.error(format!(
                    "attempted to redefine {} here",
                    TtQuote::wrap(first),
                )),
                span_redecl.help(format!(
                    "variables in TAME are immutable; {} was previously",
                    TtQuote::wrap(first),
                )),
                span_redecl
                    .help("  defined and its definition cannot be changed."),
            ],

            DanglingExpr(span) => vec![
                span.error("expression has no parent or identifier"),
                span.help("an expression must either be the child of another "),
                span.help(
                    "  expression or be assigned an identifier, otherwise ",
                ),
                span.help("  its value cannot referenced."),
            ],

            UnbalancedExpr(span) => {
                vec![span.error("there is no open expression to end here")]
            }

            InvalidExprBindContext(span) => vec![
                span.error(
                    "there is no active expression to bind this identifier to",
                ),
                span.help(
                    "an identifier must be bound to an expression before \
                        the expression is closed",
                ),
            ],
        }
    }
}
