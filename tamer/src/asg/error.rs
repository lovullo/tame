// Abstract semantic graph (ASG) errors
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
///
///
/// Note that the user may encounter an equivalent error in the source
///   document format
///     (e.g. XML via [XIR->NIR lowering](crate::nir))
///     and therefore may never see some of these errors.
/// However,
///   a source IR _may_ choose to allow certain errors through to ease the
///   burden on its maintenance/development,
///     or a system may utilize this IR directly.
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

    /// Attempted to open a package while defining another package.
    ///
    /// Packages cannot be nested.
    /// The first span represents the location of the second package open,
    ///   and the second span represents the location of the package already
    ///   being defined.
    NestedPkgStart(Span, Span),

    /// Attempted to close a package when not in a package toplevel context.
    InvalidPkgEndContext(Span),

    /// Attempted to open an expression in an invalid context.
    PkgExpected(Span),

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

    /// A template is not reachable by any other object.
    ///
    /// See [`Self::DanglingExpr`] for more information on the concept of
    ///   dangling objects.
    DanglingTpl(Span),

    /// Attempted to close an expression with no corresponding opening
    ///   delimiter.
    UnbalancedExpr(Span),

    /// Attempted to close a template with no corresponding opening
    ///   delimiter.
    UnbalancedTpl(Span),

    /// Attempted to bind the an identifier to an expression while not in an
    ///   expression context.
    ///
    /// Note that the user may encounter an error from a higher-level IR
    ///   instead of this one.
    InvalidExprBindContext(SPair),

    /// Attempted to reference an identifier as part of an expression while
    ///   not in an expression context.
    ///
    /// Ideally this situation is syntactically invalid in a source IR.
    InvalidExprRefContext(SPair),
}

impl Display for AsgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AsgError::*;

        match self {
            IdentTransition(err) => Display::fmt(&err, f),
            IdentRedefine(spair, _) => {
                write!(f, "cannot redefine {}", TtQuote::wrap(spair))
            }
            NestedPkgStart(_, _) => write!(f, "cannot nest packages"),
            InvalidPkgEndContext(_) => {
                write!(f, "invalid context for package close",)
            }
            PkgExpected(_) => write!(f, "expected package definition"),
            DanglingExpr(_) => write!(
                f,
                "dangling expression (anonymous expression has no parent)"
            ),
            DanglingTpl(_) => write!(
                f,
                "dangling template (anonymous template cannot be referenced)"
            ),
            UnbalancedExpr(_) => write!(f, "unbalanced expression"),
            UnbalancedTpl(_) => write!(f, "unbalanced template definition"),
            InvalidExprBindContext(_) => {
                write!(f, "invalid expression identifier binding context")
            }
            InvalidExprRefContext(ident) => {
                write!(
                    f,
                    "invalid context for expression identifier {}",
                    TtQuote::wrap(ident)
                )
            }
        }
    }
}

impl Error for AsgError {}

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

            NestedPkgStart(second, first) => vec![
                first.note("this package is still being defined"),
                second.error("attempted to open another package here"),
                second.help(
                    "close the package to complete its definition before \
                        attempting to open another",
                ),
            ],

            InvalidPkgEndContext(span) => vec![
                span.error("package close was not expected here"),
                span.help(
                    "a package must be closed at the same level of nesting \
                       that it was opened",
                ),
            ],

            PkgExpected(span) => {
                vec![span.error("a package definition was expected here")]
            }

            DanglingExpr(span) => vec![
                span.error(
                    "this expression is unreachable and its value \
                       cannot be used",
                ),
                span.help("an expression must either be the child of another "),
                span.help(
                    "  expression or be assigned an identifier, otherwise ",
                ),
                span.help("  its value cannot referenced."),
            ],

            DanglingTpl(span) => vec![
                span.error(
                    "this template is unreachable and can never be used",
                ),
                span.help(
                    "a template may only be anonymous if it is ephemeral ",
                ),
                span.help("  (immediately expanded)."),
                span.help("alternatively, assign this template an identifier."),
            ],

            UnbalancedExpr(span) => {
                vec![span.error("there is no open expression to close here")]
            }

            UnbalancedTpl(span) => {
                vec![span.error("there is no open template to close here")]
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

            InvalidExprRefContext(ident) => vec![ident.error(
                "cannot reference the value of an expression from outside \
                    of an expression context",
            )],
        }
    }
}
