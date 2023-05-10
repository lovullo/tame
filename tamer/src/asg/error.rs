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

use super::{
    graph::object::pkg::CanonicalNameError, visit::Cycle, TransitionError,
};

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

    /// Error while processing the canonical name for a package.
    PkgCanonicalName(CanonicalNameError),

    /// A package of this same name has already been defined.
    ///
    /// The [`SPair`]s represent the original and redefinition names
    ///   respectively.
    PkgRedeclare(SPair, SPair),

    /// Attempted to open a package while defining another package.
    ///
    /// Packages cannot be nested.
    /// The first span represents the location of the second package open,
    ///   and the second span represents the location of the package already
    ///   being defined.
    /// The [`SPair`]s are the respective package names.
    NestedPkgStart((Span, SPair), (Span, SPair)),

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

    /// Attempted to bind an identifier to an object while not in a context
    ///   that can receive an identifier binding.
    ///
    /// Note that the user may encounter an error from a higher-level IR
    ///   instead of this one.
    InvalidBindContext(SPair),

    /// Attempted to reference an identifier while not in a context that can
    ///   receive an identifier reference.
    ///
    /// Ideally this situation is syntactically invalid in a source IR.
    InvalidRefContext(SPair),

    /// Attempted to expand a template into a context that does not support
    ///   expansion.
    InvalidExpansionContext(Span),

    /// Documentation text is not valid in an expression context.
    ///
    /// This historical limitation existed because the author was unsure how
    ///   to go about rendering an equation with literate documentation
    ///   interspersed.
    /// The plan is to lift this limitation in the future.
    ///
    /// The spans represent the expression and the documentation text
    ///   respectively.
    InvalidDocContextExpr(Span, Span),

    /// A circular dependency was found where it is not permitted.
    ///
    /// A cycle almost always means that computing the value of an object
    ///   depends on first having computed itself,
    ///     which is not possible.
    UnsupportedCycle(Cycle),

    /// An opaque identifier was declared in an invalid context.
    UnexpectedOpaqueIdent(SPair),
}

impl Display for AsgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AsgError::*;

        match self {
            IdentTransition(e) => Display::fmt(&e, f),
            IdentRedefine(spair, _) => {
                write!(f, "cannot redefine {}", TtQuote::wrap(spair))
            }
            PkgCanonicalName(e) => Display::fmt(&e, f),
            PkgRedeclare(orig, _) => write!(
                f,
                "attempted to redeclare or redefine package {}",
                TtQuote::wrap(orig),
            ),
            NestedPkgStart((_, child), (_, parent)) => write!(
                f,
                "cannot define package {} while defining package {}",
                TtQuote::wrap(child),
                TtQuote::wrap(parent),
            ),
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
            InvalidBindContext(_) => {
                write!(f, "invalid identifier binding context")
            }
            InvalidRefContext(ident) => {
                write!(
                    f,
                    "invalid context for expression identifier {}",
                    TtQuote::wrap(ident)
                )
            }
            InvalidExpansionContext(_) => {
                write!(f, "invalid template expansion context",)
            }
            InvalidDocContextExpr(_, _) => {
                write!(f, "document text is not permitted within expressions")
            }
            UnsupportedCycle(cycle) => {
                write!(f, "circular dependency: {cycle}")
            }
            UnexpectedOpaqueIdent(name) => {
                write!(
                    f,
                    "opaque identifier {} declaration",
                    TtQuote::wrap(name)
                )
            }
        }
    }
}

impl Error for AsgError {}

impl From<TransitionError> for AsgError {
    fn from(e: TransitionError) -> Self {
        Self::IdentTransition(e)
    }
}

impl From<CanonicalNameError> for AsgError {
    fn from(e: CanonicalNameError) -> Self {
        Self::PkgCanonicalName(e)
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

            PkgCanonicalName(e) => e.describe(),

            PkgRedeclare(orig, redef) => vec![
                orig.note("package originally declared here"),
                redef.error("attempting to redeclare or redefine package here"),
            ],

            NestedPkgStart((second, sname), (first, fname)) => vec![
                first.note("this package is still being defined"),
                second.error("attempted to open another package here"),
                second.help(format!(
                    "end the package {} complete its definition before \
                        attempting to start the definition of {}",
                    TtQuote::wrap(fname),
                    TtQuote::wrap(sname),
                )),
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

            InvalidBindContext(name) => vec![name
                .error("an identifier binding is not valid in this context")],

            InvalidRefContext(ident) => vec![ident.error(
                "cannot reference the value of an expression from outside \
                    of an expression context",
            )],
            InvalidExpansionContext(span) => {
                vec![span.error("cannot expand a template here")]
            }

            InvalidDocContextExpr(expr_span, span) => vec![
                expr_span.note("in this expression"),
                span.error("documentation text is not permitted here"),
                span.help(
                    "this is a historical limitation that will \
                        likely be lifted in the future",
                ),
            ],
            UnsupportedCycle(cycle) => {
                // The cycle description clearly describes the cycle,
                //   but in neutral terms,
                //   since cycles may not necessarily be errors.
                let mut desc = cycle.describe();

                // (this will always be non-empty)
                if let Some(obj) = cycle.path_rev().last() {
                    // But in this context,
                    //   this _is_ a problem,
                    //   so make clear why we're pointing this out.
                    // TODO: Include an identifier name,
                    //   once `Cycle` supports it.
                    desc.extend(
                        [
                            obj.help(
                                "the value cannot be computed because its",
                            ),
                            obj.help(
                                "  definition requires first computing itself.",
                            ),
                        ]
                        .into_iter(),
                    );
                }

                desc
            }
            // TODO: This doesn't seem all that helpful.
            //   What are the circumstances under which this can be hit,
            //     and what additional information can we provide?
            UnexpectedOpaqueIdent(name) => vec![name.error(
                "an opaque identifier declaration was not expected here",
            )],
        }
    }
}
