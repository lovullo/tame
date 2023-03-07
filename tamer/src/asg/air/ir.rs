// ASG IR tokens
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

//! AIR token definitions.

use super::super::{ExprOp, FragmentText, IdentKind, Source};
use crate::{
    fmt::{DisplayWrapper, TtQuote},
    parse::util::SPair,
    span::Span,
};

#[cfg(doc)]
use super::super::graph::object::{Expr, Ident, Pkg, Tpl};

/// Create an IR able to be decomposed into types containing subsets of
///   tokens.
///
/// The purpose of this macro is to generate an IR that:
///
///   - Produces a familiar IR type that contains all tokens as variants;
///       and
///   - Is able to translate into narrower types containing subsets of those
///       variants.
///
/// This addresses the lack of type refinement---we
///   cannot create ad-hoc types that are a subset of another type,
///     e.g. an enum type that contains a subset of the variants of another
///     enum.
/// This results in awkward and verbose workarounds when we want the
///   context `match`'s exhaustiveness checking and pattern matching
///   to be delegated to a function application in that match arm;
///     we have to chose between creating separate types to hold that
///     information,
///       or use a single match.
/// The latter is unsuitable for systems created through the composition of
///   smaller subsystems,
///     and so this macro helps to alleviate the pain of the former.
///
/// This macro also removes the boilerplate of implementing
///   [`Display`](std::fmt::Display), [`Token`](crate::parse::Token), and
///   [`Object`](crate::parse::Object) by rolling the information necessary
///   for their definition into the definition of the tokens.
///
/// One of the best ways to understand all of the types involved is to look
///   at the generated documentation for an IR generated using this macro,
///     which walks the reader through the type translations.
///
/// This may be able to be generalized and useful to other IRs in the
///   future.
///
/// Arbitrary Sum Types
/// ===================
/// The above handles decomposing variants into subtypes,
///   but that mapping must be bijective.
/// But sometimes a token may need to be shared among multiple subsets for
///   use as input to parsers.
///
/// This macro also support defining explicit sum types using the `sum enum`
///   keywords,
///     at the tail of the IR definition,
///     to avoid the boilerplate of writing such a type yourself.
/// This is important for encouraging the creation of types that are
///   precisely define the input language of composable parsers.
macro_rules! sum_ir {
    (
        $(#[$attr:meta])*
        $vis:vis enum $ir:ident -> $svis:vis $sumty:ident $irname:literal {
            $(
                $(#[$iattr:meta])*
                enum $subty:ident {
                    $(
                        $(#[$svattr:meta])*
                        $svar:ident ($($svident:ident : $svty:ty),*) => {
                            span: $svspan:expr,
                            display: |$svfmt:ident| $svfmtbody:expr,
                        },
                    )+
                }
            )+
        }

        $(
            $(#[$sumattr:meta])*
            $sumvis:vis sum enum $sumsub:ident = $($sumsubty:ident)|+;
        )*
    ) => {
        // Enum consisting of all variants of all subtypes.
        //
        // This can be used as a convenient input token type,
        //   so that the caller does not have to worry about how we organize
        //   our tokens into subsets.
        $(#[$attr])*
        ///
        /// For more information,
        ///   see the following private subtypes:
        $(
            #[doc=concat!("  - [`", stringify!($subty), "`]")]
        )+
        #[derive(Debug, PartialEq)]
        $vis enum $ir {
            $(
                $(
                    $(#[$svattr])*
                    ///
                    /// _This variant will be translated into
                    #[doc=concat!(
                        "[`", stringify!($subty), "::", stringify!($svar), "`]"
                    )]
                    ///   during translation into
                    #[doc=concat!("[`", stringify!($sumty), "`]._")]
                    $svar($($svty),*),
                )+
            )+
        }

        // Each individual inner type.
        $(
            $(#[$iattr])*
            ///
            /// _This subtype is sourced from the respective variants of
            #[doc=concat!("[`", stringify!($ir), "`]")]
            ///   during translation into
            #[doc=concat!("[`", stringify!($sumty), "`]._")]
            #[allow(clippy::enum_variant_names)]  // intentional consistency
            #[derive(Debug, PartialEq)]
            $svis enum $subty {
                $(
                    $(#[$svattr])*
                    ///
                    /// _This type is sourced from
                    #[doc=concat!(
                        "[`", stringify!($ir), "::", stringify!($svar), "`]"
                    )]
                    ///   during translation from
                    #[doc=concat!("[`", stringify!($ir), "`]")]
                    ///   into
                    #[doc=concat!("[`", stringify!($sumty), "`]._")]
                    $svar($($svty),*),
                )+
            }
        )+

        // Sum type of each inner type.
        //
        // This is intended to be a private type;
        //   narrowing into subsets is an encapsulated implementation detail.
        // ---
        /// Sum type of the various subtypes sourced from
        #[doc=concat!("[`", stringify!($ir), "`].")]
        ///
        /// The public-facing type intended to be used outside of this
        ///   module is
        #[doc=concat!("[`", stringify!($ir), "`].")]
        ///
        /// By matching on this type,
        ///   you are able to work with a subset of the tokens of
        #[doc=concat!("[`", stringify!($ir), "`],")]
        ///     enabling exhaustiveness checks and other type benefits in
        ///     composable subsystems.
        #[allow(clippy::enum_variant_names)]  // intentional consistency
        #[derive(Debug, PartialEq)]
        $svis enum $sumty {
            $(
                $(#[$iattr])*
                $subty($subty),
            )+
        }

        // Narrow from $ir to sum type $sumty.
        impl From<$ir> for $sumty {
            fn from(outer: $ir) -> Self {
                match outer {
                    $( // inner
                        $( // inner variant (ivar)
                            #[allow(unused_variables)]
                            $ir::$svar($($svident),*) => Self::$subty(
                                $subty::$svar($($svident),*)
                            ),
                        )+
                    )+
                }
            }
        }

        // Widen from sum type into $ir.
        //
        // This is important for `PartiallyStitchableParseState` to
        //   allow for lookahead tokens from stitched parsers to be widened.
        impl From<$sumty> for $ir {
            fn from(x: $sumty) -> Self {
                match x {
                    $(
                        #[allow(unused_variables)]
                        $sumty::$subty(x) => x.into(),
                    )+
                }
            }
        }

        // Widen from each inner type to outer.
        //
        // This is important for `PartiallyStitchableParseState` to
        //   allow for lookahead tokens from stitched parsers to be widened.
        $(
            impl From<$subty> for $ir {
                fn from(x: $subty) -> Self {
                    match x {
                        $(
                            #[allow(unused_variables)]
                            $subty::$svar($($svident),*) => Self::$svar($($svident),*),
                        )+
                    }
                }
            }
        )*

        impl std::fmt::Display for $ir {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $( // inner
                        $( // inner variant (ivar)
                            #[allow(unused_variables)]
                            Self::$svar($($svident),*) => {
                                let $svfmt = f;
                                $svfmtbody
                            }
                        )+
                    )+
                }
            }
        }

        impl std::fmt::Display for $sumty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                        #[allow(unused_variables)]
                        $sumty::$subty(x) => std::fmt::Display::fmt(x, f),
                    )+
                }
            }
        }

        $(
            impl std::fmt::Display for $subty {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    match self {
                        $(
                            #[allow(unused_variables)]
                            Self::$svar($($svident),*) => {
                                let $svfmt = f;
                                $svfmtbody
                            }
                        )+
                    }
                }
            }
        )+

        impl crate::parse::Token for $ir {
            fn ir_name() -> &'static str {
                $irname
            }

            fn span(&self) -> crate::span::Span {
                match self {
                    $( // inner
                        $( // inner variant (ivar)
                            #[allow(unused_variables)]
                            Self::$svar($($svident),*) => (*$svspan).into(),
                        )+
                    )+
                }
            }
        }

        impl crate::parse::Token for $sumty {
            fn ir_name() -> &'static str {
                $irname
            }

            fn span(&self) -> crate::span::Span {
                match self {
                    $(
                        $sumty::$subty(x) => x.span(),
                    )+
                }
            }
        }

        $(
            impl crate::parse::Token for $subty {
                fn ir_name() -> &'static str {
                    concat!($irname, " narrowed into ", stringify!($subty))
                }

                fn span(&self) -> crate::span::Span {
                    match self {
                        $(
                            #[allow(unused_variables)]
                            Self::$svar($($svident),*) => (*$svspan).into(),
                        )+
                    }
                }
            }
        )+

        // Only implement for outer $ir,
        //   because the inner enums aren't things that we want yielded by
        //   parsers,
        //     at least at present,
        //     since they are nothing more than implementation details.
        impl crate::parse::Object for $ir {}

        // In addition to the complete sum type $sumty above,
        //   the user may also specify their own sum sum types with explicit
        //   subtypes.
        $(
            $(#[$sumattr])*
            #[derive(Debug, PartialEq)]
            $sumvis enum $sumsub {
                $(
                    $sumsubty($sumsubty),
                )+
            }

            impl crate::parse::Token for $sumsub {
                fn ir_name() -> &'static str {
                    concat!($irname, " narrowed into ", stringify!($sumsub))
                }

                fn span(&self) -> crate::span::Span {
                    match self {
                        $(
                            Self::$sumsubty(x) => x.span(),
                        )+
                    }
                }
            }

            impl std::fmt::Display for $sumsub {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$sumsubty(x) => std::fmt::Display::fmt(x, f),
                        )+
                    }
                }
            }

            $(
                impl From<$sumsubty> for $sumsub {
                    fn from(x: $sumsubty) -> Self {
                        Self::$sumsubty(x)
                    }
                }
            )+

            // Widen from each inner type to outer $ir.
            //
            // This is important for `PartiallyStitchableParseState` to
            //   allow for lookahead tokens from stitched parsers to be
            //   widened.
            impl From<$sumsub> for $ir {
                fn from(sum: $sumsub) -> Self {
                    match sum {
                        $(
                            $sumsub::$sumsubty(x) => x.into(),
                        )+
                    }
                }
            }
        )*
    }
}

sum_ir! {
    /// AIR token.
    ///
    /// These tokens mimic a public API for the ASG,
    ///   and allow parsers to be completely decoupled from the ASG object that
    ///   they will eventually aggregate data into.
    ///
    /// This IR is not intended to perform sophisticated manipulation of the
    ///   ASG---it
    ///     is intended to perform initial aggregation as part of a parsing
    ///     phase,
    ///       populating the ASG with the raw data that that will be
    ///       subsequently analyzed and rewritten.
    ///
    /// Implementation Notes
    /// ====================
    /// [`Air`] is a public token type;
    ///   it serves as input into this subsystem and is expected to be
    ///   yielded by parsers in the lowering pipeline.
    ///
    /// However,
    ///   the token is far too broad of a type for deconstructing the
    ///   subsystem into components.
    /// TAMER makes extensive use of exhaustiveness checking and constrained
    ///   domains to aid in proving system correctness.
    /// To facilitate that,
    ///   [`Air`] decomposes into [`AirSubsets`],
    ///     which is a sum type over the various subtypes listed below,
    ///     defined using [`sum_ir!`].
    /// [`Air`] contains the union of each of those subtypes' variants for
    ///   convenience,
    ///     allowing the construction of subtypes to be encapsulated and
    ///     able to vary independently from the public type.
    pub enum Air -> pub AirSubsets "AIR" {
        /// Tokens to be used by incomplete features that ought to type
        ///   check and be ignored rather than panic.
        ///
        /// This allows for a complete parse of what is known.
        enum AirTodo {
            /// Placeholder token for objects that do not yet have a proper place on
            ///   the ASG.
            Todo(span: Span) => {
                span: span,
                display: |f| write!(f, "TODO"),
            },
        }

        /// Subset of [`Air`] tokens used to define [`Pkg`]s.
        enum AirPkg {
            /// Begin a new package of identifiers.
            ///
            /// Packages are responsible for bundling together identifiers
            ///   representing subsystems that can be composed with other packages.
            ///
            /// A source language may place limits on the objects that may appear
            ///   within a given package,
            ///     but we have no such restriction.
            ///
            /// TODO: The package needs a name,
            ///   and we'll need to determine how to best represent that relative to
            ///   the project root and be considerate of symlinks.
            PkgOpen(span: Span) => {
                span: span,
                display: |f| write!(f, "open package"),
            },

            /// Complete processing of the current package.
            PkgClose(span: Span) => {
                span: span,
                display: |f| write!(f, "close package"),
            },
        }

        /// Subset of [`Air`] tokens used to define [`Expr`]s.
        enum AirExpr {
            /// Create a new [`Expr`] on the graph and place it atop of the
            ///   expression stack.
            ///
            /// If there was previously an expression ρ atop of the stack before
            ///   this operation,
            ///     a reference to this new expression will be automatically added
            ///     to ρ,
            ///       treating it as a child expression.
            /// Otherwise,
            ///   the expression will be dangling unless bound to an identifier,
            ///     which will produce an error.
            ///
            /// All expressions have an associated [`ExprOp`] that determines how
            ///   the expression will be evaluated.
            /// An expression is associated with a source location,
            ///   but is anonymous unless assigned an identifier using
            ///   [`Air::BindIdent`].
            ///
            /// Expressions are composed of references to other expressions.
            ExprOpen(op: ExprOp, span: Span) => {
                span: span,
                display: |f| write!(f, "open {op} expression"),
            },

            /// Complete the expression atop of the expression stack and pop it from
            ///   the stack.
            ExprClose(span: Span) => {
                span: span,
                display: |f| write!(f, "close expression"),
            },
        }

        /// Subset of [`Air`] tokens dealing with the binding of identifiers
        ///   to objects.
        enum AirBind {
            /// Assign an identifier to the active object.
            ///
            /// The "active" object depends on the current parsing state.
            BindIdent(id: SPair) => {
                span: id,
                display: |f| write!(
                    f,
                    "identify active object as {}",
                    TtQuote::wrap(id),
                ),
            },

            /// Reference another object identified by the given [`SPair`].
            ///
            /// Objects can be referenced before they are declared or defined,
            ///   so the provided identifier need not yet exist.
            /// However,
            ///   the identifier must eventually be bound to an object of
            ///   the appropriate type depending on context.
            RefIdent(id: SPair) => {
                span: id,
                display: |f| write!(
                    f,
                    "reference to identifier {}",
                    TtQuote::wrap(id),
                ),
            },
        }

        /// Subset of [`Air`] tokens for declaring and manipulating
        ///   [`Ident`]s.
        ///
        /// These tokens are from the early days of `tameld` when the system
        ///   was still largely a proof-of-concept;
        ///     they do not marry well with TAMER's present design are
        ///     likely to change in the future.
        enum AirIdent {
            /// Declare a resolved identifier.
            IdentDecl(name: SPair, kind: IdentKind, src: Source) => {
                span: name,
                display: |f| write!(
                    f,
                    "declaration of identifier {}",
                    TtQuote::wrap(name),
                ),
            },

            /// Declare an external identifier that must be resolved before linking.
            IdentExternDecl(name: SPair, kind: IdentKind, src: Source) => {
                span: name,
                display: |f| write!(
                    f,
                    "declaration of external identifier {}",
                    TtQuote::wrap(name),
                ),
            },

            /// Declare that an identifier depends on another for its definition.
            ///
            /// The first identifier will depend on the second
            ///   (`0 -> 1`).
            /// The spans associated with each [`SPair`] will be used
            ///   if the respective identifier has not yet been defined.
            IdentDep(name: SPair, dep: SPair) => {
                span: name,
                display: |f| write!(
                    f,
                    // TODO: Use list wrapper
                    "declaration of identifier dependency `{name} -> {dep}`",
                ),
            },

            /// Associate a code fragment with an identifier.
            ///
            /// A fragment does not have an associated span because it is
            ///   conceptually associated with all the spans from which it is
            ///   derived;
            ///     the format of the object file will change in the future to
            ///     retain this information.
            IdentFragment(name: SPair, text: FragmentText) => {
                span: name,
                display: |f| write!(
                    f,
                    "identifier {}` fragment text",
                    TtQuote::wrap(name),
                ),
            },

            /// Root an identifier at the request of some entity at the associated
            ///   span of the [`SPair`].
            ///
            /// Rooting is caused by _something_,
            ///   and the span is intended to aid in tracking down why rooting
            ///   occurred.
            IdentRoot(name: SPair) => {
                span: name,
                display: |f| write!(
                    f,
                    "rooting of identifier {}",
                    TtQuote::wrap(name),
                ),
            },
        }

        /// Subset of [`Air`] tokens for defining [`Tpl`]s.
        enum AirTpl {
            /// Create a new [`Tpl`] on the graph and switch to template parsing.
            ///
            /// Until [`Self::TplClose`] is found,
            ///   all parsed objects will be parented to the [`Tpl`] rather than the
            ///   parent [`Pkg`].
            /// Template parsing also recognizes additional nodes that can appear
            ///   only in this mode.
            ///
            /// The active expression stack will be restored after template
            ///   parsing has concluded.
            TplOpen(span: Span) => {
                span: span,
                display: |f| write!(f, "open template"),
            },

            /// Close the active [`Tpl`] and exit template parsing.
            ///
            /// The expression stack will be restored to its prior state.
            TplClose(span: Span) => {
                span: span,
                display: |f| write!(f, "close template"),
            },
        }
    }

    /// Expressions that are able to be bound to identifiers.
    ///
    /// This is the primary token set when parsing packages,
    ///   since most everything in TAMER is an expression.
    pub sum enum AirBindableExpr = AirExpr | AirBind;
}