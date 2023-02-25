// ASG identifiers
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

//! Identifiers (a type of [object](super)).

use super::{
    super::{Asg, AsgError, ObjectIndex, ObjectKind},
    Expr, Object, ObjectRel, ObjectRelFrom, ObjectRelTo, ObjectRelTy,
    ObjectRelatable, Pkg,
};
use crate::{
    diagnose::{Annotate, Diagnostic},
    diagnostic_todo,
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    num::{Dim, Dtype},
    parse::{util::SPair, Token},
    span::Span,
    sym::{st, GlobalSymbolResolve, SymbolId},
};
use std::fmt::Display;

use Ident::*;

pub type TransitionResult<T> = Result<T, (T, TransitionError)>;

/// An identifier for some object on the [`Asg`].
///
/// An identifier can be either _opaque_ declaration,
///   meaning that it stands in _place_ of a definition,
///   or _transparent_,
///     meaning that references to the identifier should "see through" it
///     and directly reference the object to which it is bound.
///
/// Invariants
/// ==========
/// The [`Ident`] variants represent object states:
///
/// ```text
///      ,--> ((Transparent))
///     /
/// (Missing) -> (Extern) -> ((Opaque)) -> ((IdentFragment)).
///     \                        ^                /
///      \                      / \              /
///       `--------------------`   `------------'
/// ```
///
/// The following invariants must hold with respect to [`Asg`]:
///   1. A [`Transparent`] identifier must have _one_ edge to the object
///        that it describes
///        (upheld by [`ObjectIndex::<Ident>::bind_definition`]).
///   2. All other identifiers must have _zero_ outgoing edges,
///        since they describe nothing available on the graph.
///      Since edges can only be added by
///        [`ObjectIndex::<Ident>::bind_definition`],
///          and since an [`Ident`] cannot transition away from
///            [`Transparent`],
///          this invariant is upheld.
#[derive(Debug, PartialEq, Clone)]
pub enum Ident {
    /// An identifier is expected to be declared or defined but is not yet
    ///   available.
    ///
    /// This variant contains the symbol representing the name of the
    ///   expected identifier.
    /// This is important,
    ///   since identifiers in TAME may be referenced before they are
    ///   defined.
    ///
    /// The [`Span`] associated with this missing identifier represents one
    ///   of the references to the identifier,
    ///     ensuring that there is always some useful information to help
    ///     debug missing identifiers.
    /// A reference to this missing identifier ought to have its own span so
    ///   that diagnostic messages can direct the user to other instances
    ///   where unknown identifiers were referenced.
    Missing(SPair),

    /// A resolved opaque identifier without a corresponding definition.
    ///
    /// This represents an identifier that has been declared with certain
    ///   type information.
    /// An identifier has a single canonical location represented by the
    ///   [`SPair`]'s [`Span`].
    ///
    /// Opaque identifiers are used when a definition is either not known,
    ///   or when the definition is not important.
    /// These identifiers take the _place_ of an object definition on the
    ///   graph,
    ///     much like a declaration in a header file.
    Opaque(SPair, IdentKind, Source),

    /// An identifier that has not yet been declared or defined.
    ///
    /// Externs are upgraded to [`Ident::Opaque`] once an identifier of
    ///   the same name is loaded.
    /// It is an error if the loaded identifier does not have a compatible
    ///   [`IdentKind`].
    ///
    /// The [`Span`] and [`Source`] of an extern represents the location of
    ///   the extern declaration.
    /// Once resolved, however,
    ///   both will instead represent the location of the concrete
    ///   identifier.
    Extern(SPair, IdentKind, Source),

    /// Opaque identifier with associated text.
    ///
    /// Code fragments are portions of the target language associated with
    ///   an identifier.
    /// They are produced by the compiler and it is the job of the
    ///   [linker][crate::ld] to put them into the correct order for the
    ///   final executable.
    IdentFragment(SPair, IdentKind, Source, FragmentText),

    /// A resolved transparent identifier that has a corresponding
    ///   definition on the graph.
    ///
    /// An identifier is transparent when the system is expected to use the
    ///   identifier only as a key for locating its associated object,
    ///     "seeing through" the identifier to reference directly the
    ///     underlying [`Object`](super::Object).
    /// This is in contrast to [`Ident::Opaque`],
    ///   which is only _declared_,
    ///   and serves _in place of_ its corresponding definition.
    ///
    /// Consequently,
    ///   this representation of an identifier is very light,
    ///   since dependents are expected to create edges to the
    ///   [`Object`](super::Object) it references rather than the identifier
    ///   itself;
    ///     this is safe since identifiers in TAME are immutable.
    Transparent(SPair),
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Missing(id) => {
                write!(f, "missing identifier {}", TtQuote::wrap(id))
            }
            Opaque(id, kind, _) => {
                write!(f, "{kind} identifier {}", TtQuote::wrap(id))
            }
            Extern(id, kind, _) => {
                write!(f, "{kind} extern identifier {}", TtQuote::wrap(id))
            }
            IdentFragment(id, kind, _, _) => write!(
                f,
                "{kind} identifier {} with compiled fragment",
                TtQuote::wrap(id)
            ),
            Transparent(id) => {
                write!(f, "transparent identifier {}", TtQuote::wrap(id))
            }
        }
    }
}

impl Ident {
    /// Identifier name.
    pub fn name(&self) -> SPair {
        match self {
            Missing(name)
            | Opaque(name, ..)
            | Extern(name, ..)
            | IdentFragment(name, ..)
            | Transparent(name) => *name,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Missing(name)
            | Opaque(name, ..)
            | Extern(name, ..)
            | IdentFragment(name, ..)
            | Transparent(name) => name.span(),
        }
    }

    /// Identifier [`IdentKind`].
    ///
    /// If the object does not have a kind
    ///   (as is the case with [`Ident::Missing`]),
    ///     [`None`] is returned.
    pub fn kind(&self) -> Option<&IdentKind> {
        match self {
            Missing(_) | Transparent(_) => None,

            Opaque(_, kind, _)
            | Extern(_, kind, _)
            | IdentFragment(_, kind, _, _) => Some(kind),
        }
    }

    /// Identifier [`Source`].
    ///
    /// If the object does not have source information
    ///   (as is the case with [`Ident::Extern`]),
    ///     [`None`] is returned.
    pub fn src(&self) -> Option<&Source> {
        match self {
            Missing(_) | Extern(_, _, _) | Transparent(_) => None,

            Opaque(_, _, src) | IdentFragment(_, _, src, _) => Some(src),
        }
    }

    /// Identifier [`FragmentText`].
    ///
    /// If the object does not have an associated code fragment,
    ///   [`None`] is returned.
    pub fn fragment(&self) -> Option<FragmentText> {
        match self {
            Missing(_) | Opaque(_, _, _) | Extern(_, _, _) | Transparent(_) => {
                None
            }

            IdentFragment(_, _, _, text) => Some(*text),
        }
    }

    /// Produce an object representing a missing identifier.
    ///
    /// This is the base state for all identifiers.
    /// The [`Span`] associated with the pair should be the span of whatever
    ///   reference triggered this declaration;
    ///     it will be later replaced with the span of the identifier once
    ///     its definition is found.
    pub fn declare(ident: SPair) -> Self {
        Missing(ident)
    }

    /// Attempt to redeclare an identifier with additional information.
    ///
    /// If an existing identifier is an [`Ident::Extern`],
    ///   then the declaration will be compared just the same,
    ///     but the identifier will be converted from an extern into an
    ///     identifier.
    /// When this happens,
    ///   the extern is said to be _resolved_.
    ///
    /// If a virtual identifier of type [`Ident::IdentFragment`] is
    ///   overridden,
    ///     then its fragment is cleared
    ///     (it returns to a [`Ident::Opaque`])
    ///     to make way for the fragment of the override.
    ///
    /// Overrides will always have their virtual flag cleared,
    ///   even if set.
    /// The compiler will hopefully have done this for us,
    ///   since the user may be confused with subsequent
    ///   [`TransitionError::NonVirtualOverride`] errors if they try to
    ///   override an override.
    ///
    /// The kind of identifier cannot change,
    ///   but the argument is provided here for convenience so that the
    ///   caller does not need to perform such a check itself.
    ///
    /// If no extern or virtual override is possible,
    ///   an identifier cannot be redeclared and this operation will fail.
    pub fn resolve(
        self,
        span: Span,
        kind: IdentKind,
        mut src: Source,
    ) -> TransitionResult<Ident> {
        match self {
            Opaque(name, ref orig_kind, ref orig_src)
            | IdentFragment(name, ref orig_kind, ref orig_src, _)
                if src.override_ =>
            {
                if !orig_src.virtual_ {
                    let err = TransitionError::NonVirtualOverride(name, span);

                    return Err((self, err));
                }

                if orig_kind != &kind {
                    let err = TransitionError::VirtualOverrideKind(
                        name,
                        orig_kind.clone(),
                        (kind, span),
                    );

                    return Err((self, err));
                }

                // Ensure that virtual flags are cleared to prohibit
                //   override-overrides.
                // The compiler should do this;
                //   this is just an extra layer of defense.
                src.virtual_ = false;

                // Note that this has the effect of clearing fragments if we
                //   originally were in state `Ident::IdentFragment`.
                Ok(Opaque(name.overwrite(span), kind, src))
            }

            // If we encountered the override _first_,
            //   flip the context by declaring a new identifier and trying
            //   to override that.
            Opaque(name, orig_kind, orig_src) if orig_src.override_ => {
                Self::declare(name)
                    .resolve(name.span(), kind, src)?
                    .resolve(span, orig_kind, orig_src)
            }

            // Same as above,
            //   but for fragments,
            //   we want to keep the _original override_ fragment.
            IdentFragment(name, orig_kind, orig_src, orig_text)
                if orig_src.override_ =>
            {
                Self::declare(name)
                    .resolve(name.span(), kind, src)?
                    .resolve(span, orig_kind, orig_src)?
                    .set_fragment(orig_text)
            }

            Extern(name, ref orig_kind, _) => {
                if orig_kind != &kind {
                    let err = TransitionError::ExternResolution(
                        name,
                        orig_kind.clone(),
                        (kind, span),
                    );

                    return Err((self, err));
                }

                Ok(Opaque(name.overwrite(span), kind, src))
            }

            // These represent the prologue and epilogue of maps.
            IdentFragment(
                _,
                IdentKind::MapHead
                | IdentKind::MapTail
                | IdentKind::RetMapHead
                | IdentKind::RetMapTail,
                ..,
            ) => Ok(self),

            Missing(name) => Ok(Opaque(name.overwrite(span), kind, src)),

            // TODO: Remove guards and catch-all for exhaustiveness check.
            _ => {
                let err = TransitionError::Redeclare(self.name(), span);

                Err((self, err))
            }
        }
    }

    /// Assertion to return self if identifier is resolved,
    ///   otherwise failing with [`UnresolvedError`].
    ///
    /// This simplifies working with identifiers without having to match on
    ///   specific variants,
    ///     and will continue to work if new variants are added in the
    ///     future that are considered to be unresolved.
    ///
    /// Since this does not cause a state transition and is useful in
    ///   contexts where ownership over the identifier is not possible,
    ///     this accepts and returns a reference to the identifier.
    ///
    /// At present,
    ///   both [`Ident::Missing`] and [`Ident::Extern`] are
    ///   considered to be unresolved.
    pub fn resolved(&self) -> Result<&Ident, UnresolvedError> {
        match self {
            Missing(name) => Err(UnresolvedError::Missing(*name)),

            Extern(name, ref kind, _) => {
                Err(UnresolvedError::Extern(*name, kind.clone()))
            }

            Opaque(..) | IdentFragment(..) | Transparent(..) => Ok(self),
        }
    }

    /// Resolve identifier against an extern declaration or produce an
    ///   extern.
    ///
    /// If the existing identifier has an assigned [`IdentKind`],
    ///   then it will be compared for equality against the given `kind`.
    /// If it matches,
    ///   then the current identifier will be returned as-is.
    /// This represents an extern resolution that occurs when a concrete
    ///   identifier is located before an extern that requires it,
    ///     or my represent a duplicate (but compatible) extern
    ///     declaration.
    ///
    /// If no kind is assigned (such as [`Ident::Missing`]),
    ///   then a new extern is produced.
    /// See for example [`Ident::Extern`].
    pub fn extern_(
        self,
        span: Span,
        kind: IdentKind,
        src: Source,
    ) -> TransitionResult<Ident> {
        match self.kind() {
            None => Ok(Extern(self.name().overwrite(span), kind, src)),
            Some(cur_kind) => {
                if cur_kind != &kind {
                    let err = TransitionError::ExternResolution(
                        self.name(),
                        cur_kind.clone(),
                        (kind, span),
                    );

                    return Err((self, err));
                }

                // Resolved successfully, so keep what we already have.
                Ok(self)
            }
        }
    }

    /// Attach a code fragment (compiled text) to an identifier.
    ///
    /// This will fail if an identifier already has a fragment,
    ///   since only the owner of the identifier should be producing
    ///   compiled code.
    /// Note, however, that an identifier's fragment may be cleared under
    ///   certain circumstances (such as symbol overrides),
    ///     making way for a new fragment to be set.
    pub fn set_fragment(self, text: FragmentText) -> TransitionResult<Ident> {
        match self {
            Opaque(sym, kind, src) => Ok(IdentFragment(sym, kind, src, text)),

            // If we get to this point in a properly functioning program (at
            // least as of the time of writing), then we have encountered a
            // fragment for a virtual identifier _after_ we have already
            // encountered the fragment for its _override_.  We therefore
            // want to keep the override.
            //
            // If this is not permissable, then we should have already
            // prevented the `resolve` transition before this fragment was
            // encountered.
            IdentFragment(_, _, ref src, ..) if src.override_ => Ok(self),

            // These represent the prologue and epilogue of maps.
            IdentFragment(
                _,
                IdentKind::MapHead
                | IdentKind::MapTail
                | IdentKind::RetMapHead
                | IdentKind::RetMapTail,
                ..,
            ) => Ok(self),

            _ => {
                let name = self.name();
                Err((self, TransitionError::BadFragmentDest(name)))
            }
        }
    }
}

/// An error attempting to transition from one [`Ident`] state to
///   another.
#[derive(Clone, Debug, PartialEq)]
pub enum TransitionError {
    /// Attempted to redeclare a concrete, non-virtual identifier without an
    ///   override.
    Redeclare(SPair, Span),

    /// Extern resolution failure.
    ///
    /// An extern could not be resolved because the provided identifier had
    ///   a type that is incompatible with the extern definition.
    ///
    // TODO: Need more granular spans for `IdentKind`.
    ExternResolution(SPair, IdentKind, (IdentKind, Span)),

    /// Attempt to override a non-virtual identifier.
    NonVirtualOverride(SPair, Span),

    /// Overriding a virtual identifier failed due to an incompatible
    ///   [`IdentKind`].
    // TODO: More granular spans for kind.
    VirtualOverrideKind(SPair, IdentKind, (IdentKind, Span)),

    /// The provided identifier is not in a state that is permitted to
    ///   receive a fragment.
    ///
    /// See [`Ident::set_fragment`].
    BadFragmentDest(SPair),
}

impl std::fmt::Display for TransitionError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TransitionError::*;

        match self {
            Redeclare(name, _) => write!(
                fmt,
                "cannot redeclare identifier {}",
                TtQuote::wrap(name),
            ),

            ExternResolution(name, expected, (given, _)) => write!(
                fmt,
                "extern {} of type {} is incompatible with type {}",
                TtQuote::wrap(name),
                TtQuote::wrap(expected),
                TtQuote::wrap(given),
            ),

            NonVirtualOverride(name, _) => write!(
                fmt,
                "non-virtual identifier {} cannot be overridden",
                TtQuote::wrap(name),
            ),

            VirtualOverrideKind(name, existing, (given, _)) => write!(
                fmt,
                "virtual identifier {} of type {} cannot be overridden with type {}",
                TtQuote::wrap(name),
                TtQuote::wrap(existing),
                TtQuote::wrap(given),
            ),

            BadFragmentDest(name) => {
                write!(fmt, "bad fragment destination: {}", TtQuote::wrap(name))
            }
        }
    }
}

impl std::error::Error for TransitionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Diagnostic for TransitionError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use TransitionError::*;

        match self {
            Redeclare(name, sdup) => vec![
                name.note("first declaration was found here"),
                sdup.error(format!("cannot redeclare {}", TtQuote::wrap(name))),
                sdup.help("identifiers in TAME are immutable and can"),
                sdup.help("  only be associated with one definition."),
            ],

            ExternResolution(name, expected, (given, sresolve)) => vec![
                name.note(format!(
                    "extern {} declared here with type {}",
                    TtQuote::wrap(name),
                    TtQuote::wrap(expected),
                )),
                sresolve.error(format!(
                    "attempted to resolve extern {} with incompatible type {}",
                    TtQuote::wrap(name),
                    TtQuote::wrap(given),
                )),
            ],

            // TODO: Does this make a lie out of `Redeclare`'s help?
            NonVirtualOverride(name, soverride) => vec![
                name.note("attempting to override this non-virtual identifier"),
                soverride.error(format!(
                    "cannot override {} because the first declaration \
                        was not virtual",
                    TtQuote::wrap(name),
                )),
                // TODO: let's see what type of help text will be useful
                //   once we've decided what is to be done with virtual
                //   identifiers generally
            ],

            VirtualOverrideKind(name, orig, (given, soverride)) => vec![
                name.note(format!(
                    "attempting to override this identifier of type {}",
                    TtQuote::wrap(orig),
                )),
                soverride.error(format!(
                    "type of this override is {}, but {} was expected",
                    TtQuote::wrap(given),
                    TtQuote::wrap(orig),
                )),
            ],

            BadFragmentDest(name) => vec![
                name.internal_error(
                    "identifier {} cannot be assigned a text fragment",
                ),
                name.help(
                    "the term 'text fragment' refers to compiled code from an",
                ),
                name.help("  object file; this error should never occur."),
            ],
        }
    }
}

/// Resolved identifier was expected.
#[derive(Clone, Debug, PartialEq)]
pub enum UnresolvedError {
    /// Expected identifier is missing and nothing about it is known.
    ///
    /// The span represents the first reference to this identifier that
    ///   caused it to be added to the graph;
    ///     it is not necessarily the _only_ reference.
    /// When reporting errors based on references to unknown identifiers,
    ///   keep this in mind to avoid duplicates.
    Missing(SPair),

    /// Expected identifier has not yet been resolved with a concrete
    ///   definition.
    Extern(SPair, IdentKind),
}

impl std::fmt::Display for UnresolvedError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use UnresolvedError::*;

        match self {
            Missing(name) => {
                write!(fmt, "unknown identifier {}", TtQuote::wrap(name))
            }

            Extern(name, kind) => write!(
                fmt,
                "unresolved extern {} of type {}",
                TtQuote::wrap(name),
                TtQuote::wrap(kind),
            ),
        }
    }
}

impl std::error::Error for UnresolvedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Diagnostic for UnresolvedError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use UnresolvedError::*;

        match self {
            // TODO: Do we want a single span, or should errors also be
            // thrown for every reference to missing?
            Missing(name) => vec![name.error(format!(
                "identifier {} has not been defined",
                TtQuote::wrap(name),
            ))],

            Extern(name, _kind) => vec![
                name.error(format!(
                    "no imported package provided a \
                        compatible definition for {}",
                    TtQuote::wrap(name),
                )),
                name.help(
                    "an extern declares an identifier so that it may be used,"
                ),
                name.help(
                    "  but with the expectation that some imported package will"
                ),
                name.help(
                    "  later provide a concrete definition for it."
                )
            ],
        }
    }
}

/// Compiled fragment for identifier.
///
/// This represents the text associated with an identifier.
pub type FragmentText = SymbolId;

/// Types of identifiers.
///
/// Here, the term _calculation_ refers to a composable expression that
///   produces a numeric result.
///
/// These are derived from [`SymType`][crate::obj::xmlo::SymType]
///   and will be generalized in the future.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IdentKind {
    /// Classification generator.
    ///
    /// This has the same number of dimensions as its highest-dimension
    ///   predicate.
    /// Every [`Class`][IdentKind::Class] has an associated generator.
    Cgen(Dim),

    /// Boolean classification.
    ///
    /// This is an artifact of an ancient system.
    /// The dimensions here refers to the dimensions of the associated
    ///   [`Cgen`][IdentKind::Cgen].
    Class(Dim),

    /// Constant value.
    Const(Dim, Dtype),

    /// Re-usable encapsulated expression.
    ///
    /// Functions are nothing more than expressions that can be re-used with
    ///   dynamic values at runtime.
    /// See also [`Lparam`][IdentKind::Lparam].
    Func(Dim, Dtype),

    /// Generating calculation.
    ///
    /// Generators are associated with iterative expressions,
    ///   such as sums and products.
    /// They always have a parent [`Rate`][IdentKind::Rate].
    Gen(Dim, Dtype),

    /// Local (non-global) parameter.
    ///
    /// Local parameters are lexically scoped to their parent expression:
    ///   - [`Func`][IdentKind::Func], where there exists one per defined
    ///     function parameter; and
    ///   - `let` expression bindings.
    ///
    /// This is not to be confused with the global
    ///   [`Param`][IdentKind::Param].
    Lparam(Dim, Dtype),

    /// Global parameter.
    ///
    /// These parameters serve as inputs to the system.
    /// Input values are bound using [`Map`][IdentKind::Map].
    Param(Dim, Dtype),

    /// Scalar result of a named calculation.
    ///
    /// The verb "rate" is historical,
    ///   since TAME was developed for insurance rating systems.
    /// This represents a named expression that yields a scalar value.
    ///
    /// This serves as a parent to [`Gen`][IdentKind::Gen].
    Rate(Dtype),

    /// Template definition.
    ///
    /// A template is used only at expansion-time and,
    ///   unlike most other things in the system,
    ///   have no runtime value.
    Tpl,

    /// User-defined data type.
    ///
    /// The only types typically defined are enums and unions of enums.
    /// The type itself has no runtime value,
    ///   but each of the enum variants have an associated value of type
    ///   [`Dtype`].
    Type(Dtype),

    /// Input map head (meta identifier generated by compiler for each input
    ///   map).
    MapHead,

    /// Input field→param mapping.
    ///
    /// These may only map to [`Param`][IdentKind::Param].
    /// The source data is arbitrary and provided at runtime.
    Map,

    /// Input map tail (meta symbol generated by compiler for each input
    ///   map).
    MapTail,

    /// Return map head (meta symbol generated by compiler for each return
    ///   map).
    RetMapHead,

    /// Return param→field mapping.
    ///
    /// Return mappings export data to calling systems.
    /// They can map back any globally defined numeric expression.
    RetMap,

    /// Return map tail (meta symbol generated by compiler for each return
    ///   map).
    RetMapTail,

    /// Arbitrary metadata.
    ///
    /// This permits the definition of static key/value data that is
    ///   compiled into the final executable.
    Meta,

    /// Rating worksheet (generated by compiler for worksheet packages).
    ///
    /// The worksheet exposes intermediate calculation values in a much more
    ///   concise form than that of the Summary Page.
    Worksheet,
}

impl IdentKind {
    pub fn as_sym(&self) -> SymbolId {
        match self {
            Self::Cgen(..) => st::L_CGEN.as_sym(),
            Self::Class(..) => st::L_CLASS.as_sym(),
            Self::Const(..) => st::L_CONST.as_sym(),
            Self::Func(..) => st::L_FUNC.as_sym(),
            Self::Gen(..) => st::L_GEN.as_sym(),
            Self::Lparam(..) => st::L_LPARAM.as_sym(),
            Self::Param(..) => st::L_PARAM.as_sym(),
            Self::Rate(..) => st::L_RATE.as_sym(),
            Self::Tpl => st::L_TPL.as_sym(),
            Self::Type(..) => st::L_TYPE.as_sym(),
            Self::MapHead => st::L_MAP_HEAD.as_sym(),
            Self::Map => st::L_MAP.as_sym(),
            Self::MapTail => st::L_MAP_TAIL.as_sym(),
            Self::RetMapHead => st::L_RETMAP_HEAD.as_sym(),
            Self::RetMap => st::L_RETMAP.as_sym(),
            Self::RetMapTail => st::L_RETMAP_TAIL.as_sym(),
            Self::Meta => st::L_META.as_sym(),
            Self::Worksheet => st::L_WORKSHEET.as_sym(),
        }
    }

    /// Whether this identifier should be automatically added as a root when
    ///   declared.
    pub fn is_auto_root(&self) -> bool {
        matches!(
            self,
            Self::Meta | Self::Map | Self::RetMap | Self::Worksheet
        )
    }
}

impl std::fmt::Display for IdentKind {
    /// Format identifier type for display to the user.
    ///
    /// TODO: We have not yet finalized how we will represent types in the
    ///   new type system,
    ///     so for now this just uses a syntax similar to Rust.
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name = self.as_sym().lookup_str();

        match self {
            Self::Cgen(dim) => {
                write!(fmt, "{}[{}; {}]", name, Dtype::Boolean, dim)
            }
            Self::Class(dim) => {
                write!(fmt, "{}[{}; {}]", name, Dtype::Boolean, dim)
            }
            Self::Const(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Func(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Gen(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Lparam(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Param(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Rate(dtype) => write!(fmt, "{}[{}; 0]", name, dtype),
            Self::Type(dtype) => write!(fmt, "{}[{}]", name, dtype),
            _ => write!(fmt, "{}", name),
        }
    }
}

/// Metadata about the source of an object.
///
/// This contains information from the symbol table that does not belong on
///   [`IdentKind`],
///     since that stores _type_ information.
///
/// TODO: This does not currently store byte offsets within the source file
///   since the original XSLT-based compiler did not have that capability;
///     this will provide that information in the future.
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Source {
    /// Name of package containing reference to this object.
    pub pkg_name: Option<SymbolId>,

    /// Relative path to the source of this object,
    ///   if not present in the current package.
    pub src: Option<SymbolId>,

    /// The identifier from which this one is derived.
    ///
    /// See [`IdentKind`] for more information on parents.
    /// For example,
    ///   a [`IdentKind::Cgen`] always has a parent [`IdentKind::Class`].
    pub parent: Option<SymbolId>,

    /// Child identifier associated with this identifier.
    ///
    /// For [`IdentKind::Class`],
    ///   this represents an associated [`IdentKind::Cgen`].
    pub yields: Option<SymbolId>,

    /// User-friendly identifier description.
    ///
    /// This is used primarily by [`IdentKind::Class`] and
    ///   [`IdentKind::Gen`].
    pub desc: Option<SymbolId>,

    /// Whether this identifier was generated by the compiler.
    ///
    /// A generated identifier is representative of an internal
    ///   implementation detail that should remain encapsulated from the
    ///   user and is subject to change over time.
    ///
    /// Identifiers created by templates are not considered to be generated.
    pub generated: bool,

    /// Related identifiers.
    ///
    /// These data represent a kluge created to add additional symbol
    /// information in two different contexts:
    ///
    ///  - [`IdentKind::Map`] includes the name of the source field; and
    ///  - [`IdentKind::Func`] lists params in order (so that the compiler
    ///    knows application order).
    ///
    /// TODO: We have `parent`, `yields`, and `from`.
    ///   We should begin to consolodate.
    pub from: Option<SymbolId>,

    /// Whether identifier is virtual (can be overridden).
    ///
    /// This feature adds complexity and will ideally be removed in the
    ///   future.
    ///
    /// See also [`override`][Source::override_].
    pub virtual_: bool,

    /// Whether identifier overrides a virtual identifier.
    ///
    /// This feature adds complexity and will ideally be removed in the
    ///   future.
    ///
    /// See also [`virtual_`][Source::virtual_].
    pub override_: bool,
}

/// Subset of [`ObjectKind`]s that are valid targets for edges from
///   [`Ident`].
///
/// See [`ObjectRel`] for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum IdentRel {
    Ident(ObjectIndex<Ident>),
    Expr(ObjectIndex<Expr>),
}

impl ObjectRel<Ident> for IdentRel {
    fn narrow<OB: ObjectRelFrom<Ident> + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>> {
        match self {
            Self::Ident(oi) => oi.filter_rel(),
            Self::Expr(oi) => oi.filter_rel(),
        }
    }

    /// Whether this edge is a cross edge to another tree.
    ///
    /// Identifiers are either transparent
    ///   (bound to a definition)
    ///   or opaque.
    /// If transparent,
    ///   then the identifier represents a definition,
    ///   and is therefore a root to that definition.
    ///
    /// Opaque identifiers at the time of writing are used by the linker
    ///   which does not reason about cross edges
    ///     (again at the time of writing).
    /// Consequently,
    ///   this will always return [`false`].
    fn is_cross_edge(&self) -> bool {
        false
    }
}

impl ObjectRelatable for Ident {
    type Rel = IdentRel;

    fn rel_ty() -> ObjectRelTy {
        ObjectRelTy::Ident
    }

    fn new_rel_dyn(
        ty: ObjectRelTy,
        oi: ObjectIndex<Object>,
    ) -> Option<IdentRel> {
        match ty {
            ObjectRelTy::Root => None,
            ObjectRelTy::Pkg => None,
            ObjectRelTy::Ident => Some(IdentRel::Ident(oi.must_narrow_into())),
            ObjectRelTy::Expr => Some(IdentRel::Expr(oi.must_narrow_into())),
            ObjectRelTy::Tpl => None,
        }
    }
}

impl From<ObjectIndex<Ident>> for IdentRel {
    fn from(value: ObjectIndex<Ident>) -> Self {
        Self::Ident(value)
    }
}

impl From<ObjectIndex<Expr>> for IdentRel {
    fn from(value: ObjectIndex<Expr>) -> Self {
        Self::Expr(value)
    }
}

impl ObjectIndex<Ident> {
    /// Bind an identifier to a `definition`,
    ///   making it [`Transparent`].
    ///
    /// If an identifier is successfully bound,
    ///   then an edge will be added to `definition`.
    /// An edge will _not_ be added if there is an error in this operation.
    ///
    /// If an identifier is already [`Transparent`],
    ///   then it is already defined and this operation will result in an
    ///   [`AsgError::IdentRedefine`] error.
    pub fn bind_definition<O: ObjectKind>(
        self,
        asg: &mut Asg,
        id: SPair,
        definition: ObjectIndex<O>,
    ) -> Result<ObjectIndex<Ident>, AsgError>
    where
        Ident: ObjectRelTo<O>,
    {
        let my_span = self.into();

        // TODO: Should we move all incoming edges to `definition`?
        //   This will complicate re-outputting source XML,
        //     but may simplify other aspects of the system.
        //   Perhaps wait until this is needed.
        self.try_map_obj(asg, |ident| match ident {
            Transparent(id) => {
                Err((ident, AsgError::IdentRedefine(id, my_span)))
            }

            Opaque(id, ..) | IdentFragment(id, ..) | Extern(id, ..) => {
                diagnostic_todo!(
                    vec![
                        id.note("must resolve definition against declaration"),
                        my_span.error(
                            "attempting to provide a definition for prior \
                                declaration"
                        ),
                        my_span.help(
                            "this identifier was previously declared and so \
                                its definition needs to be checked for \
                                compatibility with that declaration"
                        )
                    ],
                    "resolve opaque declaration {} to definition",
                    TtQuote::wrap(id),
                )
            }

            // We are okay to proceed to add an edge to the `definition`.
            // Discard the original span
            //   (which is the location of the first reference _to_ this
            //     identifier before it was defined),
            //   and use the newly provided `id` and its span.
            Missing(_) => Ok(Transparent(id)),
        })
        .map(|ident_oi| ident_oi.add_edge_to(asg, definition, None))
    }

    /// Whether this identifier is bound to the object represented by `oi`.
    ///
    /// To bind an identifier,
    ///   see [`Self::bind_definition`].
    pub fn is_bound_to<O: ObjectRelFrom<Ident> + ObjectRelatable>(
        &self,
        asg: &Asg,
        oi: ObjectIndex<O>,
    ) -> bool {
        self.edges(asg).find_map(ObjectRel::narrow) == Some(oi)
    }

    /// The source package defining this identifier,
    ///   if known.
    pub fn src_pkg(&self, asg: &Asg) -> Option<ObjectIndex<Pkg>> {
        self.incoming_edges_filtered(asg).next()
    }
}

#[cfg(test)]
mod test;
