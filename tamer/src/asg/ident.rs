// ASG identifiers
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

//! Identifiers (a type of [object][super::object]).

use crate::{
    num::{Dim, Dtype},
    sym::{st, GlobalSymbolResolve, SymbolId},
};

pub type TransitionResult<T> = Result<T, (T, TransitionError)>;

/// Identifier.
///
/// These types represent object states:
///
/// ```text
/// (Missing) -> (Extern) -> ((Ident)) -> ((IdentFragment)).
///     \                        ^               /
///      \                      / \             /
///       `--------------------`   `-----------'
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum Ident {
    /// An identifier is expected to be defined but is not yet available.
    ///
    /// This variant contains the symbol representing the name of the
    ///   expected identifier.
    /// By defining an object as missing,
    ///   this allows the graph to be built incrementally as objects are
    ///   discovered.
    Missing(SymbolId),

    /// A resolved identifier.
    ///
    /// This represents an identifier that has been declared with certain
    ///   type information.
    Ident(SymbolId, IdentKind, Source),

    /// An identifier that has not yet been resolved.
    ///
    /// Externs are upgraded to [`Ident::Ident`] once an identifier of
    ///   the same name is loaded.
    /// It is an error if the loaded identifier does not have a compatible
    ///   [`IdentKind`].
    ///
    /// The source location of an extern represents the location of the
    ///   extern declaration.
    /// Once resolved, however,
    ///   the source will instead represent the location of the concrete
    ///   identifier.
    Extern(SymbolId, IdentKind, Source),

    /// Identifier with associated text.
    ///
    /// Code fragments are portions of the target language associated with
    ///   an identifier.
    /// They are produced by the compiler and it is the job of the
    ///   [linker][crate::ld] to put them into the correct order for the
    ///   final executable.
    IdentFragment(SymbolId, IdentKind, Source, FragmentText),
}

impl Ident {
    /// Identifier name.
    pub fn name(&self) -> SymbolId {
        match self {
            Self::Missing(name, ..)
            | Self::Ident(name, ..)
            | Self::Extern(name, ..)
            | Self::IdentFragment(name, ..) => *name,
        }
    }

    /// Identifier [`IdentKind`].
    ///
    /// If the object does not have a kind
    ///   (as is the case with [`Ident::Missing`]),
    ///     [`None`] is returned.
    pub fn kind(&self) -> Option<&IdentKind> {
        match self {
            Self::Missing(..) => None,

            Self::Ident(_, kind, ..)
            | Self::Extern(_, kind, ..)
            | Self::IdentFragment(_, kind, ..) => Some(kind),
        }
    }

    /// Identifier [`Source`].
    ///
    /// If the object does not have source information
    ///   (as is the case with [`Ident::Extern`]),
    ///     [`None`] is returned.
    pub fn src(&self) -> Option<&Source> {
        match self {
            Self::Missing(..) | Self::Extern(..) => None,

            Self::Ident(_, _, src, ..) | Self::IdentFragment(_, _, src, ..) => {
                Some(src)
            }
        }
    }

    /// Identifier [`FragmentText`].
    ///
    /// If the object does not have an associated code fragment,
    ///   [`None`] is returned.
    pub fn fragment(&self) -> Option<FragmentText> {
        match self {
            Self::Missing(..) | Self::Ident(..) | Self::Extern(..) => None,

            Self::IdentFragment(_, _, _, text) => Some(*text),
        }
    }

    /// Produce an object representing a missing identifier.
    ///
    /// This is the base state for all identifiers.
    pub fn declare(ident: SymbolId) -> Self {
        Ident::Missing(ident)
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
    ///     (it returns to a [`Ident::Ident`])
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
        kind: IdentKind,
        mut src: Source,
    ) -> TransitionResult<Ident> {
        match self {
            Ident::Ident(name, ref orig_kind, ref orig_src)
            | Ident::IdentFragment(name, ref orig_kind, ref orig_src, _)
                if src.override_ =>
            {
                if !orig_src.virtual_ {
                    let err =
                        TransitionError::NonVirtualOverride { name: name };

                    return Err((self, err));
                }

                if orig_kind != &kind {
                    let err = TransitionError::VirtualOverrideKind {
                        // TODO: defer lookup to error display
                        name: name,
                        existing: orig_kind.clone(),
                        given: kind.clone(),
                    };

                    return Err((self, err));
                }

                // Ensure that virtual flags are cleared to prohibit
                // override-overrides.  The compiler should do this; this is
                // just an extra layer of defense.
                src.virtual_ = false;

                // Note that this has the effect of clearing fragments if we
                // originally were in state `Ident::IdentFragment`.
                Ok(Ident::Ident(name, kind, src))
            }

            // If we encountered the override _first_, flip the context by
            // declaring a new identifier and trying to override that.
            Ident::Ident(name, orig_kind, orig_src) if orig_src.override_ => {
                Self::declare(name)
                    .resolve(kind, src)?
                    .resolve(orig_kind, orig_src)
            }

            // Same as above, but for fragments, we want to keep the
            // _original override_ fragment.
            Ident::IdentFragment(name, orig_kind, orig_src, orig_text)
                if orig_src.override_ =>
            {
                Self::declare(name)
                    .resolve(kind, src)?
                    .resolve(orig_kind, orig_src)?
                    .set_fragment(orig_text)
            }

            Ident::Extern(name, ref orig_kind, _) => {
                if orig_kind != &kind {
                    let err = TransitionError::ExternResolution {
                        name: name,
                        expected: orig_kind.clone(),
                        given: kind.clone(),
                    };

                    return Err((self, err));
                }

                Ok(Ident::Ident(name, kind, src))
            }

            // These represent the prologue and epilogue of maps.
            Ident::IdentFragment(
                _,
                IdentKind::MapHead
                | IdentKind::MapTail
                | IdentKind::RetMapHead
                | IdentKind::RetMapTail,
                ..,
            ) => Ok(self),

            Ident::Missing(name) => Ok(Ident::Ident(name, kind, src)),

            _ => {
                let err = TransitionError::Redeclare { name: self.name() };

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
            Ident::Missing(name) => {
                Err(UnresolvedError::Missing { name: *name })
            }

            Ident::Extern(name, ref kind, ref src) => {
                Err(UnresolvedError::Extern {
                    name: *name,
                    kind: kind.clone(),
                    pkg_name: src.pkg_name,
                })
            }

            Ident::Ident(..) | Ident::IdentFragment(..) => Ok(self),
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
        kind: IdentKind,
        src: Source,
    ) -> TransitionResult<Ident> {
        match self.kind() {
            None => Ok(Ident::Extern(self.name(), kind, src)),
            Some(cur_kind) => {
                if cur_kind != &kind {
                    let err = TransitionError::ExternResolution {
                        name: self.name(),
                        expected: kind.clone(),
                        given: cur_kind.clone(),
                    };

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
            Ident::Ident(sym, kind, src) => {
                Ok(Ident::IdentFragment(sym, kind, src, text))
            }

            // If we get to this point in a properly functioning program (at
            // least as of the time of writing), then we have encountered a
            // fragment for a virtual identifier _after_ we have already
            // encountered the fragment for its _override_.  We therefore
            // want to keep the override.
            //
            // If this is not permissable, then we should have already
            // prevented the `resolve` transition before this fragment was
            // encountered.
            Ident::IdentFragment(_, _, ref src, ..) if src.override_ => {
                Ok(self)
            }

            // These represent the prologue and epilogue of maps.
            Ident::IdentFragment(
                _,
                IdentKind::MapHead
                | IdentKind::MapTail
                | IdentKind::RetMapHead
                | IdentKind::RetMapTail,
                ..,
            ) => Ok(self),

            _ => {
                let msg =
                    format!("identifier is not a Ident::Ident): {:?}", self,);

                Err((self, TransitionError::BadFragmentDest { name: msg }))
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
    Redeclare { name: SymbolId },

    /// Extern resolution failure.
    ///
    /// An extern could not be resolved because the provided identifier had
    ///   a type that is incompatible with the extern definition.
    ExternResolution {
        name: SymbolId,
        expected: IdentKind,
        given: IdentKind,
    },

    /// Attempt to override a non-virtual identifier.
    NonVirtualOverride { name: SymbolId },

    /// Overriding a virtual identifier failed due to an incompatible
    ///   [`IdentKind`].
    VirtualOverrideKind {
        name: SymbolId,
        existing: IdentKind,
        given: IdentKind,
    },

    /// The provided identifier is not in a state that is permitted to
    ///   receive a fragment.
    ///
    /// See [`Ident::set_fragment`].
    BadFragmentDest { name: String },
}

impl std::fmt::Display for TransitionError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Redeclare { name } => write!(
                fmt,
                "cannot redeclare identifier `{}`",
                name,
            ),

            Self::ExternResolution {
                name,
                expected,
                given,
            } => write!(
                fmt,
                "extern `{}` of type `{}` is incompatible with type `{}`",
                name, expected, given,
            ),

            Self::NonVirtualOverride { name } => write!(
                fmt,
                "non-virtual identifier `{}` cannot be overridden",
                name,
            ),

            Self::VirtualOverrideKind {
                name,
                existing,
                given,
            } => write!(
                fmt,
                "virtual identifier `{}` of type `{}` cannot be overridden with type `{}`",
                name, existing, given,
            ),

            Self::BadFragmentDest{name: msg} => {
                write!(fmt, "bad fragment destination: {}", msg)
            }
        }
    }
}

impl std::error::Error for TransitionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Resolved identifier was expected.
#[derive(Clone, Debug, PartialEq)]
pub enum UnresolvedError {
    /// Expected identifier is missing and nothing about it is known.
    Missing { name: SymbolId },

    /// Expected identifier has not yet been resolved with a concrete
    ///   definition.
    Extern {
        /// Identifier name.
        name: SymbolId,
        /// Expected identifier type.
        kind: IdentKind,
        /// Name of package where the extern was defined.
        pkg_name: Option<SymbolId>,
    },
}

impl std::fmt::Display for UnresolvedError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnresolvedError::Missing { name } => {
                write!(fmt, "missing expected identifier `{}`", name,)
            }

            UnresolvedError::Extern {
                name,
                kind,
                pkg_name,
            } => write!(
                fmt,
                "unresolved extern `{}` of type `{}`, declared in `{}`",
                name,
                kind,
                pkg_name.map(|s| s.lookup_str()).unwrap_or(&"<unknown>"),
            ),
        }
    }
}

impl std::error::Error for UnresolvedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
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
        match self {
            Self::Meta | Self::Map | Self::RetMap | Self::Worksheet => true,
            _ => false,
        }
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

#[cfg(test)]
mod test;
