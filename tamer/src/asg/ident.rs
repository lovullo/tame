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

            // These represent the prolog and epilogue of maps. This
            // situation will be resolved in the future.
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

            // These represent the prolog and epilogue of maps. This
            // situation will be resolved in the future.
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
mod test {
    use super::*;
    use crate::num::Dim;
    use crate::sym::{GlobalSymbolIntern, SymbolId};

    // Note that Ident has no variants capable of None
    #[test]
    fn ident_object_name() {
        let sym: SymbolId = "sym".intern();

        assert_eq!(sym, Ident::Missing(sym).name());

        assert_eq!(
            sym,
            Ident::Ident(sym, IdentKind::Meta, Source::default()).name()
        );

        assert_eq!(
            sym,
            Ident::Extern(sym, IdentKind::Meta, Source::default()).name()
        );

        assert_eq!(
            sym,
            Ident::IdentFragment(
                sym,
                IdentKind::Meta,
                Source::default(),
                "".intern()
            )
            .name()
        );
    }

    #[test]
    fn ident_object_kind() {
        let sym: SymbolId = "sym".intern();
        let kind = IdentKind::Class(Dim::Matrix);

        assert_eq!(None, Ident::Missing(sym).kind());

        assert_eq!(
            Some(&kind),
            Ident::Ident(sym, kind.clone(), Source::default()).kind()
        );

        assert_eq!(
            Some(&kind),
            Ident::Extern(sym, kind.clone(), Source::default()).kind()
        );

        assert_eq!(
            Some(&kind),
            Ident::IdentFragment(
                sym,
                kind.clone(),
                Source::default(),
                "".intern()
            )
            .kind()
        );
    }

    #[test]
    fn ident_object_src() {
        let sym: SymbolId = "sym".intern();
        let src = Source {
            desc: Some("test source".into()),
            ..Default::default()
        };

        assert_eq!(None, Ident::Missing(sym).src());

        assert_eq!(
            Some(&src),
            Ident::Ident(sym, IdentKind::Meta, src.clone()).src()
        );

        assert_eq!(
            None,
            Ident::Extern(sym, IdentKind::Meta, src.clone()).src()
        );

        assert_eq!(
            Some(&src),
            Ident::IdentFragment(
                sym,
                IdentKind::Meta,
                src.clone(),
                "".intern()
            )
            .src()
        );
    }

    #[test]
    fn ident_object_fragment() {
        let sym: SymbolId = "sym".intern();
        let text = "foo".into();

        assert_eq!(None, Ident::Missing(sym).fragment());

        assert_eq!(
            None,
            Ident::Ident(sym, IdentKind::Meta, Source::default()).fragment()
        );

        assert_eq!(
            None,
            Ident::Extern(sym, IdentKind::Meta, Source::default()).fragment()
        );

        assert_eq!(
            Some(text),
            Ident::IdentFragment(
                sym,
                IdentKind::Meta,
                Source::default(),
                text,
            )
            .fragment()
        );
    }

    #[test]
    fn ident_object_missing() {
        let sym: SymbolId = "missing".intern();
        assert_eq!(Ident::Missing(sym), Ident::declare(sym));
    }

    #[test]
    fn resolved_on_missing() {
        let sym: SymbolId = "missing".intern();

        let result = Ident::declare(sym)
            .resolved()
            .expect_err("expected error asserting resolved() on missing");

        match result {
            UnresolvedError::Missing { name: e_name } => {
                assert_eq!(sym, e_name);
            }
            _ => panic!("expected UnresolvedError {:?}", result),
        }
    }

    #[test]
    fn ident_object_ident() {
        let sym: SymbolId = "ident".intern();
        let kind = IdentKind::Meta;
        let src = Source {
            desc: Some("ident ctor".into()),
            ..Default::default()
        };

        assert_eq!(
            Ident::Ident(sym, kind.clone(), src.clone()),
            Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .unwrap(),
        );
    }

    #[test]
    fn resolved_on_ident() {
        let sym: SymbolId = "ident resolve".intern();
        let kind = IdentKind::Meta;
        let src = Source {
            desc: Some("ident ctor".into()),
            ..Default::default()
        };

        assert_eq!(
            &Ident::Ident(sym, kind.clone(), src.clone()),
            Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .unwrap()
                .resolved()
                .unwrap(),
        );
    }

    // Note that we don't care about similar sources.  It's expected
    // that the system populating the ASG will only resolve local
    // symbols, and so redeclarations should represent that multiple
    // packages have the same local symbol.
    #[test]
    fn ident_object_redeclare_same_src() {
        let sym: SymbolId = "redecl".intern();
        let kind = IdentKind::Meta;
        let src = Source::default();

        let first = Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .unwrap();

        // Resolve twice, as if we encountered two local symbols.
        let result = first
            .clone()
            .resolve(kind.clone(), src.clone())
            .expect_err("expected error redeclaring identifier");

        match result {
            (orig, TransitionError::Redeclare { name }) => {
                assert_eq!(first, orig);
                assert_eq!(sym, name);
            }
            _ => {
                panic!("expected TransitionError::Redeclare: {:?}", result)
            }
        }
    }

    mod extern_ {
        use super::*;

        #[test]
        fn ident_object() {
            let sym: SymbolId = "extern".intern();
            let kind = IdentKind::Class(Dim::Vector);
            let src = Source {
                desc: Some("extern".into()),
                ..Default::default()
            };

            assert_eq!(
                Ok(Ident::Extern(sym, kind.clone(), src.clone())),
                Ident::declare(sym).extern_(kind, src),
            );
        }

        #[test]
        fn resolved_on_extern() {
            let sym: SymbolId = "extern resolved".intern();
            let kind = IdentKind::Class(Dim::Vector);
            let pkg_name: SymbolId = "pkg/name".intern();
            let src = Source {
                pkg_name: Some(pkg_name),
                desc: Some("extern".into()),
                ..Default::default()
            };

            let result = Ident::Extern(sym, kind.clone(), src.clone())
                .resolved()
                .expect_err("expected error asserting resolved() on extern");

            match result {
                UnresolvedError::Extern {
                    name: e_name,
                    kind: e_kind,
                    pkg_name: e_pkg_name,
                } => {
                    assert_eq!(sym, e_name);
                    assert_eq!(kind, e_kind);
                    assert_eq!(Some(pkg_name), e_pkg_name);
                }
                _ => panic!("expected UnresolvedError: {:?}", result),
            }
        }

        #[test]
        fn resolved_on_extern_error_fmt_without_pkg() {
            let meta = IdentKind::Meta;
            let err = UnresolvedError::Extern {
                name: "foo".into(),
                kind: IdentKind::Meta,
                pkg_name: None,
            };

            let msg = format!("{}", err);

            assert!(msg.contains("`foo`"));
            assert!(msg.contains("in `<unknown>`"));
            assert!(msg.contains(&format!("`{}`", meta)));
        }

        #[test]
        fn resolved_on_extern_error_fmt_with_pkg() {
            let meta = IdentKind::Meta;
            let pkg = "pkg".into();

            let err = UnresolvedError::Extern {
                name: "foo".into(),
                kind: IdentKind::Meta,
                pkg_name: Some(pkg),
            };

            let msg = format!("{}", err);

            assert!(msg.contains("`foo`"));
            assert!(msg.contains(&format!("in `{}`", pkg)));
            assert!(msg.contains(&format!("`{}`", meta)));
        }

        // Extern first, then identifier
        #[test]
        fn redeclare_compatible_resolves() {
            let sym: SymbolId = "extern_re_pre".intern();
            let kind = IdentKind::Class(Dim::Matrix);
            let src = Source {
                desc: Some("okay".into()),
                ..Default::default()
            };

            // Compatible kind, should resolve.
            let result = Ident::declare(sym)
                .extern_(kind.clone(), Source::default())
                .and_then(|o| o.resolve(kind.clone(), src.clone()));

            assert_eq!(Ok(Ident::Ident(sym, kind, src)), result,);
        }

        // Identifier first, then extern
        #[test]
        fn redeclare_compatible_resolves_post() {
            let sym: SymbolId = "extern_re_post".intern();
            let kind = IdentKind::Class(Dim::Vector);
            let src = Source {
                desc: Some("okay".into()),
                ..Default::default()
            };

            // Compatible kind, should resolve.
            let result = Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .and_then(|o| o.extern_(kind.clone(), Source::default()));

            assert_eq!(Ok(Ident::Ident(sym, kind, src)), result,);
        }

        #[test]
        fn redeclare_another_extern() {
            let sym: SymbolId = "extern_extern".intern();
            let kind = IdentKind::Class(Dim::Scalar);
            let src_first = Source {
                desc: Some("first src".into()),
                ..Default::default()
            };
            let src_second = Source {
                desc: Some("second src".into()),
                ..Default::default()
            };

            let result = Ident::declare(sym)
                .extern_(kind.clone(), src_first.clone())
                .and_then(|o| o.extern_(kind.clone(), src_second));

            // Note that, if it resolves, it should keep what is
            // _existing_, meaning that it must keep the first src.
            assert_eq!(Ok(Ident::Extern(sym, kind, src_first)), result);
        }

        // Extern first, then identifier
        #[test]
        fn redeclare_post_incompatible_kind() {
            let sym: SymbolId = "extern_re_bad_post".intern();
            let kind = IdentKind::Class(Dim::Matrix);
            let src = Source {
                desc: Some("bad kind".into()),
                ..Default::default()
            };

            let orig = Ident::declare(sym)
                .extern_(kind.clone(), Source::default())
                .unwrap();

            // Incompatible kind
            let kind_bad = IdentKind::Meta;
            let result = orig.clone().resolve(kind_bad.clone(), src);

            match result {
                Err((given_orig, err @ _)) => {
                    assert_eq!(orig, given_orig);

                    if let TransitionError::ExternResolution {
                        name: e_name,
                        expected: e_expected,
                        given: e_given,
                    } = err.clone()
                    {
                        assert_eq!(sym, e_name);
                        assert_eq!(kind, e_expected);
                        assert_eq!(kind_bad, e_given);
                    }

                    // Formatted error
                    let msg = format!("{}", err);

                    assert!(msg.contains(&format!("{}", sym)));
                    assert!(msg.contains(&format!("{}", kind)));
                    assert!(msg.contains(&format!("{}", kind_bad)));
                }
                _ => panic!("expected failure: {:?}", result),
            }
        }

        // Identifier first, then extern
        #[test]
        fn redeclare_pre_incompatible_kind() {
            let sym: SymbolId = "extern_re_bad_pre".intern();
            let kind_given = IdentKind::Class(Dim::Vector);
            let src = Source {
                desc: Some("bad kind".into()),
                ..Default::default()
            };

            let orig = Ident::declare(sym)
                .resolve(kind_given.clone(), src.clone())
                .unwrap();

            // Extern with incompatible kind.
            let kind_extern = IdentKind::Meta;
            let result =
                orig.clone().extern_(kind_extern.clone(), Source::default());

            match result {
                Err((given_orig, err @ _)) => {
                    assert_eq!(orig, given_orig);

                    if let TransitionError::ExternResolution {
                        name: e_name,
                        expected: e_expected,
                        given: e_given,
                    } = err.clone()
                    {
                        assert_eq!(sym, e_name);
                        assert_eq!(kind_extern, e_expected);
                        assert_eq!(kind_given, e_given);
                    }

                    // Formatted error
                    let msg = format!("{}", err);

                    assert!(msg.contains(&format!("{}", sym)));
                    assert!(msg.contains(&format!("{}", kind_extern)));
                    assert!(msg.contains(&format!("{}", kind_given)));
                }
                _ => panic!("expected failure: {:?}", result),
            }
        }
    }

    #[test]
    fn add_fragment_to_ident() {
        let sym: SymbolId = "tofrag".intern();
        let src = Source {
            generated: true,
            ..Default::default()
        };

        let kind = IdentKind::Meta;
        let ident = Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .unwrap();
        let text = FragmentText::from("a fragment");
        let ident_with_frag = ident.set_fragment(text.clone());

        assert_eq!(
            Ok(Ident::IdentFragment(sym, kind, src, text)),
            ident_with_frag,
        );
    }

    #[test]
    fn resolved_on_fragment() {
        let sym: SymbolId = "tofrag resolved".intern();
        let src = Source {
            generated: true,
            ..Default::default()
        };

        let kind = IdentKind::Meta;
        let ident = Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .unwrap();
        let text = FragmentText::from("a fragment for resolved()");
        let ident_with_frag = ident.set_fragment(text.clone());

        assert_eq!(
            Ok(&Ident::IdentFragment(sym, kind, src, text)),
            ident_with_frag.unwrap().resolved(),
        );
    }

    #[test]
    fn add_fragment_to_fragment_fails() {
        let sym: SymbolId = "badsym".intern();
        let ident = Ident::declare(sym)
            .resolve(IdentKind::Meta, Source::default())
            .unwrap();

        let ident_with_frag = ident
            .set_fragment("orig fragment".into())
            .expect("set_fragment failed");

        // Since it's already a fragment, this should fail.
        let err = ident_with_frag
            .clone()
            .set_fragment("replacement".intern())
            .expect_err("Expected failure");

        match err {
            (orig, TransitionError::BadFragmentDest { .. }) => {
                assert_eq!(ident_with_frag, orig);
            }
            _ => panic!("expected TransitionError::BadFragmentDest: {:?}", err),
        }
    }

    mod override_ {
        use super::*;

        #[test]
        fn declare_virtual_ident_first() {
            let sym: SymbolId = "virtual".intern();
            let over_src = "src".intern();
            let kind = IdentKind::Meta;

            let virt = Ident::declare(sym)
                .resolve(
                    kind.clone(),
                    Source {
                        virtual_: true,
                        ..Default::default()
                    },
                )
                .unwrap();

            let over_src = Source {
                virtual_: true, // this needn't be set, but see below
                override_: true,
                src: Some(over_src),
                ..Default::default()
            };

            let result = virt.resolve(kind.clone(), over_src.clone());

            // Overriding should clear any virtual flag that may have
            // been set to prevent override-overrides.
            let expected_src = Source {
                virtual_: false,
                ..over_src
            };

            assert_eq!(Ok(Ident::Ident(sym, kind, expected_src)), result);
        }

        // Override is encountered before the virtual
        #[test]
        fn declare_virtual_ident_after_override() {
            let sym: SymbolId = "virtual_second".intern();
            let virt_src = "virt_src".intern();
            let kind = IdentKind::Meta;

            let over_src = Source {
                virtual_: true, // this needn't be set, but see below
                override_: true,
                ..Default::default()
            };

            let over = Ident::declare(sym)
                .resolve(kind.clone(), over_src.clone())
                .unwrap();

            let virt_src = Source {
                virtual_: true,
                src: Some(virt_src),
                ..Default::default()
            };

            let result = over.resolve(kind.clone(), virt_src.clone());

            // Overriding should clear any virtual flag that may have
            // been set to prevent override-overrides.  We should also
            // take the override source even though virtual was second.
            let expected_src = Source {
                virtual_: false,
                ..over_src
            };

            assert_eq!(Ok(Ident::Ident(sym, kind, expected_src)), result);
        }

        #[test]
        fn declare_override_non_virtual() {
            let sym: SymbolId = "non_virtual".intern();
            let kind = IdentKind::Meta;

            let non_virt = Ident::declare(sym)
                .resolve(
                    kind.clone(),
                    Source {
                        virtual_: false,
                        ..Default::default()
                    },
                )
                .unwrap();

            let over_src = Source {
                override_: true,
                ..Default::default()
            };

            // This isn't the purpose of the test, but we want to make
            // sure that the non-virtual override error occurs before
            // the kind error.
            let bad_kind = IdentKind::Cgen(Dim::Vector);

            let result = non_virt
                .clone()
                .resolve(bad_kind, over_src.clone())
                .expect_err("expected error");

            match result {
                (
                    ref orig,
                    TransitionError::NonVirtualOverride { ref name },
                ) => {
                    assert_eq!(orig, &non_virt);
                    assert_eq!(sym, *name);

                    // Formatted error
                    let (_, err) = result;
                    let msg = format!("{}", err);

                    assert!(msg.contains(&format!("{}", sym)));
                }
                (_, TransitionError::VirtualOverrideKind { .. }) => {
                    panic!("kind check must happen _after_ virtual check")
                }
                _ => panic!(
                    "expected TransitionError::VirtualOverrideKind {:?}",
                    result
                ),
            }
        }

        #[test]
        fn declare_virtual_ident_incompatible_kind() {
            let sym: SymbolId = "virtual".intern();
            let src_sym: SymbolId = "src".intern();
            let kind = IdentKind::Meta;

            let virt = Ident::declare(sym)
                .resolve(
                    kind.clone(),
                    Source {
                        virtual_: true,
                        ..Default::default()
                    },
                )
                .unwrap();

            let over_src = Source {
                override_: true,
                src: Some(src_sym),
                ..Default::default()
            };

            let bad_kind = IdentKind::Cgen(Dim::Vector);
            let result = virt
                .clone()
                .resolve(bad_kind.clone(), over_src.clone())
                .expect_err("expected error");

            match result {
                (
                    ref orig,
                    TransitionError::VirtualOverrideKind {
                        ref name,
                        ref existing,
                        ref given,
                    },
                ) => {
                    assert_eq!(orig, &virt);

                    assert_eq!(sym, *name);
                    assert_eq!(&kind, existing);
                    assert_eq!(&bad_kind, given);

                    // Formatted error
                    let (_, err) = result;
                    let msg = format!("{}", err);

                    assert!(msg.contains(&format!("{}", sym)));
                    assert!(msg.contains(&format!("{}", kind)));
                    assert!(msg.contains(&format!("{}", bad_kind)));
                }
                _ => panic!(
                    "expected TransitionError::VirtualOverrideKind {:?}",
                    result
                ),
            }
        }

        // Encounter virtual first and override second should cause the
        // fragment to be cleared to make way for the new fragment.
        #[test]
        fn declare_override_virtual_ident_fragment_virtual_first() {
            let sym: SymbolId = "virtual".intern();
            let over_src = "src".intern();
            let kind = IdentKind::Meta;

            // Remember: override is going to come first...
            let over_src = Source {
                override_: true,
                src: Some(over_src),
                ..Default::default()
            };

            // ...and virt second.
            let virt_src = Source {
                virtual_: true,
                ..Default::default()
            };

            let over = Ident::declare(sym)
                .resolve(kind.clone(), over_src.clone())
                .unwrap();

            // So we should _keep_ this fragment, since it represent the
            // override, even though it's appearing first.
            let text = FragmentText::from("keep me");
            let over_frag = over.set_fragment(text.clone());

            assert_eq!(
                Ok(Ident::IdentFragment(
                    sym,
                    kind.clone(),
                    over_src.clone(),
                    text.clone(),
                )),
                over_frag,
            );

            let result =
                over_frag.unwrap().resolve(kind.clone(), virt_src.clone());

            // Overriding should _not_ have cleared the fragment since
            // the override was encountered _first_, so we want to keep
            // its fragment.
            assert_eq!(
                Ok(Ident::IdentFragment(
                    sym,
                    kind.clone(),
                    over_src.clone(),
                    text.clone()
                )),
                result
            );

            // Finally, after performing this transition, we will
            // inevitably encounter the fragment for the virtual
            // identifier, which we must ignore.  So we must make sure
            // that encountering it will not cause an error, because we
            // still have an IdentFragment at this point.
            assert_eq!(
                Ok(Ident::IdentFragment(
                    sym,
                    kind,
                    over_src.clone(),
                    text.clone()
                )),
                result.unwrap().set_fragment("virt fragment".into()),
            );
        }

        // Encountering _override_ first and virtual second should _not_
        // clear the fragment, otherwise the virtual fragment will take
        // precedence over the override.
        #[test]
        fn declare_override_virtual_ident_fragment_override_first() {
            let sym: SymbolId = "virtual".intern();
            let over_src = "src".intern();
            let kind = IdentKind::Meta;

            let virt_src = Source {
                virtual_: true,
                ..Default::default()
            };

            let virt = Ident::declare(sym)
                .resolve(kind.clone(), virt_src.clone())
                .unwrap();
            let text = FragmentText::from("remove me");
            let virt_frag = virt.set_fragment(text.clone());

            assert_eq!(
                Ok(Ident::IdentFragment(sym, kind.clone(), virt_src, text)),
                virt_frag,
            );

            let over_src = Source {
                override_: true,
                src: Some(over_src),
                ..Default::default()
            };

            let result =
                virt_frag.unwrap().resolve(kind.clone(), over_src.clone());

            // The act of overriding the object should have cleared any
            // existing fragment, making way for a new fragment to take its
            // place as soon as it is discovered.  (So, back to an
            // Ident::Ident.)
            assert_eq!(Ok(Ident::Ident(sym, kind, over_src)), result);
        }

        #[test]
        fn declare_override_virtual_ident_fragment_incompatible_type() {
            let sym: SymbolId = "virtual".intern();
            let over_src = "src".intern();
            let kind = IdentKind::Meta;

            let virt_src = Source {
                virtual_: true,
                ..Default::default()
            };

            let virt = Ident::declare(sym)
                .resolve(kind.clone(), virt_src.clone())
                .unwrap();
            let virt_frag = virt.set_fragment("".into()).unwrap();

            let over_src = Source {
                override_: true,
                src: Some(over_src),
                ..Default::default()
            };

            let bad_kind = IdentKind::Cgen(Dim::Vector);
            let result = virt_frag
                .clone()
                .resolve(bad_kind.clone(), over_src.clone())
                .expect_err("expected error");

            match result {
                (
                    ref orig,
                    TransitionError::VirtualOverrideKind {
                        ref name,
                        ref existing,
                        ref given,
                    },
                ) => {
                    assert_eq!(orig, &virt_frag);

                    assert_eq!(sym, *name);
                    assert_eq!(&kind, existing);
                    assert_eq!(&bad_kind, given);

                    // Formatted error
                    let (_, err) = result;
                    let msg = format!("{}", err);

                    assert!(msg.contains(&format!("{}", sym)));
                    assert!(msg.contains(&format!("{}", kind)));
                    assert!(msg.contains(&format!("{}", bad_kind)));
                }
                _ => panic!(
                    "expected TransitionError::VirtualOverrideKind {:?}",
                    result
                ),
            }
        }
    }

    fn add_ident_kind_ignores(given: IdentKind, expected: IdentKind) {
        let sym: SymbolId = "tofrag".intern();
        let src = Source {
            generated: true,
            ..Default::default()
        };

        let obj = Ident::declare(sym).resolve(given, src.clone()).unwrap();

        let fragment = "a fragment".intern();
        let obj_with_frag = obj.set_fragment(fragment);

        assert_eq!(
            Ok(Ident::IdentFragment(sym, expected, src, fragment)),
            obj_with_frag,
        );
    }

    #[test]
    fn add_fragment_to_ident_map_head() {
        add_ident_kind_ignores(IdentKind::MapHead, IdentKind::MapHead)
    }

    #[test]
    fn add_fragment_to_ident_map_tail() {
        add_ident_kind_ignores(IdentKind::MapTail, IdentKind::MapTail)
    }

    #[test]
    fn add_fragment_to_ident_retmap_head() {
        add_ident_kind_ignores(IdentKind::RetMapHead, IdentKind::RetMapHead)
    }

    #[test]
    fn add_fragment_to_ident_retmap_tail() {
        add_ident_kind_ignores(IdentKind::RetMapTail, IdentKind::RetMapTail)
    }
}
