// Objects represented on ASG
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

//! Objects represented by the ASG.
//!
//! _This is a private module.
//!  See [`super`] for available exports._

use super::ident::IdentKind;
use crate::ir::legacyir::SymAttrs;
use crate::sym::Symbol;
use std::result::Result;

pub type TransitionResult<T> = Result<T, (T, TransitionError)>;

/// Type of object.
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
pub enum IdentObject<'i> {
    /// An identifier is expected to be defined but is not yet available.
    ///
    /// This variant contains the symbol representing the name of the
    ///   expected identifier.
    /// By defining an object as missing,
    ///   this allows the graph to be built incrementally as objects are
    ///   discovered.
    Missing(&'i Symbol<'i>),

    /// A resolved identifier.
    ///
    /// This represents an identifier that has been declared with certain
    ///   type information.
    Ident(&'i Symbol<'i>, IdentKind, Source<'i>),

    /// An identifier that has not yet been resolved.
    ///
    /// Externs are upgraded to [`IdentObject::Ident`] once an identifier of
    ///   the same name is loaded.
    /// It is an error if the loaded identifier does not have a compatible
    ///   [`IdentKind`].
    ///
    /// The source location of an extern represents the location of the
    ///   extern declaration.
    /// Once resolved, however,
    ///   the source will instead represent the location of the concrete
    ///   identifier.
    Extern(&'i Symbol<'i>, IdentKind, Source<'i>),

    /// Identifier with associated text.
    ///
    /// Code fragments are portions of the target language associated with
    ///   an identifier.
    /// They are produced by the compiler and it is the job of the
    ///   [linker][crate::ld] to put them into the correct order for the
    ///   final executable.
    IdentFragment(&'i Symbol<'i>, IdentKind, Source<'i>, FragmentText),
}

/// Retrieve information about an [`IdentObject`].
///
/// APIs should adhere to this trait rather than a concrete object type such
///   as [`IdentObject`];
///     this allows other representations to be used,
///       while still permitting the use of matching on [`IdentObject`]
///       through the use of [`ident`](IdentObjectData::as_ident).
///
/// Since an object implementing this trait may not be an identifier
///   (e.g. an expression),
///   even [`name`](IdentObjectData::name)---which
///     is used by all [`IdentObject`] variants---returns
///     an [`Option`].
/// These methods also provide a convenient alternative to `match`ing on
///   data that may not be present in all variants.
pub trait IdentObjectData<'i> {
    /// Identifier name.
    ///
    /// If the object is not an identifier,
    ///   [`None`] is returned.
    fn name(&self) -> Option<&'i Symbol<'i>>;

    /// Identifier [`IdentKind`].
    ///
    /// If the object does not have a kind
    ///   (as is the case with [`IdentObject::Missing`]),
    ///     [`None`] is returned.
    fn kind(&self) -> Option<&IdentKind>;

    /// Identifier [`Source`].
    ///
    /// If the object does not have source information
    ///   (as is the case with [`IdentObject::Extern`]),
    ///     [`None`] is returned.
    fn src(&self) -> Option<&Source<'i>>;

    /// Identifier [`FragmentText`].
    ///
    /// If the object does not have an associated code fragment,
    ///   [`None`] is returned.
    fn fragment(&self) -> Option<&FragmentText>;

    /// IdentObject as an identifier ([`IdentObject`]).
    ///
    /// If the object is not or cannot be faithfully converted into an
    ///   [`IdentObject`],
    ///     [`None`] is returned.
    /// For example,
    ///   expressions will always yield [`None`].
    ///
    /// This allows pattern matching on [`IdentObject`] variants regardless
    ///   of the underlying object type.
    fn as_ident(&self) -> Option<&IdentObject<'i>>;
}

impl<'i> std::fmt::Display for IdentObject<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.name() {
            Some(n) => write!(f, "{}", n),
            _ => write!(f, "missing"),
        }
    }
}

impl<'i> IdentObjectData<'i> for IdentObject<'i> {
    fn name(&self) -> Option<&'i Symbol<'i>> {
        match self {
            Self::Missing(name)
            | Self::Ident(name, _, _)
            | Self::Extern(name, _, _)
            | Self::IdentFragment(name, _, _, _) => Some(name),
        }
    }

    fn kind(&self) -> Option<&IdentKind> {
        match self {
            Self::Missing(_) => None,
            Self::Ident(_, kind, _)
            | Self::Extern(_, kind, _)
            | Self::IdentFragment(_, kind, _, _) => Some(kind),
        }
    }

    fn src(&self) -> Option<&Source<'i>> {
        match self {
            Self::Missing(_) | Self::Extern(_, _, _) => None,
            Self::Ident(_, _, src) | Self::IdentFragment(_, _, src, _) => {
                Some(src)
            }
        }
    }

    fn fragment(&self) -> Option<&FragmentText> {
        match self {
            Self::Missing(_) | Self::Ident(_, _, _) | Self::Extern(_, _, _) => {
                None
            }
            Self::IdentFragment(_, _, _, text) => Some(text),
        }
    }

    /// Expose underlying [`IdentObject`].
    ///
    /// This will never be [`None`] for this implementation.
    /// However,
    ///   other [`IdentObjectData`] implementations may still result in
    ///   [`None`],
    ///     so it's important _not_ to rely on this as an excuse to be lazy
    ///       with unwrapping.
    #[inline]
    fn as_ident(&self) -> Option<&IdentObject<'i>> {
        Some(&self)
    }
}

/// Objects as a state machine.
pub trait IdentObjectState<'i, T>
where
    T: IdentObjectState<'i, T>,
{
    /// Produce an object representing a missing identifier.
    ///
    /// This is the base state for all identifiers.
    fn declare(ident: &'i Symbol<'i>) -> T;

    /// Attempt to transition to a concrete identifier.
    ///
    /// For specific information on compatibility rules,
    ///   see implementers of this trait,
    ///   since rules may vary between implementations.
    fn resolve(self, kind: IdentKind, src: Source<'i>) -> TransitionResult<T>;

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
    ///   both [`IdentObject::Missing`] and [`IdentObject::Extern`] are
    ///   considered to be unresolved.
    fn resolved(&self) -> Result<&T, UnresolvedError>;

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
    /// If no kind is assigned (such as [`IdentObject::Missing`]),
    ///   then a new extern is produced.
    /// See for example [`IdentObject::Extern`].
    fn extern_(self, kind: IdentKind, src: Source<'i>) -> TransitionResult<T>;

    /// Attach a code fragment (compiled text) to an identifier.
    ///
    /// This will fail if an identifier already has a fragment,
    ///   since only the owner of the identifier should be producing
    ///   compiled code.
    /// Note, however, that an identifier's fragment may be cleared under
    ///   certain circumstances (such as symbol overrides),
    ///     making way for a new fragment to be set.
    fn set_fragment(self, text: FragmentText) -> TransitionResult<T>;
}

impl<'i> IdentObjectState<'i, IdentObject<'i>> for IdentObject<'i> {
    fn declare(ident: &'i Symbol<'i>) -> Self {
        IdentObject::Missing(ident)
    }

    /// Attempt to redeclare an identifier with additional information.
    ///
    /// If an existing identifier is an [`IdentObject::Extern`],
    ///   then the declaration will be compared just the same,
    ///     but the identifier will be converted from an extern into an
    ///     identifier.
    /// When this happens,
    ///   the extern is said to be _resolved_.
    ///
    /// If a virtual identifier of type [`IdentObject::IdentFragment`] is
    ///   overridden,
    ///     then its fragment is cleared
    ///     (it returns to a [`IdentObject::Ident`])
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
    fn resolve(
        self,
        kind: IdentKind,
        mut src: Source<'i>,
    ) -> TransitionResult<IdentObject<'i>> {
        match self {
            IdentObject::Ident(name, ref orig_kind, ref orig_src)
            | IdentObject::IdentFragment(
                name,
                ref orig_kind,
                ref orig_src,
                _,
            ) if src.override_ => {
                if !orig_src.virtual_ {
                    let err = TransitionError::NonVirtualOverride {
                        name: name.to_string(),
                    };

                    return Err((self, err));
                }

                if orig_kind != &kind {
                    let err = TransitionError::VirtualOverrideKind {
                        name: name.to_string(),
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
                // originally were in state `IdentObject::IdentFragment`.
                Ok(IdentObject::Ident(name, kind, src))
            }

            // If we encountered the override _first_, flip the context by
            // declaring a new identifier and trying to override that.
            IdentObject::Ident(name, orig_kind, orig_src)
                if orig_src.override_ =>
            {
                Self::declare(name)
                    .resolve(kind, src)?
                    .resolve(orig_kind, orig_src)
            }

            // Same as above, but for fragments, we want to keep the
            // _original override_ fragment.
            IdentObject::IdentFragment(
                name,
                orig_kind,
                orig_src,
                orig_text,
            ) if orig_src.override_ => Self::declare(name)
                .resolve(kind, src)?
                .resolve(orig_kind, orig_src)?
                .set_fragment(orig_text),

            IdentObject::Extern(name, ref orig_kind, _) => {
                if orig_kind != &kind {
                    let err = TransitionError::ExternResolution {
                        name: name.to_string(),
                        expected: orig_kind.clone(),
                        given: kind.clone(),
                    };

                    return Err((self, err));
                }

                Ok(IdentObject::Ident(name, kind, src))
            }

            // These represent the prolog and epilogue of maps. This
            // situation will be resolved in the future.
            IdentObject::IdentFragment(_, IdentKind::MapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::MapTail, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapTail, _, _) => {
                Ok(self)
            }

            IdentObject::Missing(name) => {
                Ok(IdentObject::Ident(name, kind, src))
            }

            _ => {
                let err = TransitionError::Redeclare {
                    name: self.name().unwrap().to_string(),
                };

                Err((self, err))
            }
        }
    }

    fn resolved(&self) -> Result<&IdentObject<'i>, UnresolvedError> {
        match self {
            IdentObject::Missing(name) => Err(UnresolvedError::Missing {
                name: name.to_string(),
            }),

            IdentObject::Extern(name, ref kind, ref src) => {
                Err(UnresolvedError::Extern {
                    name: name.to_string(),
                    kind: kind.clone(),
                    pkg_name: src.pkg_name.map(|s| s.to_string()),
                })
            }

            IdentObject::Ident(_, _, _)
            | IdentObject::IdentFragment(_, _, _, _) => Ok(self),
        }
    }

    fn extern_(
        self,
        kind: IdentKind,
        src: Source<'i>,
    ) -> TransitionResult<IdentObject<'i>> {
        match self.kind() {
            None => Ok(IdentObject::Extern(self.name().unwrap(), kind, src)),
            Some(cur_kind) => {
                if cur_kind != &kind {
                    let err = TransitionError::ExternResolution {
                        name: self.name().unwrap().to_string(),
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

    fn set_fragment(
        self,
        text: FragmentText,
    ) -> TransitionResult<IdentObject<'i>> {
        match self {
            IdentObject::Ident(sym, kind, src) => {
                Ok(IdentObject::IdentFragment(sym, kind, src, text))
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
            IdentObject::IdentFragment(_, _, ref src, _) if src.override_ => {
                Ok(self)
            }

            // These represent the prolog and epilogue of maps. This
            // situation will be resolved in the future.
            IdentObject::IdentFragment(_, IdentKind::MapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::MapTail, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapTail, _, _) => {
                Ok(self)
            }

            _ => {
                let msg = format!(
                    "identifier is not a IdentObject::Ident): {:?}",
                    self,
                );

                Err((self, TransitionError::BadFragmentDest { name: msg }))
            }
        }
    }
}

/// An error attempting to transition from one [`IdentObject`] state to
///   another.
#[derive(Clone, Debug, PartialEq)]
pub enum TransitionError {
    /// Attempted to redeclare a concrete, non-virtual identifier without an
    ///   override.
    Redeclare { name: String },

    /// Extern resolution failure.
    ///
    /// An extern could not be resolved because the provided identifier had
    ///   a type that is incompatible with the extern definition.
    ExternResolution {
        name: String,
        expected: IdentKind,
        given: IdentKind,
    },

    /// Attempt to override a non-virtual identifier.
    NonVirtualOverride { name: String },

    /// Overriding a virtual identifier failed due to an incompatible
    ///   [`IdentKind`].
    VirtualOverrideKind {
        name: String,
        existing: IdentKind,
        given: IdentKind,
    },

    /// The provided identifier is not in a state that is permitted to
    ///   receive a fragment.
    ///
    /// See [`IdentObjectState::set_fragment`].
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
    Missing { name: String },

    /// Expected identifier has not yet been resolved with a concrete
    ///   definition.
    Extern {
        /// Identifier name.
        name: String,
        /// Expected identifier type.
        kind: IdentKind,
        /// Name of package where the extern was defined.
        pkg_name: Option<String>,
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
                pkg_name.as_ref().unwrap_or(&"<unknown>".into()),
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
pub type FragmentText = String;

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
pub struct Source<'i> {
    /// Name of package containing reference to this object.
    pub pkg_name: Option<&'i Symbol<'i>>,

    /// Relative path to the source of this object,
    ///   if not present in the current package.
    pub src: Option<&'i Symbol<'i>>,

    /// The identifier from which this one is derived.
    ///
    /// See [`IdentKind`] for more information on parents.
    /// For example,
    ///   a [`IdentKind::Cgen`] always has a parent [`IdentKind::Class`].
    pub parent: Option<&'i Symbol<'i>>,

    /// Child identifier associated with this identifier.
    ///
    /// For [`IdentKind::Class`],
    ///   this represents an associated [`IdentKind::Cgen`].
    pub yields: Option<&'i Symbol<'i>>,

    /// User-friendly identifier description.
    ///
    /// This is used primarily by [`IdentKind::Class`] and
    ///   [`IdentKind::Gen`].
    pub desc: Option<String>,

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
    pub from: Option<Vec<&'i Symbol<'i>>>,

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

impl<'i> From<SymAttrs<'i>> for Source<'i> {
    /// Raise Legacy IR [`SymAttrs`].
    ///
    /// This simply extracts a subset of fields from the source attributes.
    fn from(attrs: SymAttrs<'i>) -> Self {
        Source {
            pkg_name: attrs.pkg_name,
            src: attrs.src,
            generated: attrs.generated,
            parent: attrs.parent,
            yields: attrs.yields,
            desc: attrs.desc,
            from: attrs.from,
            virtual_: attrs.virtual_,
            override_: attrs.override_,
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::ident::Dim;
    use super::*;
    use crate::sym::SymbolIndex;

    mod ident_object_data {
        use super::*;

        // Note that IdentObject has no variants capable of None
        #[test]
        fn ident_object_name() {
            let sym = symbol_dummy!(1, "sym");

            assert_eq!(Some(&sym), IdentObject::Missing(&sym).name());

            assert_eq!(
                Some(&sym),
                IdentObject::Ident(&sym, IdentKind::Meta, Source::default())
                    .name()
            );

            assert_eq!(
                Some(&sym),
                IdentObject::Extern(&sym, IdentKind::Meta, Source::default())
                    .name()
            );

            assert_eq!(
                Some(&sym),
                IdentObject::IdentFragment(
                    &sym,
                    IdentKind::Meta,
                    Source::default(),
                    FragmentText::default(),
                )
                .name()
            );
        }

        #[test]
        fn ident_object_kind() {
            let sym = symbol_dummy!(1, "sym");
            let kind = IdentKind::Class(Dim::from_u8(5));

            assert_eq!(None, IdentObject::Missing(&sym).kind());

            assert_eq!(
                Some(&kind),
                IdentObject::Ident(&sym, kind.clone(), Source::default())
                    .kind()
            );

            assert_eq!(
                Some(&kind),
                IdentObject::Extern(&sym, kind.clone(), Source::default())
                    .kind()
            );

            assert_eq!(
                Some(&kind),
                IdentObject::IdentFragment(
                    &sym,
                    kind.clone(),
                    Source::default(),
                    FragmentText::default()
                )
                .kind()
            );
        }

        #[test]
        fn ident_object_src() {
            let sym = symbol_dummy!(1, "sym");
            let src = Source {
                desc: Some("test source".into()),
                ..Default::default()
            };

            assert_eq!(None, IdentObject::Missing(&sym).src());

            assert_eq!(
                Some(&src),
                IdentObject::Ident(&sym, IdentKind::Meta, src.clone()).src()
            );

            assert_eq!(
                None,
                IdentObject::Extern(&sym, IdentKind::Meta, src.clone()).src()
            );

            assert_eq!(
                Some(&src),
                IdentObject::IdentFragment(
                    &sym,
                    IdentKind::Meta,
                    src.clone(),
                    FragmentText::default()
                )
                .src()
            );
        }

        #[test]
        fn ident_object_fragment() {
            let sym = symbol_dummy!(1, "sym");
            let text: FragmentText = "foo".into();

            assert_eq!(None, IdentObject::Missing(&sym).fragment());

            assert_eq!(
                None,
                IdentObject::Ident(&sym, IdentKind::Meta, Source::default())
                    .fragment()
            );

            assert_eq!(
                None,
                IdentObject::Extern(&sym, IdentKind::Meta, Source::default())
                    .fragment()
            );

            assert_eq!(
                Some(&text),
                IdentObject::IdentFragment(
                    &sym,
                    IdentKind::Meta,
                    Source::default(),
                    text.clone(),
                )
                .fragment()
            );
        }

        #[test]
        fn ident_object_as_ident() {
            let sym = symbol_dummy!(1, "sym");
            let ident = IdentObject::Missing(&sym);

            // Since we _are_ an IdentObject, we should return a reference
            // to ourselves.  We want this, not a clone.
            assert!(std::ptr::eq(
                &ident as *const _,
                ident.as_ident().unwrap() as *const _,
            ));
        }
    }

    mod ident_object_state {
        use super::*;

        #[test]
        fn ident_object_missing() {
            let sym = symbol_dummy!(1, "missing");
            assert_eq!(IdentObject::Missing(&sym), IdentObject::declare(&sym));
        }

        #[test]
        fn resolved_on_missing() {
            let sym = symbol_dummy!(1, "missing");

            let result = IdentObject::declare(&sym)
                .resolved()
                .expect_err("expected error asserting resolved() on missing");

            match result {
                UnresolvedError::Missing { name: e_name } => {
                    assert_eq!(sym.to_string(), e_name);
                }
                _ => panic!("expected UnresolvedError {:?}", result),
            }
        }

        #[test]
        fn ident_object_ident() {
            let sym = symbol_dummy!(1, "ident");
            let kind = IdentKind::Meta;
            let src = Source {
                desc: Some("ident ctor".into()),
                ..Default::default()
            };

            assert_eq!(
                IdentObject::Ident(&sym, kind.clone(), src.clone()),
                IdentObject::declare(&sym)
                    .resolve(kind.clone(), src.clone())
                    .unwrap(),
            );
        }

        #[test]
        fn resolved_on_ident() {
            let sym = symbol_dummy!(1, "ident resolve");
            let kind = IdentKind::Meta;
            let src = Source {
                desc: Some("ident ctor".into()),
                ..Default::default()
            };

            assert_eq!(
                &IdentObject::Ident(&sym, kind.clone(), src.clone()),
                IdentObject::declare(&sym)
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
            let sym = symbol_dummy!(1, "redecl");
            let kind = IdentKind::Meta;
            let src = Source::default();

            let first = IdentObject::declare(&sym)
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
                    assert_eq!(*sym, name);
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
                let sym = symbol_dummy!(1, "extern");
                let kind = IdentKind::Class(Dim::from_u8(1));
                let src = Source {
                    desc: Some("extern".into()),
                    ..Default::default()
                };

                assert_eq!(
                    Ok(IdentObject::Extern(&sym, kind.clone(), src.clone())),
                    IdentObject::declare(&sym).extern_(kind, src),
                );
            }

            #[test]
            fn resolved_on_extern() {
                let sym = symbol_dummy!(1, "extern resolved");
                let kind = IdentKind::Class(Dim::from_u8(1));
                let pkg_name = symbol_dummy!(2, "pkg/name");
                let src = Source {
                    pkg_name: Some(&pkg_name),
                    desc: Some("extern".into()),
                    ..Default::default()
                };

                let result =
                    IdentObject::Extern(&sym, kind.clone(), src.clone())
                        .resolved()
                        .expect_err(
                            "expected error asserting resolved() on extern",
                        );

                match result {
                    UnresolvedError::Extern {
                        name: e_name,
                        kind: e_kind,
                        pkg_name: e_pkg_name,
                    } => {
                        assert_eq!(sym.to_string(), e_name);
                        assert_eq!(kind, e_kind);
                        assert_eq!(Some(pkg_name.to_string()), e_pkg_name);
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
                let pkg = "pkg".to_string();

                let err = UnresolvedError::Extern {
                    name: "foo".into(),
                    kind: IdentKind::Meta,
                    pkg_name: Some(pkg.clone()),
                };

                let msg = format!("{}", err);

                assert!(msg.contains("`foo`"));
                assert!(msg.contains(&format!("in `{}`", pkg)));
                assert!(msg.contains(&format!("`{}`", meta)));
            }

            // Extern first, then identifier
            #[test]
            fn redeclare_compatible_resolves() {
                let sym = symbol_dummy!(1, "extern_re_pre");
                let kind = IdentKind::Class(Dim::from_u8(10));
                let src = Source {
                    desc: Some("okay".into()),
                    ..Default::default()
                };

                // Compatible kind, should resolve.
                let result = IdentObject::declare(&sym)
                    .extern_(kind.clone(), Source::default())
                    .and_then(|o| o.resolve(kind.clone(), src.clone()));

                assert_eq!(Ok(IdentObject::Ident(&sym, kind, src)), result,);
            }

            // Identifier first, then extern
            #[test]
            fn redeclare_compatible_resolves_post() {
                let sym = symbol_dummy!(1, "extern_re_post");
                let kind = IdentKind::Class(Dim::from_u8(10));
                let src = Source {
                    desc: Some("okay".into()),
                    ..Default::default()
                };

                // Compatible kind, should resolve.
                let result = IdentObject::declare(&sym)
                    .resolve(kind.clone(), src.clone())
                    .and_then(|o| o.extern_(kind.clone(), Source::default()));

                assert_eq!(Ok(IdentObject::Ident(&sym, kind, src)), result,);
            }

            #[test]
            fn redeclare_another_extern() {
                let sym = symbol_dummy!(1, "extern_extern");
                let kind = IdentKind::Class(Dim::from_u8(20));
                let src_first = Source {
                    desc: Some("first src".into()),
                    ..Default::default()
                };
                let src_second = Source {
                    desc: Some("second src".into()),
                    ..Default::default()
                };

                let result = IdentObject::declare(&sym)
                    .extern_(kind.clone(), src_first.clone())
                    .and_then(|o| o.extern_(kind.clone(), src_second));

                // Note that, if it resolves, it should keep what is
                // _existing_, meaning that it must keep the first src.
                assert_eq!(
                    Ok(IdentObject::Extern(&sym, kind, src_first)),
                    result
                );
            }

            // Extern first, then identifier
            #[test]
            fn redeclare_post_incompatible_kind() {
                let sym = symbol_dummy!(1, "extern_re_bad_post");
                let kind = IdentKind::Class(Dim::from_u8(10));
                let src = Source {
                    desc: Some("bad kind".into()),
                    ..Default::default()
                };

                let orig = IdentObject::declare(&sym)
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
                            assert_eq!(sym.to_string(), e_name);
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
                let sym = symbol_dummy!(1, "extern_re_bad_pre");
                let kind_given = IdentKind::Class(Dim::from_u8(10));
                let src = Source {
                    desc: Some("bad kind".into()),
                    ..Default::default()
                };

                let orig = IdentObject::declare(&sym)
                    .resolve(kind_given.clone(), src.clone())
                    .unwrap();

                // Extern with incompatible kind.
                let kind_extern = IdentKind::Meta;
                let result = orig
                    .clone()
                    .extern_(kind_extern.clone(), Source::default());

                match result {
                    Err((given_orig, err @ _)) => {
                        assert_eq!(orig, given_orig);

                        if let TransitionError::ExternResolution {
                            name: e_name,
                            expected: e_expected,
                            given: e_given,
                        } = err.clone()
                        {
                            assert_eq!(sym.to_string(), e_name);
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
            let sym = symbol_dummy!(1, "tofrag");
            let src = Source {
                generated: true,
                ..Default::default()
            };

            let kind = IdentKind::Meta;
            let ident = IdentObject::declare(&sym)
                .resolve(kind.clone(), src.clone())
                .unwrap();
            let text = FragmentText::from("a fragment");
            let ident_with_frag = ident.set_fragment(text.clone());

            assert_eq!(
                Ok(IdentObject::IdentFragment(&sym, kind, src, text)),
                ident_with_frag,
            );
        }

        #[test]
        fn resolved_on_fragment() {
            let sym = symbol_dummy!(1, "tofrag resolved");
            let src = Source {
                generated: true,
                ..Default::default()
            };

            let kind = IdentKind::Meta;
            let ident = IdentObject::declare(&sym)
                .resolve(kind.clone(), src.clone())
                .unwrap();
            let text = FragmentText::from("a fragment for resolved()");
            let ident_with_frag = ident.set_fragment(text.clone());

            assert_eq!(
                Ok(&IdentObject::IdentFragment(&sym, kind, src, text)),
                ident_with_frag.unwrap().resolved(),
            );
        }

        #[test]
        fn add_fragment_to_fragment_fails() {
            let sym = symbol_dummy!(1, "badsym");
            let ident = IdentObject::declare(&sym)
                .resolve(IdentKind::Meta, Source::default())
                .unwrap();

            let ident_with_frag = ident
                .set_fragment("orig fragment".into())
                .expect("set_fragment failed");

            // Since it's already a fragment, this should fail.
            let err = ident_with_frag
                .clone()
                .set_fragment("replacement".to_string())
                .expect_err("Expected failure");

            match err {
                (orig, TransitionError::BadFragmentDest { name: str })
                    if str.contains("badsym") =>
                {
                    assert_eq!(ident_with_frag, orig);
                }
                _ => panic!(
                    "expected TransitionError::BadFragmentDest: {:?}",
                    err
                ),
            }
        }

        mod override_ {
            use super::*;

            #[test]
            fn declare_virtual_ident_first() {
                let sym = symbol_dummy!(1, "virtual");
                let over_src = symbol_dummy!(2, "src");
                let kind = IdentKind::Meta;

                let virt = IdentObject::declare(&sym)
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
                    src: Some(&over_src),
                    ..Default::default()
                };

                let result = virt.resolve(kind.clone(), over_src.clone());

                // Overriding should clear any virtual flag that may have
                // been set to prevent override-overrides.
                let expected_src = Source {
                    virtual_: false,
                    ..over_src
                };

                assert_eq!(
                    Ok(IdentObject::Ident(&sym, kind, expected_src)),
                    result
                );
            }

            // Override is encountered before the virtual
            #[test]
            fn declare_virtual_ident_after_override() {
                let sym = symbol_dummy!(1, "virtual_second");
                let virt_src = symbol_dummy!(2, "virt_src");
                let kind = IdentKind::Meta;

                let over_src = Source {
                    virtual_: true, // this needn't be set, but see below
                    override_: true,
                    ..Default::default()
                };

                let over = IdentObject::declare(&sym)
                    .resolve(kind.clone(), over_src.clone())
                    .unwrap();

                let virt_src = Source {
                    virtual_: true,
                    src: Some(&virt_src),
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

                assert_eq!(
                    Ok(IdentObject::Ident(&sym, kind, expected_src)),
                    result
                );
            }

            #[test]
            fn declare_override_non_virtual() {
                let sym = symbol_dummy!(1, "non_virtual");
                let kind = IdentKind::Meta;

                let non_virt = IdentObject::declare(&sym)
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
                let bad_kind = IdentKind::Cgen(Dim::from_u8(1));

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
                        assert_eq!(*sym, *name);

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
                let sym = symbol_dummy!(1, "virtual");
                let src_sym = symbol_dummy!(2, "src");
                let kind = IdentKind::Meta;

                let virt = IdentObject::declare(&sym)
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
                    src: Some(&src_sym),
                    ..Default::default()
                };

                let bad_kind = IdentKind::Cgen(Dim::from_u8(1));
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

                        assert_eq!(*sym, *name);
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
                let sym = symbol_dummy!(1, "virtual");
                let over_src = symbol_dummy!(2, "src");
                let kind = IdentKind::Meta;

                // Remember: override is going to come first...
                let over_src = Source {
                    override_: true,
                    src: Some(&over_src),
                    ..Default::default()
                };

                // ...and virt second.
                let virt_src = Source {
                    virtual_: true,
                    ..Default::default()
                };

                let over = IdentObject::declare(&sym)
                    .resolve(kind.clone(), over_src.clone())
                    .unwrap();

                // So we should _keep_ this fragment, since it represent the
                // override, even though it's appearing first.
                let text = FragmentText::from("keep me");
                let over_frag = over.set_fragment(text.clone());

                assert_eq!(
                    Ok(IdentObject::IdentFragment(
                        &sym,
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
                    Ok(IdentObject::IdentFragment(
                        &sym,
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
                    Ok(IdentObject::IdentFragment(
                        &sym,
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
                let sym = symbol_dummy!(1, "virtual");
                let over_src = symbol_dummy!(2, "src");
                let kind = IdentKind::Meta;

                let virt_src = Source {
                    virtual_: true,
                    ..Default::default()
                };

                let virt = IdentObject::declare(&sym)
                    .resolve(kind.clone(), virt_src.clone())
                    .unwrap();
                let text = FragmentText::from("remove me");
                let virt_frag = virt.set_fragment(text.clone());

                assert_eq!(
                    Ok(IdentObject::IdentFragment(
                        &sym,
                        kind.clone(),
                        virt_src,
                        text
                    )),
                    virt_frag,
                );

                let over_src = Source {
                    override_: true,
                    src: Some(&over_src),
                    ..Default::default()
                };

                let result =
                    virt_frag.unwrap().resolve(kind.clone(), over_src.clone());

                // The act of overriding the object should have cleared any
                // existing fragment, making way for a new fragment to take its
                // place as soon as it is discovered.  (So, back to an
                // IdentObject::Ident.)
                assert_eq!(
                    Ok(IdentObject::Ident(&sym, kind, over_src)),
                    result
                );
            }

            #[test]
            fn declare_override_virtual_ident_fragment_incompatible_type() {
                let sym = symbol_dummy!(1, "virtual");
                let over_src = symbol_dummy!(2, "src");
                let kind = IdentKind::Meta;

                let virt_src = Source {
                    virtual_: true,
                    ..Default::default()
                };

                let virt = IdentObject::declare(&sym)
                    .resolve(kind.clone(), virt_src.clone())
                    .unwrap();
                let virt_frag = virt.set_fragment("".into()).unwrap();

                let over_src = Source {
                    override_: true,
                    src: Some(&over_src),
                    ..Default::default()
                };

                let bad_kind = IdentKind::Cgen(Dim::from_u8(1));
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

                        assert_eq!(*sym, *name);
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
            let sym = symbol_dummy!(1, "tofrag");
            let src = Source {
                generated: true,
                ..Default::default()
            };

            let obj = IdentObject::declare(&sym)
                .resolve(given, src.clone())
                .unwrap();

            let fragment = "a fragment".to_string();
            let obj_with_frag = obj.set_fragment(fragment.clone());

            assert_eq!(
                Ok(IdentObject::IdentFragment(&sym, expected, src, fragment)),
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

    #[test]
    fn source_from_sym_attrs() {
        let nsym = symbol_dummy!(1, "name");
        let ssym = symbol_dummy!(2, "src");
        let psym = symbol_dummy!(3, "parent");
        let ysym = symbol_dummy!(4, "yields");
        let fsym = symbol_dummy!(5, "from");

        let attrs = SymAttrs {
            pkg_name: Some(&nsym),
            src: Some(&ssym),
            generated: true,
            parent: Some(&psym),
            yields: Some(&ysym),
            desc: Some("sym desc".to_string()),
            from: Some(vec![&fsym]),
            virtual_: true,
            override_: true,
            ..Default::default()
        };

        assert_eq!(
            Source {
                pkg_name: Some(&nsym),
                src: Some(&ssym),
                generated: attrs.generated,
                parent: attrs.parent,
                yields: attrs.yields,
                desc: Some("sym desc".to_string()),
                from: Some(vec![&fsym]),
                virtual_: true,
                override_: true,
            },
            attrs.into(),
        );
    }
}
