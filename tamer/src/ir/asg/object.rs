// Objects represented on ASG
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
    Extern(&'i Symbol<'i>, IdentKind),

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
///       through the use of [`ident`](IdentObjectState::ident).
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

impl<'i> IdentObjectData<'i> for IdentObject<'i> {
    fn name(&self) -> Option<&'i Symbol<'i>> {
        match self {
            Self::Missing(name)
            | Self::Ident(name, _, _)
            | Self::Extern(name, _)
            | Self::IdentFragment(name, _, _, _) => Some(name),
        }
    }

    fn kind(&self) -> Option<&IdentKind> {
        match self {
            Self::Missing(_) => None,
            Self::Ident(_, kind, _)
            | Self::Extern(_, kind)
            | Self::IdentFragment(_, kind, _, _) => Some(kind),
        }
    }

    fn src(&self) -> Option<&Source<'i>> {
        match self {
            Self::Missing(_) | Self::Extern(_, _) => None,
            Self::Ident(_, _, src) | Self::IdentFragment(_, _, src, _) => {
                Some(src)
            }
        }
    }

    fn fragment(&self) -> Option<&FragmentText> {
        match self {
            Self::Missing(_) | Self::Ident(_, _, _) | Self::Extern(_, _) => {
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
    fn missing(ident: &'i Symbol<'i>) -> T;

    /// Produce an object representing a concrete identifier.
    fn ident(name: &'i Symbol<'i>, kind: IdentKind, src: Source<'i>) -> T;

    /// Produce an object representing an extern.
    fn extern_(name: &'i Symbol<'i>, kind: IdentKind) -> T;

    /// Attempt to redeclare an identifier with additional information.
    ///
    /// For specific information on compatibility rules,
    ///   see implementers of this trait,
    ///   since rules may vary between implementations.
    fn redeclare(self, kind: IdentKind, src: Source<'i>)
        -> TransitionResult<T>;

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
    fn missing(ident: &'i Symbol<'i>) -> Self {
        IdentObject::Missing(ident)
    }

    fn ident(name: &'i Symbol<'i>, kind: IdentKind, src: Source<'i>) -> Self {
        IdentObject::Ident(name, kind, src)
    }

    fn extern_(name: &'i Symbol<'i>, kind: IdentKind) -> Self {
        IdentObject::Extern(name, kind)
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
    /// The kind of identifier cannot change,
    ///   but the argument is provided here for convenience so that the
    ///   caller does not need to perform such a check itself.
    fn redeclare(
        mut self,
        kind: IdentKind,
        src: Source<'i>,
    ) -> TransitionResult<IdentObject<'i>> {
        match self {
            IdentObject::Ident(_, _, ref mut orig_src)
                if orig_src.virtual_ && src.override_ =>
            {
                *orig_src = src;
                Ok(self)
            }

            // TODO: no override-override
            IdentObject::IdentFragment(name, _, orig_src, _)
                if orig_src.virtual_ && src.override_ =>
            {
                // clears fragment, which is no longer applicable
                Ok(IdentObject::Ident(name, kind, src))
            }

            IdentObject::Missing(name) | IdentObject::Ident(name, _, _) => {
                Ok(IdentObject::Ident(name, kind, src))
            }

            // TODO: incompatible (check now-dangling commits)
            _ => Ok(self),
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

            IdentObject::IdentFragment(_, IdentKind::MapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::MapTail, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapHead, _, _)
            | IdentObject::IdentFragment(_, IdentKind::RetMapTail, _, _) => {
                Ok(self)
            }

            // TODO remove these ignores when fixed
            IdentObject::IdentFragment(
                sym,
                IdentKind::Map,
                Source {
                    virtual_: true,
                    override_: true,
                    ..
                },
                _,
            ) => {
                eprintln!(
                    "ignoring virtual and overridden map object: {}",
                    sym
                );
                Ok(self)
            }
            IdentObject::IdentFragment(
                sym,
                IdentKind::RetMap,
                Source {
                    virtual_: true,
                    override_: true,
                    ..
                },
                _,
            ) => {
                eprintln!(
                    "ignoring virtual and overridden retmap object: {}",
                    sym
                );
                Ok(self)
            }

            _ => {
                let msg = format!(
                    "identifier is not a IdentObject::Ident): {:?}",
                    self,
                );

                Err((self, TransitionError::BadFragmentDest(msg)))
            }
        }
    }
}

/// An error attempting to transition from one [`IdentObject`] state to
///   another.
///
/// TODO: Provide enough information to construct a useful message.
#[derive(Debug, PartialEq)]
pub enum TransitionError {
    /// An attempt to redeclare an identifier with additional information
    ///   has failed because the provided information was not compatible
    ///   with the original declaration.
    ///
    /// See [`IdentObjectState::redeclare`].
    Incompatible(String),

    /// The provided identifier is not in a state that is permitted to
    ///   receive a fragment.
    ///
    /// See [`IdentObjectState::set_fragment`].
    BadFragmentDest(String),
}

impl std::fmt::Display for TransitionError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Incompatible(msg) => {
                write!(fmt, "object incompatible: {}", msg)
            }

            Self::BadFragmentDest(msg) => {
                write!(fmt, "bad fragment destination: {}", msg)
            }
        }
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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");

            assert_eq!(Some(&sym), IdentObject::Missing(&sym).name());

            assert_eq!(
                Some(&sym),
                IdentObject::Ident(&sym, IdentKind::Meta, Source::default())
                    .name()
            );

            assert_eq!(
                Some(&sym),
                IdentObject::Extern(&sym, IdentKind::Meta).name()
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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
            let kind = IdentKind::Class(Dim::from_u8(5));

            assert_eq!(None, IdentObject::Missing(&sym).kind());

            assert_eq!(
                Some(&kind),
                IdentObject::Ident(&sym, kind.clone(), Source::default())
                    .kind()
            );

            assert_eq!(
                Some(&kind),
                IdentObject::Extern(&sym, kind.clone()).kind()
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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
            let src = Source {
                desc: Some("test source".into()),
                ..Default::default()
            };

            assert_eq!(None, IdentObject::Missing(&sym).src());

            assert_eq!(
                Some(&src),
                IdentObject::Ident(&sym, IdentKind::Meta, src.clone()).src()
            );

            assert_eq!(None, IdentObject::Extern(&sym, IdentKind::Meta).src());

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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
            let text: FragmentText = "foo".into();

            assert_eq!(None, IdentObject::Missing(&sym).fragment());

            assert_eq!(
                None,
                IdentObject::Ident(&sym, IdentKind::Meta, Source::default())
                    .fragment()
            );

            assert_eq!(
                None,
                IdentObject::Extern(&sym, IdentKind::Meta).fragment()
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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
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
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "missing");
            assert_eq!(IdentObject::Missing(&sym), IdentObject::missing(&sym));
        }

        #[test]
        fn ident_object_ident() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "missing");
            let kind = IdentKind::Meta;
            let src = Source {
                desc: Some("ident ctor".into()),
                ..Default::default()
            };

            assert_eq!(
                IdentObject::Ident(&sym, kind.clone(), src.clone()),
                IdentObject::ident(&sym, kind.clone(), src.clone()),
            );
        }

        #[test]
        fn ident_object_extern() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "missing");
            let kind = IdentKind::Class(Dim::from_u8(1));

            assert_eq!(
                IdentObject::Extern(&sym, kind.clone()),
                IdentObject::extern_(&sym, kind.clone()),
            );
        }

        // TODO: incompatible
        #[test]
        fn redeclare_returns_existing_compatible() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "symdup");

            let first =
                IdentObject::ident(&sym, IdentKind::Meta, Source::default());

            // Same declaration a second time
            assert_eq!(
                Ok(first.clone()),
                first.clone().redeclare(
                    first.kind().unwrap().clone(),
                    first.src().unwrap().clone(),
                )
            );
        }

        #[test]
        fn add_fragment_to_ident() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "tofrag");
            let src = Source {
                generated: true,
                ..Default::default()
            };

            let kind = IdentKind::Meta;
            let ident = IdentObject::ident(&sym, kind.clone(), src.clone());
            let text = FragmentText::from("a fragment");
            let ident_with_frag = ident.set_fragment(text.clone());

            assert_eq!(
                Ok(IdentObject::IdentFragment(&sym, kind, src, text)),
                ident_with_frag,
            );
        }

        #[test]
        fn add_fragment_to_fragment_fails() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "badsym");
            let ident =
                IdentObject::ident(&sym, IdentKind::Meta, Source::default());

            let ident_with_frag = ident
                .set_fragment("orig fragment".into())
                .expect("set_fragment failed");

            // Since it's already a fragment, this should fail.
            let err = ident_with_frag
                .clone()
                .set_fragment("replacement".to_string())
                .expect_err("Expected failure");

            match err {
                (orig, TransitionError::BadFragmentDest(str))
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

        // TODO: incompatible
        #[test]
        fn declare_override_virtual_ident() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "virtual");
            let over_src = Symbol::new_dummy(SymbolIndex::from_u32(2), "src");
            let kind = IdentKind::Meta;

            let virt = IdentObject::ident(
                &sym,
                kind.clone(),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            );

            let over_src = Source {
                override_: true,
                src: Some(&over_src),
                ..Default::default()
            };

            let result = virt.redeclare(kind.clone(), over_src.clone());

            assert_eq!(Ok(IdentObject::Ident(&sym, kind, over_src)), result);
        }

        // TODO: incompatible
        #[test]
        fn declare_override_virtual_ident_fragment() {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "virtual");
            let over_src = Symbol::new_dummy(SymbolIndex::from_u32(2), "src");
            let kind = IdentKind::Meta;

            let virt_src = Source {
                virtual_: true,
                ..Default::default()
            };

            let virt = IdentObject::ident(&sym, kind.clone(), virt_src.clone());
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
                virt_frag.unwrap().redeclare(kind.clone(), over_src.clone());

            // The act of overriding the object should have cleared any
            // existing fragment, making way for a new fragment to take its
            // place as soon as it is discovered.  (So, back to an
            // IdentObject::Ident.)
            assert_eq!(Ok(IdentObject::Ident(&sym, kind, over_src)), result);
        }

        fn add_ident_kind_ignores(given: IdentKind, expected: IdentKind) {
            let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "tofrag");
            let src = Source {
                generated: true,
                ..Default::default()
            };

            let obj = IdentObject::ident(&sym, given, src.clone());

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
        let nsym = Symbol::new_dummy(SymbolIndex::from_u32(1), "name");
        let ssym = Symbol::new_dummy(SymbolIndex::from_u32(2), "src");
        let psym = Symbol::new_dummy(SymbolIndex::from_u32(3), "parent");
        let ysym = Symbol::new_dummy(SymbolIndex::from_u32(4), "yields");
        let fsym = Symbol::new_dummy(SymbolIndex::from_u32(5), "from");

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
