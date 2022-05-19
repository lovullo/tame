// Read from xmlo and immediately lower to ASG IR
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

//! Lower [`xmlo` IR](crate::obj::xmlo) into [`Asg`].
//!
//! [`AsgBuilder`] is exclusively responsible for this lowering operation
//!   within the context of [`xmlo` object files](super).
//!
//! Usage
//! =====
//! [`AsgBuilder`] accepts any [`Iterator`] of [`XmloToken`]s,
//!   but is intended to be paired with [`XmloReader`](super::XmloReader).
//!
//! To fully load an `xmlo` file and its dependencies,
//!   begin with [`AsgBuilder::import_xmlo`] and an empty
//!   [`AsgBuilderState`].
//! Each call accepts the previous state and returns a new state that may be
//!   used both for introspection and for the next call to `import_xmlo`.
//! [`AsgBuilderState::found`] can be used to recursively load relative
//!   package paths;
//!     it is wrapped in an [`Option`] so that [`take`](Option::take) can be
//!       used to take ownership over the data.

use super::{
    reader::{XmloResult, XmloToken},
    SymAttrs, SymType, XmloError,
};
use crate::{
    asg::{Asg, AsgError, IdentKind, Source},
    sym::SymbolId,
};
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::hash::BuildHasher;
use std::{collections::HashSet, result};

pub type Result<S> = std::result::Result<AsgBuilderState<S>, AsgBuilderError>;

/// Builder state between imports.
///
/// [`AsgBuilder::import_xmlo`] is designed to return its state after having
///   been invoked,
///     which contains in part a set of relative package import paths that
///     were discovered during processing (`found`).
/// This state can then be fed back to `import_xmlo` when recursively
///   importing those packages.
///
/// The values [`name`](AsgBuilderState::name) and
///   [`relroot`](AsgBuilderState::relroot) are set only once when the first
///   package is processed.
/// A package is considered to be the first if `name` is [`None`].
#[derive(Debug, Default)]
pub struct AsgBuilderState<S>
where
    S: BuildHasher,
{
    /// Relative paths to imported packages that have been discovered.
    ///
    /// The caller will use these to perform recursive loads.
    /// This is contained within an [`Option`] so that the caller can `take`
    ///   ownership over its contents.
    ///
    /// See [`AsgBuilder::import_xmlo`] for behavior when this value is
    ///   [`None`].
    pub found: Option<HashSet<SymbolId, S>>,

    /// Program name once discovered.
    ///
    /// This will be set by the first package encountered.
    pub name: Option<SymbolId>,

    /// Relative path to project root once discovered.
    ///
    /// This will be set by the first package encountered.
    pub relroot: Option<SymbolId>,
}

impl<S> AsgBuilderState<S>
where
    S: BuildHasher + Default,
{
    /// Create a new, empty state.
    pub fn new() -> Self {
        Default::default()
    }

    /// Whether this is the first discovered package.
    ///
    /// This is true if [`name`](AsgBuilderState::name) is [`None`].
    fn is_first(&self) -> bool {
        self.name.is_none()
    }
}

/// Populate [`Asg`] with data derived from `xmlo` files.
///
/// This accepts anything capable of iterating over [`XmloResult`].
///
/// For more information on what data are processed,
///   see [`AsgBuilderState`].
/// See the [module-level documentation](self) for example usage.
pub trait AsgBuilder<S>
where
    S: BuildHasher,
{
    /// Import [`XmloResult`]s into an [`Asg`].
    ///
    /// This is an IR lowering operation.
    /// The [`XmloResult`] produces data gleaned from
    ///   [`xmlo`](crate::obj::xmlo),
    ///     and this process lowers it into the IR [`asg`](crate::asg).
    ///
    /// Each call to this method augments the provided [`AsgBuilderState`];
    ///   see its documentation for more information.
    /// Its initial value can be provided as [`Default::default`].
    fn import_xmlo(
        &mut self,
        xmlo: impl Iterator<Item = XmloResult<XmloToken>>,
        state: AsgBuilderState<S>,
    ) -> Result<S>;
}

/// Internal state machine for [`AsgBuilder`].
///
/// This will likely be worked into [`AsgBuilderState`] eventually and
/// exists during a transition to the streaming parsers.
#[derive(Debug, Clone, Copy)]
enum AsgBuilderInternalState {
    /// The "old way" of doing things; not yet refactored.
    None,

    /// Processing symbol dependencies.
    SymDep(SymbolId),
}

impl<S> AsgBuilder<S> for Asg
where
    S: BuildHasher + Default,
{
    fn import_xmlo(
        &mut self,
        mut xmlo: impl Iterator<Item = XmloResult<XmloToken>>,
        mut state: AsgBuilderState<S>,
    ) -> Result<S> {
        let mut elig = None;
        let first = state.is_first();
        let found = state.found.get_or_insert(Default::default());

        // Package currently being processed.
        let mut pkg_name = None;

        use AsgBuilderInternalState as IS;
        let mut istate = IS::None;

        while let Some(ev) = xmlo.next() {
            match (istate, ev?) {
                (IS::None, XmloToken::PkgName(name)) => {
                    pkg_name = Some(name);

                    if first {
                        state.name = Some(name);
                    }
                }

                (IS::None, XmloToken::PkgRootPath(relroot)) => {
                    if first {
                        state.relroot = Some(relroot);
                    }
                }

                (IS::None, XmloToken::PkgEligClassYields(pkg_elig)) => {
                    elig = Some(pkg_elig);
                }

                (IS::None, XmloToken::PkgProgramFlag) => {
                    // Unused
                }

                (IS::None | IS::SymDep(_), XmloToken::SymDepStart(sym, _)) => {
                    istate = IS::SymDep(sym);
                }

                (IS::SymDep(sym), XmloToken::Symbol(dep_sym, _)) => {
                    self.add_dep_lookup(sym, dep_sym);
                }

                (IS::None, XmloToken::SymDecl(sym, attrs, _span)) => {
                    if let Some(sym_src) = attrs.src {
                        found.insert(sym_src);
                    } else {
                        let extern_ = attrs.extern_;
                        let kindval = (&attrs).try_into()?;

                        let mut src: Source = attrs.into();

                        // This used to come from SymAttrs in the old XmloReader.
                        if src.pkg_name.is_none() {
                            src.pkg_name = pkg_name;
                        }

                        // Existing convention is to omit @src of local package
                        // (in this case, the program being linked)
                        if first {
                            src.pkg_name = None;
                        }
                        if extern_ {
                            self.declare_extern(sym, kindval, src)?;
                        } else {
                            self.declare(sym, kindval, src)?;
                        }
                    }
                }

                // Fragments follow SymDeps.
                (
                    IS::None | IS::SymDep(_),
                    XmloToken::Fragment(sym, text, _),
                ) => {
                    istate = IS::None;
                    self.set_fragment(sym, text)?;
                }

                // We don't need to read any further than the end of the
                // header (symtable, sym-deps, fragments).  Note that this
                // may change in the future, in which case this
                // responsibility can be delegated to the linker (to produce
                // an `Iterator` that stops at EOH).
                (IS::None, XmloToken::Eoh(_)) => break,

                (istate, ev) => {
                    todo!("unexpected state transition: {istate:?} -> {ev:?}")
                }
            }
        }

        if let Some(elig_sym) = elig {
            self.add_root(
                self.lookup(elig_sym)
                    .ok_or(AsgBuilderError::BadEligRef(elig_sym))?,
            );
        }

        Ok(state)
    }
}

/// Error populating graph with [`XmloResult`]-derived data.
#[derive(Debug, PartialEq)]
pub enum AsgBuilderError {
    /// Error with the source `xmlo` file.
    XmloError(XmloError),

    /// Symbol type was not provided.
    MissingType,

    /// Number of symbol dimensions were not provided.
    MissingDim,

    /// Symbol dtype was not provided.
    MissingDtype,

    /// [`Asg`] operation error.
    AsgError(AsgError),

    /// Eligibility classification references unknown identifier.
    ///
    /// This is generated by the compiler and so should never happen.
    /// (That's not to say that it won't, but it shouldn't.)
    BadEligRef(SymbolId),
}

impl Display for AsgBuilderError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::XmloError(e) => e.fmt(fmt),
            Self::MissingType => write!(fmt, "missing symbol type"),
            Self::MissingDim => write!(fmt, "missing dim"),
            Self::MissingDtype => write!(fmt, "missing dtype"),
            Self::AsgError(e) => e.fmt(fmt),
            Self::BadEligRef(name) => write!(
                fmt,
                "internal error: package elig references nonexistant symbol `{}`",
                name,
            ),
        }
    }
}

impl From<XmloError> for AsgBuilderError {
    fn from(src: XmloError) -> Self {
        Self::XmloError(src)
    }
}

impl From<AsgError> for AsgBuilderError {
    fn from(src: AsgError) -> Self {
        Self::AsgError(src)
    }
}

impl Error for AsgBuilderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::XmloError(e) => Some(e),
            Self::AsgError(e) => Some(e),
            _ => None,
        }
    }
}

impl TryFrom<SymAttrs> for IdentKind {
    type Error = AsgBuilderError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: SymAttrs) -> result::Result<Self, Self::Error> {
        Self::try_from(&attrs)
    }
}

impl TryFrom<&SymAttrs> for IdentKind {
    type Error = AsgBuilderError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: &SymAttrs) -> result::Result<Self, Self::Error> {
        let ty = attrs.ty.as_ref().ok_or(Self::Error::MissingType)?;

        macro_rules! ident {
            ($to:expr) => {
                Ok($to)
            };
            ($to:expr, dim) => {
                Ok($to(attrs.dim.ok_or(Self::Error::MissingDim)?))
            };
            ($to:expr, dtype) => {
                Ok($to(attrs.dtype.ok_or(Self::Error::MissingDtype)?))
            };
            ($to:expr, dim, dtype) => {
                Ok($to(
                    attrs.dim.ok_or(Self::Error::MissingDim)?,
                    attrs.dtype.ok_or(Self::Error::MissingDtype)?,
                ))
            };
        }

        match ty {
            SymType::Cgen => ident!(Self::Cgen, dim),
            SymType::Class => ident!(Self::Class, dim),
            SymType::Const => ident!(Self::Const, dim, dtype),
            SymType::Func => ident!(Self::Func, dim, dtype),
            SymType::Gen => ident!(Self::Gen, dim, dtype),
            SymType::Lparam => ident!(IdentKind::Lparam, dim, dtype),
            SymType::Param => ident!(IdentKind::Param, dim, dtype),
            SymType::Rate => ident!(IdentKind::Rate, dtype),
            SymType::Tpl => ident!(IdentKind::Tpl),
            SymType::Type => ident!(IdentKind::Type, dtype),
            SymType::MapHead => ident!(IdentKind::MapHead),
            SymType::Map => ident!(IdentKind::Map),
            SymType::MapTail => ident!(IdentKind::MapTail),
            SymType::RetMapHead => ident!(IdentKind::RetMapHead),
            SymType::RetMap => ident!(IdentKind::RetMap),
            SymType::RetMapTail => ident!(IdentKind::RetMapTail),
            SymType::Meta => ident!(IdentKind::Meta),
            SymType::Worksheet => ident!(IdentKind::Worksheet),
        }
    }
}

// These tests are coupled with BaseAsg, which is not ideal.
#[cfg(test)]
mod test {
    use super::*;
    use crate::asg::{DefaultAsg, FragmentText, IdentKind, IdentObject};
    use crate::num::{Dim, Dtype};
    use crate::obj::xmlo::{SymAttrs, SymType};
    use crate::span::{DUMMY_SPAN, UNKNOWN_SPAN};
    use crate::sym::GlobalSymbolIntern;
    use std::collections::hash_map::RandomState;

    type Sut = DefaultAsg;
    type SutState<'i> = AsgBuilderState<RandomState>;

    #[test]
    fn gets_data_from_package_event() {
        let mut sut = Sut::new();

        let name = "name".intern();
        let relroot = "some/path".into();

        let evs = vec![
            Ok(XmloToken::PkgName(name)),
            Ok(XmloToken::PkgRootPath(relroot)),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        assert_eq!(Some(name), state.name);
        assert_eq!(Some(relroot), state.relroot);
    }

    #[test]
    fn xmlo_error_returned() {
        let mut sut = Sut::new();

        let evs = vec![Err(XmloError::UnassociatedSym(DUMMY_SPAN))];
        let result = sut.import_xmlo(evs.into_iter(), SutState::new());

        assert_eq!(
            AsgBuilderError::XmloError(XmloError::UnassociatedSym(DUMMY_SPAN)),
            result.expect_err("expected error to be proxied"),
        );
    }

    #[test]
    fn adds_elig_as_root() {
        let mut sut = Sut::new();
        let elig_sym = "elig".intern();

        // The symbol must be on the graph, or it'll fail.
        let elig_node = sut
            .declare(elig_sym, IdentKind::Meta, Default::default())
            .unwrap();

        let evs = vec![Ok(XmloToken::PkgEligClassYields(elig_sym))];

        sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // TODO: Graph should be encapsulated.
        sut.graph.contains_edge(sut.root(), elig_node.into());
    }

    #[test]
    fn adds_sym_deps() {
        let mut sut = Sut::new();

        let sym_from = "from".intern();
        let sym_to1 = "to1".intern();
        let sym_to2 = "to2".intern();

        let evs = vec![
            Ok(XmloToken::SymDepStart(sym_from, DUMMY_SPAN)),
            Ok(XmloToken::Symbol(sym_to1, DUMMY_SPAN)),
            Ok(XmloToken::Symbol(sym_to2, DUMMY_SPAN)),
        ];

        let _ = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect("unexpected failure");

        let node_from = sut.lookup(sym_from).expect("from node not added");
        let node_to1 = sut.lookup(sym_to1).expect("to1 node not added");
        let node_to2 = sut.lookup(sym_to2).expect("to2 node not added");

        assert!(sut.has_dep(node_from, node_to1));
        assert!(sut.has_dep(node_from, node_to2));
    }

    #[test]
    fn sym_decl_with_src_not_added_and_populates_found() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let src_a = "src_a".intern();
        let src_b = "src_b".intern();

        let evs = vec![
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    src: Some(src_a),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    src: Some(src_b),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
        ];

        let mut founds = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect("unexpected failure")
            .found
            .unwrap()
            .iter()
            .map(|s| *s)
            .collect::<Vec<_>>();

        // Just to remove nondeterminism in case the iteration order happens
        // to change (we're using RandomState).
        founds.sort();

        assert_eq!(vec![src_a, src_b], founds);

        // Symbols with `src` set are external and should not be added to
        // the graph.
        assert!(sut.lookup(sym).is_none());
    }

    #[test]
    fn sym_decl_added_to_graph() {
        let mut sut = Sut::new();

        let sym_extern = "sym_extern".intern();
        let sym_non_extern = "sym_non_extern".intern();
        let sym_map = "sym_map".intern();
        let sym_retmap = "sym_retmap".intern();
        let pkg_name = "pkg name".intern();

        let evs = vec![
            // Note that externs should not be recognized as roots even if
            // their type would be.
            Ok(XmloToken::SymDecl(
                sym_extern,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            // These three will be roots
            Ok(XmloToken::SymDecl(
                sym_non_extern,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloToken::SymDecl(
                sym_map,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloToken::SymDecl(
                sym_retmap,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::RetMap),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // Both above symbols were local (no `src`).
        assert!(state.found.unwrap().len() == 0);

        // TODO: Graph should be encapsulated.
        let root = sut.root();
        assert!(sut
            .graph
            .contains_edge(root, sut.lookup(sym_non_extern).unwrap().into()));
        assert!(sut
            .graph
            .contains_edge(root, sut.lookup(sym_map).unwrap().into()));
        assert!(sut
            .graph
            .contains_edge(root, sut.lookup(sym_retmap).unwrap().into()));

        // Note that each of these will have their package names cleared
        // since this is considered to be the first package encountered.

        assert_eq!(
            &IdentObject::Extern(
                sym_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym_extern).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                sym_non_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym_non_extern).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                sym_map,
                IdentKind::Map,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym_map).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                sym_retmap,
                IdentKind::RetMap,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym_retmap).unwrap()).unwrap(),
        );
    }

    // See above test, where pkg_name was cleared.
    #[test]
    fn sym_decl_pkg_name_retained_if_not_first() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let pkg_name = "pkg name".intern();

        // This is all that's needed to not consider this to be the first
        // package, so that pkg_name is retained below.
        let state = AsgBuilderState::<RandomState> {
            name: Some(pkg_name),
            ..Default::default()
        };

        let evs = vec![Ok(XmloToken::SymDecl(
            sym,
            SymAttrs {
                pkg_name: Some(pkg_name),
                ty: Some(SymType::Meta),
                ..Default::default()
            },
            UNKNOWN_SPAN,
        ))];

        let _ = sut.import_xmlo(evs.into_iter(), state).unwrap();

        assert_eq!(
            // `pkg_name` retained
            &IdentObject::Ident(
                sym,
                IdentKind::Meta,
                Source {
                    pkg_name: Some(pkg_name),
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym).unwrap()).unwrap(),
        );
    }

    // This used to be set in SymAttrs by XmloReader,
    //   but that's no longer true with the new reader.
    #[test]
    fn sym_decl_pkg_name_set_if_empty_and_not_first() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let pkg_name = "pkg name".intern();

        let state = AsgBuilderState::<RandomState> {
            name: Some("first pkg".into()),
            ..Default::default()
        };

        let evs = vec![
            Ok(XmloToken::PkgName(pkg_name)),
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
        ];

        let _ = sut.import_xmlo(evs.into_iter(), state).unwrap();

        assert_eq!(
            // `pkg_name` retained
            &IdentObject::Ident(
                sym,
                IdentKind::Meta,
                Source {
                    pkg_name: Some(pkg_name),
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(sym).unwrap()).unwrap(),
        );
    }

    #[test]
    fn ident_kind_conversion_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let bad_attrs = SymAttrs::default();

        let evs = vec![Ok(XmloToken::SymDecl(sym, bad_attrs, UNKNOWN_SPAN))];

        sut.import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected IdentKind conversion error");
    }

    #[test]
    fn declare_extern_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();

        let evs = vec![
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            // Incompatible
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    extern_: true,
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
        ];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected extern error");

        assert!(matches!(result, AsgBuilderError::AsgError(_)));
    }

    #[test]
    fn declare_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();

        let evs = vec![
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            // Redeclare
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
        ];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected declare error");

        assert!(matches!(result, AsgBuilderError::AsgError(_)));
    }

    #[test]
    fn sets_fragment() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let frag = FragmentText::from("foo");

        let evs = vec![
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                DUMMY_SPAN,
            )),
            Ok(XmloToken::Fragment(sym, frag.clone(), DUMMY_SPAN)),
        ];

        let _ = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        let node = sut
            .lookup(sym)
            .expect("ident/fragment was not added to graph");

        assert_eq!(
            Some(&IdentObject::IdentFragment(
                sym,
                IdentKind::Meta,
                Default::default(),
                frag
            )),
            sut.get(node),
        );
    }

    #[test]
    fn error_missing_ident_for_fragment() {
        let mut sut = Sut::new();

        let sym = "sym".intern();

        // Note: missing `SymDecl`.
        let evs = vec![Ok(XmloToken::Fragment(sym, "foo".into(), DUMMY_SPAN))];

        sut.import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected error for fragment without ident");
    }

    #[test]
    fn fragment_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let frag = FragmentText::from("foo");

        let evs = vec![
            Ok(XmloToken::SymDecl(
                sym,
                SymAttrs {
                    // Invalid fragment destination
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                DUMMY_SPAN,
            )),
            Ok(XmloToken::Fragment(sym, frag.clone(), DUMMY_SPAN)),
        ];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected fragment set failure");

        assert!(matches!(
            result,
            AsgBuilderError::AsgError(AsgError::ObjectTransition(_))
        ));

        let node = sut
            .lookup(sym)
            .expect("ident/fragment was not added to graph");

        // The identifier should not have been modified on failure.
        assert!(matches!(
            sut.get(node).unwrap(),
            IdentObject::Extern(_, _, _)
        ));
    }

    #[test]
    fn stops_at_eoh() {
        let mut sut = Sut::new();

        let pkg_name = "pkg name".intern();

        let evs = vec![
            // Stop here.
            Ok(XmloToken::Eoh(DUMMY_SPAN)),
            // Shouldn't make it to this one.
            Ok(XmloToken::PkgName(pkg_name)),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // Should still be true because we didn't get to the `PkgName`
        // event.
        assert!(state.is_first());
    }

    macro_rules! test_kind {
        ($name:ident, $src:expr => $dest:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    Ok($dest),
                    SymAttrs {
                        ty: Some($src),
                        ..Default::default()
                    }
                    .try_into()
                );
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim) => {
            #[test]
            fn $name() {
                let dim = Dim::Vector;

                assert_eq!(
                    Ok($dest(Dim::Vector)),
                    SymAttrs {
                        ty: Some($src),
                        dim: Some(dim),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(AsgBuilderError::MissingDim, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dtype) => {
            #[test]
            fn $name() {
                let dtype = Dtype::Float;

                assert_eq!(
                    Ok($dest(dtype)),
                    SymAttrs {
                        ty: Some($src),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dtype
                let result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(AsgBuilderError::MissingDtype, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim, dtype) => {
            #[test]
            fn $name() {
                let dim = Dim::Vector;
                let dtype = Dtype::Float;

                assert_eq!(
                    Ok($dest(Dim::Vector, dtype)),
                    SymAttrs {
                        ty: Some($src),
                        dim: Some(dim),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let dim_result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    dtype: Some(dtype),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(AsgBuilderError::MissingDim, dim_result);

                // no dtype
                let dtype_result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    dim: Some(dim),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(AsgBuilderError::MissingDtype, dtype_result);
            }
        };
    }

    test_kind!(cgen, SymType::Cgen => IdentKind::Cgen, dim);
    test_kind!(class, SymType::Class => IdentKind::Class, dim);
    test_kind!(r#const, SymType::Const => IdentKind::Const, dim, dtype);
    test_kind!(func, SymType::Func => IdentKind::Func, dim, dtype);
    test_kind!(gen, SymType::Gen => IdentKind::Gen, dim, dtype);
    test_kind!(lparam, SymType::Lparam => IdentKind::Lparam, dim, dtype);
    test_kind!(param, SymType::Param => IdentKind::Param, dim, dtype);
    test_kind!(rate, SymType::Rate => IdentKind::Rate, dtype);
    test_kind!(tpl, SymType::Tpl => IdentKind::Tpl);
    test_kind!(r#type, SymType::Type => IdentKind::Type, dtype);
    test_kind!(maphead, SymType::MapHead => IdentKind::MapHead);
    test_kind!(map, SymType::Map => IdentKind::Map);
    test_kind!(maptail, SymType::MapTail => IdentKind::MapTail);
    test_kind!(retmaphead, SymType::RetMapHead => IdentKind::RetMapHead);
    test_kind!(retmap, SymType::RetMap => IdentKind::RetMap);
    test_kind!(retmaptail, SymType::RetMapTail => IdentKind::RetMapTail);
    test_kind!(meta, SymType::Meta => IdentKind::Meta);
    test_kind!(worksheet, SymType::Worksheet => IdentKind::Worksheet);
}
