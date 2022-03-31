// Read from xmlo and immediately lower to ASG IR
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

//! Lower [`xmlo` IR](crate::obj::xmlo) into [`Asg`].
//!
//! [`AsgBuilder`] is exclusively responsible for this lowering operation
//!   within the context of [`xmlo` object files](super).
//!
//! Usage
//! =====
//! [`AsgBuilder`] accepts any [`Iterator`] of [`XmloEvent`]s,
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
    reader::{XmloEvent, XmloResult},
    XmloError,
};
use crate::asg::{
    Asg, AsgError, IdentKind, IdentKindError, IdentObjectState, ObjectRef,
    Source,
};
use crate::sym::SymbolId;
use std::collections::HashSet;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::hash::BuildHasher;

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
///
/// [`roots`](AsgBuilderState::roots) is added to as certain identifiers are
///   discovered that should be used as starting points for a topological
///   sort.
/// This is used by the linker to only include dependencies that are
///   actually used by a particular program.
#[derive(Debug, Default)]
pub struct AsgBuilderState<S>
where
    S: BuildHasher,
{
    /// Discovered roots.
    ///
    /// Roots represent starting points for a topological sort of the
    ///   graph.
    /// They are meaningful to the linker.
    pub roots: Vec<ObjectRef>,

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
pub trait AsgBuilder<O, S>
where
    O: IdentObjectState<O>,
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
        xmlo: impl Iterator<Item = XmloResult<XmloEvent>>,
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

impl<O, S, G> AsgBuilder<O, S> for G
where
    O: IdentObjectState<O>,
    S: BuildHasher + Default,
    G: Asg<O>,
{
    fn import_xmlo(
        &mut self,
        mut xmlo: impl Iterator<Item = XmloResult<XmloEvent>>,
        mut state: AsgBuilderState<S>,
    ) -> Result<S> {
        let mut elig = None;
        let first = state.is_first();
        let found = state.found.get_or_insert(Default::default());

        use AsgBuilderInternalState as IS;
        let mut istate = IS::None;

        while let Some(ev) = xmlo.next() {
            match (istate, ev?) {
                (IS::None, XmloEvent::PkgName(name)) => {
                    if first {
                        state.name = Some(name);
                    }
                }

                (IS::None, XmloEvent::PkgRootPath(relroot)) => {
                    if first {
                        state.relroot = Some(relroot);
                    }
                }

                (IS::None, XmloEvent::PkgEligClassYields(pkg_elig)) => {
                    elig = Some(pkg_elig);
                }

                (IS::None, XmloEvent::PkgProgramFlag) => {
                    // Unused
                }

                (IS::None | IS::SymDep(_), XmloEvent::SymDepStart(sym, _)) => {
                    istate = IS::SymDep(sym);
                }

                (IS::SymDep(sym), XmloEvent::Symbol(dep_sym, _)) => {
                    self.add_dep_lookup(sym, dep_sym);
                }

                (IS::None, XmloEvent::SymDecl(sym, attrs, _span)) => {
                    if let Some(sym_src) = attrs.src {
                        found.insert(sym_src);
                    } else {
                        let extern_ = attrs.extern_;
                        let kindval = (&attrs).try_into()?;

                        let mut src: Source = attrs.into();

                        // Existing convention is to omit @src of local package
                        // (in this case, the program being linked)
                        if first {
                            src.pkg_name = None;
                        }

                        let link_root = matches!(
                            kindval,
                            IdentKind::Meta
                                | IdentKind::Map
                                | IdentKind::RetMap
                        );

                        if extern_ {
                            self.declare_extern(sym, kindval, src)?;
                        } else {
                            let node = self.declare(sym, kindval, src)?;

                            if link_root {
                                state.roots.push(node);
                            }
                        }
                    }
                }

                // Fragments follow SymDeps.
                (
                    IS::None | IS::SymDep(_),
                    XmloEvent::Fragment(sym, text, _),
                ) => {
                    istate = IS::None;

                    let frag = self
                        .lookup(sym)
                        .ok_or(AsgBuilderError::MissingFragmentIdent(sym))?;

                    self.set_fragment(frag, text)?;
                }

                // We don't need to read any further than the end of the
                // header (symtable, sym-deps, fragments).  Note that this
                // may change in the future, in which case this
                // responsibility can be delegated to the linker (to produce
                // an `Iterator` that stops at EOH).
                (IS::None, XmloEvent::Eoh) => break,

                (istate, ev) => {
                    todo!("unexpected state transition: {istate:?} -> {ev:?}")
                }
            }
        }

        if let Some(elig_sym) = elig {
            state.roots.push(
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

    /// Error parsing into [`IdentKind`].
    IdentKindError(IdentKindError),

    /// [`Asg`] operation error.
    AsgError(AsgError),

    /// Fragment encountered for an unknown identifier.
    MissingFragmentIdent(SymbolId),

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
            Self::IdentKindError(e) => e.fmt(fmt),
            Self::AsgError(e) => e.fmt(fmt),

            Self::MissingFragmentIdent(name) => write!(
                fmt,
                "encountered fragment for unknown identifier `{}`",
                name,
            ),

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

impl From<IdentKindError> for AsgBuilderError {
    fn from(src: IdentKindError) -> Self {
        Self::IdentKindError(src)
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
            Self::IdentKindError(e) => Some(e),
            Self::AsgError(e) => Some(e),
            _ => None,
        }
    }
}

// These tests are coupled with BaseAsg, which is not ideal.
#[cfg(test)]
mod test {
    use super::*;
    use crate::asg::{DefaultAsg, FragmentText, IdentObject};
    use crate::obj::xmlo::{SymAttrs, SymType};
    use crate::span::{DUMMY_SPAN, UNKNOWN_SPAN};
    use crate::sym::GlobalSymbolIntern;
    use std::collections::hash_map::RandomState;

    type Sut<'i> = DefaultAsg<IdentObject>;
    type SutState<'i> = AsgBuilderState<RandomState>;

    #[test]
    fn gets_data_from_package_event() {
        let mut sut = Sut::new();

        let name = "name".intern();
        let relroot = "some/path".into();

        let evs = vec![
            Ok(XmloEvent::PkgName(name)),
            Ok(XmloEvent::PkgRootPath(relroot)),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        assert_eq!(Some(name), state.name);
        assert_eq!(Some(relroot), state.relroot);
    }

    #[test]
    fn xmlo_error_returned() {
        let mut sut = Sut::new();

        let evs = vec![Err(XmloError::UnexpectedRoot)];
        let result = sut.import_xmlo(evs.into_iter(), SutState::new());

        assert_eq!(
            AsgBuilderError::XmloError(XmloError::UnexpectedRoot),
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

        let evs = vec![Ok(XmloEvent::PkgEligClassYields(elig_sym))];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        assert!(state.roots.contains(&elig_node));
    }

    #[test]
    fn adds_sym_deps() {
        let mut sut = Sut::new();

        let sym_from = "from".intern();
        let sym_to1 = "to1".intern();
        let sym_to2 = "to2".intern();

        let evs = vec![
            Ok(XmloEvent::SymDepStart(sym_from, DUMMY_SPAN)),
            Ok(XmloEvent::Symbol(sym_to1, DUMMY_SPAN)),
            Ok(XmloEvent::Symbol(sym_to2, DUMMY_SPAN)),
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
            Ok(XmloEvent::SymDecl(
                sym,
                SymAttrs {
                    src: Some(src_a),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloEvent::SymDecl(
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
            Ok(XmloEvent::SymDecl(
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
            Ok(XmloEvent::SymDecl(
                sym_non_extern,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloEvent::SymDecl(
                sym_map,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            Ok(XmloEvent::SymDecl(
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

        assert_eq!(
            vec![
                sut.lookup(sym_non_extern).unwrap(),
                sut.lookup(sym_map).unwrap(),
                sut.lookup(sym_retmap).unwrap(),
            ],
            state.roots
        );

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

        let evs = vec![Ok(XmloEvent::SymDecl(
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

    #[test]
    fn ident_kind_conversion_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let bad_attrs = SymAttrs::default();

        let evs = vec![Ok(XmloEvent::SymDecl(sym, bad_attrs, UNKNOWN_SPAN))];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected IdentKind conversion error");

        assert!(matches!(result, AsgBuilderError::IdentKindError(_)));
    }

    #[test]
    fn declare_extern_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                sym,
                SymAttrs {
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            // Incompatible
            Ok(XmloEvent::SymDecl(
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
            Ok(XmloEvent::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            )),
            // Redeclare
            Ok(XmloEvent::SymDecl(
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
            Ok(XmloEvent::SymDecl(
                sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                DUMMY_SPAN,
            )),
            Ok(XmloEvent::Fragment(sym, frag.clone(), DUMMY_SPAN)),
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
        let evs = vec![Ok(XmloEvent::Fragment(sym, "foo".into(), DUMMY_SPAN))];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected error for fragment without ident");

        assert_eq!(AsgBuilderError::MissingFragmentIdent(sym), result,);
    }

    #[test]
    fn fragment_error_propagates() {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let frag = FragmentText::from("foo");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                sym,
                SymAttrs {
                    // Invalid fragment destination
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                DUMMY_SPAN,
            )),
            Ok(XmloEvent::Fragment(sym, frag.clone(), DUMMY_SPAN)),
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
            Ok(XmloEvent::Eoh),
            // Shouldn't make it to this one.
            Ok(XmloEvent::PkgName(pkg_name)),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // Should still be true because we didn't get to the `PkgName`
        // event.
        assert!(state.is_first());
    }
}
