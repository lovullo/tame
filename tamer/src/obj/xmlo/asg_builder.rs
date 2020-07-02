// Read from xmlo and immediately lower to ASG IR
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

//! Lower [Legacy IR](crate::ir::legacyir) derived from [`XmloEvent`]
//!   into [`Asg`].
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
//!
//! ```
//! use tamer::global;
//! use tamer::ir::asg::{DefaultAsg, IdentObject};
//! use tamer::obj::xmlo::{AsgBuilder, AsgBuilderState, XmloReader};
//! use tamer::sym::{DefaultInterner, Interner};
//! use fxhash::FxBuildHasher;
//! use std::io::BufReader;
//!
//! let src_xmlo: &[u8] = br#"<package>
//!     <preproc:symtable>
//!       <preproc:sym name="foo" type="cgen" src="dep/package" />
//!     </preproc:symtable>
//!     <preproc:fragments>
//!     </preproc:fragments>
//!   </package>"#;
//!
//! let interner = DefaultInterner::new();
//! let xmlo = XmloReader::new(src_xmlo, &interner);
//! let mut asg = DefaultAsg::<'_, IdentObject, global::ProgIdentSize>::new();
//!
//! let state = asg.import_xmlo(xmlo, AsgBuilderState::<'_, FxBuildHasher, _>::new());
//!
//! // Use `state.found` to recursively load dependencies.
//! let AsgBuilderState { found, .. } = state.expect("unexpected failure");
//! assert_eq!(
//!     vec![&"dep/package"],
//!     found.unwrap().iter().collect::<Vec<_>>(),
//! );
//! ```

use super::reader::{XmloError, XmloEvent, XmloResult};
use crate::ir::asg::{
    Asg, AsgError, IdentKind, IdentKindError, IdentObjectState, IndexType,
    ObjectRef, Source,
};
use crate::sym::Symbol;
use std::collections::HashSet;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::hash::BuildHasher;

pub type Result<'i, S, Ix> =
    std::result::Result<AsgBuilderState<'i, S, Ix>, AsgBuilderError>;

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
pub struct AsgBuilderState<'i, S, Ix>
where
    S: BuildHasher,
    Ix: IndexType,
{
    /// Discovered roots.
    ///
    /// Roots represent starting points for a topological sort of the
    ///   graph.
    /// They are meaningful to the linker.
    pub roots: Vec<ObjectRef<Ix>>,

    /// Relative paths to imported packages that have been discovered.
    ///
    /// The caller will use these to perform recursive loads.
    /// This is contained within an [`Option`] so that the caller can `take`
    ///   ownership over its contents.
    ///
    /// See [`AsgBuilder::import_xmlo`] for behavior when this value is
    ///   [`None`].
    pub found: Option<HashSet<&'i str, S>>,

    /// Program name once discovered.
    ///
    /// This will be set by the first package encountered.
    pub name: Option<&'i Symbol<'i>>,

    /// Relative path to project root once discovered.
    ///
    /// This will be set by the first package encountered.
    pub relroot: Option<String>,
}

impl<'i, S, Ix> AsgBuilderState<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: IndexType,
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
pub trait AsgBuilder<'i, O, S, Ix>
where
    O: IdentObjectState<'i, O>,
    S: BuildHasher,
    Ix: IndexType,
{
    /// Import [`XmloResult`]s into an [`Asg`].
    ///
    /// This is an IR lowering operation.
    /// The [`XmloResult`] produces data gleaned from
    ///   [`legacyir`](crate::ir::legacyir),
    ///     and this process lowers it into the IR [`asg`](crate::ir::asg).
    ///
    /// Each call to this method augments the provided [`AsgBuilderState`];
    ///   see its documentation for more information.
    /// Its initial value can be provided as [`Default::default`].
    fn import_xmlo(
        &mut self,
        xmlo: impl Iterator<Item = XmloResult<XmloEvent<'i>>>,
        state: AsgBuilderState<'i, S, Ix>,
    ) -> Result<'i, S, Ix>;
}

impl<'i, O, S, Ix, G> AsgBuilder<'i, O, S, Ix> for G
where
    O: IdentObjectState<'i, O>,
    S: BuildHasher + Default,
    Ix: IndexType,
    G: Asg<'i, O, Ix>,
{
    fn import_xmlo(
        &mut self,
        mut xmlo: impl Iterator<Item = XmloResult<XmloEvent<'i>>>,
        mut state: AsgBuilderState<'i, S, Ix>,
    ) -> Result<'i, S, Ix> {
        let mut elig = None;
        let first = state.is_first();
        let found = state.found.get_or_insert(Default::default());

        while let Some(ev) = xmlo.next() {
            match ev? {
                XmloEvent::Package(attrs) => {
                    if first {
                        state.name = attrs.name;
                        state.relroot = attrs.relroot;
                    }

                    elig = attrs.elig;
                }

                XmloEvent::SymDeps(sym, deps) => {
                    // Maps should not pull in symbols since we may end up
                    // mapping to params that are never actually used.
                    // TODO: Can these just be removed from the xmlo files
                    //   rather than adding exceptions?
                    // TODO: No string comparison.
                    if !sym.starts_with(":map:") {
                        for dep_sym in deps {
                            self.add_dep_lookup(sym, dep_sym);
                        }
                    }
                }

                XmloEvent::SymDecl(sym, attrs) => {
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

                XmloEvent::Fragment(sym, text) => {
                    let frag = self.lookup(sym).ok_or(
                        AsgBuilderError::MissingFragmentIdent(sym.to_string()),
                    )?;

                    self.set_fragment(frag, text)?;
                }

                // We don't need to read any further than the end of the
                // header (symtable, sym-deps, fragments).  Note that this
                // may change in the future, in which case this
                // responsibility can be delegated to the linker (to produce
                // an `Iterator` that stops at EOH).
                XmloEvent::Eoh => break,
            }
        }

        if let Some(elig_sym) = elig {
            state
                .roots
                .push(self.lookup(elig_sym).ok_or(
                    AsgBuilderError::BadEligRef(elig_sym.to_string()),
                )?);
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
    MissingFragmentIdent(String),

    /// Eligibility classification references unknown identifier.
    ///
    /// This is generated by the compiler and so should never happen.
    /// (That's not to say that it won't, but it shouldn't.)
    BadEligRef(String),
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
    use crate::ir::asg::{DefaultAsg, FragmentText, IdentObject};
    use crate::ir::legacyir::{PackageAttrs, SymAttrs, SymType};
    use crate::sym::SymbolIndex;
    use std::collections::hash_map::RandomState;

    type SutIx = u8;
    type Sut<'i> = DefaultAsg<'i, IdentObject<'i>, SutIx>;
    type SutState<'i> = AsgBuilderState<'i, RandomState, SutIx>;

    #[test]
    fn gets_data_from_package_event() {
        let mut sut = Sut::new();

        let name = symbol_dummy!(1, "name");
        let relroot = "some/path".to_string();

        let evs = vec![Ok(XmloEvent::Package(PackageAttrs {
            name: Some(&name),
            relroot: Some(relroot.clone()),
            ..Default::default()
        }))];

        let state = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect("parsing of proper PackageAttrs must succeed");

        assert_eq!(Some(&name), state.name);
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
        let elig_sym = symbol_dummy!(1, "elig");

        // The symbol must be on the graph, or it'll fail.
        let elig_node = sut
            .declare(&elig_sym, IdentKind::Meta, Default::default())
            .unwrap();

        let evs = vec![Ok(XmloEvent::Package(PackageAttrs {
            elig: Some(&elig_sym),
            ..Default::default()
        }))];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        assert!(state.roots.contains(&elig_node));
    }

    #[test]
    fn adds_sym_deps() {
        let mut sut = Sut::new();

        let sym_from = symbol_dummy!(1, "from");
        let sym_to1 = symbol_dummy!(2, "to1");
        let sym_to2 = symbol_dummy!(3, "to2");

        let evs =
            vec![Ok(XmloEvent::SymDeps(&sym_from, vec![&sym_to1, &sym_to2]))];

        let _ = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect("unexpected failure");

        let node_from = sut.lookup(&sym_from).expect("from node not added");
        let node_to1 = sut.lookup(&sym_to1).expect("to1 node not added");
        let node_to2 = sut.lookup(&sym_to2).expect("to2 node not added");

        assert!(sut.has_dep(node_from, node_to1));
        assert!(sut.has_dep(node_from, node_to2));
    }

    #[test]
    fn ignores_map_sym_deps() {
        let mut sut = Sut::new();

        let sym_from = symbol_dummy!(1, ":map:sym");
        let sym_to = symbol_dummy!(2, "to");

        let evs = vec![Ok(XmloEvent::SymDeps(&sym_from, vec![&sym_to]))];

        let _ = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect("unexpected failure");

        assert!(sut.lookup(&sym_from).is_none());
    }

    #[test]
    fn sym_decl_with_src_not_added_and_populates_found() {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let src_a = symbol_dummy!(2, "src_a");
        let src_b = symbol_dummy!(3, "src_b");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    src: Some(&src_a),
                    ..Default::default()
                },
            )),
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    src: Some(&src_b),
                    ..Default::default()
                },
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

        assert_eq!(vec![&src_a as &str, &src_b as &str], founds);

        // Symbols with `src` set are external and should not be added to
        // the graph.
        assert!(sut.lookup(&sym).is_none());
    }

    #[test]
    fn sym_decl_added_to_graph() {
        let mut sut = Sut::new();

        let sym_extern = symbol_dummy!(1, "sym_extern");
        let sym_non_extern = symbol_dummy!(2, "sym_non_extern");
        let sym_map = symbol_dummy!(3, "sym_map");
        let sym_retmap = symbol_dummy!(4, "sym_retmap");
        let pkg_name = symbol_dummy!(5, "pkg name");

        let evs = vec![
            // Note that externs should not be recognized as roots even if
            // their type would be.
            Ok(XmloEvent::SymDecl(
                &sym_extern,
                SymAttrs {
                    pkg_name: Some(&pkg_name),
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            // These three will be roots
            Ok(XmloEvent::SymDecl(
                &sym_non_extern,
                SymAttrs {
                    pkg_name: Some(&pkg_name),
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            Ok(XmloEvent::SymDecl(
                &sym_map,
                SymAttrs {
                    pkg_name: Some(&pkg_name),
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
            )),
            Ok(XmloEvent::SymDecl(
                &sym_retmap,
                SymAttrs {
                    pkg_name: Some(&pkg_name),
                    ty: Some(SymType::RetMap),
                    ..Default::default()
                },
            )),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // Both above symbols were local (no `src`).
        assert!(state.found.unwrap().len() == 0);

        assert_eq!(
            vec![
                sut.lookup(&sym_non_extern).unwrap(),
                sut.lookup(&sym_map).unwrap(),
                sut.lookup(&sym_retmap).unwrap(),
            ],
            state.roots
        );

        // Note that each of these will have their package names cleared
        // since this is considered to be the first package encountered.

        assert_eq!(
            &IdentObject::Extern(
                &sym_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(&sym_extern).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                &sym_non_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(&sym_non_extern).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                &sym_map,
                IdentKind::Map,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(&sym_map).unwrap()).unwrap(),
        );

        assert_eq!(
            &IdentObject::Ident(
                &sym_retmap,
                IdentKind::RetMap,
                Source {
                    pkg_name: None,
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(&sym_retmap).unwrap()).unwrap(),
        );
    }

    // See above test, where pkg_name was cleared.
    #[test]
    fn sym_decl_pkg_name_retained_if_not_first() {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let pkg_name = symbol_dummy!(2, "pkg name");

        // This is all that's needed to not consider this to be the first
        // package, so that pkg_name is retained below.
        let state = AsgBuilderState::<'_, RandomState, SutIx> {
            name: Some(&pkg_name),
            ..Default::default()
        };

        let evs = vec![Ok(XmloEvent::SymDecl(
            &sym,
            SymAttrs {
                pkg_name: Some(&pkg_name),
                ty: Some(SymType::Meta),
                ..Default::default()
            },
        ))];

        let _ = sut.import_xmlo(evs.into_iter(), state).unwrap();

        assert_eq!(
            // `pkg_name` retained
            &IdentObject::Ident(
                &sym,
                IdentKind::Meta,
                Source {
                    pkg_name: Some(&pkg_name),
                    ..Default::default()
                },
            ),
            sut.get(sut.lookup(&sym).unwrap()).unwrap(),
        );
    }

    #[test]
    fn ident_kind_conversion_error_propagates() {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let bad_attrs = SymAttrs::default();

        let evs = vec![Ok(XmloEvent::SymDecl(&sym, bad_attrs))];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected IdentKind conversion error");

        assert!(matches!(result, AsgBuilderError::IdentKindError(_)));
    }

    #[test]
    fn declare_extern_error_propagates() {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            // Incompatible
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    extern_: true,
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
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

        let sym = symbol_dummy!(1, "sym");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            // Redeclare
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
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

        let sym = symbol_dummy!(1, "sym");
        let frag = FragmentText::from("foo");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            Ok(XmloEvent::Fragment(&sym, frag.clone())),
        ];

        let _ = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        let node = sut
            .lookup(&sym)
            .expect("ident/fragment was not added to graph");

        assert_eq!(
            Some(&IdentObject::IdentFragment(
                &sym,
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

        let sym = symbol_dummy!(1, "sym");

        // Note: missing `SymDecl`.
        let evs = vec![Ok(XmloEvent::Fragment(&sym, "foo".into()))];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected error for fragment without ident");

        assert_eq!(
            AsgBuilderError::MissingFragmentIdent(sym.to_string()),
            result,
        );
    }

    #[test]
    fn fragment_error_propagates() {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let frag = FragmentText::from("foo");

        let evs = vec![
            Ok(XmloEvent::SymDecl(
                &sym,
                SymAttrs {
                    // Invalid fragment destination
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
            )),
            Ok(XmloEvent::Fragment(&sym, frag.clone())),
        ];

        let result = sut
            .import_xmlo(evs.into_iter(), SutState::new())
            .expect_err("expected fragment set failure");

        assert!(matches!(
            result,
            AsgBuilderError::AsgError(AsgError::ObjectTransition(_))
        ));

        let node = sut
            .lookup(&sym)
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

        let pkg_name = symbol_dummy!(1, "pkg name");

        let evs = vec![
            // Stop here.
            Ok(XmloEvent::Eoh),
            // Shouldn't make it to this one.
            Ok(XmloEvent::Package(PackageAttrs {
                name: Some(&pkg_name),
                ..Default::default()
            })),
        ];

        let state = sut.import_xmlo(evs.into_iter(), SutState::new()).unwrap();

        // Should still be true because we didn't get to the `PackageAttrs`
        // event.
        assert!(state.is_first());
    }
}
