// Section/Sections IR representation
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

//! Sections of a linked [`xmle`](super) object file.
//!
//! An [`XmleSections`] object is responsible for placing provided
//!   identifiers into the appropriate section,
//!     but _it must be provided properly ordered data_.
//! This ordering is the result of [`sort`](super::lower::sort),
//!   which places the relocatable object code fragments in the order
//!   necessary for execution.

use crate::asg::{IdentKind, IdentObject, UnresolvedError};
use crate::sym::SymbolId;
use fxhash::FxHashSet;
use std::mem::take;
use std::result::Result;

pub type PushResult<T = ()> = Result<T, SectionsError>;

/// Sections of a linked `xmle` file.
///
/// For more information on these sections,
///   see the [parent module](super).
pub trait XmleSections<'a> {
    /// Push an object into the appropriate section.
    ///
    /// Objects are expected to be properly sorted relative to their order
    ///   of execution so that their text fragments are placed in the
    ///   correct order in the final program text.
    fn push(&mut self, ident: &'a IdentObject) -> PushResult;

    /// Take the list of objects present in the linked file.
    ///
    /// The order of these objects does not matter.
    fn take_deps(&mut self) -> Vec<&'a IdentObject>;

    /// Take the ordered text fragments for the `map` section.
    fn take_map(&mut self) -> Vec<SymbolId>;

    /// Take the set of external identifiers mapped into this system.
    fn take_map_froms(&mut self) -> FxHashSet<SymbolId>;

    /// Take the ordered text fragments for the `retmap` section.
    fn take_retmap(&mut self) -> Vec<SymbolId>;

    /// Take the ordered text fragments for the `static` section.
    fn take_static(&mut self) -> Vec<SymbolId>;

    /// Take the ordered text fragments for the `exec` section.
    fn take_exec(&mut self) -> Vec<SymbolId>;
}

/// Sections of a linked `xmle` file.
///
/// For more information on these sections,
///   see the [parent module](super).
#[derive(Debug, Default, PartialEq)]
pub struct Sections<'a> {
    /// List of objects present in the linked file.
    ///
    /// The order of these objects does not matter.
    deps: Vec<&'a IdentObject>,

    /// External identifiers mapped into this system.
    map_froms: FxHashSet<SymbolId>,

    /// Ordered text fragments of `map` section.
    map: Vec<SymbolId>,

    /// Order text fragments of `retmap` section.
    retmap: Vec<SymbolId>,

    /// Ordered text fragments of `static` section.
    st: Vec<SymbolId>,

    /// Ordered text fragments of `exec` section.
    exec: Vec<SymbolId>,
}

impl<'a> Sections<'a> {
    /// New collection of empty sections.
    #[inline]
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl<'a> XmleSections<'a> for Sections<'a> {
    fn push(&mut self, ident: &'a IdentObject) -> PushResult {
        // TODO: This can go away once we stop treating root as an ident
        if matches!(ident, IdentObject::Root) {
            return Ok(());
        }

        self.deps.push(ident);

        // TODO: This cannot happen, so use an API without Option.
        let name = ident.name();
        let frag = ident.fragment();

        match ident.resolved()?.kind() {
            Some(kind) => match kind {
                IdentKind::Cgen(..)
                | IdentKind::Gen(..)
                | IdentKind::Lparam(..) => {
                    // These types do not have fragments.
                }
                IdentKind::Meta
                | IdentKind::Worksheet
                | IdentKind::Param(..)
                | IdentKind::Type(..)
                | IdentKind::Func(..)
                | IdentKind::Const(..) => {
                    self.st.push(expect_frag(name, frag)?)
                }
                IdentKind::MapHead | IdentKind::Map | IdentKind::MapTail => {
                    self.map.push(expect_frag(name, frag)?);

                    if let Some(from) =
                        ident.src().expect("missing map src").from
                    {
                        self.map_froms.insert(from);
                    }
                }
                IdentKind::RetMapHead
                | IdentKind::RetMap
                | IdentKind::RetMapTail => {
                    self.retmap.push(expect_frag(name, frag)?)
                }
                // TODO: Why do templates have fragments?
                IdentKind::Class(..) | IdentKind::Rate(..) | IdentKind::Tpl => {
                    self.exec.push(expect_frag(name, frag)?)
                }
            },
            None => {
                // TODO: This should not be possible; ensure that with types
                // or turn this into a panic.  It would certainly be a
                // compiler bug and there is no use in trying to be nice
                // about a situation where something went terribly, horribly
                // wrong.
                return Err(SectionsError::MissingObjectKind(ident.name()));
            }
        }

        Ok(())
    }

    #[inline]
    fn take_deps(&mut self) -> Vec<&'a IdentObject> {
        take(&mut self.deps)
    }

    #[inline]
    fn take_static(&mut self) -> Vec<SymbolId> {
        take(&mut self.st)
    }

    #[inline]
    fn take_map(&mut self) -> Vec<SymbolId> {
        take(&mut self.map)
    }

    #[inline]
    fn take_map_froms(&mut self) -> FxHashSet<SymbolId> {
        take(&mut self.map_froms)
    }

    #[inline]
    fn take_retmap(&mut self) -> Vec<SymbolId> {
        take(&mut self.retmap)
    }

    #[inline]
    fn take_exec(&mut self) -> Vec<SymbolId> {
        take(&mut self.exec)
    }
}

fn expect_frag(
    ident_name: SymbolId,
    frag: Option<SymbolId>,
) -> PushResult<SymbolId> {
    frag.ok_or(SectionsError::MissingFragment(ident_name))
}

/// Error during [`Sections`] building.
#[derive(Debug, PartialEq)]
pub enum SectionsError {
    /// An unresolved object was encountered during sorting.
    ///
    /// An unresolved object means that the graph has an incomplete picture
    ///   of the program,
    ///     and so sorting cannot be reliably performed.
    /// Since all objects are supposed to be resolved prior to sorting,
    ///   this represents either a problem with the program being compiled
    ///   or a bug in the compiler itself.
    UnresolvedObject(UnresolvedError),

    /// Identifier is missing an expected text fragment.
    MissingFragment(SymbolId),

    /// The kind of an object encountered during sorting could not be
    ///   determined.
    ///
    /// Sorting uses the object kind to place objects into their appropriate
    ///   sections.
    /// It should never be the case that a resolved object has no kind,
    ///   so this likely represents a compiler bug.
    MissingObjectKind(SymbolId),
}

impl From<UnresolvedError> for SectionsError {
    fn from(err: UnresolvedError) -> Self {
        Self::UnresolvedObject(err)
    }
}

impl std::error::Error for SectionsError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::UnresolvedObject(err) => Some(err),
            _ => None,
        }
    }
}

impl std::fmt::Display for SectionsError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnresolvedObject(err) => err.fmt(fmt),
            Self::MissingFragment(name) => write!(
                fmt,
                "missing text fragment for object `{}` (this may be a compiler bug!)",
                name,
            ),
            Self::MissingObjectKind(name) => write!(
                fmt,
                "missing object kind for object `{}` (this may be a compiler bug!)",
                name,
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::asg::{IdentKind, IdentObject, Source};
    use crate::num::{Dim, Dtype};
    use crate::sym::GlobalSymbolIntern;

    type Sut<'a> = Sections<'a>;

    #[test]
    fn sections_empty() {
        let mut sut = Sut::new();

        assert!(sut.take_deps().is_empty());
        assert!(sut.take_map_froms().is_empty());
        assert!(sut.take_map().is_empty());
        assert!(sut.take_retmap().is_empty());
        assert!(sut.take_static().is_empty());
        assert!(sut.take_exec().is_empty());
    }

    #[test]
    fn sections_push_adds_dep() -> PushResult {
        let mut sut = Sut::new();

        let a = IdentObject::IdentFragment(
            "a".intern(),
            IdentKind::Const(Dim::Scalar, Dtype::Integer),
            Default::default(),
            "fraga".intern(),
        );

        // Different section than a, to be sure that we still add it.
        let b = IdentObject::IdentFragment(
            "b".intern(),
            IdentKind::MapHead,
            Default::default(),
            "fragb".intern(),
        );

        sut.push(&a)?;
        sut.push(&b)?;

        assert_eq!(sut.take_deps(), vec![&a, &b],);

        Ok(())
    }

    // TODO: This can be removed once we no longer treat Root as an
    //   identifier.
    #[test]
    fn push_ignores_root() {
        let mut sut = Sut::new();

        sut.push(&IdentObject::Root).unwrap();
        assert!(sut.take_deps().is_empty());
    }

    // Certain identifiers have no fragments because the code is associated
    // with their parents (for now, anyway).
    #[test]
    fn idents_not_needing_fragments() -> PushResult {
        let mut sut = Sut::new();

        let cgen = IdentObject::Ident(
            "cgen".intern(),
            IdentKind::Cgen(Dim::Vector),
            Default::default(),
        );

        let gen = IdentObject::Ident(
            "gen".intern(),
            IdentKind::Gen(Dim::Vector, Dtype::Integer),
            Default::default(),
        );

        let lparam = IdentObject::Ident(
            "lparam".intern(),
            IdentKind::Lparam(Dim::Vector, Dtype::Integer),
            Default::default(),
        );

        sut.push(&cgen)?;
        sut.push(&gen)?;
        sut.push(&lparam)?;

        // They should be added as deps...
        assert_eq!(sut.take_deps(), vec![&cgen, &gen, &lparam]);

        // ...but not added to any sections.
        assert!(sut.take_map_froms().is_empty());
        assert!(sut.take_map().is_empty());
        assert!(sut.take_retmap().is_empty());
        assert!(sut.take_static().is_empty());
        assert!(sut.take_exec().is_empty());

        Ok(())
    }

    #[test]
    fn sections_map_froms_is_uniq() -> PushResult {
        let mut sut_a = Sections::new();
        let mut sut_b = Sections::new();

        let a = IdentObject::IdentFragment(
            "a".intern(),
            IdentKind::Map,
            Source {
                from: Some("froma".intern()),
                ..Default::default()
            },
            "mapa".intern(),
        );

        let b = IdentObject::IdentFragment(
            "a".intern(),
            IdentKind::Map,
            Source {
                from: Some("fromb".intern()),
                ..Default::default()
            },
            "mapb".intern(),
        );

        // A contains duplicates.
        sut_a.push(&a)?;
        sut_a.push(&a)?;
        sut_a.push(&b)?;

        // B does not.
        sut_b.push(&a)?;
        sut_b.push(&b)?;

        let a_froms = sut_a.take_map_froms();

        // They should compare the same.
        assert_eq!(a_froms, sut_b.take_map_froms());

        // And should use the proper ids.
        assert!(a_froms.contains(&"froma".intern()));
        assert!(a_froms.contains(&"fromb".intern()));

        Ok(())
    }

    macro_rules! add_syms {
        ($sut:ident, { $($name:ident: $kind:expr,)* }) => {
            $(
                let $name = IdentObject::IdentFragment(
                    stringify!($name).intern(),
                    $kind,
                    Default::default(),
                    stringify!($kind).intern(), // fragment
                );

                $sut.push(&$name)?;
            )*
        };
    }

    macro_rules! fragvec {
        ($($name:ident),*) => {
            vec![
                $(&$name),*
            ].into_iter().map(|x| x.fragment().unwrap()).collect::<Vec<SymbolId>>()
        }
    }

    #[test]
    fn push_sorts_fragments_into_sections() -> PushResult {
        let mut sut = Sections::new();

        add_syms!(sut, {
            cgen: IdentKind::Cgen(Dim::Scalar),
            class: IdentKind::Class(Dim::Matrix),
            const_: IdentKind::Const(Dim::Scalar, Dtype::Boolean),
            func: IdentKind::Func(Dim::Vector, Dtype::Integer),
            gen: IdentKind::Gen(Dim::Vector, Dtype::Boolean),
            lparam: IdentKind::Lparam(Dim::Matrix, Dtype::Float),
            param: IdentKind::Param(Dim::Scalar, Dtype::Integer),
            rate: IdentKind::Rate(Dtype::Integer),
            tpl: IdentKind::Tpl,
            ty: IdentKind::Type(Dtype::Integer),
            maphead: IdentKind::MapHead,
            map: IdentKind::Map,
            maptail: IdentKind::MapTail,
            retmaphead: IdentKind::RetMapHead,
            retmap: IdentKind::RetMap,
            retmaptail: IdentKind::RetMapTail,
            meta: IdentKind::Meta,
            worksheet: IdentKind::Worksheet,
        });

        assert_eq!(sut.take_map(), fragvec![maphead, map, maptail]);
        assert_eq!(sut.take_retmap(), fragvec![retmaphead, retmap, retmaptail]);

        assert_eq!(
            sut.take_static(),
            fragvec![const_, func, param, ty, meta, worksheet]
        );

        assert_eq!(sut.take_exec(), fragvec![class, rate, tpl]);

        Ok(())
    }
}
