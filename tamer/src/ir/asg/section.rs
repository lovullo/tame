// Section/Sections IR representation
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

//! Ordered sections of ASG object references.
//!
//! These sections are the result of an ordering operation from
//!   [`SortableAsg::sort`].
//!
//! [`SortableAsg::sort`]: super::SortableAsg::sort

use super::IdentObjectData;
use crate::sym::SymbolId;
use fxhash::FxHashSet;
use std::collections::hash_set;
use std::iter::Chain;
use std::slice::Iter;

/// A section of an [object file](crate::obj).
///
/// Most sections will only need a `body`, but some innlude `head` and `tail`
///   information. Rather than dealing with those differently, each `Section`
///   will have a `head` and `tail` that are empty by default.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Section<'a, T> {
    head: Vec<&'a T>,
    body: Vec<&'a T>,
    tail: Vec<&'a T>,
}

impl<'a, T> Section<'a, T> {
    /// New empty section.
    pub fn new() -> Self {
        Self {
            head: Vec::new(),
            body: Vec::new(),
            tail: Vec::new(),
        }
    }

    /// The length of the `Section`
    #[inline]
    pub fn len(&self) -> usize {
        self.head.len() + self.body.len() + self.tail.len()
    }

    /// Check if the `Section` is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Push an `IdentObject` into a `Section`'s head
    #[inline]
    pub fn push_head(&mut self, obj: &'a T) {
        self.head.push(obj)
    }

    /// Push an `IdentObject` into a `Section`'s body
    #[inline]
    pub fn push_body(&mut self, obj: &'a T) {
        self.body.push(obj)
    }

    /// Push an `IdentObject` into a `Section`'s tail
    #[inline]
    pub fn push_tail(&mut self, obj: &'a T) {
        self.tail.push(obj)
    }

    /// Construct a new iterator visiting each head, body, and tail object
    ///   in order.
    #[inline]
    pub fn iter(&self) -> SectionIter<T> {
        SectionIter(
            self.head
                .iter()
                .chain(self.body.iter())
                .chain(self.tail.iter()),
        )
    }
}

/// Iterator over the head, body, and tail of a [`Section`].
///
/// This iterator should be created with [`Section::iter`].
///
/// This hides the complex iterator type from callers.
pub struct SectionIter<'a, T>(
    Chain<Chain<Iter<'a, &'a T>, Iter<'a, &'a T>>, Iter<'a, &'a T>>,
);

impl<'a, T> Iterator for SectionIter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|x| *x)
    }
}

/// ASG objects organized into logical sections.
///
/// These sections may not necessarily correspond directly to sections of an
///   [object file](crate::obj).
// TODO: Remove pub
#[derive(Debug, Default, PartialEq)]
pub struct Sections<'a, T> {
    pub map: Section<'a, T>,
    pub retmap: Section<'a, T>,
    pub meta: Section<'a, T>,
    pub worksheet: Section<'a, T>,
    pub params: Section<'a, T>,
    pub types: Section<'a, T>,
    pub funcs: Section<'a, T>,
    pub consts: Section<'a, T>,
    pub rater: Section<'a, T>,
}

impl<'a, T: IdentObjectData> Sections<'a, T> {
    /// New collection of empty sections.
    #[inline]
    pub fn new() -> Self {
        Self {
            map: Section::new(),
            retmap: Section::new(),
            meta: Section::new(),
            worksheet: Section::new(),
            params: Section::new(),
            types: Section::new(),
            funcs: Section::new(),
            consts: Section::new(),
            rater: Section::new(),
        }
    }

    /// Construct an iterator over each of the individual sections in
    ///   arbitrary order.
    ///
    /// Each individual section is ordered as stated in [`Section::iter`],
    ///   but you should not rely on the order that the sections themselves
    ///   appear in;
    ///     they may change or be combined in the future.
    /// At the time of writing,
    ///   they are chained in the same order in which they are defined
    ///   on the [`Sections`] struct.
    #[inline]
    pub fn iter_all(&self) -> SectionsIter<T> {
        SectionsIter(SectionsIterType::All(
            self.map
                .iter()
                .chain(self.retmap.iter())
                .chain(self.meta.iter())
                .chain(self.worksheet.iter())
                .chain(self.params.iter())
                .chain(self.types.iter())
                .chain(self.funcs.iter())
                .chain(self.consts.iter())
                .chain(self.rater.iter()),
        ))
    }

    /// Construct an iterator over the static sections in arbitrary order.
    ///
    /// These sections contain fragments that do not depend on any external
    ///   inputs and can therefore be executed a single time when the
    ///   program is loaded into memory.
    ///
    /// Each individual section is ordered as stated in [`Section::iter`],
    ///   but you should not rely on the order that the sections themselves
    ///   appear in;
    ///     they may change or be combined in the future.
    #[inline]
    pub fn iter_static(&self) -> SectionsIter<T> {
        SectionsIter(SectionsIterType::Static(
            self.meta
                .iter()
                .chain(self.worksheet.iter())
                .chain(self.params.iter())
                .chain(self.types.iter())
                .chain(self.funcs.iter())
                .chain(self.consts.iter()),
        ))
    }

    /// Construct an iterator over the map section.
    #[inline]
    pub fn iter_map(&self) -> SectionsIter<T> {
        SectionsIter(SectionsIterType::Single(self.map.iter()))
    }

    /// Iterate over each unique map `from` entry.
    ///
    /// Multiple mappings may reference the same source field,
    ///   which would produce duplicate values if they are not filtered.
    #[inline]
    pub fn iter_map_froms_uniq(&self) -> hash_set::IntoIter<SymbolId> {
        self.iter_map()
            .filter_map(|ident| {
                ident.src().expect("internal error: missing map src").from
            })
            .collect::<FxHashSet<SymbolId>>()
            .into_iter()
    }

    /// Construct an iterator over the return map section.
    #[inline]
    pub fn iter_retmap(&self) -> SectionsIter<T> {
        SectionsIter(SectionsIterType::Single(self.retmap.iter()))
    }

    /// Construct an iterator over the executable `rater` section.
    #[inline]
    pub fn iter_exec(&self) -> SectionsIter<T> {
        SectionsIter(SectionsIterType::Single(self.rater.iter()))
    }
}

// Compose the chained iterator type for [`SectionsIter`].
// This could be further abstracted away,
//   but it's likely that `Sections` will be simplified in the future.
type SIter<'a, T> = SectionIter<'a, T>;
type CSIter1<'a, T, L> = Chain<L, SIter<'a, T>>;
type CSIter2<'a, T, L> = CSIter1<'a, T, CSIter1<'a, T, L>>;
type CSIter4<'a, T, L> = CSIter2<'a, T, CSIter2<'a, T, L>>;
type CSIter8<'a, T, L> = CSIter4<'a, T, CSIter4<'a, T, L>>;

type SIter6<'a, T> = CSIter4<'a, T, CSIter1<'a, T, SIter<'a, T>>>;
type SIter9<'a, T> = CSIter8<'a, T, SIter<'a, T>>;

/// Types of iterators encapsulated by [`SectionsIter`].
enum SectionsIterType<'a, T> {
    All(SIter9<'a, T>),
    Static(SIter6<'a, T>),
    Single(SIter<'a, T>),
}

/// Iterator over each of the sections.
///
/// This iterator should be created with [`Sections::iter_all`].
///
/// This hides the complex iterator type from callers.
pub struct SectionsIter<'a, T>(SectionsIterType<'a, T>);

impl<'a, T> Iterator for SectionsIter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            SectionsIterType::All(inner) => inner.next(),
            SectionsIterType::Static(inner) => inner.next(),
            SectionsIterType::Single(inner) => inner.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            SectionsIterType::All(inner) => inner.size_hint(),
            SectionsIterType::Static(inner) => inner.size_hint(),
            SectionsIterType::Single(inner) => inner.size_hint(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::asg::{IdentKind, IdentObject, Source};
    use crate::sym::GlobalSymbolIntern;

    type Sut<'a, 'i> = Section<'a, IdentObject>;

    #[test]
    fn section_empty() {
        let section = Sut::new();

        assert!(section.head.is_empty());
        assert!(section.body.is_empty());
        assert!(section.tail.is_empty());
    }

    #[test]
    fn section_head() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.head.is_empty());

        section.push_head(&obj);

        assert_eq!(Some(&&obj), section.head.get(0));
    }

    #[test]
    fn section_body() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.body.is_empty());

        section.push_body(&obj);

        let body = section.body;
        assert_eq!(Some(&&obj), body.get(0));
    }

    #[test]
    fn section_tail() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.tail.is_empty());

        section.push_tail(&obj);

        assert_eq!(Some(&&obj), section.tail.get(0));
    }

    #[test]
    fn section_len() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert_eq!(0, section.len());
        section.push_head(&obj);
        assert_eq!(1, section.len());
        section.push_body(&obj);
        assert_eq!(2, section.len());
        section.push_tail(&obj);
        assert_eq!(3, section.len());
    }

    #[test]
    fn section_is_empty_head() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.is_empty());
        section.push_head(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_is_empty_body() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.is_empty());
        section.push_body(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_is_empty_tail() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());

        assert!(section.is_empty());
        section.push_tail(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_iterator() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing("sym".intern());
        let expect = vec![&obj, &obj, &obj];

        section.push_head(&obj);
        section.push_body(&obj);
        section.push_tail(&obj);

        let collection: Vec<_> = section.iter().collect();

        assert_eq!(expect, collection);
    }

    #[test]
    fn sections_iter_all() {
        let mut sections = Sections::new();

        let objs = (0..=10)
            .map(|i| IdentObject::Missing(i.to_string().into()))
            .collect::<Vec<_>>();

        sections.map.head.push(&objs[0]);
        sections.map.body.push(&objs[1]);
        sections.map.tail.push(&objs[2]);
        sections.retmap.body.push(&objs[3]);
        sections.meta.body.push(&objs[4]);
        sections.worksheet.body.push(&objs[5]);
        sections.params.body.push(&objs[6]);
        sections.types.body.push(&objs[7]);
        sections.funcs.body.push(&objs[8]);
        sections.consts.body.push(&objs[9]);
        sections.rater.body.push(&objs[10]);

        assert_eq!(
            sections.iter_all().collect::<Vec<_>>(),
            objs.iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn sections_iter_map_froms_uniq() {
        let mut sut_a = Sections::new();
        let mut sut_b = Sections::new();

        let a = IdentObject::Ident(
            "a".intern(),
            IdentKind::Map,
            Source {
                from: Some("froma".intern()),
                ..Default::default()
            },
        );

        let b = IdentObject::Ident(
            "a".intern(),
            IdentKind::Map,
            Source {
                from: Some("fromb".intern()),
                ..Default::default()
            },
        );

        // A contains duplicates.
        sut_a.map.body.push(&a);
        sut_a.map.body.push(&a);
        sut_a.map.body.push(&b);

        // B does not.
        sut_b.map.body.push(&a);
        sut_b.map.body.push(&b);

        // They should compare the same.
        assert_eq!(
            sut_a.iter_map_froms_uniq().collect::<Vec<_>>(),
            sut_b.iter_map_froms_uniq().collect::<Vec<_>>(),
        );
    }
}
