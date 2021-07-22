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
    pub fn len(&self) -> usize {
        self.head.len() + self.body.len() + self.tail.len()
    }

    /// Check if the `Section` is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Push an `IdentObject` into a `Section`'s head
    pub fn push_head(&mut self, obj: &'a T) {
        self.head.push(obj)
    }

    /// Push an `IdentObject` into a `Section`'s body
    pub fn push_body(&mut self, obj: &'a T) {
        self.body.push(obj)
    }

    /// Push an `IdentObject` into a `Section`'s tail
    pub fn push_tail(&mut self, obj: &'a T) {
        self.tail.push(obj)
    }

    /// Merge the parts of a `Section` into one [`SectionIterator`]
    ///
    /// The `Section` internals need to be iterated as a group so we needed to
    ///   create a custom iterator, [`SectionIterator`] to do this for us. This
    ///   method allows us to access the iterator.
    ///
    /// ```
    /// use tamer::ir::asg::{IdentObject, Section};
    /// use tamer::sym::{DefaultInterner, Interner};
    ///
    /// let interner = DefaultInterner::new();
    /// let mut section = Section::new();
    /// let obj = IdentObject::Missing(interner.intern("ident"));
    /// let expect = vec![&obj, &obj, &obj];
    ///
    /// section.push_head(&obj);
    /// section.push_body(&obj);
    /// section.push_tail(&obj);
    /// let section_iter = section.iter();
    ///
    /// for object in section_iter {
    ///     assert_eq!(&obj, object);
    /// }
    /// ```
    pub fn iter(&self) -> SectionIterator<T> {
        SectionIterator {
            inner: Box::new(
                self.head
                    .iter()
                    .chain(self.body.iter())
                    .chain(self.tail.iter())
                    .copied(),
            ),
        }
    }
}

/// Wrapper for an Iterator
///
/// This allows us to iterate over all parts of a [`Section`].
pub struct SectionIterator<'a, T> {
    inner: Box<dyn Iterator<Item = &'a T> + 'a>,
}

impl<'a, T> Iterator for SectionIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// Sections that need to be written to a buffer
///
/// All the properties are public [`Section`] objects and will be accessed
///   directly by the the objects interacting with them.
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

impl<'a, T> Sections<'a, T> {
    /// New collection of empty sections.
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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::asg::IdentObject;
    use crate::sym::{Symbol, SymbolIndex};

    lazy_static! {
        static ref SYM: Symbol<'static> = symbol_dummy!(1, "sym");
    }

    type Sut<'a, 'i> = Section<'a, IdentObject<'i>>;

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
        let obj = IdentObject::Missing(&SYM);

        assert!(section.head.is_empty());

        section.push_head(&obj);

        assert_eq!(Some(&&obj), section.head.get(0));
    }

    #[test]
    fn section_body() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);

        assert!(section.body.is_empty());

        section.push_body(&obj);

        let body = section.body;
        assert_eq!(Some(&&obj), body.get(0));
    }

    #[test]
    fn section_tail() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);

        assert!(section.tail.is_empty());

        section.push_tail(&obj);

        assert_eq!(Some(&&obj), section.tail.get(0));
    }

    #[test]
    fn section_len() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);

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
        let obj = IdentObject::Missing(&SYM);

        assert!(section.is_empty());
        section.push_head(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_is_empty_body() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);

        assert!(section.is_empty());
        section.push_body(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_is_empty_tail() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);

        assert!(section.is_empty());
        section.push_tail(&obj);
        assert!(!section.is_empty());
    }

    #[test]
    fn section_iterator() {
        let mut section = Sut::new();
        let obj = IdentObject::Missing(&SYM);
        let expect = vec![&obj, &obj, &obj];

        section.push_head(&obj);
        section.push_body(&obj);
        section.push_tail(&obj);

        let collection: Vec<_> = section.iter().collect();

        assert_eq!(expect, collection);
    }
}
