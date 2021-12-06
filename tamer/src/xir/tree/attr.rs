// XIRT attributes
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

//! XIRT attributes.
//!
//! Attributes are represented by [`Attr`].
//!
//! See [parent module](super) for additional documentation.

use super::QName;
use crate::{span::Span, sym::SymbolId};
use std::fmt::Display;

mod parse;

/// Element attribute.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attr {
    name: QName,
    value: SymbolId,
    /// Spans for the attribute name and value respectively.
    span: (Span, Span),
}

impl Attr {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    #[inline]
    pub fn new(name: QName, value: SymbolId, span: (Span, Span)) -> Self {
        Self { name, value, span }
    }

    /// Attribute name.
    #[inline]
    pub fn name(&self) -> QName {
        self.name
    }

    /// Retrieve the value from the attribute.
    ///
    /// Since [`SymbolId`] implements [`Copy`],
    ///   this returns an owned value.
    #[inline]
    pub fn value(&self) -> SymbolId {
        self.value
    }
}

impl Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`@{}=\"{}\"` at {}", self.name, self.value, self.span.0)
    }
}

/// List of attributes.
///
/// Attributes are ordered in XIR so that this IR will be suitable for code
///   formatters and linters.
///
/// This abstraction will allow us to manipulate the internal data so that
///   it is suitable for a particular task in the future
///     (e.g. O(1) lookups by attribute name).
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct AttrList {
    attrs: Vec<Attr>,
}

impl AttrList {
    /// Construct a new, empty attribute list.
    pub fn new() -> Self {
        Self { attrs: vec![] }
    }

    /// Add an attribute to the end of the attribute list.
    pub fn push(mut self, attr: Attr) -> Self {
        self.attrs.push(attr);
        self
    }

    /// Search for an attribute of the given `name`.
    ///
    /// _You should use this method only when a linear search makes sense._
    ///
    /// This performs an `O(n)` linear search in the worst case.
    /// Future implementations may perform an `O(1)` lookup under certain
    ///   circumstances,
    ///     but this should not be expected.
    pub fn find(&self, name: QName) -> Option<&Attr> {
        self.attrs.iter().find(|attr| attr.name() == name)
    }

    /// Returns [`true`] if the list contains no attributes.
    pub fn is_empty(&self) -> bool {
        self.attrs.is_empty()
    }
}

impl From<Vec<Attr>> for AttrList {
    fn from(attrs: Vec<Attr>) -> Self {
        AttrList { attrs }
    }
}

impl<const N: usize> From<[Attr; N]> for AttrList {
    fn from(attrs: [Attr; N]) -> Self {
        AttrList {
            attrs: attrs.into(),
        }
    }
}

// See [`super::test`] for tests related to attributes.
