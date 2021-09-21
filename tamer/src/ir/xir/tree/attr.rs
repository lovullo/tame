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
//! See [parent module](super) for documentation.

use super::{AttrValue, QName};
use crate::{span::Span, sym::SymbolIndexSize};

/// Element attribute.
///
/// TODO: This doesn't yet handle whitespace for alignment of attributes;
///         deferring this until it's actually needed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attr<Ix: SymbolIndexSize> {
    name: QName<Ix>,
    value: AttrValue<Ix>,
    /// Spans for the attribute name and value respectively.
    span: (Span, Span),
}

impl<Ix: SymbolIndexSize> Attr<Ix> {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    #[inline]
    pub fn new(
        name: QName<Ix>,
        value: AttrValue<Ix>,
        span: (Span, Span),
    ) -> Self {
        Attr { name, value, span }
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
pub struct AttrList<Ix: SymbolIndexSize> {
    attrs: Vec<Attr<Ix>>,
}

impl<Ix: SymbolIndexSize> AttrList<Ix> {
    /// Construct a new, empty attribute list.
    pub fn new() -> Self {
        Self { attrs: vec![] }
    }

    /// Add an attribute to the end of the attribute list.
    pub fn push(&mut self, attr: Attr<Ix>) {
        self.attrs.push(attr)
    }
}

impl<Ix: SymbolIndexSize> From<Vec<Attr<Ix>>> for AttrList<Ix> {
    fn from(attrs: Vec<Attr<Ix>>) -> Self {
        AttrList { attrs }
    }
}

impl<Ix: SymbolIndexSize, const N: usize> From<[Attr<Ix>; N]> for AttrList<Ix> {
    fn from(attrs: [Attr<Ix>; N]) -> Self {
        AttrList {
            attrs: attrs.into(),
        }
    }
}

// See [`super::test`].
