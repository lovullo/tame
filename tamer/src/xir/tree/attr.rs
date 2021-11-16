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

/// An attribute.
///
/// Attributes come in two flavors:
///   attributes with simple atoms ([`SimpleAttr`]),
///   and extensible attributes composed of a list of fragments with
///     associated spans ([`AttrParts`]).
///
/// If you do not care about the distinction between the two types,
///   use the API provided by this enum for common functionality.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Attr {
    Simple(SimpleAttr),
    Extensible(AttrParts),
}

impl Attr {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    ///
    /// This attribute's value cannot be extended,
    ///   but it can be cheaply converted into [`Attr::Extensible`] via
    ///   [`Attr::parts`] or [`From`].
    #[inline]
    pub fn new(name: QName, value: SymbolId, span: (Span, Span)) -> Self {
        Self::Simple(SimpleAttr::new(name, value, span))
    }

    /// Construct a new attribute whose value will be incrementally
    ///   constructed.
    ///
    /// This is intended for use with
    ///   [`Token::AttrValueFragment`](super::Token::AttrValueFragment),
    ///     which provides for string concatenation while maintaining
    ///     [`Span`] resolution and being zero-copy.
    #[inline]
    pub fn new_extensible_with_capacity(
        name: QName,
        name_span: Span,
        capacity: usize,
    ) -> Self {
        Self::Extensible(AttrParts::with_capacity(name, name_span, capacity))
    }

    /// Create an attribute from a list of value fragments and their spans.
    ///
    /// This is intended not only for convenience,
    ///   but also to permit pre-allocating buffers,
    ///   or re-using them in conjunction with [`AttrParts::into_fragments`].
    #[inline]
    pub fn from_fragments(
        name: QName,
        name_span: Span,
        frags: Vec<(SymbolId, Span)>,
    ) -> Self {
        Self::Extensible(AttrParts {
            name,
            name_span,
            value_frags: frags,
        })
    }

    /// The inner [`AttrParts`] representing an attribute and its value
    ///   fragments.
    ///
    /// This provides the inner [`AttrParts`] needed to begin pushing value
    ///   fragments.
    ///
    /// If the attribute has no parts (is a [`SimpleAttr`]),
    ///   it will be converted into an extensible attribute with one value
    ///   fragment and then returned.
    #[inline]
    pub fn parts(self) -> AttrParts {
        match self {
            Self::Simple(attr) => attr.into(),
            Self::Extensible(parts) => parts,
        }
    }

    /// Attribute name.
    #[inline]
    pub fn name(&self) -> QName {
        match self {
            Self::Simple(attr) => attr.name,
            Self::Extensible(attr) => attr.name,
        }
    }

    /// Retrieve an atom from the attribute.
    ///
    /// An atom is cost-free if either
    ///   (a) this is [`Attr::Simple`]; or
    ///   (b) this is [`Attr::Extensible`] with one fragment.
    /// Otherwise,
    ///   this panics with a TODO,
    ///   since we haven't had a need for merging attributes [yet].
    ///
    /// Since [`SymbolId`] implements [`Copy`],
    ///   this returns an owned value.
    #[inline]
    pub fn value_atom(&self) -> SymbolId {
        match self {
            Self::Simple(attr) => attr.value,
            Self::Extensible(attr) if attr.value_frags.len() == 1 => {
                attr.value_frags[0].0
            }
            // We'll probably want to just merge and generate a new
            //   SymbolId,
            //     possibly having a separate variant of this method if we
            //     want to guarantee a cost-free conversion as a Result.
            Self::Extensible(attr) => todo!(
                "Multi-fragment attribute atom requested: {:?}",
                attr.value_frags
            ),
        }
    }
}

/// Element attribute with an atomic value.
///
/// This should be used in place of [`AttrParts`] whenever the attribute is
///   a simple [`QName`]/[`SymbolId`] pair.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SimpleAttr {
    name: QName,
    value: SymbolId,
    /// Spans for the attribute name and value respectively.
    span: (Span, Span),
}

impl SimpleAttr {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    #[inline]
    pub fn new(name: QName, value: SymbolId, span: (Span, Span)) -> Self {
        Self { name, value, span }
    }
}

/// Element attribute with a value composed of multiple fragments.
///
/// This should be used when one or more of these properties is desirable:
///   1. Zero-copy concatenation with respect to [symbols](crate::sym);
///   2. High-resolution [`Span`]s for each constituent fragment; and/or
///   3. You need to parse a XIR stream with
///        [`Token::AttrValueFragment`](super::Token::AttrValueFragment).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AttrParts {
    name: QName,
    name_span: Span,

    /// Ordered value fragments and their associated [`Span`]s.
    ///
    /// When writing,
    ///   fragments will be concatenated in order without any delimiters.
    value_frags: Vec<(SymbolId, Span)>,
}

impl AttrParts {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    #[inline]
    pub fn with_capacity(
        name: QName,
        name_span: Span,
        capacity: usize,
    ) -> Self {
        Self {
            name,
            name_span,
            value_frags: Vec::with_capacity(capacity),
        }
    }
}

impl AttrParts {
    /// Append a new value fragment and its associated span.
    ///
    /// Value fragments are intended to be concatenated on write without a
    ///   delimiter,
    ///     and are associated with
    ///     [`Token::AttrValueFragment`](super::Token::AttrValueFragment).
    #[inline]
    pub fn push_value(&mut self, value: SymbolId, span: Span) {
        self.value_frags.push((value, span));
    }

    /// Retrieve a read-only list of ordered value fragments and their
    ///   associated spans.
    ///
    /// If you want to consume the vector to re-use it for future
    ///   [`AttrParts`],
    ///     see [`into_fragments`](AttrParts::into_fragments).
    #[inline]
    pub fn value_fragments(&self) -> &Vec<(SymbolId, Span)> {
        &self.value_frags
    }

    /// Consume [`AttrParts`],
    ///   yielding its internal fragment buffer.
    ///
    /// This allows the buffer to be re-used for future [`AttrParts`],
    ///   avoiding additional heap allocations.
    #[inline]
    pub fn into_fragments(self) -> Vec<(SymbolId, Span)> {
        self.value_frags
    }
}

impl From<SimpleAttr> for AttrParts {
    fn from(attr: SimpleAttr) -> Self {
        Self {
            name: attr.name,
            name_span: attr.span.0,
            value_frags: vec![(attr.value, attr.span.1)],
        }
    }
}

impl From<Attr> for AttrParts {
    fn from(attr: Attr) -> Self {
        match attr {
            Attr::Simple(inner) => inner.into(),
            Attr::Extensible(inner) => inner,
        }
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

// See also [`super::test`] for many more tests related to attributes.
#[cfg(test)]
mod test {
    use super::*;
    use crate::{convert::ExpectInto, sym::GlobalSymbolIntern};

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case, 1".intern());
        static ref S2: Span =
            Span::from_byte_interval((0, 0), "test case, 2".intern());
    }

    #[test]
    fn attr_into_attr_parts() {
        let name = "attr".unwrap_into();
        let value = "value".intern();

        let attr = SimpleAttr {
            name,
            value,
            span: (*S, *S2),
        };

        let result = attr.clone().into();

        assert_eq!(
            AttrParts {
                name,
                name_span: *S,
                value_frags: vec![(value, *S2)],
            },
            result,
        );

        // Enum should also be able to do it
        assert_eq!(result, Attr::Simple(attr.clone()).into(),);
        assert_eq!(result, Attr::Simple(attr).parts(),);
    }

    #[test]
    fn push_attr_part() {
        let name = "pushattr".unwrap_into();
        let value1 = "first".intern();
        let value2 = "second".intern();

        let mut attr = Attr::new_extensible_with_capacity(name, *S, 2).parts();

        attr.push_value(value1, *S);
        attr.push_value(value2, *S2);

        assert_eq!(&vec![(value1, *S), (value2, *S2)], attr.value_fragments());
    }

    #[test]
    fn attr_from_parts() {
        let name = "pushattr".unwrap_into();
        let value1 = "first".intern();
        let value2 = "second".intern();

        let attr =
            Attr::from_fragments(name, *S, vec![(value1, *S), (value2, *S2)])
                .parts();

        assert_eq!(&vec![(value1, *S), (value2, *S2)], attr.value_fragments());
    }

    #[test]
    fn into_fragments_to_reuse_buffer_for_parts() {
        let name = "partbuffer".unwrap_into();
        let value1 = "first".intern();
        let value2 = "second".intern();
        let value3 = "third".intern();

        let frags = vec![(value1, *S2), (value2, *S)];

        let mut attr1 = Attr::from_fragments(name, *S, frags).parts();
        attr1.push_value(value3, *S2);

        // Notice that the value is owned, and so we can call
        // `from_fragments` again to re-use the buffer.
        assert_eq!(
            vec![(value1, *S2), (value2, *S), (value3, *S2)],
            attr1.into_fragments(),
        );
    }
}
