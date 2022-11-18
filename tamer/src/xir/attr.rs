// XIRT attributes
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

//! XIRT attributes.
//!
//! Attributes are represented by [`Attr`].
//!
//! See [parent module](super) for additional documentation.

mod parse;

use super::QName;
use crate::{
    parse::Token,
    span::{Span, SpanLenSize},
    sym::SymbolId,
};
use std::fmt::Display;

pub use parse::{AttrParseError, AttrParseState};

/// Element attribute.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr(pub QName, pub SymbolId, pub AttrSpan);

/// Spans associated with attribute key and value.
///
/// The diagram below illustrates the behavior of `AttrSpan`.
/// Note that the extra spaces surrounding the `=` are intentional to
///   illustrate what the behavior ought to be.
/// Spans are represented by `[---]` intervals,
///   with the byte offset at each end,
///   and the single-letter span name centered below the interval.
/// `+` represents intersecting `-` and `|` lines.
///
/// ```text
///   <foo bar  =  "baz" />
///        [-]     [+-+]
///        5 7    13| |17
///        |K       |Q||
///        |        | ||
///        |        [-]|
///        |       14 16
///        |         V |
///        [-----------]
///              A
/// ```
///
/// Above we have
///
///   - `A` = [`AttrSpan::span`];
///   - `K` = [`AttrSpan::key_span`];
///   - `V` = [`AttrSpan::value_span`]; and
///   - `Q` = [`AttrSpan::value_span_with_quotes`].
///
/// Note that this object assumes that the key and value span are adjacent
///   to one-another in the same [`span::Context`](crate::span::Context).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrSpan(pub Span, pub Span);

impl AttrSpan {
    /// A [`Span`] covering the entire attribute token,
    ///   including the key,
    ///   _quoted_ value,
    ///   and everything in-between.
    pub fn span(&self) -> Span {
        let AttrSpan(k, _) = self;

        // TODO: Move much of this into `Span`.
        k.context().span(
            k.offset(),
            self.value_span_with_quotes()
                .endpoints_saturated()
                .1
                .offset()
                .saturating_sub(k.offset())
                .try_into()
                .unwrap_or(SpanLenSize::MAX),
        )
    }

    /// The span associated with the name of the key.
    ///
    /// This does _not_ include the following `=` or any surrounding
    ///   whitespace.
    pub fn key_span(&self) -> Span {
        let AttrSpan(k, _) = self;
        *k
    }

    /// The span associated with the string value _inside_ the quotes,
    ///   not including the quotes themselves.
    ///
    /// See [`AttrSpan`]'s documentation for an example.
    pub fn value_span(&self) -> Span {
        let AttrSpan(_, v) = self;
        *v
    }

    /// The span associated with the string value _including_ the
    ///   surrounding quotes.
    ///
    /// See [`AttrSpan`]'s documentation for an example.
    pub fn value_span_with_quotes(&self) -> Span {
        let AttrSpan(_, v) = self;

        v.context()
            .span(v.offset().saturating_sub(1), v.len().saturating_add(2))
    }
}

impl Attr {
    /// Construct a new simple attribute with a name, value, and respective
    ///   [`Span`]s.
    #[inline]
    pub fn new(name: QName, value: SymbolId, span: (Span, Span)) -> Self {
        Self(name, value, AttrSpan(span.0, span.1))
    }

    /// Attribute name.
    #[inline]
    pub fn name(&self) -> QName {
        self.0
    }

    /// Retrieve the value from the attribute.
    ///
    /// Since [`SymbolId`] implements [`Copy`],
    ///   this returns an owned value.
    #[inline]
    pub fn value(&self) -> SymbolId {
        self.1
    }

    /// [`AttrSpan`] for this attribute.
    ///
    /// The attribute span allows deriving a number of different spans;
    ///   see [`AttrSpan`] for more information.
    pub fn attr_span(&self) -> &AttrSpan {
        match self {
            Attr(.., span) => span,
        }
    }

    /// Replace the attribute's value with a new one that has been derived
    ///   from the original.
    ///
    /// _This does not update the value span!_
    /// The intent is for this operation to be used during term rewriting,
    ///   where the replaced value is _derived from_ the original and so
    ///   should retain the original span to provide the proper context to
    ///   the user.
    pub fn replace_value_derived(self, value_new: SymbolId) -> Self {
        match self {
            Self(name, _, span) => Self(name, value_new, span),
        }
    }
}

impl Token for Attr {
    fn ir_name() -> &'static str {
        // This may be used by multiple things,
        //   but it's primarily used by XIRF.
        "XIRF"
    }

    fn span(&self) -> Span {
        match self {
            Attr(.., attr_span) => attr_span.span(),
        }
    }
}

impl crate::parse::Object for Attr {}

impl Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Do not display value since it can contain any information and
        //   mess up formatted output.
        // If we wish to display that information in the future,
        //   then we ought to escape and elide it,
        //   but we must furthermore make sure that it makes sense in all
        //     contexts;
        //       many diagnostic messages today expect that outputting an
        //       attribute will output the name of that attribute and
        //       nothing more.
        match self {
            Self(key, _value, _) => write!(f, "@{key}"),
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

    pub fn extend<T: IntoIterator<Item = Attr>>(mut self, iter: T) -> Self {
        self.attrs.extend(iter);
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

impl FromIterator<Attr> for AttrList {
    fn from_iter<T: IntoIterator<Item = Attr>>(iter: T) -> Self {
        iter.into_iter().collect::<Vec<Attr>>().into()
    }
}

impl<const N: usize> From<[Attr; N]> for AttrList {
    fn from(attrs: [Attr; N]) -> Self {
        AttrList {
            attrs: attrs.into(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::span::dummy::DUMMY_CONTEXT as DC;

    use super::*;

    // See docblock for [`AttrSpan`].
    const A: Span = DC.span(5, 13); // Entire attribute token
    const K: Span = DC.span(5, 3); //  Key
    const V: Span = DC.span(14, 3); // Value without quotes
    const Q: Span = DC.span(13, 5); // Value with quotes

    #[test]
    fn attr_span_token() {
        assert_eq!(AttrSpan(K, V).span(), A);
    }

    #[test]
    fn attr_span_value_with_quotes() {
        assert_eq!(AttrSpan(K, V).value_span_with_quotes(), Q);
    }

    #[test]
    fn attr_span_key() {
        assert_eq!(AttrSpan(K, V).key_span(), K);
    }

    #[test]
    fn attr_span_value() {
        assert_eq!(AttrSpan(K, V).value_span(), V);
    }
}
