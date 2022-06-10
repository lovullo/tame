// TAMER formatting helpers
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

//! Typed formatting helpers.
//!
//! These types create composable formatters for use with [`Display`].
//! Whereas [`Display`] operates on data owned by the type implementing it,
//!   these formatters compose into functions that operate on data provided
//!   _to_ it.
//! Consequently,
//!   formatters are simply types,
//!   and writes can be streamed just as they are with [`Display`].
//!
//! There are two types of wrappers:
//!
//!   - [`DisplayWrapper`] formats objects as atoms; and
//!   - [`ListDisplayWrapper`] maps a [`DisplayWrapper`] to each of its
//!       items as atoms,
//!         where the specific wrapper used depends on the position of the
//!         item within the list and the properties of the list itself.
//!
//! [`DisplayWrapper::wrap`] and [`ListDisplayWrapper::wrap`] can be used to
//!   associate wrappers with data,
//!     effectively creating a custom [`Display`] implementation controlled
//!     by the caller.
//! This is _not_ a substitute for canonical object representations that own
//!   their own [`Display`] implementation,
//!     but is suitable where display is context-dependent.
//!
//! For example:
//!
//! ```
//! # use tamer::{fmt::*, xir::fmt::*};
//! let attrs = ["foo", "bar", "baz"];
//!
//! assert_eq!(
//!    AndQualConjList::<"attribute", "attributes", Tt<XmlAttr>>::wrap(&attrs)
//!        .to_string(),
//!    "attributes `@foo`, `@bar`, and `@baz`",
//! );
//!
//! assert_eq!(
//!    AndQualConjList::<"attribute", "attributes", Tt<XmlAttr>>::wrap(&attrs[0..1])
//!        .to_string(),
//!    "attribute `@foo`",
//! );
//!
//! assert_eq!(
//!     AndConjList::<Raw>::wrap(&["toil", "trouble"]).to_string(),
//!     "toil and trouble",
//! );
//! ```
//!
//! This is in contrast to the approach taken by (for example)
//!   the [diagnostic system](crate::diagnose),
//!     which is to produce a data structure representing the data to be
//!     formatted.
//! This system is not useful as,
//!   and is not intended to be,
//!   an IR for more sophisticated manipulation of data prior to output.

use std::{
    fmt::{Display, Formatter, Result},
    marker::PhantomData,
};

/// Wrapper for a [`Display`]-able type.
///
/// See the [module-level documentation](super) for more information.
pub trait DisplayWrapper {
    /// Transform inner data and output using the provided [`Formatter`].
    ///
    /// If a [`Formatter`] is not available,
    ///   [`ListDisplayWrapper::wrap`] may be used to produce a
    ///   [`Display`]-able object instead.
    fn fmt<T: Display>(inner: T, f: &mut Formatter) -> Result;

    /// Associate data with a [`DisplayWrapper`] for rendering using
    ///   [`Display`].
    ///
    /// This has the effect of creating an arbitrary [`Display`]
    ///   implementation for the wrapped object,
    ///     which will work well with [`format!`] and anything else that
    ///     does not have access to an explicit [`Formatter`].
    fn wrap<T: Display>(inner: T) -> Wrap<Self, T> {
        Wrap {
            inner,
            _phantom: Default::default(),
        }
    }
}

/// Wrapper with associated data.
///
/// This has the effect of creating an arbitrary [`Display`] implementation
///   for the wrapped data,
///     which will work well with [`format!`] and anything else that does
///     not have access to an explicit [`Formatter`].
pub struct Wrap<W: DisplayWrapper + ?Sized, T: Display> {
    inner: T,
    _phantom: PhantomData<W>,
}

impl<W: DisplayWrapper, T: Display> Display for Wrap<W, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        W::fmt(&self.inner, f)
    }
}

/// Echo data as-is without any wrapping.
///
/// This is primarily used at the root of a wrapper composition.
pub struct Raw;

impl DisplayWrapper for Raw {
    fn fmt<T: Display>(inner: T, f: &mut Formatter) -> Result {
        inner.fmt(f)
    }
}

/// Prefix data with a static [`str`].
///
/// See also [`Suffix`] and [`Delim`].
pub struct Prefix<const PREFIX: &'static str, W: DisplayWrapper>(
    PhantomData<W>,
);

impl<const PREFIX: &'static str, W: DisplayWrapper> DisplayWrapper
    for Prefix<PREFIX, W>
{
    fn fmt<T: Display>(inner: T, f: &mut Formatter) -> Result {
        f.write_str(PREFIX)?;
        W::fmt(inner, f)
    }
}

/// Suffix data with a static [`str`].
///
/// See also [`Prefix`] and [`Delim`].
pub struct Suffix<const SUFFIX: &'static str, W: DisplayWrapper>(
    PhantomData<W>,
);

impl<const SUFFIX: &'static str, W: DisplayWrapper> DisplayWrapper
    for Suffix<SUFFIX, W>
{
    fn fmt<T: Display>(inner: T, f: &mut Formatter) -> Result {
        W::fmt(inner, f)?;
        f.write_str(SUFFIX)
    }
}

/// Surround a value in delimiters.
///
/// See also [`Prefix`] and [`Suffix`].
pub type Delim<const LEFT: &'static str, const RIGHT: &'static str, W> =
    Prefix<LEFT, Suffix<RIGHT, W>>;

/// Denote text that would conventionally be delimited in a teletypewriter
///   font.
///
/// This produces a markdown-style quote using backticks.
///
/// NB: This does not defend against nested quotes,
///   so this is _not_ safe against format escapes.
pub type Tt<W> = Delim<"`", "`", W>;

/// Prefix with a single space.
pub type Sp<W> = Prefix<" ", W>;

/// Wrapper for a list that maps each element to a context-specific
///   [`DisplayWrapper`].
///
/// This uses the slice API for wrapping since [`Display`] takes objects by
///   non-mutable reference,
///     and so we cannot consume an iterator.
///
/// The associated types define the wrappers to use for items in various
///   positions depending on the length of the list.
/// They were chosen to be somewhat intuitive given the necessary use cases.
///
/// See the [module-level documentation](super) for examples.
pub trait ListDisplayWrapper {
    /// Wrapper to use when the list contains only a single item.
    type Single: DisplayWrapper = Raw;
    /// Wrapper for the first item in a multi-item list.
    type First: DisplayWrapper = Raw;
    /// Wrapper for all but the first and last items in a multi-item list.
    type Middle: DisplayWrapper = Raw;
    /// Wrapper for the last item of a list containing a pair of items.
    type LastOfPair: DisplayWrapper = Raw;
    /// Wrapper for the last item of a list containing more than two items.
    type LastOfMany: DisplayWrapper = Raw;

    /// Format a slice using the provided wrappers.
    ///
    /// If a [`Formatter`] is not available,
    ///   [`ListDisplayWrapper::wrap`] may be used to produce a
    ///   [`Display`]-able object instead.
    fn fmt<T: Display>(list: &[T], f: &mut Formatter) -> Result {
        let maxi = list.len().saturating_sub(1);

        // This can be further abstracted away using the above primitives,
        //   if ever we have a use.
        for next in list.into_iter().enumerate() {
            match next {
                (0, x) if maxi == 0 => {
                    Self::Single::fmt(x, f)?;
                }

                (0, x) => {
                    Self::First::fmt(x, f)?;
                }

                (i, x) if maxi == i => {
                    if i == 1 {
                        Self::LastOfPair::fmt(x, f)?;
                    } else {
                        Self::LastOfMany::fmt(x, f)?;
                    }
                }

                (_, x) => {
                    Self::Middle::fmt(x, f)?;
                }
            }
        }

        Ok(())
    }

    /// Associate data with a [`ListDisplayWrapper`] for rendering using
    ///   [`Display`].
    ///
    /// This has the effect of creating an arbitrary [`Display`]
    ///   implementation for the wrapped slice,
    ///     which will work well with [`format!`] and anything else that
    ///     does not have access to an explicit [`Formatter`].
    fn wrap<T: Display>(list: &[T]) -> ListWrap<Self, T> {
        ListWrap {
            list,
            _phantom: Default::default(),
        }
    }
}

/// Format each item of a slice using a [`DisplayWrapper`] formatter,
///   outputting an English list with a serial comma and conjunctive term.
///
/// No formatting is done to a single item,
///   and the serial comma is omitted for only two items.
/// It is assumed that the items are not complete sentences,
///   and nested lists are not expected
///     (in such a case you'd want to replace the inner list with
///       semicolons and a more robust abstraction is needed).
///
/// The use of the serial comma (also known as the Oxford comma) is the
///   preference of the author.
///
/// For example:
///   If we have a slice `[1, 2, 3]`,
///     this will output "1, 2, and 3".
///   If we have a slice `[1, 2]`,
///     it will omit the serial comma and output "1 and 2",
///       since no person would write "1, and 2" unless they wished to
///       place particularly dramatic emphasis on the first item.
///
/// To output a qualifier before the list,
///   see [`QualConjList`].
pub struct ConjList<const CONJ: &'static str, W: DisplayWrapper>(
    PhantomData<W>,
);

impl<const CONJ: &'static str, W: DisplayWrapper> ListDisplayWrapper
    for ConjList<CONJ, W>
{
    type Single = W;
    type First = W;
    type Middle = Prefix<", ", W>;
    type LastOfPair = Sp<Prefix<CONJ, Sp<W>>>;
    // Comma after the penultimate term (serial/Oxford comma).
    type LastOfMany = Prefix<", ", Prefix<CONJ, Sp<W>>>;
}

/// A list of values with a serial comma and the term "and" as a
///   conjunction between the penultimate and final items.
pub type AndConjList<W> = ConjList<"and", W>;
/// A list of values with a serial comma and the term "or" as a
///   conjunction between the penultimate and final items.
///
/// Terminology note:
///   English refers to the term "or" here as a conjunction between words,
///     which differs from "or" in logic as a disjunction.
pub type OrConjList<W> = ConjList<"or", W>;

/// Format each item of a slice using a [`DisplayWrapper`] formatter,
///   outputting an English list with a serial comma and conjunctive term,
///   qualified by a term that is singular or plural depending on the length
///   of the list.
///
/// See [`ConjList`] for more information;
///   this operates identically but with the addition of the qualifier.
pub struct QualConjList<
    const QUAL_ONE: &'static str,
    const QUAL_MANY: &'static str,
    const CONJ: &'static str,
    W: DisplayWrapper,
>(PhantomData<W>);

impl<
        const QUAL_ONE: &'static str,
        const QUAL_MANY: &'static str,
        const CONJ: &'static str,
        W: DisplayWrapper,
    > ListDisplayWrapper for QualConjList<QUAL_ONE, QUAL_MANY, CONJ, W>
{
    type Single =
        Prefix<QUAL_ONE, Sp<<ConjList<CONJ, W> as ListDisplayWrapper>::Single>>;
    type First =
        Prefix<QUAL_MANY, Sp<<ConjList<CONJ, W> as ListDisplayWrapper>::First>>;
    type Middle = <ConjList<CONJ, W> as ListDisplayWrapper>::Middle;
    type LastOfPair = <ConjList<CONJ, W> as ListDisplayWrapper>::LastOfPair;
    type LastOfMany = <ConjList<CONJ, W> as ListDisplayWrapper>::LastOfMany;
}

/// A list of values with a serial comma and the term "and" as a
///   conjunction between the penultimate and final items.
///     prefixed with a singular or plural qualifier term depending on the
///     length of the list.
pub type AndQualConjList<
    const QUAL_ONE: &'static str,
    const QUAL_MANY: &'static str,
    W,
> = QualConjList<QUAL_ONE, QUAL_MANY, "and", W>;

/// A list of values with a serial comma and the term "or" as a
///   conjunction between the penultimate and final items.
///     prefixed with a singular or plural qualifier term depending on the
///     length of the list.
///
/// Terminology note:
///   English refers to the term "or" here as a conjunction between words,
///     which differs from "or" in logic as a disjunction.
pub type OrQualConjList<
    const QUAL_ONE: &'static str,
    const QUAL_MANY: &'static str,
    W,
> = QualConjList<QUAL_ONE, QUAL_MANY, "or", W>;

/// List wrapper with associated data.
///
/// This has the effect of creating an arbitrary [`Display`] implementation
///   for the wrapped list,
///     which will work well with [`format!`] and anything else that does
///     not have access to an explicit [`Formatter`].
pub struct ListWrap<'a, W: ListDisplayWrapper + ?Sized, T: Display> {
    list: &'a [T],
    _phantom: PhantomData<W>,
}

impl<'a, W: ListDisplayWrapper + ?Sized, T: Display> Display
    for ListWrap<'a, W, T>
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        W::fmt(self.list, f)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn prefix() {
        assert_eq!(Prefix::<"@!", Raw>::wrap("foo").to_string(), "@!foo",);

        assert_eq!(
            Prefix::<"1", Prefix::<"2", Raw>>::wrap("bar").to_string(),
            "12bar",
        );
    }

    #[test]
    fn suffix() {
        assert_eq!(Suffix::<"!@", Raw>::wrap("foo").to_string(), "foo!@",);

        assert_eq!(
            Suffix::<"1", Suffix::<"2", Raw>>::wrap("bar").to_string(),
            "bar21",
        );
    }

    #[test]
    fn delimit() {
        assert_eq!(Delim::<"[", "]", Raw>::wrap("foo").to_string(), "[foo]",);

        assert_eq!(
            Delim::<"(", ")", Delim::<"|", "|", Raw>>::wrap("bar").to_string(),
            "(|bar|)",
        );
    }

    // Certain types are trivially verifiable by their definition,
    //   and so have no tests.

    #[test]
    fn conj_list_single() {
        assert_eq!(
            ConjList::<"unused", Raw>::wrap(&["single"]).to_string(),
            "single",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            ConjList::<"unused", Delim<"(", ")", Raw>>::wrap(&["single"])
                .to_string(),
            "(single)",
        );
    }

    #[test]
    fn conj_list_double() {
        assert_eq!(
            ConjList::<"and", Raw>::wrap(&["first", "second"]).to_string(),
            "first and second",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            ConjList::<"or", Delim<"(", ")", Raw>>::wrap(&["first", "second"])
                .to_string(),
            "(first) or (second)",
        );
    }

    #[test]
    fn conj_list_many() {
        assert_eq!(
            ConjList::<"and", Raw>::wrap(&["first", "second", "third"])
                .to_string(),
            "first, second, and third",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            ConjList::<"or", Delim<"[", "]", Raw>>::wrap(&[
                "first", "second", "third"
            ])
            .to_string(),
            "[first], [second], or [third]",
        );
    }

    #[test]
    fn qualified_conj_list_single() {
        assert_eq!(
            QualConjList::<"thing", "things", "unused", Raw>::wrap(&["a"])
                .to_string(),
            "thing a",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            QualConjList::<"thing", "things", "unused", Delim<"(", ")", Raw>>::wrap(&["a"])
                .to_string(),
            "thing (a)",
        );
    }

    #[test]
    fn qualified_conj_list_double() {
        assert_eq!(
            QualConjList::<"thing", "things", "and", Raw>::wrap(&["a", "b"])
                .to_string(),
            "things a and b",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            QualConjList::<"thing", "things", "or", Delim<"(", ")", Raw>>::wrap(&["a", "b"])
                .to_string(),
            "things (a) or (b)",
        );
    }

    #[test]
    fn qualified_conj_list_many() {
        assert_eq!(
            QualConjList::<"thing", "things", "and", Raw>::wrap(&[
                "a", "b", "c"
            ])
            .to_string(),
            "things a, b, and c",
        );

        // Ensure that we're actually mapping.
        assert_eq!(
            QualConjList::<"thing", "things", "or", Delim<"(", ")", Raw>>::wrap(&["a", "b", "c"])
                .to_string(),
            "things (a), (b), or (c)",
        );
    }
}
