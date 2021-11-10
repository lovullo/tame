// XIR string and escape context
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

//! [`XirString`] and escape context.
//!
//! Safety
//! ======
//! The purpose of this type is to provide safety against XML injection by
//!   encapsulating all responsibility within a single object.
//! The idea is simple:
//!   if you have a safely constructed [`XirString`],
//!     it can be safely used in any context without worrying about these
//!     critical problems:
//!
//!  1. XML injection (via lack of escaping); and
//!  2. Erroneous multiple escape/unescape.
//!
//! Both of these problems are solved by ensuring that the proper context
//!   for a given [`SymbolId`] is always maintained---a
//!     symbol is either valid to be written or not.
//! Similarly,
//!   we must know whether a symbol is escaped or not to know whether it
//!   ought to be unescaped while reading.
//!
//! This context also ensures that we will not erroneously unescape or
//!   re-escape values at multiple points in a program,
//!     leading to incorrect data at best and vulnerabilities at worst.
//!
//! To ensure this safety,
//!   it is important that types understand how to convert between
//!   one-another in well-defined ways.
//! It should not be possible "just assume" that a value has already been
//!   escaped.
//! Given that,
//!   the constructors for this type are private to this module;
//!     readers know the escape status and can produce the proper type and
//!     internal types know how to translate between one-another,
//!       but anything else making those assumptions is considered unsafe.
//!
//! Outside of this module,
//!   it is assumed that the rest of the system is working with _unescaped_
//!   values---afterall,
//!     why would other parts of the system not dealing with XML directly
//!     take it upon themselves to deal with XML directly?
//! Given that,
//!   other modules can only read unescaped values and construct
//!   [`XirString`] using unescaped values.
//! If we permitted retrieving raw escaped [`SymbolId`]s,
//!   then we could construct from it another [`XirString`] that is
//!   considered to be unescaped,
//!     which would result in a double-escape if it were read using
//!     [`XirString::escaped`].

use crate::sym::{
    CIdentStaticSymbolId, GlobalSymbolInternBytes, GlobalSymbolInternUnchecked,
    GlobalSymbolResolve, SymbolId, UriStaticSymbolId,
};
use std::{
    borrow::Cow,
    fmt::Display,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use super::Error;

/// An XML string that requires escaping before writing.
///
/// This type must be used in contexts where writing to an XML document is
///   not safe without proper escaping,
///     and where reading may require unescaping.
///
#[derive(Debug, Clone, Copy)]
pub struct XirString<S: XirStringEscaper = DefaultXirStringEscaper> {
    unescaped: SymbolId,
    escaped: Option<SymbolId>,

    _escaper: PhantomData<S>,
}

// Since we implement Copy,
//   ensure this size meets our expectations both as a sanity check and to
//   ensure that attention is brought to this if it ever changes.
const_assert!(std::mem::size_of::<XirString>() <= std::mem::size_of::<usize>());

// A note about this type:
//   Both fields are optional,
//     but it is not valid to have both be `None`.
//   To ensure that this is not possible,
//     (a) the fields must remain private;
//     (b) all constructors must initialize at least one of the fields; and
//     (c) mutation must reconstruct using those constructors.
//   This makes it possible to prove that the invariant always holds.
impl<S: XirStringEscaper> XirString<S> {
    pub(super) fn from_escaped_raw(escaped: &[u8]) -> Result<Self, Error> {
        let esc_sym = escaped.intern_utf8()?;

        Ok(Self {
            escaped: Some(esc_sym),
            unescaped: match S::unescape_bytes(escaped)? {
                // We got back what we sent in,
                //   so this value is fixed.
                Cow::Borrowed(orig) => {
                    debug_assert!(orig == escaped);
                    esc_sym
                }

                // The value was rewritten,
                //   meaning that the original was escaped.
                // We can't assume that it's valid UTF-8.
                Cow::Owned(unesc) => unesc.intern_utf8()?,
            },

            _escaper: PhantomData,
        })
    }

    pub const fn new_unescaped(sym: SymbolId) -> Self {
        Self {
            escaped: None,
            unescaped: sym,
            _escaper: PhantomData,
        }
    }

    const fn new_fixed(sym: SymbolId) -> Self {
        Self {
            escaped: Some(sym),
            unescaped: sym,
            _escaper: PhantomData,
        }
    }

    pub const unsafe fn assume_fixed(sym: SymbolId) -> Self {
        Self::new_fixed(sym)
    }

    /// Construct a constant escaped attribute from a static C-style symbol.
    pub const fn st_cid(sym: CIdentStaticSymbolId) -> Self {
        Self::new_fixed(sym.as_sym())
    }

    /// Construct a constant escaped attribute from a static URI symbol.
    ///
    /// URIs are expected _not_ to contain quotes.
    pub const fn st_uri(sym: UriStaticSymbolId) -> Self {
        Self::new_fixed(sym.as_sym())
    }

    // TODO: This unnecessarily allocates a symbol that'll just be written
    //   and not needed thereafter.
    #[inline]
    pub(super) fn into_escaped(self) -> SymbolId {
        self.escaped.unwrap_or_else(|| S::escape(self.unescaped))
    }

    #[inline]
    pub fn unescaped(&self) -> SymbolId {
        self.unescaped
    }
}

impl PartialEq for XirString {
    fn eq(&self, other: &Self) -> bool {
        self.unescaped == other.unescaped
    }
}

impl Eq for XirString {}

impl Hash for XirString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.unescaped.hash(state);
    }
}

impl From<SymbolId> for XirString {
    fn from(sym: SymbolId) -> Self {
        Self::new_unescaped(sym)
    }
}

impl Into<SymbolId> for XirString {
    fn into(self) -> SymbolId {
        self.unescaped
    }
}

impl Display for XirString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.unescaped.fmt(f)
    }
}

pub trait XirStringEscaper {
    fn escape_bytes(value: &[u8]) -> Cow<[u8]>;

    fn unescape_bytes(value: &[u8]) -> Result<Cow<[u8]>, Error>;

    fn escape(sym: SymbolId) -> SymbolId {
        match Self::escape_bytes(sym.lookup_str().as_bytes()) {
            // We got back what we sent in,
            //   so this value is fixed.
            Cow::Borrowed(_) => sym,

            // The value changed,
            //   so we must allocate a new symbol.
            // SAFETY: The unescaped symbol is valid UTF-8 unless it was
            //     unsafely allocated.
            //   Given that escaping does not introduce any invalid UTF-8
            //     sequences
            //       (as is trivially verified by reading its implementation),
            //       we can skip the UTF-8 check.
            Cow::Owned(esc) => unsafe { esc[..].intern_utf8_unchecked() },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct QuickXmlXirStringEscaper {}

impl XirStringEscaper for QuickXmlXirStringEscaper {
    #[inline]
    fn escape_bytes(value: &[u8]) -> Cow<[u8]> {
        quick_xml::escape::escape(value)
    }

    #[inline]
    fn unescape_bytes(value: &[u8]) -> Result<Cow<[u8]>, Error> {
        // For some reason,
        //   quick-xml has made EscapeError explicitly private to the crate,
        //     and so it is opaque to us.
        // They have, however,
        //   implemented `From<EscapeError> for Error`,
        //   which we will use here.
        Ok(quick_xml::escape::unescape(value)
            .map_err(quick_xml::Error::from)?)
    }
}

pub type DefaultXirStringEscaper = QuickXmlXirStringEscaper;

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::GlobalSymbolIntern;

    type Sut<S> = XirString<S>;

    #[test]
    fn create_from_and_retrieve_unescaped() {
        let sym = "foo".intern();
        let sut = Sut::new_unescaped(sym);

        // Converting to a symbol yields the _unescaped_ value.
        assert_eq!(sym, sut.into());

        // An explicit method is also provided when the clarity is deemed
        //   necessary.
        assert_eq!(sym, sut.unescaped());
    }

    // The unescaped values are used to identify the SUT.
    #[test]
    fn eq_on_unescape() {
        let sym = "equal".intern();
        assert_eq!(Sut::new_unescaped(sym), Sut::new_unescaped(sym));
    }

    #[test]
    fn escapes_using_escaper() {
        const GIVEN: &str = "str to escape";
        const EXPECTED: &str = "ESCAPED";

        struct MockEscaper {}

        impl XirStringEscaper for MockEscaper {
            fn escape_bytes<'a>(value: &'a [u8]) -> Cow<'a, [u8]> {
                assert_eq!(GIVEN.as_bytes(), value);
                Cow::Owned(EXPECTED.as_bytes().to_owned())
            }

            fn unescape_bytes(_: &[u8]) -> Result<Cow<[u8]>, Error> {
                unreachable!("not used in this test")
            }
        }

        let sut = Sut::<MockEscaper>::new_unescaped(GIVEN.intern());

        // Note that this uses the MockEscaper defined above,
        //   _not_ quick_xml.
        assert_eq!(EXPECTED.intern(), sut.into_escaped());
    }

    // Simple sanity check to ensure that the default escaper actually does
    //   some sort of escaping.
    #[test]
    fn default_escaper_escapes() {
        assert_eq!(
            "foo&lt;bar".intern(),
            Sut::<DefaultXirStringEscaper>::new_unescaped("foo<bar".intern())
                .into_escaped()
        );
    }
}
