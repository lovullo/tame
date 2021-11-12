// XIR string escaping and unescaping
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

//! Escaping and unescaping for writers and readers respectively.
//!
//! An [`Escaper`] is required by XIR readers and writers.
//! An escaper may perform caching to avoid unnecessary work,
//!   so it is advantageous to provide the _same_ instance to all readers
//!   and writers.
//! [`Escaper`] methods use interior mutability to facilitate this,
//!   since TAMER streams lowering operations where possible,
//!   meaning that multiple readers and writers will require references
//!     to the [`Escaper`].
//!
//! Safety
//! ======
//! The purpose of this type is to provide safety against XML injection by
//!   encapsulating all responsibility within a single object.
//! The idea is simple:
//!   a [`SymbolId`] _always_ represents an unescaped string.
//! This prevents, primarily,
//!
//!  1. XML injection (via lack of escaping); and
//!  2. Erroneous multiple escape/unescape.
//!
//! This module is the _only_ part of the system that has access to raw,
//!   escaped values.
//! Outside of this module,
//!   it is assumed that the rest of the system is working with _unescaped_
//!   values---afterall,
//!     why would other parts of the system not dealing with XML directly
//!     take it upon themselves to deal with XML directly?
//! If we permitted retrieving raw escaped [`SymbolId`]s,
//!   then we run the risk of that value being used to construct a XIR
//!   stream and be subsequently double-encoded upon writing.

use crate::sym::{
    GlobalSymbolInternBytes, GlobalSymbolInternUnchecked, GlobalSymbolResolve,
    SymbolId,
};
use std::borrow::Cow;

use super::Error;

/// XIR escaper and unescaper.
///
/// Escapers are responsible for parsing XML escape sequences as necessary
///   on read,
///     and properly escaping characters on write.
/// This is the only part of the system defending XIR against XML
///   injection.
///
/// Escapers must use interior mutability for any internal state
///   (e.g. caching),
///     since multiple readers and writers will require references.
pub trait Escaper: Default {
    /// Escape raw bytes such that they become suitable for writing into an
    ///   XML document as text.
    ///
    /// This value must be escaped such that subsequence unescaping
    ///   (using [`unescape_bytes`](Escaper::unescape_bytes))
    ///   will result in the same value.
    fn escape_bytes(value: &[u8]) -> Cow<[u8]>;

    /// Unescape raw bytes such that any relevant escape sequences are
    ///   parsed into their text representation.
    fn unescape_bytes(value: &[u8]) -> Result<Cow<[u8]>, Error>;

    /// Escape the given symbol and produce a [`SymbolId`] representing
    ///   the escaped value suitable for writing.
    fn escape(&self, sym: SymbolId) -> SymbolId {
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

    /// Unescape the provided raw value and return a [`SymbolId`]
    ///   representing the unescaped value.
    fn unescape_intern<'a>(
        &self,
        escaped: &'a [u8],
    ) -> Result<SymbolId, Error> {
        Ok(match Self::unescape_bytes(escaped)? {
            // We got back what we sent in,
            //   so this value is fixed.
            Cow::Borrowed(orig) => {
                debug_assert!(orig == escaped);
                orig.intern_utf8()?
            }

            // The value was rewritten,
            //   meaning that the original was escaped.
            // We can't assume that it's valid UTF-8.
            Cow::Owned(unesc) => unesc.intern_utf8()?,
        })
    }
}

/// Escape and unescape using [`quick_xml`].
#[derive(Debug, Clone, Copy, Default)]
pub struct QuickXmlEscaper {}

impl Escaper for QuickXmlEscaper {
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

/// Perform no escaping or unescaping.
///
/// _This should be removed after development of the XIR-based readers!_
#[cfg(not(feature = "wip-xmlo-xir-reader"))]
#[derive(Debug, Clone, Copy, Default)]
pub struct NullEscaper {}

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
impl Escaper for NullEscaper {
    #[inline]
    fn escape_bytes(value: &[u8]) -> Cow<[u8]> {
        Cow::Borrowed(value)
    }

    #[inline]
    fn unescape_bytes(_value: &[u8]) -> Result<Cow<[u8]>, Error> {
        panic!("NullEscaper should not be used for unescaping")
    }
}

pub type DefaultEscaper = QuickXmlEscaper;

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::GlobalSymbolIntern;

    // Simple sanity check to ensure that the default escaper actually does
    //   some sort of escaping.
    #[test]
    fn default_escaper_escapes() {
        let sut = DefaultEscaper::default();

        assert_eq!(
            "foo&lt;bar".intern(),
            sut.escape("foo<bar".intern()).into(),
        );
    }
}
