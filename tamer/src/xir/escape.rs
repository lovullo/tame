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
//! For more information on caching employed by TAMER to improve
//!   performance,
//!     see [`CachingEscaper`].
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

use fxhash::FxHashMap;

use crate::sym::{
    GlobalSymbolInternBytes, GlobalSymbolInternUnchecked, GlobalSymbolResolve,
    SymbolId,
};
use std::{borrow::Cow, cell::RefCell, collections::hash_map::Entry};

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
    #[inline]
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
    #[inline]
    fn unescape(&self, escaped: SymbolId) -> Result<SymbolId, Error> {
        Ok(
            match Self::unescape_bytes(escaped.lookup_str().as_bytes())? {
                // We got back what we sent in,
                //   so this value is fixed.
                Cow::Borrowed(_) => escaped,

                // The value was rewritten,
                //   meaning that the original was escaped.
                // We can't assume that it's valid UTF-8.
                Cow::Owned(unesc) => unesc.intern_utf8()?,
            },
        )
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

/// Cache escaped and unescaped [`SymbolId`]s.
///
/// _This cache should be shared between all readers and writers._
///
/// This takes advantage of the efficiency of the string internment system
///   to avoid the costs of escaping/unescaping if we've already encountered
///   the requested symbol previously.
///
/// There are a number of ways this is beneficial:
///
/// When a string is read,
///   its escaped [`SymbolId`] and associated unescaped [`SymbolId`] are
///   stored in a two-way mapping.
/// If another reader encounters the same [`SymbolId`],
///   it does not need to spend the time attempting to unescape it,
///   and will simply re-use the existing cached [`SymbolId`].
///
/// When a writer encounters a [`SymbolId`]
///   (representing the _unescaped_ value),
///   it is able to retrieve from cache the escaped [`SymbolId`] that was
///     originally encountered by a reader,
///       thereby saving it the time of re-escaping.
///
/// Escaped Representation
/// ======================
/// Note that this means that the escaped value will be the same as the
///   _first_ time that unescaped value was read
///   (there are many different ways to escape the same value);
///     an [`Escaper`] _does not_ guarantee a canonical escaped
///     representation.
///
/// While this appears to add a source of nondeterminism that undermines
///   reproducible builds,
///     it is mitigated by applying ordering to how files are loaded,
///     which is necessary to mitigate much more serious sources of
///       filesystem-based nondeterminism.
///
/// If this is burdensome in the future
///   (e.g. when writing a code formatter that needs to retain escapes),
///   there are other potential mitigations,
///     including modifying [`Escaper`] to accept spans as context or
///       augmenting XIR with an unescape hint.
#[derive(Debug, Default)]
pub struct CachingEscaper<S: Escaper> {
    /// Inner [`Escaper`] to be invoked to populate the cache.
    inner: S,
    /// Map from unescaped [`SymbolId`]s to their escaped represeation.
    toesc: RefCell<FxHashMap<SymbolId, SymbolId>>,
    /// Map from escaped [`SymbolId`]s to their unescaped value.
    tounesc: RefCell<FxHashMap<SymbolId, SymbolId>>,
}

impl<S: Escaper> CachingEscaper<S> {
    pub fn new(inner: S) -> Self {
        // TODO: Capacity that is at least double the length of the static
        // symbols.
        Self {
            inner,
            ..Default::default()
        }
    }

    pub fn into_inner(self) -> S {
        self.inner
    }
}

impl<S: Escaper> Escaper for CachingEscaper<S> {
    #[inline]
    fn escape_bytes(value: &[u8]) -> Cow<[u8]> {
        S::escape_bytes(value)
    }

    #[inline]
    fn unescape_bytes(value: &[u8]) -> Result<Cow<[u8]>, Error> {
        S::unescape_bytes(value)
    }

    #[inline]
    fn escape(&self, unescaped: SymbolId) -> SymbolId {
        *self.toesc.borrow_mut().entry(unescaped).or_insert_with(|| {
            let escaped = self.inner.escape(unescaped);

            // Later requests to unescape this newly escaped symbol will
            //   yield the unescaped value provided here.
            self.tounesc
                .borrow_mut()
                .entry(escaped)
                .or_insert(unescaped);

            escaped
        })
    }

    #[inline]
    fn unescape(&self, escaped: SymbolId) -> Result<SymbolId, Error> {
        Ok(match self.tounesc.borrow_mut().entry(escaped) {
            Entry::Occupied(unescaped) => *unescaped.get(),
            Entry::Vacant(entry) => {
                let unescaped = *entry.insert(self.inner.unescape(escaped)?);

                // There are many escaped representations for the same
                //   unescaped value.
                // We will keep the first one that we encountered.
                self.toesc.borrow_mut().entry(unescaped).or_insert(escaped);

                unescaped
            }
        })
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

#[cfg(feature = "wip-xmlo-xir-reader")]
pub type DefaultEscaper = CachingEscaper<QuickXmlEscaper>;
#[cfg(not(feature = "wip-xmlo-xir-reader"))]
pub type DefaultEscaper = NullEscaper;

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::GlobalSymbolIntern;

    // Simple sanity check to ensure that the default escaper actually does
    //   some sort of escaping.
    #[cfg(feature = "wip-xmlo-xir-reader")]
    #[test]
    fn default_escaper_escapes() {
        let sut = DefaultEscaper::default();

        assert_eq!(
            "foo&lt;bar".intern(),
            sut.escape("foo<bar".intern()).into(),
        );
    }

    mod cache {
        use super::*;
        use std::{collections::HashMap, result};

        // Maintain counts of calls rather than providing stubs,
        //   to avoid `RefCell<Rc<Refcell<Option<SymbolId>>>>` for
        //   concurrent access.
        #[derive(Debug, Default)]
        struct StubEscaper {
            escape_map: HashMap<SymbolId, SymbolId>,
            unescape_map: HashMap<SymbolId, SymbolId>,
            escape_count: RefCell<FxHashMap<SymbolId, usize>>,
            unescape_count: RefCell<FxHashMap<SymbolId, usize>>,
        }

        impl Escaper for StubEscaper {
            fn escape_bytes(_: &[u8]) -> Cow<[u8]> {
                unreachable!("escape_bytes should not be called")
            }

            fn unescape_bytes(_: &[u8]) -> result::Result<Cow<[u8]>, Error> {
                unreachable!("unescape_bytes should not be called")
            }

            fn escape(&self, given: SymbolId) -> SymbolId {
                *self.escape_count.borrow_mut().entry(given).or_default() += 1;
                *self.escape_map.get(&given).expect("unexpected escape")
            }

            fn unescape(&self, given: SymbolId) -> Result<SymbolId, Error> {
                *self.unescape_count.borrow_mut().entry(given).or_default() +=
                    1;
                Ok(*self.unescape_map.get(&given).expect("unexpected unescape"))
            }
        }

        #[test]
        fn caching_escaper_unescape() {
            let esc = "escaped".intern();
            let unesc = "unescaped".intern();

            let sut = CachingEscaper::new(StubEscaper {
                escape_map: [(unesc, esc)].into(),
                unescape_map: [(esc, unesc)].into(),
                ..Default::default()
            });

            // Invoke unescape more than once to ensure caching occurs.
            assert_eq!(sut.unescape(esc).unwrap(), unesc);
            assert_eq!(sut.unescape(esc).unwrap(), unesc);

            // And escape once, using a previous unescaped result.
            assert_eq!(sut.escape(unesc), esc);

            // We should have invoked the underlying escaper only once for
            //   the unescape operation.
            let stub = sut.into_inner();
            assert_eq!(stub.unescape_count.borrow().get(&esc), Some(&1));

            // And, having previously encountered the escaped value from
            //   unescaping,
            //     we should _not_ have invoked the escaper _at all_ when we
            //     escaped the value.
            // This means that previously encountered escapes will always
            //   take precedence over any explicit escape result.
            assert_eq!(stub.escape_count.borrow().get(&unesc), None);
        }

        #[test]
        fn caching_escaper_escape() {
            let esc = "escaped".intern();
            let unesc = "unescaped".intern();

            let sut = CachingEscaper::new(StubEscaper {
                escape_map: [(unesc, esc)].into(),
                unescape_map: [(esc, unesc)].into(),
                ..Default::default()
            });

            // Invoke escape more than once to ensure caching occurs.
            assert_eq!(sut.escape(unesc), esc);
            assert_eq!(sut.escape(unesc), esc);

            // And unescape once, using a previous escaped result.
            assert_eq!(sut.unescape(esc).unwrap(), unesc);

            // We should have invoked the underlying escaper only once for
            //   the escape operation.
            let stub = sut.into_inner();
            assert_eq!(stub.escape_count.borrow().get(&unesc), Some(&1));

            // And, having previously encountered the unescaped value from
            //   escaping,
            //     we should _not_ have invoked the escaper _at all_ when we
            //     unescaped the value.
            // This means that previously encountered unescapes will always
            //   take precedence over any explicit unescape result.
            assert_eq!(stub.unescape_count.borrow().get(&esc), None);
        }
    }
}
