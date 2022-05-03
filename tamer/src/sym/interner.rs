// String interner
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

//! Interners used to intern values as symbols.
//!
//! See the [parent module](super) for more information.
//!
//!
//! Using Interners Directly (Without Global State)
//! ===============================================
//! Please do not do this unless you have a compelling use case and know
//!   what you are doing,
//!     including understanding how to mitigate mixing of [`SymbolId`]s,
//!       such as with newtypes or encapsulation.
//! Otherwise,
//!   use the global interners instead,
//!     as documented in the [parent module](super).
//!
//! ```
//! use tamer::sym::{DefaultProgInterner, Interner, SymbolId};
//!
//! // Inputs to be interned
//! let a = "foo";
//! let b = &"foo".to_string();
//! let c = "foobar";
//! let d = &c[0..3];
//!
//! // Interners employ interior mutability and so do not need to be
//! // declared `mut`
//! let interner = DefaultProgInterner::new();
//!
//! let (ia, ib, ic, id) = (
//!     interner.intern(a),
//!     interner.intern(b),
//!     interner.intern(c),
//!     interner.intern(d),
//! );
//!
//! assert_eq!(ia, ib);
//! assert_eq!(ia, id);
//! assert_eq!(ib, id);
//! assert_ne!(ia, ic);
//!
//! // Only "foo" and "foobar" are interned
//! assert_eq!(2, interner.len());
//! assert!(interner.contains("foo"));
//! assert!(interner.contains("foobar"));
//! assert!(!interner.contains("something else"));
//!
//! // Symbols can also be looked up by index.
//! assert_eq!("foo", interner.index_lookup(ia).unwrap());
//! ```

use super::{SymbolId, SymbolIndexSize};
use crate::global;
use bumpalo::Bump;
use fxhash::FxBuildHasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::hash::BuildHasher;
use std::str::{from_utf8, from_utf8_unchecked, Utf8Error};

/// Create, store, compare, and retrieve interned values.
///
/// Interners accept string slices and produce values of type [`SymbolId`].
/// The same [`SymbolId`] will always be returned for a given string,
///   allowing symbols to be compared for equality cheaply by comparing
///   integers.
/// Symbol locations in memory are fixed for the lifetime of the interner,
///   and can be retrieved as [`str`] using
///   [`index_lookup`](Interner::index_lookup).
///
/// If you care whether a value has been interned yet or not,
///   see [`intern_soft`][Interner::intern_soft`] and
///     [`contains`](Interner::contains).
///
/// See the [module-level documentation](self) for an example.
/// For interfaces to the global interners that indirectly use these
///   methods,
///     see the [parent module](super).
pub trait Interner<'i, Ix: SymbolIndexSize> {
    /// Intern a string slice or return an existing [`SymbolId`].
    ///
    /// If the provided string has already been interned,
    ///   then an existing [`SymbolId`] will be returned.
    /// Otherwise,
    ///   the string will be interned and a new [`SymbolId`] allocated.
    ///
    /// To retrieve an existing symbol _without_ interning,
    ///   see [`intern_soft`](Interner::intern_soft).
    fn intern(&self, value: &str) -> SymbolId<Ix>;

    /// Retrieve an existing intern for the provided string slice.
    ///
    /// Unlike [`intern`](Interner::intern),
    ///   this will _not_ intern the string if it has not already been
    ///   interned.
    fn intern_soft(&self, value: &str) -> Option<SymbolId<Ix>>;

    /// Copy the provided slice into the intern pool and produce a symbol,
    ///   but do not intern the symbol.
    ///
    /// The symbol will never compare equal to any other symbol,
    ///   regardless of the underlying string.
    /// Consequently,
    ///   this evades the cost of hashing the string,
    ///   allowing for a [`SymbolId`] to be used in place of [`String`].
    ///
    /// See "Uninterned Symbols" in the documentation of the
    ///   [`sym` module](super) for more information.
    fn clone_uninterned(&self, value: &str) -> SymbolId<Ix>;

    /// Determine whether the given value has already been interned.
    ///
    /// This is equivalent to `intern_soft(value).is_some()`.
    fn contains(&self, value: &str) -> bool;

    /// Number of interned strings in this interner's pool.
    ///
    /// This count will increase each time a unique string is interned.
    /// It does not increase when a string is already interned.
    fn len(&self) -> usize;

    /// Look up a symbol's string value by its [`SymbolId`].
    ///
    /// This will always return a [`str`] as long as the provided
    ///   `index` represents a symbol interned with this interner.
    /// If the index is not found,
    ///   the result is [`None`].
    ///
    /// [`str`] requires significantly more storage than an appropriate
    ///   [`SymbolId`] and should only be used when a string value must be
    ///   written (e.g. to a file or displayed to the user).
    fn index_lookup(&'i self, index: SymbolId<Ix>) -> Option<&'i str>;

    /// Intern a byte slice as a UTF-8 string.
    ///
    /// This method is intended as a performance optimization to avoid
    ///   unnecessary UTF-8 checks when a byte slice has already been
    ///   interned.
    ///
    /// This first checks to see if the provided byte slice matches an
    ///   existing intern,
    ///     returning the symbol if found.
    /// This allows us to skip the cost of a UTF-8 check for strings that
    ///   have already been encountered,
    ///     since their presence in the pool means that the string was
    ///     either
    ///       (a) already interned as a valid UTF-8 string; or
    ///       (b) was interned using an unsafe function.
    /// In the case of (b),
    ///   the safety violation is the fault of the original caller,
    ///     and there's nothing we can do about it now.
    ///
    /// Note that this optimization is only beneficial when a string has
    ///   already been interned.
    /// To avoid the cost of UTF-8 checks entirely,
    ///   see [`Interner::intern_utf8_unchecked`].
    ///
    /// If the byte slice does not represent a valid UTF-8 string,
    ///   a [`Utf8Error`] will be returned along with a reference to the
    ///   provided byte string.
    /// The purpose of this pair is to simplify error conversions
    ///   using `?` so that errors can contain additional context.
    fn intern_utf8<'a>(
        &self,
        value: &'a [u8],
    ) -> Result<SymbolId<Ix>, (Utf8Error, &'a [u8])>;

    /// Intern an assumed-UTF-8 slice of bytes or return an existing
    ///   [`SymbolId`].
    ///
    /// Safety
    /// ======
    /// This function is unsafe because it uses [`from_utf8_unchecked`].
    /// It is provided for convenience when interning from trusted binary
    ///   data
    ///     (such as [object files][]).
    ///
    /// [object files]: crate::obj
    unsafe fn intern_utf8_unchecked(&self, value: &[u8]) -> SymbolId<Ix> {
        self.intern(from_utf8_unchecked(value))
    }

    /// Copy the provided assumed-UTF-8 slice of bytes into the intern pool
    ///   and produce a symbol,
    ///     but do not intern the symbol.
    ///
    /// See [`clone_uninterned`](Interner::clone_uninterned) for more
    ///   information.
    ///
    /// Safety
    /// ======
    /// This function is unsafe because it uses [`from_utf8_unchecked`].
    /// It is provided for convenience when interning from trusted binary
    ///   data
    ///     (such as [object files][]).
    ///
    /// [object files]: crate::obj
    unsafe fn clone_uninterned_utf8_unchecked(
        &self,
        value: &[u8],
    ) -> SymbolId<Ix> {
        self.clone_uninterned(from_utf8_unchecked(value))
    }
}

/// An interner backed by an [arena](bumpalo).
///
/// Since all symbols exist until the interner itself is freed,
///   an arena is a much more efficient and appropriate memory allocation
///   strategy.
/// This also provides a stable location in memory for symbol data.
///
/// For the recommended configuration,
///   see [`DefaultInterner`].
///
/// See the [module-level documentation](self) for examples and more
///   information on how to use this interner.
pub struct ArenaInterner<'i, S, Ix = global::ProgSymSize>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
{
    /// Storage for interned strings.
    arena: Bump,

    /// Interned strings by [`SymbolId`].
    ///
    /// The first index must always be populated during initialization to
    ///   ensure that [`SymbolId`] will never be `0`.
    ///
    /// These string slices are stored in `arena`.
    strings: RefCell<Vec<&'i str>>,

    /// Map of interned strings to their respective [`SymbolId`].
    ///
    /// This allows us to determine whether a string has already been
    ///   interned and, if so, to return its corresponding symbol.
    map: RefCell<HashMap<&'i str, SymbolId<Ix>, S>>,
}

impl<'i, S, Ix> ArenaInterner<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
    <Ix as TryFrom<usize>>::Error: Debug,
{
    /// Initialize a new interner with no initial capacity.
    ///
    /// Prefer [`with_capacity`](ArenaInterner::with_capacity) when possible.
    #[inline]
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Initialize a new interner with an initial capacity for the
    ///   underlying [`HashMap`].
    ///
    /// The given `capacity` has no affect on arena allocation.
    /// Specifying initial capacity is important only for the map of strings
    ///   to symbols because it will reallocate and re-hash its contents
    ///   once capacity is exceeded.
    /// See benchmarks.
    ///
    /// If reallocation is a major concern,
    ///   a [consistent hashing algorithm][consistent] could be considered,
    ///   but the implementation will still incur the cost of copying
    ///     the [`HashMap`]'s contents to a new location in memory.
    ///
    /// [consistent]: https://en.wikipedia.org/wiki/Consistent_hashing
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        let mut strings = Vec::<_>::with_capacity(capacity);

        // The first index is not used since SymbolId cannot be 0.
        strings.push("");

        Self {
            arena: Bump::new(),
            strings: RefCell::new(strings),
            map: RefCell::new(HashMap::with_capacity_and_hasher(
                capacity,
                Default::default(),
            )),
        }
    }

    #[inline]
    fn get_next_symbol_id(syms: &mut Vec<&'i str>) -> SymbolId<Ix> {
        let next_index: Ix = syms
            .len()
            .try_into()
            .expect("internal error: SymbolId range exhausted");

        // This is not actually unsafe because next_index is always >0
        // from initialization.
        debug_assert!(Ix::new(next_index).is_some()); // != 0 check
        unsafe { SymbolId::from_int_unchecked(next_index) }
    }

    #[inline]
    fn copy_slice_into_arena(&self, value: &str) -> &'i str {
        unsafe {
            &*(from_utf8_unchecked(
                self.arena.alloc_slice_clone(value.as_bytes()),
            ) as *const str)
        }
    }

    /// Intern the provided value without looking for an existing intern.
    ///
    /// _This is an internal function that should only be used after having
    ///   already checked for an existing intern._
    /// It exists only to share common logic across methods.
    #[inline]
    fn intern_without_lookup(&self, value: &str) -> SymbolId<Ix> {
        let mut syms = self.strings.borrow_mut();

        let id = Self::get_next_symbol_id(&mut syms);
        let clone = self.copy_slice_into_arena(value);

        self.map.borrow_mut().insert(clone, id);
        syms.push(clone);

        id
    }
}

impl<'i, S, Ix> Interner<'i, Ix> for ArenaInterner<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
    <Ix as TryFrom<usize>>::Error: Debug,
{
    fn intern(&self, value: &str) -> SymbolId<Ix> {
        if let Some(sym) = self.map.borrow().get(value) {
            return *sym;
        }

        self.intern_without_lookup(value)
    }

    #[inline]
    fn intern_soft(&self, value: &str) -> Option<SymbolId<Ix>> {
        self.map.borrow().get(value).map(|sym| *sym)
    }

    fn clone_uninterned(&self, value: &str) -> SymbolId<Ix> {
        let mut syms = self.strings.borrow_mut();

        let id = Self::get_next_symbol_id(&mut syms);
        syms.push(self.copy_slice_into_arena(value));

        id
    }

    fn intern_utf8<'a>(
        &self,
        value: &'a [u8],
    ) -> Result<SymbolId<Ix>, (Utf8Error, &'a [u8])> {
        // Check the raw byte slice _before_ performing a UTF-8 check.
        // Note that `from_utf8_unchecked` is simply a transmute,
        //   so this check incurs only a hashing cost.
        if let Some(sym) = self.map.borrow().get(
            // SAFETY: This is only being used to check if the byte slice
            // matches an existing intern, which must them already be UTF-8
            // (unless an unsafe method was used to add it to begin with).
            unsafe { from_utf8_unchecked(value) },
        ) {
            return Ok(*sym);
        }

        // The string is not yet interned, so we must perform a UTF-8 check
        //   and can then proceed to intern as we normally would.
        // This does incur a double hashing cost,
        //   just like `intern`.
        Ok(self.intern_without_lookup(
            from_utf8(value).map_err(|err| (err, value))?,
        ))
    }

    #[inline]
    fn contains(&self, value: &str) -> bool {
        self.map.borrow().contains_key(value)
    }

    #[inline]
    fn len(&self) -> usize {
        self.map.borrow().len()
    }

    fn index_lookup(&'i self, index: SymbolId<Ix>) -> Option<&'i str> {
        self.strings.borrow().get(index.as_usize()).map(|str| *str)
    }
}

/// Interner using the [Fx Hash][fxhash] hashing function.
///
/// _This is currently the hash function used by [`DefaultInterner`]._
///
/// If denial of service is not a concern,
///   then this will outperform the default
///     [`DefaultHasher`](std::collections::hash_map::DefaultHasher)
///     (which uses SipHash at the time of writing).
///
/// See intern benchmarks for a comparison.
pub type FxArenaInterner<'i, Ix = global::ProgSymSize> =
    ArenaInterner<'i, FxBuildHasher, Ix>;

/// Recommended [`Interner`] and configuration (size-agnostic).
///
/// The choice of this default relies on the assumption that
///   denial-of-service attacks against the hash function are not a
///   concern.
///
/// For more information on the hashing algorithm,
///   see [`FxArenaInterner`].
pub type DefaultInterner<'i, Ix = global::ProgSymSize> =
    FxArenaInterner<'i, Ix>;

/// Recommended [`Interner`] and configuration for compilers and linkers
///   processing one or more packages.
pub type DefaultProgInterner<'i> = DefaultInterner<'i, global::ProgSymSize>;

// Note that these tests assert on standalone interners, not on the globals;
//   see the `global` sibling package for those tests.
#[cfg(test)]
mod test {
    use super::*;

    type Sut<'i> = DefaultInterner<'i>;

    #[test]
    fn recognizes_equal_strings() {
        let a = "foo";
        let b = a.to_string();
        let c = "bar";
        let d = c.to_string();

        let sut = Sut::new();

        let (ia, ib, ic, id) =
            (sut.intern(a), sut.intern(&b), sut.intern(c), sut.intern(&d));

        assert_eq!(ia, ib);
        assert_eq!(ic, id);
        assert_ne!(ia, ic);
    }

    #[test]
    fn symbol_id_increases_with_each_new_intern() {
        let sut = Sut::new();

        // Remember that identifiers begin at 1
        assert_eq!(
            SymbolId::test_from_int(1),
            sut.intern("foo"),
            "First index should be 1"
        );

        assert_eq!(
            SymbolId::test_from_int(1),
            sut.intern("foo"),
            "Index should not increment for already-interned symbols"
        );

        assert_eq!(
            SymbolId::test_from_int(2),
            sut.intern("bar"),
            "Index should increment for new symbols"
        );
    }

    #[test]
    fn length_increases_with_each_new_intern() {
        let sut = Sut::new();

        assert_eq!(0, sut.len(), "invalid empty len");

        sut.intern("foo");
        assert_eq!(1, sut.len(), "increment len");

        // duplicate
        sut.intern("foo");
        assert_eq!(1, sut.len(), "do not increment len on duplicates");

        sut.intern("bar");
        assert_eq!(2, sut.len(), "increment len (2)");
    }

    #[test]
    fn can_check_wither_string_is_interned() {
        let sut = Sut::new();

        assert!(!sut.contains("foo"), "recognize missing value");
        sut.intern("foo");
        assert!(sut.contains("foo"), "recognize interned value");
    }

    #[test]
    fn intern_soft() {
        let sut = Sut::new();

        assert_eq!(None, sut.intern_soft("foo"));

        let foo = sut.intern("foo");
        assert_eq!(Some(foo), sut.intern_soft("foo"));
    }

    #[test]
    fn uninterned_symbol_does_not_compare_equal_to_same_string() {
        let sut = Sut::new();
        let s = "foo";
        let interned = sut.intern(s);
        let uninterned = sut.clone_uninterned(s);

        // The symbols themselves will never be equal...
        assert_ne!(uninterned, interned);

        // ...but their underlying strings are.
        assert_eq!(sut.index_lookup(uninterned), sut.index_lookup(interned));
    }

    // Unlike the previous test, this makes sure that allocating an
    // uninterned symbol is actually not being interned, in that interning
    // another symbol after that won't return an uninterned symbol.
    #[test]
    fn allocating_uninterned_symbol_does_not_intern() {
        let sut = Sut::new();
        let s = "foo";

        // Alloc unintenrned _first_
        let uninterned1 = sut.clone_uninterned(s);
        let uninterned2 = sut.clone_uninterned(s);
        let interned1 = sut.intern(s);
        let interned2 = sut.intern(s);

        assert_ne!(uninterned1, interned1);
        assert_ne!(uninterned2, interned1);
        assert_ne!(uninterned1, uninterned2);

        // But we shouldn't have tainted normal interner behavior.
        assert_eq!(interned1, interned2);
    }

    #[test]
    fn new_with_capacity() {
        let n = 512;
        let sut = Sut::with_capacity(n);

        // note that this is not publicly available
        assert!(sut.map.borrow().capacity() >= n);
    }

    #[test]
    fn intern_utf8_unchecked() {
        let sut = Sut::new();

        let a = sut.intern("foo");
        let b = unsafe { sut.intern_utf8_unchecked(b"foo") };

        assert_eq!(a, b);
    }

    #[test]
    fn lookup_symbol_by_index() {
        let sut = Sut::new();

        // Symbol does not yet exist.
        assert!(sut.index_lookup(SymbolId::test_from_int(1)).is_none());

        let sym = sut.intern("foo");
        assert_eq!("foo", sut.index_lookup(sym).unwrap());
    }

    #[test]
    fn intern_utf8_with_new_valid_utf8_bytes() {
        let sut = Sut::new();

        let bytes = "valid".as_bytes();
        let sym = sut.intern_utf8(bytes).expect("unexpected failure");

        assert_eq!(sut.intern("valid"), sym);
    }

    #[test]
    fn intern_utf8_with_existing_valid_utf8_bytes() {
        let sut = Sut::new();

        let s = "valid";

        // Intern normally _first_ so that the `intern_utf8` call will
        // return an existing intern.
        sut.intern(s);
        let sym = sut.intern_utf8(s.as_bytes()).expect("unexpected failure");

        assert_eq!(sut.intern("valid"), sym);
    }

    #[test]
    fn intern_utf8_fails_with_invalid_utf8_bytes() {
        let sut = Sut::new();

        // Invalid two-byte encoding.
        let bytes = &[0b11000000u8];
        let result = sut.intern_utf8(bytes);

        match (result, from_utf8(bytes)) {
            (_, Ok(_)) => panic!("test string is valid UTF-8"),
            (Ok(_), _) => panic!("expected error"),
            (Err((given_err, given_u8)), Err(expected)) => {
                assert_eq!(given_u8, bytes);
                assert_eq!(given_err, expected);
            }
        }
    }
}
