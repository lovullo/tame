// String interner
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

//! Interners used to intern values as symbols.
//!
//! See the [parent module](super) for more information.

use super::{Symbol, SymbolId, SymbolIndexSize};
use crate::global;
use bumpalo::Bump;
use fxhash::FxBuildHasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::hash::BuildHasher;

/// Create, store, compare, and retrieve [`Symbol`] values.
///
/// Interners accept string slices and produce values of type [`Symbol`].
/// A reference to the same [`Symbol`] will always be returned for a given
///   string,
///     allowing symbols to be compared for equality cheaply by comparing
///     pointers.
/// Symbol locations in memory are fixed for the lifetime of the interner.
///
/// If you care whether a value has been interned yet or not,
///   see [`intern_soft`][Interner::intern_soft`] and
///     [`contains`](Interner::contains).
///
/// See the [module-level documentation](self) for an example.
pub trait Interner<'i, Ix: SymbolIndexSize> {
    /// Intern a string slice or return an existing [`Symbol`].
    ///
    /// If the provided string has already been interned,
    ///   then a reference to the existing [`Symbol`] will be returned.
    /// Otherwise,
    ///   the string will be interned and a new [`Symbol`] created.
    ///
    /// The lifetime of the returned symbol is bound to the lifetime of the
    ///   underlying intern pool.
    ///
    /// To retrieve an existing symbol _without_ interning,
    ///   see [`intern_soft`](Interner::intern_soft).
    fn intern(&'i self, value: &str) -> &'i Symbol<'i, Ix>;

    /// Retrieve an existing intern for the string slice `s`.
    ///
    /// Unlike [`intern`](Interner::intern),
    ///   this will _not_ intern the string if it has not already been
    ///   interned.
    fn intern_soft(&'i self, value: &str) -> Option<&'i Symbol<'i, Ix>>;

    /// Determine whether the given value has already been interned.
    fn contains(&self, value: &str) -> bool;

    /// Number of interned strings.
    ///
    /// This count will increase each time a unique string is interned.
    /// It does not increase when a string is already interned.
    fn len(&self) -> usize;

    /// Look up a previously interned [`Symbol`] by its [`SymbolId`].
    ///
    /// This will always return a [`Symbol`] as long as the provided `index`
    ///   represents a symbol interned with this interner.
    /// If the index is not found,
    ///   the result is [`None`].
    ///
    /// This method is most useful when storing [`Symbol`] is not possible
    ///   or desirable.
    /// For example,
    ///   borrowed [`Symbol`] references require lifetimes,
    ///   whereas [`SymbolId`] is both owned _and_ [`Copy`].
    /// [`SymbolId`] is also much smaller than [`Symbol`].
    fn index_lookup(
        &'i self,
        index: SymbolId<Ix>,
    ) -> Option<&'i Symbol<'i, Ix>>;

    /// Intern an assumed-UTF8 slice of bytes or return an existing
    ///   [`Symbol`].
    ///
    /// Safety
    /// ======
    /// This function is unsafe because it uses
    ///   [`std::str::from_utf8_unchecked`].
    /// It is provided for convenience when interning from trusted binary
    ///   data
    ///     (such as [object files][]).
    ///
    /// [object files]: crate::obj
    unsafe fn intern_utf8_unchecked(
        &'i self,
        value: &[u8],
    ) -> &'i Symbol<'i, Ix> {
        self.intern(std::str::from_utf8_unchecked(value))
    }
}

/// An interner backed by an [arena](bumpalo).
///
/// Since interns exist until the interner itself is freed,
///   an arena is a much more efficient and appropriate memory allocation
///   strategy.
/// This further provides a stable location in memory for symbol data.
///
/// For the recommended configuration,
///   see [`DefaultInterner`].
///
/// See the [module-level documentation](self) for examples and more
///   information on how to use this interner.
pub struct ArenaInterner<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
{
    /// String and [`Symbol`] storage.
    arena: Bump,

    /// Symbol references by index.
    ///
    /// This vector enables looking up a [`Symbol`] using its
    ///   [`SymbolId`].
    ///
    /// The first index must always be populated during initialization to
    ///   ensure that [`SymbolId`] will never be `0`.
    indexes: RefCell<Vec<&'i Symbol<'i, Ix>>>,

    /// Map of interned strings to their respective [`Symbol`].
    ///
    /// Both strings and symbols are allocated within `arena`.
    map: RefCell<HashMap<&'i str, &'i Symbol<'i, Ix>, S>>,
}

impl<'i, S, Ix> ArenaInterner<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
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
        let mut indexes = Vec::<&'i Symbol<'i, Ix>>::with_capacity(capacity);

        // The first index is not used since SymbolId cannot be 0.
        indexes.push(Ix::dummy_sym());

        Self {
            arena: Bump::new(),
            indexes: RefCell::new(indexes),
            map: RefCell::new(HashMap::with_capacity_and_hasher(
                capacity,
                Default::default(),
            )),
        }
    }
}

impl<'i, S, Ix> Interner<'i, Ix> for ArenaInterner<'i, S, Ix>
where
    S: BuildHasher + Default,
    Ix: SymbolIndexSize,
    <Ix as TryFrom<usize>>::Error: Debug,
{
    fn intern(&'i self, value: &str) -> &'i Symbol<'i, Ix> {
        let mut map = self.map.borrow_mut();

        if let Some(sym) = map.get(value) {
            return sym;
        }

        let mut syms = self.indexes.borrow_mut();

        let next_index: Ix = syms
            .len()
            .try_into()
            .expect("internal error: SymbolId range exhausted");

        // This is not actually unsafe because next_index is always >0
        // from initialization.
        debug_assert!(Ix::new(next_index).is_some()); // != 0 check
        let id = unsafe { SymbolId::from_int_unchecked(next_index) };

        // Copy string slice into the arena.
        let clone: &'i str = unsafe {
            &*(std::str::from_utf8_unchecked(
                self.arena.alloc_slice_clone(value.as_bytes()),
            ) as *const str)
        };

        // Symbols are also stored within the arena, adjacent to the
        // string.  This ensures that both have stable locations in memory.
        let sym: &'i Symbol<'i, Ix> = self.arena.alloc(Symbol::new(id, clone));

        map.insert(clone, sym);
        syms.push(sym);

        sym
    }

    #[inline]
    fn intern_soft(&'i self, value: &str) -> Option<&'i Symbol<'i, Ix>> {
        self.map.borrow().get(value).map(|sym| *sym)
    }

    #[inline]
    fn contains(&self, value: &str) -> bool {
        self.map.borrow().contains_key(value)
    }

    #[inline]
    fn len(&self) -> usize {
        self.map.borrow().len()
    }

    fn index_lookup(
        &'i self,
        index: SymbolId<Ix>,
    ) -> Option<&'i Symbol<'i, Ix>> {
        self.indexes.borrow().get(index.as_usize()).map(|sym| *sym)
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
pub type FxArenaInterner<'i, Ix> = ArenaInterner<'i, FxBuildHasher, Ix>;

/// Recommended [`Interner`] and configuration.
///
/// The choice of this default relies on the assumption that
///   denial-of-service attacks against the hash function are not a
///   concern.
///
/// For more information on the hashing algorithm,
///   see [`FxArenaInterner`].
pub type DefaultInterner<'i, Ix> = FxArenaInterner<'i, Ix>;

/// Interner for individual packages and their dependencies.
///
/// This type should be preferred to [`DefaultPkgInterner`] when only a
///   single package's symbols are being processed,
///     since it can be better packed into structs.
pub type DefaultPkgInterner<'i> = DefaultInterner<'i, global::PkgSymSize>;

/// Interner for entire programs.
///
/// This interner holds symbols with a larger underyling datatype than
///   [`DefaultPkgInterner`].
/// It is intended for use by linkers or anything else that needs to process
///   a large number of packages in a program simultaneously.
pub type DefaultProgInterner<'i> = DefaultInterner<'i, global::ProgSymSize>;

#[cfg(test)]
mod test {
    use super::*;

    type Sut<'i> = DefaultInterner<'i, global::ProgSymSize>;

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
        assert_eq!(&ia, &ib);
        assert_eq!(*ia, *ib);

        assert_eq!(ic, id);
        assert_eq!(&ic, &id);
        assert_eq!(*ic, *id);

        assert_ne!(ia, ic);
        assert_ne!(&ia, &ic);
        assert_ne!(*ia, *ic);
    }

    #[test]
    fn symbol_id_increases_with_each_new_intern() {
        let sut = Sut::new();

        // Remember that identifiers begin at 1
        assert_eq!(
            SymbolId::from_int(1),
            sut.intern("foo").index(),
            "First index should be 1"
        );

        assert_eq!(
            SymbolId::from_int(1),
            sut.intern("foo").index(),
            "Index should not increment for already-interned symbols"
        );

        assert_eq!(
            SymbolId::from_int(2),
            sut.intern("bar").index(),
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
        assert!(sut.index_lookup(SymbolId::from_int(1)).is_none());

        let sym = sut.intern("foo");
        assert_eq!(Some(sym), sut.index_lookup(sym.index()));
        assert_eq!(Some(sym), sut.index_lookup(sym.into()));
    }
}
