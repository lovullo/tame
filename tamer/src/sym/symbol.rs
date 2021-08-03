// String internment symbol objects
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

//! Symbol objects for string internment system.
//!
//! See the [parent module](super) for more information.

use crate::global;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug};
use std::num::{NonZeroU16, NonZeroU32, NonZeroU8};
use std::ops::Deref;

/// Unique symbol identifier.
///
/// _Do not construct this value yourself;_
///   use an [`Interner`].
///
/// This newtype helps to prevent other indexes from being used where a
///   symbol index is expected.
/// Note, however, that it provides no defense against mixing symbol indexes
///   between multiple [`Interner`]s.
///
/// The index `0` is never valid because of
///   [`SymbolIndexSize::NonZero`],
///     which allows us to have `Option<SymbolId>` at no space cost.
///
/// [`Interner`]: super::Interner
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SymbolId<Ix: SymbolIndexSize>(Ix::NonZero);
assert_eq_size!(Option<Symbol<u16>>, Symbol<u16>);

impl<Ix: SymbolIndexSize> SymbolId<Ix> {
    /// Construct index from a non-zero `u16` value.
    ///
    /// Panics
    /// ------
    /// Will panic if `n == 0`.
    pub fn from_int(n: Ix) -> SymbolId<Ix> {
        SymbolId(Ix::new(n).unwrap())
    }

    /// Construct index from an unchecked non-zero `u16` value.
    ///
    /// This does not verify that `n > 0` and so must only be used in
    ///   contexts where this invariant is guaranteed to hold.
    /// Unlike [`from_int`](SymbolId::from_int),
    ///   this never panics.
    pub unsafe fn from_int_unchecked(n: Ix) -> SymbolId<Ix> {
        SymbolId(Ix::new_unchecked(n))
    }

    /// Construct index from a non-zero `u16` value.
    ///
    /// Panics
    /// ------
    /// Will panic if `n == 0`.
    pub fn from_u16(n: u16) -> SymbolId<u16> {
        SymbolId::from_int(n)
    }

    /// Construct index from a non-zero `u32` value.
    ///
    /// Panics
    /// ------
    /// Will panic if `n == 0`.
    pub fn from_u32(n: u32) -> SymbolId<u32> {
        SymbolId::from_int(n)
    }

    pub fn as_usize(self) -> usize {
        self.0.into().as_usize()
    }
}

impl<Ix: SymbolIndexSize> From<SymbolId<Ix>> for usize
where
    <Ix as TryInto<usize>>::Error: Debug,
{
    fn from(value: SymbolId<Ix>) -> usize {
        value.0.into().as_usize()
    }
}

impl<'i, Ix: SymbolIndexSize> From<&Symbol<'i, Ix>> for SymbolId<Ix> {
    fn from(sym: &Symbol<'i, Ix>) -> Self {
        sym.index()
    }
}

/// An integer type paired with its respective `NonZero` type that may be
///   used to index symbols.
///
/// The trait is name as such so that error messages make it clear that a
///   primitive type has to be explicitly accounted for.
///
/// This trait must be implemented on a primitive type like [`u16`].
pub trait SymbolIndexSize:
    Sized
    + Copy
    + Debug
    + PartialEq
    + Eq
    + TryFrom<usize>
    + TryInto<usize>
    + 'static
{
    /// The associated `NonZero*` type (e.g. [`NonZeroU16`]).
    type NonZero: Copy + Into<Self> + Debug;

    /// A symbol with a static lifetime suitable for placement at index 0 in
    ///   the string interment table,
    ///     which is not a valid [`SymbolId`] value.
    fn dummy_sym() -> &'static Symbol<'static, Self>;

    /// Construct a new non-zero value from the provided primitive value.
    ///
    /// If the value is `0`, the result will be [`None`].
    fn new(n: Self) -> Option<Self::NonZero>;

    /// Construct a new non-zero value from the provided primitive value
    ///   without checking whether the value is non-zero.
    unsafe fn new_unchecked(n: Self) -> Self::NonZero;

    /// Convert primitive value into a [`usize`].
    fn as_usize(self) -> usize;
}

macro_rules! supported_symbol_index {
    ($prim:ty, $nonzero:ty, $dummy:ident) => {
        impl SymbolIndexSize for $prim {
            type NonZero = $nonzero;

            fn dummy_sym() -> &'static Symbol<'static, Self> {
                &$dummy
            }

            fn new(n: Self) -> Option<Self::NonZero> {
                Self::NonZero::new(n)
            }

            unsafe fn new_unchecked(n: Self) -> Self::NonZero {
                Self::NonZero::new_unchecked(n)
            }

            fn as_usize(self) -> usize {
                self as usize
            }
        }
    };
}

supported_symbol_index!(u8, NonZeroU8, DUMMY_SYM_8);
supported_symbol_index!(u16, NonZeroU16, DUMMY_SYM_16);
supported_symbol_index!(u32, NonZeroU32, DUMMY_SYM_32);

/// Interned string.
///
/// A reference to this symbol is returned each time the same string is
///   interned with the same [`Interner`];
///     as such,
///       symbols can be compared for equality by pointer;
///         the underlying symbol id need not be used.
///
/// Each symbol is identified by a unique integer
///   (see [`index`](Symbol::index)).
/// The use of integers creates a more dense range of values than pointers,
///     which allows callers to use a plain [`Vec`] as a map instead of
///     something far more expensive like
///     [`HashSet`](std::collections::HashSet);
///       this is especially beneficial for portions of the system that make
///         use of nearly all interned symbols,
///           like the ASG.
/// A [`SymbolId`] can be mapped back into its [`Symbol`] by calling
///   [`Interner::index_lookup`] on the same interner that produced it.
///
/// The symbol also stores a string slice referencing the interned string
///   itself,
///     whose lifetime is that of the [`Interner`]'s underlying data store.
/// Dereferencing the symbol will expose the underlying slice.
///
/// [`Interner`]: super::Interner
/// [`Interner::index_lookup`]: super::Interner::index_lookup
#[derive(Copy, Clone, Debug)]
pub struct Symbol<'i, Ix: SymbolIndexSize> {
    index: SymbolId<Ix>,
    str: &'i str,
}

/// Interned string within a single package.
///
/// This type should be preferred to [`ProgSymbol`] when only a single
///   package's symbols are being processed.
pub type PkgSymbol<'i> = Symbol<'i, global::PkgSymSize>;

/// Interned string within an entire program.
///
/// This symbol type is preconfigured to accommodate a larger number of
///   symbols than [`PkgSymbol`] and is situable for use in a linker.
/// Use this type only when necessary.
pub type ProgSymbol<'i> = Symbol<'i, global::ProgSymSize>;

impl<'i, Ix: SymbolIndexSize> Symbol<'i, Ix> {
    /// Construct a new interned value.
    ///
    /// _This must only be done by an [`Interner`]._
    /// As such,
    ///   this function is not public.
    ///
    /// For test builds (when `cfg(test)`),
    ///   `new_dummy` is available to create symbols for tests.
    ///
    /// [`Interner`]: super::Interner
    #[inline]
    pub(super) fn new(index: SymbolId<Ix>, str: &'i str) -> Symbol<'i, Ix> {
        Self { index, str }
    }

    /// Retrieve unique symbol index.
    ///
    /// This is a densely-packed identifier that can be used as an index for
    ///   mapping.
    /// See [`SymbolId`] for more information.
    #[inline]
    pub fn index(&self) -> SymbolId<Ix> {
        self.index
    }

    /// Construct a new interned value _for testing_.
    ///
    /// This is a public version of [`Symbol::new`] available for test
    ///   builds.
    /// This separate name is meant to strongly imply that you should not be
    ///   doing this otherwise.
    ///
    /// See also `dummy_symbol!`.
    #[cfg(test)]
    #[inline(always)]
    pub fn new_dummy(index: SymbolId<Ix>, str: &'i str) -> Symbol<'i, Ix> {
        Self::new(index, str)
    }
}

impl<'i, Ix: SymbolIndexSize> PartialEq for Symbol<'i, Ix> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self as *const _, other as *const _)
            || std::ptr::eq(self.str.as_ptr(), other.str.as_ptr())
    }
}

impl<'i, Ix: SymbolIndexSize> Eq for Symbol<'i, Ix> {}

impl<'i, Ix: SymbolIndexSize> Deref for Symbol<'i, Ix> {
    type Target = str;

    /// Dereference to interned string slice.
    ///
    /// This allows for symbols to be used where strings are expected.
    #[inline]
    fn deref(&self) -> &str {
        self.str
    }
}

impl<'i, Ix: SymbolIndexSize> fmt::Display for Symbol<'i, Ix> {
    /// Display name of underlying string.
    ///
    /// Since symbols contain pointers to their interned slices,
    ///   we effectively get this for free.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

lazy_static! {
    /// Dummy 8-bit [`Symbol`] for use at index `0`.
    ///
    /// A symbol must never have an index of `0`,
    ///   so this can be used as a placeholder.
    /// The chosen [`SymbolId`] here does not matter since this will
    ///   never be referenced.
    static ref DUMMY_SYM_8: Symbol<'static, u8> =
        Symbol::new(SymbolId::from_int(1), "!BADSYMREF!");

    /// Dummy 16-bit [`Symbol`] for use at index `0`.
    ///
    /// A symbol must never have an index of `0`,
    ///   so this can be used as a placeholder.
    /// The chosen [`SymbolId`] here does not matter since this will
    ///   never be referenced.
    static ref DUMMY_SYM_16: Symbol<'static, u16> =
        Symbol::new(SymbolId::from_int(1), "!BADSYMREF!");

    /// Dummy 32-bit [`Symbol`] for use at index `0`.
    ///
    /// A symbol must never have an index of `0`,
    ///   so this can be used as a placeholder.
    /// The chosen [`SymbolId`] here does not matter since this will
    ///   never be referenced.
    static ref DUMMY_SYM_32: Symbol<'static, u32> =
        Symbol::new(SymbolId::from_int(1), "!BADSYMREF!");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn self_compares_eq() {
        let sym = Symbol::new(SymbolId::from_int(1u16), "str");

        assert_eq!(&sym, &sym);
    }

    #[test]
    fn copy_compares_equal() {
        let sym = Symbol::new(SymbolId::from_int(1u16), "str");
        let cpy = sym;

        assert_eq!(sym, cpy);
    }

    // Integer values are for convenience, not identity.  They cannot be
    // used as a unique identifier across different interners.
    #[test]
    fn same_index_different_slices_compare_unequal() {
        let a = Symbol::new(SymbolId::from_int(1u16), "a");
        let b = Symbol::new(SymbolId::from_int(1u16), "b");

        assert_ne!(a, b);
    }

    // As mentioned above, ids are _not_ the identity of the symbol.  If
    // two values point to the same location in memory, they are assumed
    // to have come from the same interner, and should therefore have
    // the same index this should never happen unless symbols are
    // being created without the use of interners, which is unsupported.
    //
    // This test is a cautionary tale.
    #[test]
    fn different_index_same_slices_compare_equal() {
        let slice = "str";

        let a = Symbol::new(SymbolId::from_int(1u16), slice);
        let b = Symbol::new(SymbolId::from_int(2u16), slice);

        assert_eq!(a, b);
    }

    #[test]
    fn cloned_symbols_compare_equal() {
        let sym = Symbol::new(SymbolId::from_int(1u16), "foo");

        assert_eq!(sym, sym.clone());
    }

    // &Symbol can be used where string slices are expected (this won't
    // compile otherwise).
    #[test]
    fn ref_can_be_used_as_string_slice() {
        let slice = "str";
        let sym_slice: &str = &Symbol::new(SymbolId::from_int(1u16), slice);

        assert_eq!(slice, sym_slice);
    }

    // For use when we can guarantee proper ids.
    #[test]
    fn can_create_index_unchecked() {
        assert_eq!(SymbolId::from_int(1u32), unsafe {
            SymbolId::from_int_unchecked(1)
        });
    }

    #[test]
    fn can_retrieve_symbol_index() {
        let index = SymbolId::from_int(1u16);

        assert_eq!(index, Symbol::new(index, "").index());
    }

    #[test]
    fn displays_as_interned_value() {
        let sym = Symbol::new(SymbolId::from_int(1u16), "foo");

        assert_eq!(format!("{}", sym), sym.str);
    }
}
