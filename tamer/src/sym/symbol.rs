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

//! Symbol objects representing interned strings.
//!
//! See the [parent module](super) for more information.

use super::{DefaultPkgInterner, DefaultProgInterner, Interner};
use crate::global;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::num::{NonZeroU16, NonZeroU32};
use std::ops::Deref;
use std::thread::LocalKey;

/// Unique symbol identifier produced by an [`Interner`].
///
/// Use one of [`PkgSymbolId`] or [`ProgSymbolId`] unless a generic size is
///   actually needed
///     (e.g. implementations shared between a compiler and linker).
///
/// This newtype helps to prevent other indexes from being used where a
///   symbol index is expected.
/// Note, however, that it provides no defense against mixing symbol indexes
///   between multiple [`Interner`]s;
///     you should create your own newtypes to resolve that concern.
///
/// The index `0` is never valid because of
///   [`SymbolIndexSize::NonZero`],
///     which allows us to have `Option<SymbolId>` at no space cost.
///
/// Symbol Strings
/// ==============
/// [`SymbolId`] intentionally omits the [`Display`] trait to ensure that
///   compile-time errors occur when symbols are used in contexts where
///   strings are expected.
/// To resolve a [`SymbolId`] into the string that it represents,
///   see either [`GlobalSymbolResolve::lookup_str`] or
///   [`Interner::index_lookup`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId<Ix: SymbolIndexSize>(Ix::NonZero);
assert_eq_size!(Option<SymbolId<u16>>, SymbolId<u16>);

/// Identifier of a symbol within a single package.
///
/// This type should be preferred to [`ProgSymbolId`] when only a single
///   package's symbols are being processed.
pub type PkgSymbolId = SymbolId<global::PkgSymSize>;

/// Identifier of a symbol within an entire program.
///
/// This symbol type is preconfigured to accommodate a larger number of
///   symbols than [`PkgSymbolId`] and is suitable for use in a linker.
/// Use this type only when necessary.
pub type ProgSymbolId = SymbolId<global::ProgSymSize>;

impl<Ix: SymbolIndexSize> SymbolId<Ix> {
    /// Construct index from an unchecked non-zero `u16` value.
    ///
    /// This does not verify that `n > 0` and so must only be used in
    ///   contexts where this invariant is guaranteed to hold.
    pub(super) unsafe fn from_int_unchecked(n: Ix) -> SymbolId<Ix> {
        SymbolId(Ix::new_unchecked(n))
    }

    pub fn as_usize(self) -> usize {
        self.0.into().as_usize()
    }

    /// Construct index from a non-zero value for testing.
    ///
    /// Panics
    /// ------
    /// Will panic if `n == 0`.
    #[cfg(test)]
    pub fn test_from_int(n: Ix) -> SymbolId<Ix> {
        SymbolId(Ix::new(n).unwrap())
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
    + Hash
    + 'static
{
    /// The associated `NonZero*` type (e.g. [`NonZeroU16`]).
    type NonZero: Copy + Into<Self> + Debug + PartialEq + Eq + Hash;

    /// Global interner for this index type.
    type Interner: Interner<'static, Self>;

    /// Construct a new non-zero value from the provided primitive value.
    ///
    /// If the value is `0`, the result will be [`None`].
    fn new(n: Self) -> Option<Self::NonZero>;

    /// Construct a new non-zero value from the provided primitive value
    ///   without checking whether the value is non-zero.
    unsafe fn new_unchecked(n: Self) -> Self::NonZero;

    /// Convert primitive value into a [`usize`].
    fn as_usize(self) -> usize;

    /// Perform an operation using the global interner for this index type.
    ///
    /// This solves the problem of determining which global interner must be
    ///   used for a given [`SymbolIndexSize`] without having to resort to
    ///   dynamic dispatch.
    fn with_static_interner<F, R>(f: F) -> R
    where
        F: FnOnce(&'static Self::Interner) -> R;
}

macro_rules! supported_symbol_index {
    ($prim:ty, $nonzero:ty, $interner:ty, $global:ident) => {
        thread_local! {
            pub(super) static $global: $interner = <$interner>::new();
        }

        impl SymbolIndexSize for $prim {
            type NonZero = $nonzero;
            type Interner = $interner;

            fn new(n: Self) -> Option<Self::NonZero> {
                Self::NonZero::new(n)
            }

            unsafe fn new_unchecked(n: Self) -> Self::NonZero {
                Self::NonZero::new_unchecked(n)
            }

            fn as_usize(self) -> usize {
                self as usize
            }

            fn with_static_interner<F, R>(f: F) -> R
            where
                F: FnOnce(&'static Self::Interner) -> R,
            {
                with_static_interner(&$global, f)
            }
        }
    };
}

type StaticPkgInterner = DefaultPkgInterner<'static>;
type StaticProgInterner = DefaultProgInterner<'static>;

supported_symbol_index!(u16, NonZeroU16, StaticPkgInterner, INTERNER_PKG);
supported_symbol_index!(u32, NonZeroU32, StaticProgInterner, INTERNER_PROG);

/// A string retrieved from the intern pool using a [`SymbolId`].
///
/// The lifetime of the inner string is constrained to the lifetime of the
///   interner itself.
/// For global interners,
///   this means that the string slice has a `'static` lifetime.
///
/// [`SymbolStr`] requires significantly more storage than an appropriate
///   [`SymbolId`] and should only be used when a string value must be
///   written (e.g. to a file or displayed to the user).
///
/// This value is intended to be short-lived.
#[derive(Debug, Default, Clone)]
pub struct SymbolStr<'i>(&'i str);

impl<'i> SymbolStr<'i> {
    pub fn as_str(&self) -> &'i str {
        self.0
    }

    /// Create a [`SymbolStr`] from a string for testing.
    ///
    /// _This function is only available for tests for convenience!_
    /// `SymbolStr` must always represent a real, interned string in
    ///   non-test code.
    #[cfg(test)]
    pub fn test_from_str(s: &'i str) -> Self {
        SymbolStr(s)
    }
}

impl<'i> SymbolStr<'i> {
    pub(super) fn from_interned_slice(slice: &'i str) -> SymbolStr<'i> {
        SymbolStr(slice)
    }
}

impl<'i, T: Deref<Target = str>> PartialEq<T> for SymbolStr<'i> {
    fn eq(&self, other: &T) -> bool {
        self.0 == other.deref()
    }
}

impl PartialEq<SymbolStr<'_>> for &str {
    fn eq(&self, other: &SymbolStr<'_>) -> bool {
        *self == other.0
    }
}

impl<'i> Display for SymbolStr<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// Once we have unsafe_impls stabalized,
//   we should prevent `SymbolStr` from crossing threads.
// TAMER does not use threads at the time of writing,
//   so this isn't a practical concern.
// If we _do_ want to pass between threads,
//   we need to ensure the thread holding the interner lives longer than all
//   other threads.
//impl<'i> !Send for SymbolStr<'i> {}
//impl<'i> !Sync for SymbolStr<'i> {}

impl<'i> Deref for SymbolStr<'i> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &'i str {
        self.as_str()
    }
}

/// Acquire a static reference to a global interner.
///
/// Global interners are static and thread-local.
/// They are created using the [`thread_local!`] macro,
///   which produces a [`LocalKey`] that provides access with a lifetime
///     that cannot exceed that of the closure.
/// This is a problem,
///   because we must return a value from the interner's storage.
///
/// This function transmutes the lifetime of [`LocalKey`] back to
///   `'static`.
/// This has the benefit of requiring no further casting of the [`Interner`],
///   since the lifetime of its storage is already `'static`,
///     and so the retrieved interner can be used to return a static string
///     slice without any further unsafe code.
///
/// This lifetime transmutation is expected to be safe,
///   because the thread-local storage is never deallocated,
///     and the storage is only accessible to one thread.
fn with_static_interner<F, R, I, Ix>(key: &'static LocalKey<I>, f: F) -> R
where
    Ix: SymbolIndexSize,
    I: Interner<'static, Ix> + 'static,
    F: FnOnce(&'static I) -> R,
{
    key.with(|interner| {
        f(unsafe {
            // These type annotations are inferred, but please leave
            // them here; transmute is especially dangerous, and we want
            // to be sure reality always matches our expectations.
            std::mem::transmute::<&I, &'static I>(interner)
        })
    })
}

/// Resolve a [`SymbolId`] to the string value it represents using a global
///   interner.
///
/// This exists as its own trait
///   (rather than simply adding to [`SymbolId`])
///   to make it easy to see what systems rely on global state.
pub trait GlobalSymbolResolve {
    /// Resolve a [`SymbolId`] allocated using a global interner.
    ///
    /// This name is intended to convey that this operation has a cost---a
    ///   lookup is performed on the global interner pool,
    ///     which requires locking and so comes at a (small) cost.
    /// This shouldn't be done more than is necessary.
    fn lookup_str(&self) -> SymbolStr<'static>;
}

impl<Ix: SymbolIndexSize> GlobalSymbolResolve for SymbolId<Ix> {
    fn lookup_str(&self) -> SymbolStr<'static> {
        Ix::with_static_interner(|interner| {
            interner.index_lookup(*self).unwrap()
        })
    }
}

/// Intern a string using a global interner.
///
/// This provides a convenient API that creates the appearance that string
///   interning is a core Rust language feature
///   (e.g. `"foo".intern()`).
/// This speaks to the rationale of introducing global interners to begin
///   with---mainly
///     that symbols are so pervasive that they may as well be a language
///     feature so that they are more natural to work with.
///
/// This will automatically intern using the proper global interner based on
///   the resolved [`SymbolIndexSize`].
/// In most situations within real (non-test) code,
///   Rust is able to infer this itself and so it looks quite natural.
pub trait GlobalSymbolIntern<Ix: SymbolIndexSize> {
    /// Intern a string using a global interner.
    ///
    /// See [`crate::sym`] for more information.
    fn intern(self) -> SymbolId<Ix>;

    /// Copy the provided slice into the intern pool and produce a symbol
    ///   using a global interner,
    ///     but do not intern the symbol.
    ///
    /// See [`crate::sym`] for more information.
    fn clone_uninterned(self) -> SymbolId<Ix>;
}

/// Intern a byte slice using a global interner.
///
/// See also [`GlobalSymbolIntern`].
/// This uses [`Interner::intern_utf8_unchecked`].
pub trait GlobalSymbolInternUnchecked<Ix: SymbolIndexSize> {
    /// Intern a byte slice using a global interner.
    ///
    /// See also [`GlobalSymbolIntern::intern`].
    ///
    /// Safety
    /// ======
    /// This function is unsafe because it uses
    ///   [`Interner::intern_utf8_unchecked`].
    /// It is provided for convenience when interning from trusted binary
    ///   data
    ///     (such as [object files][]).
    ///
    /// [object files]: crate::obj
    unsafe fn intern_utf8_unchecked(self) -> SymbolId<Ix>;

    /// Copy the provided assumed-UTF-8 byte slice into the intern pool and
    ///   produce a symbol using a global interner,
    ///     but do not intern the symbol.
    ///
    /// See also [`GlobalSymbolIntern::clone_uninterned`].
    ///
    /// Safety
    /// ======
    /// This function is unsafe because it uses
    ///   [`Interner::intern_utf8_unchecked`].
    /// It is provided for convenience when interning from trusted binary
    ///   data
    ///     (such as [object files][]).
    ///
    /// [object files]: crate::obj
    unsafe fn clone_uninterned_utf8_unchecked(self) -> SymbolId<Ix>;
}

impl<Ix: SymbolIndexSize> GlobalSymbolIntern<Ix> for &str {
    fn intern(self) -> SymbolId<Ix> {
        Ix::with_static_interner(|interner| interner.intern(self))
    }

    fn clone_uninterned(self) -> SymbolId<Ix> {
        Ix::with_static_interner(|interner| interner.clone_uninterned(self))
    }
}

impl<Ix: SymbolIndexSize> GlobalSymbolInternUnchecked<Ix> for &[u8] {
    unsafe fn intern_utf8_unchecked(self) -> SymbolId<Ix> {
        Ix::with_static_interner(|interner| {
            interner.intern_utf8_unchecked(self)
        })
    }

    unsafe fn clone_uninterned_utf8_unchecked(self) -> SymbolId<Ix> {
        Ix::with_static_interner(|interner| {
            interner.clone_uninterned_utf8_unchecked(self)
        })
    }
}

impl<T, Ix> From<T> for SymbolId<Ix>
where
    T: Deref<Target = str>,
    Ix: SymbolIndexSize,
{
    fn from(value: T) -> Self {
        value.intern()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn self_compares_eq() {
        let sym = SymbolId::test_from_int(1u16);

        assert_eq!(&sym, &sym);
    }

    #[test]
    fn copy_compares_equal() {
        let sym = SymbolId::test_from_int(1u16);
        let cpy = sym;

        assert_eq!(sym, cpy);
    }

    // For use when we can guarantee proper ids.
    #[test]
    fn can_create_index_unchecked() {
        assert_eq!(SymbolId::test_from_int(1u32), unsafe {
            SymbolId::from_int_unchecked(1)
        });
    }

    mod global {
        use super::*;

        #[test]
        fn str_lookup_using_global_interner() {
            INTERNER_PKG.with(|interner| {
                let given = "test global intern";
                let sym = interner.intern(given);

                assert_eq!(given, sym.lookup_str());
            });
        }

        #[test]
        fn str_intern_uses_global_interner() {
            // This creates the illusion of a core Rust language feature
            let sym = "foo".intern();

            assert_eq!("foo", sym.lookup_str());

            INTERNER_PKG.with(|interner| {
                assert_eq!(
                    sym,
                    interner.intern("foo"),
                    "GlobalSymbolIntern<&str>::intern must use the global interner"
                );
            });
        }

        #[test]
        fn clone_uninterned() {
            let sym: PkgSymbolId = "foo".clone_uninterned();
            assert_eq!("foo", sym.lookup_str());
        }
    }
}
