// String internment symbol objects
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

//! Symbol objects representing interned strings.
//!
//! See the [parent module](super) for more information.

use super::{DefaultInterner, Interner};
use crate::{diagnostic_panic, global};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::num::{NonZeroU16, NonZeroU32};
use std::ops::Deref;
use std::str::Utf8Error;
use std::thread::LocalKey;

/// Unique symbol identifier produced by an [`Interner`].
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
/// To resolve a [`SymbolId`] into the string that it represents,
///   see either [`GlobalSymbolResolve::lookup_str`] or
///   [`Interner::index_lookup`].
///
/// Symbols allocated using the global interner will automatically resolve
///   to strings via [`Display`].
/// _This should be done at the last moment_ before outputting,
///   such as before writing to an object file or displaying an error to the
///   user.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId<Ix: SymbolIndexSize = global::ProgSymSize>(
    pub(super) Ix::NonZero,
);
assert_eq_size!(Option<SymbolId<u16>>, SymbolId<u16>);

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
    + Display
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

type Static16Interner = DefaultInterner<'static, u16>;
type Static32Interner = DefaultInterner<'static, u32>;

thread_local! {
    pub(super) static INTERNER_16: Static16Interner = super::prefill::st16::fill(
        Static16Interner::with_capacity(global::INIT_GLOBAL_INTERNER_CAPACITY)
    );

    pub(super) static INTERNER_32: Static32Interner = super::prefill::st::fill(
        Static32Interner::with_capacity(global::INIT_GLOBAL_INTERNER_CAPACITY)
    );
}

supported_symbol_index!(u16, NonZeroU16, Static16Interner, INTERNER_16);
supported_symbol_index!(u32, NonZeroU32, Static32Interner, INTERNER_32);

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
    ///
    /// Panics
    /// ======
    /// This will panic if the symbol cannot be found.
    /// Such a situation should never occur if the interner is being used
    ///   properly and would represent a bug in the program.
    ///
    /// If a panic is a problem
    ///   (e.g. if you are looking up a symbol as _part_ of a panic),
    ///   use [`GlobalSymbolResolve::try_lookup_str`].
    fn lookup_str(&self) -> &'static str;

    /// Attempt to resolve a [`SymbolId`] allocated using a global interner.
    ///
    /// Unlike [`GlobalSymbolResolve::lookup_str`],
    ///   this cannot panic.
    ///
    /// This does not include an error because the failure can only occur
    ///   when an index lookup fails;
    ///     [`Option`] would have been used,
    ///       but [`Result`] is idiomatic with `try_*` functions.
    fn try_lookup_str(&self) -> Result<&'static str, ()>;
}

impl<Ix: SymbolIndexSize> GlobalSymbolResolve for SymbolId<Ix> {
    fn lookup_str(&self) -> &'static str {
        Ix::with_static_interner(|interner| {
            interner.index_lookup(*self).unwrap_or_else(|| {
                // If the system is being used properly, this should never
                // happen (we'd only look up symbols allocated through this
                // interner).
                diagnostic_panic!(
                    vec![], // no span information available
                    "failed to resolve SymbolId({}) using global \
                         interner of length {}",
                    self.0.into(),
                    interner.len()
                )
            })
        })
    }

    fn try_lookup_str(&self) -> Result<&'static str, ()> {
        Ix::with_static_interner(|interner| interner.index_lookup(*self))
            .ok_or(())
    }
}

impl<Ix: SymbolIndexSize> Display for SymbolId<Ix> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.lookup_str())
    }
}

impl<Ix: SymbolIndexSize> Debug for SymbolId<Ix> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // We have to be careful here when looking up the symbol, since this
        // may be called during a panic, and we don't want to panic yet
        // again if we cannot find the symbol.
        write!(
            f,
            "SymbolId({} \"{}\")",
            self.0.into(),
            self.try_lookup_str().unwrap_or(&"<#!UNKNOWN_SYMBOL>")
        )
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
pub trait GlobalSymbolInternBytes<Ix: SymbolIndexSize>
where
    Self: Sized,
{
    /// Intern a byte slice using a global interner.
    ///
    /// This first checks to see if the provided slice has already been
    ///   interned.
    /// If so,
    ///   we are able to save time by not checking for UTF-8 validity.
    /// Otherwise,
    ///   we intern the slice in the usual way,
    ///     failing if it does not represent a valid UTF-8 string.
    ///
    /// For further explanation,
    ///   see [`Interner::intern_utf8`].
    fn intern_utf8(self) -> Result<SymbolId<Ix>, (Utf8Error, Self)>;
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

impl<Ix: SymbolIndexSize> GlobalSymbolInternBytes<Ix> for &[u8] {
    fn intern_utf8(self) -> Result<SymbolId<Ix>, (Utf8Error, Self)> {
        Ix::with_static_interner(|interner| interner.intern_utf8(self))
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
            INTERNER_32.with(|interner| {
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

            INTERNER_32.with(|interner| {
                assert_eq!(
                    sym,
                    interner.intern("foo"),
                    "GlobalSymbolIntern<&str>::intern must use the global interner"
                );
            });
        }

        #[test]
        fn clone_uninterned() {
            let sym: SymbolId = "foo".clone_uninterned();
            assert_eq!("foo", sym.lookup_str());
        }
    }
}
