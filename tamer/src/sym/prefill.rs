// Pre-interned strings
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

//! Pre-interned strings.
//!
//! These strings are expected to be encountered nearly every run,
//!   and substitute static strings that would otherwise appear hard-coded
//!   in the system and have to be interned to be compared against other
//!   values.
//!
//! See the [parent module](super) for more information.

use super::{Interner, SymbolId, SymbolIndexSize};
use crate::global;
use std::array;
use std::ops::Deref;

/// A size that is as small as possible to hold the necessary number of
///   values.
type StaticSymbolSize = u8;

/// Statically-allocated symbol.
///
/// This symbol is generated at compile-time and expected to be available in
///   the 32-bit global interner once it has been initialized.
///
/// This symbol contains a number of `const` methods,
///   allowing for this symbol to be easily used to construct static
///   newtypes.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StaticSymbolId(StaticSymbolSize);

impl StaticSymbolId {
    /// Cast static symbol into a [`SymbolId`] suitable for the global
    ///   program-level interner.
    ///
    /// This is safe since global interner will always contain this
    ///   symbol before it can be read.
    pub const fn as_prog_sym(self) -> SymbolId<global::ProgSymSize> {
        SymbolId(unsafe {
            <global::ProgSymSize as SymbolIndexSize>::NonZero::new_unchecked(
                self.0 as global::ProgSymSize,
            )
        })
    }

    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<StaticSymbolId> for SymbolId<global::ProgSymSize> {
    fn from(st: StaticSymbolId) -> Self {
        st.as_prog_sym()
    }
}

/// Generate a newtype containing a [`StaticSymbolId`] that derefs to its
///   inner value.
macro_rules! static_symbol_newtype {
    ($(#[$attr:meta])* $name:ident) => {
        $(#[$attr])*
        #[doc=""]
        #[doc="This will [`Deref`] into a [`StaticSymbolId`]."]
        pub struct $name(StaticSymbolId);

        impl $name {
            const fn new(id: StaticSymbolSize) -> Self {
                Self(StaticSymbolId(id))
            }
        }

        impl Deref for $name {
            type Target = StaticSymbolId;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

/// Generate a series of newtypes and the macro `static_symbol_ty!` which
///   can be used to take a short identifier and convert it into its full
///   type identifier.
macro_rules! static_symbol_newtypes {
    ($($(#[$attr:meta])* $short:ident: $ty:ident),*) => {
        $(
            static_symbol_newtype!($(#[$attr])* $ty);
        )*

        macro_rules! static_symbol_ty {
            $(
                ($short) => {
                    $ty
                };
            )*

            // Just some string value; a misc case.
            (str) => {
                StaticSymbolId
            };
        }
    }
}

/// Generate symbols for preinterned strings.
///
/// These symbols,
///   rather than being generated by the global internment system,
///   are generated statically.
/// Once the global interner is initialized
///   (see [parent module](`super`)),
///   which is on first access,
///   these symbols will reference valid values.
macro_rules! static_symbol_consts {
    (@i $i:expr; $name:ident: $ty:ident $str:expr, $($tail:tt)*) => {
        #[doc=concat!(
            "Interned `",
            stringify!($ty),
            "` string `\"",
            $str,
            "\"`."
        )]
        #[allow(non_upper_case_globals)]
        pub const $name: static_symbol_ty!($ty) =
            <static_symbol_ty!($ty)>::new($i);

        // Recurse until no tail is left (terminating condition below).
        static_symbol_consts!{
            // This will result in 1 + 1 + 1 + 1 ... and will eventually hit
            // the recursion limit if we have too many static symbols, after
            // which time we may have to switch methodology.
            @i $i + 1;

            $($tail)*
        }
    };

    // Terminating condition.
    (@i $i:expr;) => {}
}

/// Statically allocate [`SymbolId`]s for the provided symbols,
///   and schedule their static strings to be interned upon initialization
///   of the global interner.
///
/// This generates [`fill`],
///   which the global interners call by default.
/// Any interner may optionally invoke this,
///   immediately after initialization,
///     /before/ any internment requests.
macro_rules! static_symbols {
    ($($name:ident : $ty:ident $str:expr),*) => {
        /// Static symbols (pre-allocated).
        ///
        /// Each of the constants in this module represent a [`SymbolId`]
        ///   statically allocated at compile-time.
        /// The strings that they represent are automatically populated into
        ///   the global interners when the interner is first accessed.
        ///
        /// _You should always use the generated constant to reference these
        ///    symbols!_
        /// Do not rely upon their integer value,
        ///   as it _will_ change over time.
        /// The sole exception is to use marker symbols to identify ranges
        ///   of symbols;
        ///     see [`MarkStaticSymbolId`].
        ///
        /// See [`crate::sym`] for more information on static symbols.
        ///
        /// `static` is a keyword in Rust,
        ///   so we shorten the module name to `st`.
        pub mod st {
            use super::*;

            static_symbol_consts! {
                // Index 0 is not valid, so begin at 1
                @i 1;

                $(
                    $name: $ty $str,
                )*
            }
        }

        /// Fill a new interner with static symbols.
        ///
        /// Panics
        /// ======
        /// This function will panic if the interner has any symbols,
        ///   which would cause misalignment with the generated constants.
        pub(super) fn fill<'a, I, Ix>(interner: I) -> I
        where
            I: Interner<'a, Ix>,
            Ix: SymbolIndexSize
        {
            assert!(
                interner.len() == 0,
                "cannot fill non-empty Interner with static symbols"
            );

            // This array does not exist as a constant, because that would
            //   require that we count the number of items first for the
            //   sake of the type definition.
            // This is more convenient.
            array::IntoIter::new([
                $(
                    $str,
                )*
            ]).for_each(|sym| { interner.intern(sym); });

            interner
        }
    }
}

static_symbol_newtypes! {
    /// A symbol suitable as a C-style identifier.
    ///
    /// This is the traditional `[a-zA-Z_][a-zA-Z0-9_]*`,
    ///   common in many programming languages.
    cid: CIdentStaticSymbolId,

    /// This symbol serves only as a marker in the internment pool to
    ///   delimit symbol ranges;
    ///     its string value is incidental and should not be relied upon.
    mark: MarkStaticSymbolId
}

// Static symbols that will have their strings interned upon global
//   interner initialization.
//
// Each of these generates a constant of the same name with a [`SymbolId`].
// This symbol is constant,
//   generated at compile-time,
//   and is intended to be used with a global interner.
// Since a global interner is initialized on first use,
//   which in turn populates the interner using [`fill`] above,
//   this constant will always represent a valid global symbol within the
//   context of reads.
//
// The constants are not all-uppercase,
//   which creates the illusion that the symbols were dynamically generated;
//     this isn't entirely false,
//       given that internment _is_ a runtime operation even for these
//       symbols.
//
// Certain symbols are Rust identifiers,
//   and therefore begin with a capital letter;
//     this is also done by rustc
//       (see https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_span/symbol.rs.html).
//
// See parent documentation for more information.
//
// These end up in the `st` module,
//   which is re-exported by the parent module.
static_symbols! {
    // Index begins at 1, since 0 is reserved during interner initialization
    True: cid "true",
    False: cid "false",

    // [Symbols will be added here as they are needed.]

    // Marker indicating the end of the static symbols
    END_STATIC: mark "{{end static}}"
}

#[cfg(test)]
mod test {
    use super::st;
    use crate::sym::{GlobalSymbolIntern, SymbolId};

    #[test]
    fn global_sanity_check() {
        // If we _don't_ prefill, make sure we're not starting at the first
        // offset when interning, otherwise it'll look correct.
        let new: SymbolId = "force offset".intern();

        assert!(
            new.as_usize() > st::END_STATIC.as_usize(),
            "a new global symbol allocation was not > END_STATIC, \
             indicating that prefill is not working!"
        );

        // Further sanity check to make sure indexes align as expected,
        // not that you wouldn't otherwise notice that the whole system is
        // broken, but this ought to offer a more direct hint as to what
        // went wrong.
        assert_eq!(st::True.as_prog_sym(), "true".intern());
        assert_eq!(st::False.as_prog_sym(), "false".intern());
    }
}
