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

/// Generate a newtype containing a condensed [`SymbolId`].
macro_rules! static_symbol_newtype {
    ($(#[$attr:meta])* $name:ident<$size:ty>) => {
        $(#[$attr])*
        #[doc=""]
        /// This is a statically-allocated symbol.
        ///
        /// This symbol is generated at compile-time and expected to be
        ///   available in the 32-bit global interner once it has been
        ///   initialized.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(<$size as SymbolIndexSize>::NonZero);

        impl $name {
            const fn new(id: $size) -> Self {
                Self(unsafe {
                    <$size as SymbolIndexSize>::NonZero::new_unchecked(id)
                })
            }

            /// Cast static symbol into a [`SymbolId`] suitable for the global
            ///   program-level interner.
            ///
            /// This is safe since global interner will always contain this
            ///   symbol before it can be read.
            pub const fn as_sym(self) -> SymbolId<$size> {
                SymbolId(self.0)
            }

            pub const fn as_usize(self) -> usize {
                self.0.get() as usize
            }
        }

        impl From<$name> for SymbolId<$size> {
            fn from(st: $name) -> Self {
                st.as_sym()
            }
        }
    };
}

/// Generate a series of newtypes and the macro `static_symbol_ty!` which
///   can be used to take a short identifier and convert it into its full
///   type identifier.
macro_rules! static_symbol_newtypes {
    ($($(#[$attr:meta])* $short:ident: $ty:ident<$size:ty>,)*) => {
        $(
            static_symbol_newtype!($(#[$attr])* $ty<$size>);
        )*

        macro_rules! static_symbol_ty {
            $(
                ($short) => {
                    $ty
                };
            )*
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
    (@i $i:expr; <$size:ty> $name:ident: $ty:ident $str:expr, $($tail:tt)*) => {
        #[doc=concat!(
            "Interned `",
            stringify!($ty),
            "` string `\"",
            $str,
            "\"`."
        )]
        pub const $name: static_symbol_ty!($ty) =
            <static_symbol_ty!($ty)>::new($i);

        // Recurse until no tail is left (terminating condition below).
        static_symbol_consts!{
            // This will result in 1 + 1 + 1 + 1 ... and will eventually hit
            // the recursion limit if we have too many static symbols, after
            // which time we may have to switch methodology.
            @i $i + 1;
            <$size>

            $($tail)*
        }
    };

    // Terminating condition.
    (@i $i:expr; <$size:ty>) => {}
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
    (<$size:ty>; $($name:ident : $ty:ident $str:expr),*) => {
        static_symbol_consts! {
            // Index 0 is not valid, so begin at 1
            @i 1;
            <$size>

            $(
                $name: $ty $str,
            )*
        }

        /// Fill a new interner with static symbols.
        ///
        /// Panics
        /// ======
        /// This function will panic if the interner has any symbols,
        ///   which would cause misalignment with the generated constants.
        pub(in super::super) fn fill<'a, I, Ix>(interner: I) -> I
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
    cid: CIdentStaticSymbolId<global::ProgSymSize>,

    /// Base-10 (decimal) integer value as a string.
    dec: DecStaticSymbolId<global::ProgSymSize>,

    /// A symbol resembling a QName of the form `prefix:local`.
    ///
    /// A symbol of this type does _not_ mean that the symbol is intended to
    ///   be a QName;
    ///     this is merely a way to describe it.
    /// For example,
    ///   `map:head` is intended as an identifier type,
    ///     not a QName.
    qname: QnameIdentStaticSymbolId<global::ProgSymSize>,

    /// This symbol serves only as a marker in the internment pool to
    ///   delimit symbol ranges;
    ///     its string value is incidental and should not be relied upon.
    mark: MarkStaticSymbolId<global::ProgSymSize>,

    /// Symbol representing a URI.
    ///
    /// This is intended for use primarily as an XML namespace.
    /// URIs are expected to _not_ contain quotes and other characters that
    ///   may need escaping in XML attributes.
    uri: UriStaticSymbolId<global::ProgSymSize>,

    /// Static 16-bit [`Span`](crate::span::Span) context.
    ///
    /// These contexts are intended for use in generated code where a better
    ///   context cannot be derived.
    ctx: ContextStaticSymbolId<u16>,
}

/// Static symbols (pre-allocated).
///
/// Each of the constants in this module represent a [`SymbolId`] statically
///   allocated at compile-time.
/// The strings that they represent are automatically populated into the
///   global interners when the interner is first accessed.
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
///
/// The constants follow a naming convention:
///   - `L_` indicates that the identifier is all-lowercase.
pub mod st {
    use super::*;

    // Convert `0 ≤ n ≤ 9` into a static symbol representing a single
    //   decimal digit.
    //
    // Panics
    // ======
    // This will panic if `n > 9`.
    pub fn decimal1(n: u8) -> DecStaticSymbolId {
        assert!(n < 10);

        // The symbols are expected to be in a very specific position in the
        //   pool (n+1).
        // This is verified by tests at the bottom of this file.
        DecStaticSymbolId(unsafe {
            <global::ProgSymSize as SymbolIndexSize>::NonZero::new_unchecked(
                (n as global::ProgSymSize) + 1,
            )
        })
    }

    impl From<u8> for DecStaticSymbolId {
        // Convert `0 ≤ n ≤ 9` into a static symbol representing a single
        //   decimal digit.
        //
        // See [`decimal1`].
        fn from(n: u8) -> Self {
            decimal1(n)
        }
    }

    static_symbols! {
        <global::ProgSymSize>;

        // Decimal strings are expected to be at index (n+1).
        // See `decimal1`.
        N0: dec "0",
        N1: dec "1",
        N2: dec "2",
        N3: dec "3",
        N4: dec "4",
        N5: dec "5",
        N6: dec "6",
        N7: dec "7",
        N8: dec "8",
        N9: dec "9",


        L_BOOLEAN: cid "boolean",
        L_CGEN: cid "cgen",
        L_CLASS: cid "class",
        L_CONST: cid "const",
        L_DEP: cid "dep",
        L_DESC: cid "desc",
        L_DIM: cid "dim",
        L_DTYPE: cid "dtype",
        L_EMPTY: cid "empty",
        L_FALSE: cid "false",
        L_FLOAT: cid "float",
        L_FUNC: cid "func",
        L_GEN: cid "gen",
        L_GENERATED: cid "generated",
        L_INTEGER: cid "integer",
        L_L: cid "l",
        L_LPARAM: cid "lparam",
        L_MAP: cid "map",
        L_MAP_HEAD: qname "map:head",
        L_MAP_TAIL: qname "map:tail",
        L_META: cid "meta",
        L_NAME: cid "name",
        L_PACKAGE: cid "package",
        L_PARAM: cid "param",
        L_PARENT: cid "parent",
        L_PREPROC: cid "preproc",
        L_PROGRAM: cid "program",
        L_RATE: cid "rate",
        L_RETMAP: cid "retmap",
        L_RETMAP_HEAD: qname "retmap:head",
        L_RETMAP_TAIL: qname "retmap:tail",
        L_SRC: cid "src",
        L_SYM: cid "sym",
        L_TITLE: cid "title",
        L_TPL: cid "tpl",
        L_TRUE: cid "true",
        L_TYPE: cid "type",
        L_UUROOTPATH: cid "__rootpath",
        L_WORKSHEET: cid "worksheet",
        L_XMLNS: cid "xmlns",
        L_YIELDS: cid "yields",

        URI_LV_RATER: uri "http://www.lovullo.com/rater",
        URI_LV_PREPROC: uri "http://www.lovullo.com/rater/preproc",
        URI_LV_LINKER: uri "http://www.lovullo.com/rater/linker",

        // [Symbols will be added here as they are needed.]

        // Marker indicating the end of the static symbols
        //   (this must always be last).
        END_STATIC: mark "{{end}}"
    }
}

/// Static 16-bit symbols (pre-allocated).
///
/// These symbols are intended for situations where a smaller symbol size is
///   necessary.
/// Presently,
///   this includes only the [`Span`](crate::span::Span) context.
///
/// See also [st](super::st) for general static symbols.
pub mod st16 {
    use super::*;

    static_symbols! {
        <u16>;

        // Special contexts.
        CTX_LINKER: ctx "#!LINKER",

        // [Symbols will be added here as they are needed.]

        // Marker indicating the end of the static symbols
        //   (this must always be last).
        END_STATIC: mark "{{end}}"
    }
}

#[cfg(test)]
mod test {
    use super::{st, st16, DecStaticSymbolId};
    use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve, SymbolId};

    #[test]
    fn global_sanity_check_st() {
        // If we _don't_ prefill, make sure we're not starting at the first
        // offset when interning, otherwise it'll look correct.
        let new: SymbolId = "force offset".intern();

        assert!(
            new.as_usize() > st::END_STATIC.as_usize(),
            "a new global symbol allocation was not > END_STATIC, \
             indicating that prefill is either not working or that \
             the prefill contains duplicate strings!"
        );

        // Further sanity check to make sure indexes align as expected,
        // not that you wouldn't otherwise notice that the whole system is
        // broken, but this ought to offer a more direct hint as to what
        // went wrong.
        assert_eq!(st::L_TRUE.as_sym(), "true".intern());
        assert_eq!(st::L_FALSE.as_sym(), "false".intern());
    }

    #[test]
    fn global_sanity_check_st16() {
        // If we _don't_ prefill, make sure we're not starting at the first
        // offset when interning, otherwise it'll look correct.
        let new: SymbolId<u16> = "force offset".intern();

        assert!(
            new.as_usize() > st16::END_STATIC.as_usize(),
            "a new 16-bit global symbol allocation was not > END_STATIC, \
             indicating that prefill is either not working or that \
             the prefill contains duplicate strings!"
        );
    }

    #[test]
    fn decimal1_0_to_9() {
        for n in 0..=9 {
            assert_eq!(st::decimal1(n).as_sym().lookup_str(), n.to_string());

            // From<u8>
            assert_eq!(
                DecStaticSymbolId::from(n).as_sym().lookup_str(),
                n.to_string()
            );
        }
    }

    #[test]
    #[should_panic]
    fn decimal1_gt_9_panics() {
        st::decimal1(10);
    }
}
