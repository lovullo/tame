// String internment
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

//! String internment system.
//!
//! Interned strings are represented by [`Symbol`],
//!   created by an [`Interner`]:
//~
//!   - [`ArenaInterner`] - Intern pool backed by an [arena][] for fast
//!     and stable allocation.
//!     - [`DefaultInterner`] - The currently recommended intern pool
//!         configuration for symbol interning.
//!     - [`FxArenaInterner`] - Intern pool backed by an [arena][] using the
//!         [Fx Hash][fxhash] hashing algorithm.
//!
//! Interners return symbols by reference which allows for `O(1)` comparison
//!   by pointer.
//!
//! [arena]: bumpalo
//!
//! ```
//! use tamer::sym::{Interner, DefaultInterner, Symbol, SymbolId};
//!
//! // Inputs to be interned
//! let a = "foo";
//! let b = &"foo".to_string();
//! let c = "foobar";
//! let d = &c[0..3];
//!
//! // Interners employ interior mutability and so do not need to be
//! // declared `mut`
//! let interner = DefaultInterner::new();
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
//! // All interns can be cloned and clones are eq
//! assert_eq!(*ia, ia.clone());
//!
//! // Only "foo" and "foobar" are interned
//! assert_eq!(2, interner.len());
//! assert!(interner.contains("foo"));
//! assert!(interner.contains("foobar"));
//! assert!(!interner.contains("something else"));
//!
//! // Each symbol has an associated, densely-packed integer value
//! // that can be used for indexing
//! assert_eq!(SymbolId::from_int(1u16), ia.index());
//! assert_eq!(SymbolId::from_int(1u16), ib.index());
//! assert_eq!(SymbolId::from_int(2u16), ic.index());
//! assert_eq!(SymbolId::from_int(1u16), id.index());
//!
//! // Symbols can also be looked up by index.
//! assert_eq!(Some(ia), interner.index_lookup(ia.index()));
//! ```
//!
//! What Is String Interning?
//! =========================
//! _[String interning][]_ is a process by which a single copy of a string
//!   is stored immutably in memory as part of a _pool_.
//! When the same string is later encountered,
//!   a reference to the string in the pool is used rather than allocating a
//!   new string.
//!  Interned strings are typically referred to as "symbols" or "atoms".
//!
//! String comparison then amounts to comparing pointers (`O(1)`)
//!     rather than having to scan the string (`O(n)`).
//! There is, however, a hashing cost of interning strings,
//!   as well as looking up strings in the intern pool.
//!
//! [string interning]: https://en.wikipedia.org/wiki/String_interning
//!
//!
//! Internment Mechanism
//! ====================
//! The current [`DefaultInterner`] is [`FxArenaInterner`],
//!   which is an [arena][]-allocated intern pool mapped by the
//!   [Fx Hash][fxhash] hash function:
//!
//! 1. Strings are compared against the existing intern pool using a
//!      [`HashMap`].
//! 2. If a string has not yet been interned:
//!    - The string is copied into the arena-backed pool;
//!    - A new [`Symbol`] is allocated adjacent to it in the arena holding
//!       a string slice referencing the arena-allocated string; and
//!    - The symbol is stored as the value in the [`HashMap`] for that key.
//! 3. Otherwise, a reference to the existing [`Symbol`] is returned.
//!
//! Since the arena provides a stable location in memory,
//!   and all symbols are immutable,
//!   [`ArenaInterner`] is able to safely return any number of references to
//!     a single [`Symbol`],
//!       bound to the lifetime of the arena itself.
//! Since the [`Symbol`] contains the string slice,
//!   it also acts as a [smart pointer] for the interned string itself,
//!     allowing [`Symbol`] to be used in any context where `&str` is
//!     expected.
//! Dropping a [`Symbol`] does _not_ affect the underlying arena-allocated
//!   data.
//!
//! [smart pointer]: https://doc.rust-lang.org/book/ch15-00-smart-pointers.html
//!
//! Each symbol also has an associated integer index value
//!   (see [`Symbol::index`]),
//!     which provides a dense range of values suitable for use in vectors
//!       as an alternative to [`HashMap`] for mapping symbol data.
//! A [`SymbolId`] can be mapped back into its associated [`Symbol`]
//!   using [`Interner::index_lookup`].
//!
//! Since a reference to the same [`Symbol`] is returned for each
//!   [`Interner::intern`] and [`Interner::intern_soft`] call,
//!     symbols can be compared by pointer in `O(1)` time.
//! Symbols also implement [`Copy`],
//!   and will still compare equal to other symbols referencing the same
//!   interned value by comparing the underlying string slice pointers.
//!
//! This implementation was heavily motivated by [Rustc's own internment
//!   system][rustc-intern],
//!     but differs in significant ways:
//!
//!   - This implementation stores string references in [`Symbol`] rather
//!       than relying on a global singleton [`Interner`];
//!   - Consequently, associates the lifetime of interned strings with that
//!       of the underlying arena rather than casting to `&'static`;
//!   - Retrieves symbol values by pointer reference without requiring use
//!       of [`Interner`] or a locking mechanism; and
//!   - Stores [`Symbol`] objects in the arena rather than within a vector
//!       indexed by [`SymbolId`].
//!
//!
//! Name Mangling
//! =============
//! Interners do not perform [name mangling][].
//! For future consideration,
//!   see [RFC 2603][rfc-2603] and the [Itanium ABI][itanium-abi].
//!
//! [name mangling]: https://en.wikipedia.org/wiki/Name_mangling
//! [rfc-2603]: https://rust-lang.github.io/rfcs/2603-symbol-name-mangling-v2.html
//! [itanium-abi]: http://refspecs.linuxbase.org/cxxabi-1.86.html#mangling
//!
//!
//! Related Work and Further Reading
//! ================================
//! String interning is often tightly coupled with symbols (in the generic
//!   sense),
//!     sometimes called atoms.
//! Symbols can often be either interned,
//!   and therefore compared for equivalency,
//!   or _uninterned_,
//!     which makes them unique even to symbols of the same name.
//! Interning may also be done automatically by a language for performance.
//! Languages listed below that allow for explicit interning may also
//!   perform automatic interning as well
//!     (for example, `'symbol` in Lisp and `lowercase_vars` as atoms in
//!       Erlang).
//!
//! | Language | Interned | Uninterned |
//! | -------- | -------- | ---------- |
//! | [Erlang][] | [`list_to_atom`][edt] | _(None)_ |
//! | [GNU Emacs Lisp][] | [`intern`][es], [`intern-soft`][es] | [`make-symbol`][es], [`gensym`][es] |
//! | [GNU Guile][] | [`string->symbol`][gs], [`gensym`][gs] | [`make-symbol`][gu] |
//! | [JavaScript][] | [`Symbol.for`][js] | [`Symbol`][js] |
//! | [Java][] | [`intern`][jvs] | _(None)_ |
//! | [Lua][] | _(Automatic for string performance)_ | _(None)_ |
//! | [MIT/GNU Scheme][] | [`intern`][ms], [`intern-soft`][ms], [`string->symbol`][ms] | [`string->uninterned-symbol`][ms], [`generate-uninterned-symbol`][ms] |
//! | [PHP][] | _(Automatic for string [performance][pp])_ | _(None)_ |
//! | [Python][] | [`sys.intern`][pys] | _(None)_ |
//! | [R6RS Scheme][] | [`string->symbol`][r6s] | _(None)_ |
//! | [Racket][] | [`string->symbol`][rs], [`string->unreadable-symbol`][rs] | [`string->uninterned-symbol`][rs], [`gensym`][rs] |
//!
//! [gnu guile]: https://www.gnu.org/software/guile/
//! [gs]: https://www.gnu.org/software/guile/manual/html_node/Symbol-Primitives.html#Symbol-Primitives
//! [gu]: https://www.gnu.org/software/guile/manual/html_node/Symbol-Uninterned.html#Symbol-Uninterned
//! [gnu emacs lisp]: https://www.gnu.org/software/emacs/
//! [es]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html
//! [racket]: https://racket-lang.org/
//! [rs]: https://docs.racket-lang.org/reference/symbols.html
//! [r6rs scheme]: http://www.r6rs.org/
//! [r6s]: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html
//! [mit/gnu scheme]: https://www.gnu.org/software/mit-scheme/
//! [ms]: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Symbols.html
//! [javascript]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
//! [js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
//! [java]: http://openjdk.java.net/
//! [jvs]: https://cr.openjdk.java.net/~iris/se/12/latestSpec/api/java.base/java/lang/String.html#intern()
//! [php]: https://www.php.net/
//! [pp]: https://wiki.php.net/rfc/performanceimprovements
//! [erlang]: https://erlang.org/
//! [edt]: http://erlang.org/doc/reference_manual/data_types.html
//! [lua]: https://www.lua.org/
//! [python]: https://www.python.org/
//! [pys]: https://docs.python.org/3/library/sys.html
//!
//! More information:
//!   - Wikipedia entry on [string interning][].
//!   - The [flyweight pattern][] in object-oriented programming is a type
//!     of interning.
//!   - [RFC 1845][rfc-1845] gives an example of string interning using
//!       `Rc<str>`.
//!   - Emacs directly exposes the intern pool at runtime as
//!     [`obarray`][es].
//!   - [`string-cache`][rust-string-cache] is a string interning system
//!       for Rust developed by Mozilla for Servo.
//!   - [`string-interner`][rust-string-interner] is another string
//!       interning library for Rust.
//!   - [Rustc interns strings as `Symbol`s][rustc-intern] using an
//!       [arena allocator][rustc-arena] and avoids `Rc` by representing
//!       symbols as integer values and converting them to strings using a
//!       global pool and unsafe rust to cast to a `static` slice.
//!     - Rustc identifies symbols by integer value encapsulated within a
//!         `Symbol`.
//!     - Rustc's [`newtype_index!` macro][rustc-nt] uses
//!         [`NonZeroU32`] so that [`Option`] uses no
//!         additional space (see [pull request `53315`][rustc-nt-pr]).
//!     - Differences between TAMER and Rustc's implementations are outlined
//!         above.
//!
//! [flyweight pattern]: https://en.wikipedia.org/wiki/Flyweight_pattern
//! [rust-string-cache]: https://github.com/servo/string-cache
//! [rust-string-interner]: https://github.com/robbepop/string-interner
//! [rfc-1845]: https://rust-lang.github.io/rfcs/1845-shared-from-slice.html
//! [rustc-intern]: https://doc.rust-lang.org/nightly/nightly-rustc/syntax/ast/struct.Name.html
//! [rustc-arena]: https://doc.rust-lang.org/nightly/nightly-rustc/arena/index.html
//! [rustc-nt]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_index/macro.newtype_index.html
//! [rustc-nt-pr]: https://github.com/rust-lang/rust/pull/53315
//!
//! The hash function chosen for this module is [Fx Hash][fxhash].
//!
//!  - Rustc previously used the [Fowler-Noll-Vo (FNV)][fnv] hash
//!      function,
//!        but [now uses Fx Hash][rustc-fx].
//!    This was extracted into the [`fxhash`][fxhash] crate,
//!      which is used by TAMER.
//!    - TAMER originally used FNV,
//!         but benchmarks showed that Fx Hash was more performant.
//!  - Benchmarks for other hash functions,
//!      including FNV,
//!      can be found at the [`hash-rs`][hash-rs] project.
//!
//! [fnv]: https://doc.servo.org/fnv/
//! [rustc-fx]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_data_structures/fx/index.html
//! [hash-rs]: https://github.com/Gankra/hash-rs

use crate::global;
use bumpalo::Bump;
use fxhash::FxBuildHasher;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::hash::BuildHasher;
use std::num::{NonZeroU16, NonZeroU32, NonZeroU8};
use std::ops::Deref;
use std::{fmt, fmt::Debug};

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
    unsafe fn from_int_unchecked(n: Ix) -> SymbolId<Ix> {
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
}

impl<Ix: SymbolIndexSize> From<SymbolId<Ix>> for usize
where
    <Ix as TryInto<usize>>::Error: Debug,
{
    fn from(value: SymbolId<Ix>) -> usize {
        value.0.into().into_usize()
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
    fn into_usize(self) -> usize;
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

            fn into_usize(self) -> usize {
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
    #[inline]
    fn new(index: SymbolId<Ix>, str: &'i str) -> Symbol<'i, Ix> {
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
        self.indexes
            .borrow()
            .get(index.0.into().into_usize())
            .map(|sym| *sym)
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

/// Concisely define dummy symbols for testing.
#[cfg(test)]
macro_rules! symbol_dummy {
    ($id:expr, $name:expr) => {
        Symbol::new_dummy(SymbolId::from_int($id), $name);
    };
}

#[cfg(test)]
mod test {
    use super::*;

    mod symbol {
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

    mod interner {
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
}
