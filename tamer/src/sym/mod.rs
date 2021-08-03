// String internment system
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
//! [`HashMap`]: std::collections::HashMap
//! [`NonZeroU32`]: std::num::NonZeroU32
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

mod interner;
mod symbol;

pub use interner::{
    ArenaInterner, DefaultInterner, DefaultPkgInterner, DefaultProgInterner,
    FxArenaInterner, Interner,
};
pub use symbol::{PkgSymbol, ProgSymbol, Symbol, SymbolId, SymbolIndexSize};

/// Concisely define dummy symbols for testing.
#[cfg(test)]
macro_rules! symbol_dummy {
    ($id:expr, $name:expr) => {
        Symbol::new_dummy(SymbolId::from_int($id), $name);
    };
}
