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
//! Interned strings are represented by an integer [`SymbolId`],
//!   created by an [`Interner`].
//!
//!   - [`ArenaInterner`] - Intern pool backed by an [arena][] for fast
//!     and stable allocation.
//!     - [`FxArenaInterner`] - Intern pool backed by an [arena][] using the
//!         [Fx Hash][fxhash] hashing algorithm.
//!     - [`DefaultInterner`] - The currently recommended intern pool
//!         configuration for symbol interning (size-agnostic).
//!     - [`DefaultPkgInterner`] - The currently recommended intern pool
//!         configuration for individual packages and their imports.
//!     - [`DefaultProgInterner`] - The currently recommended intern pool
//!         configuration for all packages within a program.
//!
//! Interners represent symbols as integer values which allows for `O(1)`
//!   comparison of any arbitrary interned value,
//!     regardless of length.
//!
//! The most common way to intern strings is using the global static
//!   interners,
//!     which offer several conveniences that are discussed below.
//! However,
//!   interners may also be used standalone without requiring global state.
//!
//! [arena]: bumpalo
//!
//! ```
//! use tamer::sym::{GlobalSymbolIntern, GlobalSymbolResolve, PkgSymbolId};
//!
//! // Interns are represented by `SymbolId`.  You should choose one of
//! // `ProgSymbolId` or `PkgSymbolId`, unless both must be supported.
//! let foo: PkgSymbolId = "foo".intern();
//! assert_eq!(foo, foo);
//!
//! // Interning the same string twice returns the same intern
//! assert_eq!(foo, "foo".intern());
//!
//! // All interns can be freely copied.
//! let foo2 = foo;
//! assert_eq!(foo, foo2);
//!
//! // Different strings intern to different values
//! assert_ne!(foo, "bar".intern());
//!
//! // Interned slices can be looked up by their symbol id.
//! assert_eq!(&"foo", &foo.lookup_str());
//! ```
//!
//! What Is String Interning?
//! =========================
//! _[String interning][]_ is a process by which a single copy of a string
//!   is stored immutably in memory as part of a _pool_.
//! Once a string has been interned,
//!   attempting to intern it again will always return the same [`SymbolId`].
//! Interned strings are typically referred to as "symbols" or "atoms".
//!
//! String comparison then amounts to comparing integer values (`O(1)`)
//!     rather than having to scan the string (`O(n)`).
//! There is, however, a hashing cost of interning strings,
//!   as well as looking up strings in the intern pool (both `O(1)`).
//!
//! It is expected that strings are interned as soon as they are encountered,
//!   which is likely to be from source inputs or previously compiled object
//!   files.
//! Processing stages will then hold the interned [`SymbolId`] and use those
//!   for any needed comparsions,
//!     without any need to look up the string from the pool.
//! Strings should only be looked up
//!   (using [`GlobalSymbolResolve::lookup_str`] or
//!   [`Interner::index_lookup`]) when they need to be written
//!     (e.g. into a target or displayed to the user).
//!
//! [`SymbolId`] is monotonically increasing from 1,
//!   making it a useful densely-packed index as an alternative [`HashMap`]
//!   when most of the symbols will be represented as part of the map.
//! This also means that strings can be interned in bulk and have a
//!   predictable relationship to one-another---for
//!     example,
//!       if strings are interned in lexographic order,
//!         their [`SymbolId`]s will reflect that same ordering,
//!           so long as those strings have not previously been interned.
//! Bulk insertion should therefore be done before processing user input.
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
//!    - A new integer [`SymbolId`] index is allocated;
//!    - The string is copied into the arena-backed pool at that new index;
//!        and
//!    - The string is hashed and will resolve to the new [`SymbolId`] for
//!        future lookups and internment attempts.
//! 3. Otherwise, the existing [`SymbolId`] associated with the provided
//!      string is returned.
//!
//! The string associated with a [`SymbolId`] can be looked up from the pool
//!   using [`GlobalSymbolResolve::lookup_str`] for global interners,
//!     or [`Interner::index_lookup`] otherwise.
//! Interned strings are represented by [`SymbolStr`],
//!   which can be dereferenced into [`&str`].
//! Symbols allocated using a global interner will have a `'static`
//!   lifetime.
//!
//! Since [`SymbolId`] is an integer value,
//!   it implements [`Copy`] and will still compare equal to other symbols
//!   referencing the same interned value.
//!
//! This implementation was heavily motivated by [Rustc's own internment
//!   system][rustc-intern].
//!
//! [`HashMap`]: std::collections::HashMap
//! [`NonZeroU32`]: std::num::NonZeroU32
//!
//!
//! Symbol Index Sizes
//! ------------------
//! [`SymbolId`] is generic over [`SymbolIndexSize`],
//!   which is implemented for
//!   [`global::PkgSymSize`](crate::global::PkgSymSize) and
//!   [`global::ProgSymSize`](crate::global::ProgSymSize).
//! This allows the compiler---which processes far less data than the
//!   linker---to use a smaller index size.
//! This is desirable for certain core data structures,
//!   like spans,
//!   which try to pack a lot of information into 64-bit structures.
//!
//! But the cost is that of another trait bound on any systems that must
//!   accommodate any [`SymbolIndexSize`]
//! Systems should therefore favor one of these two types if they are not
//!   shared between e.g. compilers and linkers:
//!
//!   - [`PkgSymbolId`] for individual packages and their imports; and
//!   - [`ProgSymbolId`] for all packages in a program.
//!
//! Note that _it is not permissable to cast between different index sizes_!
//!   Even though a [`PkgSymbolId`] could fit within the index size of a
//!     [`ProgSymbolId`],
//!       for example,
//!       they use _different_ interners with their own distinct index
//!       sets.
//! A system should avoid using multiple interners at the same time,
//!   and trait bounds will make such a mistake painfully obvious.
//!
//! Global Interners
//! ----------------
//! TAMER offers two thread-local global interners that intern strings with
//!   a `'static` lifetime,
//!     simplifying the handling of lifetimes;
//!       they produce symbols of type [`PkgSymbolId`] and [`ProgSymbolId`]
//!       and are intended for packages and entire programs respectively.
//! These interners are lazily initialized on first use.
//! Symbols from the two interners cannot be mixed;
//!   you must use the largest [`SymbolIndexSize`] needed.
//!
//! Global interners were introduced because symbols are used by virtually
//!   every part of the system,
//!     which polluted everything with interner lifetimes.
//! This suggested that the interner should be treated instead as if it were
//!   a part of Rust itself,
//!     and treated no differently than other core memory allocation.
//!
//! All [`SymbolStr`] objects returned from global interners hold a
//!   `'static` lifetime to simplify lifetime management and borrowing.
//! However,
//!   these should not be used in place of [`SymbolId`] if the string value
//!   is not actually needed.
//!
//! Global interners are exposed via friendly APIs using two traits:
//!
//!   - [`GlobalSymbolIntern`] provides an `intern` method that can be used
//!       on any [`&str`] (e.g. `"foo".intern()`); and
//!   - [`GlobalSymbolResolve`] provides a `lookup_str` method on
//!       [`SymbolId`] which resolves the symbol using the appropriate
//!       global interner,
//!         producing a [`SymbolStr`] holding a reference to the `'static`
//!         string slice within the pool.
//!
//! These traits are intentionally separate so that it is clear how a
//!   particular package or object makes use of symbols.
//! If this distinction proves too cumbersome,
//!   then they may be combined in the future.
//!
//! TAMER does not currently utilize threads,
//!   and global interners are never dropped,
//!   and so [`SymbolStr`] will always refer to a valid string.
//!
//! There is no mechanism preventing [`SymbolId`] from one interner from
//!   being used with another beyond [`SymbolIndexSize`] bounds;
//!     if you utilize interners for any other purpose,
//!       it is advised that you create newtypes for their [`SymbolId`]s.
//!
//! Uninterned Symbols
//! ------------------
//! Interners are able to allocate a [`SymbolId`] without interning,
//!   which will produce a symbol that cannot compare equal to any other
//!   symbol and avoids the hashing cost required to perform interning.
//! This is useful for a couple of reasons:
//!
//!   1. To create a symbol that is guaranteed to be unique,
//!        even if the same string value was previously interned; and
//!   2. To store a string without a hashing cost,
//!        making [`SymbolId`] a suitable substitute for [`String`] when the
//!        string will never need the benefits of internment.
//!
//!  The second option allows all data structures to consistently carry
//!    [`SymbolId`] and let the owner of those data decide whether it is
//!    appropriate to incur a hashing cost;
//!      using [`String`] forces that decision upon users of the data
//!      structure,
//!        and also makes for an awkward and confusing API.
//!
//! Related Work and Further Reading
//! ================================
//! String interning is used in a variety of systems and languages.
//! Symbols can typically be either interned,
//!   and therefore compared for equivalency,
//!   or _uninterned_,
//!     which makes them unique even to symbols of the same name.
//! Interning may also be done automatically by a language as a performance
//!   optimization,
//!     or by a compiler for storage in an object file such as ELF.
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
//!   - [Rustc interns strings as `Symbol`s][rustc-intern] using a
//!       global [arena allocator][rustc-arena]  and unsafe rust to cast to
//!       a `static` slice.
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
//! [rustc-intern]: https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_span/symbol.rs.html
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
pub use symbol::{
    GlobalSymbolIntern, GlobalSymbolInternUnchecked, GlobalSymbolResolve,
    PkgSymbolId, ProgSymbolId, SymbolId, SymbolIndexSize, SymbolStr,
};
