// String internment
//
//  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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
//! This system is currently a thin wrapper providing an abstraction for
//!   future evolution.
//!
//! Interned strings are represented by [`Symbol`]:
//!
//!   - [`SymbolRc`] - Wrapper around [`Rc`]-backed [`str`] slices.
//!
//! Symbols are created, stored, compared, and retrieved by
//!   an [`Interner`]:
//!
//!   - [`DefaultSetInterner`] - The currently recommended intern pool
//!       configuration for symbol interning.
//!   - [`HashSetInterner`] - Intern pool backed by a [`HashSet`].
//!   - [`FnvHashSetInterner`] - Intern pool backed by a [`HashSet`] using the
//!       Fowler-Noll-Vo hashing algorithm.
//!
//! Note that the [`DefaultSetInterner`] has a configuration chosen for
//!   efficient _symbol interning_;
//!     see its documentation for more information and caveats,
//!       since it may not fit your particular use case.
//!
//! All [`Symbol`] values implement [`Clone`] and can be passed around
//!   freely without associated lifetimes.
//!
//! ```
//! use tamer::sym::{Interner, DefaultSetInterner};
//!
//! // Inputs to be interned
//! let a = "foo";
//! let b = &"foo".to_string();
//! let c = "foobar";
//! let d = &c[0..3];
//!
//! let mut interner = DefaultSetInterner::new();
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
//! assert_eq!(ia, ia.clone());
//!
//! // Only "foo" and "foobar" are interned
//! assert_eq!(2, interner.len());
//! assert!(interner.contains("foo"));
//! assert!(interner.contains("foobar"));
//! assert!(!interner.contains("something else"));
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
//! String comparison then amounts to comparing pointers (`O(1)`) in the
//!   best case (see Equality of Interns below)
//!     rather than having to scan the string (`O(n)`).
//! There is, however, a cost of interning strings,
//!   as well as comparing new strings to the intern pool.
//!
//! Symbols can be used as keys to a map to associate unique data with each
//!   intern,
//!     while at the same time doubling as an intern pool.
//!
//! From the perspective of Rust,
//!   this also provides a convenient alternative to passing around
//!   lifetimes,
//!     which becomes unwieldy when shared data is used in many places
//!       throughout the system.
//! Each [`Symbol`] implements [`Clone`].
//!
//! [string interning]: https://en.wikipedia.org/wiki/String_interning
//!
//!
//! Internment Mechanism
//! ====================
//! An internment mechanism is easily implemented in Rust using [`Rc`],
//!   which is the approach taken by [`SymbolRc`].
//! While this does have a minor runtime overhead,
//!   the simplicity afforded by the implementation makes it attractive
//!   until it becomes a bottleneck.
//! Considering all of the low-hanging fruit in the TAME→TAMER
//!   transition,
//!     this is not expected to be a bottleneck for quite some time.
//!
//! Rust implemented [`From`] for slices for [`Rc`] in [RFC 1845][rfc-1845],
//!   where it explicitly used string interning as a use case.
//! A trivial string interner can be implemented using
//!   `HashSet<Rc<str>>`,
//!     which is done by [`HashSetInterner`] using [`SymbolRc`].
//! However, the best mechanism by which to perform interning must be
//!   determined on a case-by-case basis.
//! For example,
//!   if a mapping between strings and some value is required,
//!   then the pool can be combined with the mapping by using a
//!     [`HashMap`](std::collections::HashMap)
//!     with a `Rc<str>` key type.
//!
//! [rfc-1845]: https://rust-lang.github.io/rfcs/1845-shared-from-slice.html
//!
//! Further, we may want to implement a more robust internment system in the
//!   future,
//!     possibly using existing crates.
//!
//! To accommodate the above situations,
//!   this implementation is generalized and defers implementation details
//!   to individual implementers.
//!
//! [smart pointer]: https://doc.rust-lang.org/book/ch15-00-smart-pointers.html
//!
//!
//! Lifetime of Interns
//! -------------------
//! Implementations need not bother with trying to free up space in the
//!   intern pool when strings are no longer in use,
//!     since compilation processes are short-lived.
//! This gives much more flexibility in implementation.
//!
//!
//! Equality of Interns
//! -------------------
//! Two [`Symbol`] values must be considered [`Eq`] if their wrapped
//!   values are `Eq`.
//! This requirement,
//!   together with the requirement that equal values share the same hash
//!     (via [`Hash`]),
//!     permit use of hash tables
//!       (such as [`HashSet`] and [`HashMap`](std::collections::HashMap))
//!       to store interned values.
//!
//! Comparing [`Rc`] values is usually cheap because checks are first
//!   performed by comparing pointers to the underlying data.
//! If they represent the same interned symbol,
//!   then pointers will match.
//! If not,
//!   the underlying values will be compared.
//! Since they are known to be different,
//!   this comparison should go quickly,
//!   unless the two strings being compared share a common prefix.
//!
//! In situations where it is not acceptable to fall back to a potentially
//!   expensive negative case,
//!     you may compare interned values by memory location using
//!     [`Symbol::ptr_eq`]
//!       (so named after [`Rc::ptr_eq`]):
//!
//! ```
//! use tamer::sym::{Symbol, SymbolRc};
//!
//! let a = SymbolRc::new("foo");
//! let b = SymbolRc::new(&"foo".to_string());
//!
//! assert_eq!(a, b);
//! assert_eq!(*a, *b);
//! assert!(a.ptr_eq(&a.clone()));
//! assert!(!a.ptr_eq(&b));
//! ```
//!
//!
//! Name Mangling
//! =============
//! Interners do not perform [name mangling][],
//!   nor is it yet clear if they should.
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
//!       `Rc<str>`,
//!         which is used by [`SymbolRc`].
//!   - Emacs directly exposes the intern pool at runtime as
//!     [`obarray`][es].
//!   - [`string-cache`][rust-string-cache] is a string interning system
//!       for Rust developed by Mozilla for Servo.
//!   - [`string-interner`][rust-string-interner] is another string
//!       interning library for Rust.
//!
//! [flyweight pattern]: https://en.wikipedia.org/wiki/Flyweight_pattern
//! [rust-string-cache]: https://github.com/servo/string-cache
//! [rust-string-interner]: https://github.com/robbepop/string-interner

use ::fnv::FnvBuildHasher;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::hash::{BuildHasher, Hash};
use std::ops::Deref;
use std::rc::Rc;

/// Reference to an interned string.
///
/// This encapsulates implementation details regarding the internment
///   process and allows it to change over time,
///     or even vary between subsystems.
///
/// An interned string must be treated as if it were the underlying
///   interned type `T` by implementing [`Deref`].
/// Further,
///   since interned string should be mere references to data,
///   they ought to be cheap to copy.
/// However,
///   since reference counting may be involved,
///   we must use [`Clone`] instead of [`Copy`].
///
/// Storing Interns
/// ===============
/// _This is a wrapper for an interned value;
///   it does not necessary mean that a value was actually interned!_
/// It ought to be created using an [`Interner`],
///   which is responsible for creation, storage, and retrieval of interned
///   values.
pub trait Symbol: Deref<Target = str> + Clone + Eq + Hash {
    /// Represent `value` as an interned string.
    ///
    /// _This does not perform internment;_ see
    /// [`Interner`].  This merely provides a wrapper for interned
    /// values which are then stored by an `Interner`.
    fn new(value: &str) -> Self;

    /// Compare underlying memory location for equality with the memory
    /// location of another interned value.
    ///
    /// Symbol strings are [`Eq`] one-another if their underlying values
    ///   are also `Eq`.
    /// This allows comparing underlying memory locations for equality,
    ///   both to ensure that internment is actually being performed,
    ///   and to permit separate internment systems sharing the same string
    ///     set.
    fn ptr_eq(&self, other: &Self) -> bool;
}

/// An `Rc`-backed reference to an interned string.
///
/// This is a zero-cost abstraction and should provide no additional
///   overhead over using `Rc` directly.
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SymbolRc(Rc<str>);

impl Symbol for SymbolRc {
    /// Represent `value` as an interned string.
    ///
    /// _This does not perform internment;_ see
    /// [`Interner`].  This merely provides a wrapper for interned
    /// values which are then stored by an `Interner`.
    #[inline]
    fn new(value: &str) -> Self {
        Self(value.into())
    }

    /// Compare underlying memory location for equality with the memory
    /// location of another interned value.
    ///
    /// ```
    /// use tamer::sym::{Symbol, SymbolRc};
    ///
    /// let a = SymbolRc::new("foo");
    /// let b = SymbolRc::new(&"foo".to_string());
    ///
    /// assert!(a.ptr_eq(&a.clone()));
    /// assert!(!a.ptr_eq(&b));
    /// ```
    ///
    /// For more information,
    ///   see [`Symbol::ptr_eq`].
    #[inline]
    fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Deref for SymbolRc {
    type Target = str;

    /// Retrieve underlying string slice.
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Clone for SymbolRc {
    /// Clone a reference to the interned value.
    ///
    /// This creates another pointer to the same internal value, increasing
    /// the strong reference count on the underlying [`Rc`].
    #[inline]
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl From<&str> for SymbolRc {
    #[inline]
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

/// Create, store, compare, and retrieve [`Symbol`] values.
///
/// Interners are generic over the type of their interned values.
/// They accept string slices and produce values of type [`Symbol`].
///
/// The intended use case is to attempt to blindly
///   [`intern`](Interner::intern) slices;
///     if it has already been interned,
///       then you will receive a copy of the existing [`Symbol`] value.
/// If it hasn't been interned,
///   then it will be added.
///
/// If you care whether a value has been interned yet or not,
///   use [`contains`](Interner::contains).
///
/// See the [module-level documentation](self) for an example.
pub trait Interner<T: Symbol> {
    /// Intern a string slice or return an existing matching intern.
    ///
    /// If the provided string has already been interned,
    ///   then the existing [`Symbol`] value will be cloned and returned.
    /// Otherwise,
    ///   the string will be interned and a clone returned.
    ///
    /// To retrieve an existing intern _without_ interning,
    ///   see [`intern_soft`](Interner::intern_soft).
    fn intern(&mut self, value: &str) -> T;

    /// Retrieve an existing intern for the string slice `s`.
    ///
    /// Unlike [`intern`](Interner::intern),
    ///   this will _not_ intern the string if it has not already been
    ///   interned.
    fn intern_soft(&mut self, value: &str) -> Option<T>;

    /// Determine whether the given value has already been interned.
    fn contains(&self, value: &str) -> bool;

    /// Number of interned strings.
    ///
    /// This count will increase each time a unique string is interned.
    /// It does not increase when a string is already interned.
    fn len(&self) -> usize;
}

/// An interner backed by a [`HashSet`].
///
/// This interner is appropriate to be used when symbols need only be
///   interned and have no associated data.
///
/// For the recommended configuration,
///   see [`DefaultSetInterner`].
///
/// See the [module-level documentation](self) for an example.
pub struct HashSetInterner<T = SymbolRc, S = RandomState>
where
    T: Symbol,
    S: BuildHasher,
{
    /// Intern pool.
    map: HashSet<T, S>,
}

impl<T, S> HashSetInterner<T, S>
where
    T: Symbol,
    S: BuildHasher + Default,
{
    /// Create a new interner with an underlying [`HashSet`].
    ///
    /// Prefer [`with_capacity`](HashSetInterner::with_capacity) over this
    ///   function if capacity can be reasonably estimated.
    #[inline]
    pub fn new() -> Self {
        Self {
            map: HashSet::with_hasher(Default::default()),
        }
    }

    /// Create a new interner with at least the specified capacity for the
    ///   underlying [`HashSet`].
    ///
    /// This method of construction should be used any time a capacity can
    ///   be predicated since it reduces the chance of reallocations as
    ///   interns exceed the capacity.
    /// For example,
    ///   if interns are used to represent symbols and most packages contain
    ///   at least `N` symbols,
    ///     then `capacity` ought to be ≥N.
    ///
    /// For more information,
    ///   see [`HashSet::with_capacity`].
    ///
    /// See also `intern` benchmarks.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashSet::with_capacity_and_hasher(
                capacity,
                Default::default(),
            ),
        }
    }
}

impl<T, S> Interner<T> for HashSetInterner<T, S>
where
    T: Symbol,
    S: BuildHasher,
{
    fn intern(&mut self, value: &str) -> T {
        let intern = T::new(value);

        if !self.map.contains(&intern) {
            self.map.insert(intern.clone());
            return intern;
        }

        self.map.get(&intern).unwrap().clone()
    }

    #[inline]
    fn intern_soft(&mut self, value: &str) -> Option<T> {
        self.map.get(&T::new(value)).map(|s| s.clone())
    }

    #[inline]
    fn contains(&self, value: &str) -> bool {
        self.map.contains(&T::new(value))
    }

    #[inline]
    fn len(&self) -> usize {
        self.map.len()
    }
}

/// Interner using the [Fowler-Noll-Vo][fnv] (FNV) hashing function.
///
/// _This is currently the hash function used by [`DefaultSetInterner`]._
///
/// If the intern pool will contains strings primarily of length ≤ 60,
///   and if denial of service is not a concern,
///   then use of the FNV hash function will outperform the default
///     [`DefaultHasher`](std::collections::hash_map::DefaultHasher)
///     (which uses SipHash at the time of writing).
/// See intern benchmarks for a comparison.
/// For a comparison of various hashing algorithms,
///   see the [`hash-rs`][hash-rs] project.
///
/// [hash-rs]: https://github.com/Gankra/hash-rs
pub type FnvHashSetInterner<T> = HashSetInterner<T, FnvBuildHasher>;

/// Recommended configuration for set-based interners.
///
/// The choice of this default relies on certain assumptions:
///   - Denial-of-service attacks against the hash function are not a
///       concern; and
///   - Strings to be interned are mostly ≤ 60 characters.
///
/// For more information on the hashing algorithm,
///   see [`FnvHashSetInterner`].
pub type DefaultSetInterner = FnvHashSetInterner<SymbolRc>;

#[cfg(test)]
mod test {
    use super::*;

    mod interned_rc {
        use super::*;

        /// Dereferencing should fall through all layers to the underlying `T`.
        #[test]
        fn derefs_to_inner_value() {
            let expected = "foo";
            let sut = SymbolRc::new(expected);

            assert_eq!(expected, &*sut);
        }

        /// Symbol string must be easily cloned and must represent the same
        /// underlying data.
        #[test]
        fn clones_equal() {
            let expected = "bar";
            let sut = SymbolRc::new(expected);

            // This is the true test
            assert!(sut.ptr_eq(&sut.clone()));
            assert_eq!(sut, sut.clone());

            // But we also want to verify derefs.
            assert_eq!(*sut, *sut.clone());
        }

        #[test]
        fn not_ptr_eq_if_memory_locations_differ() {
            let a = SymbolRc::new("foo");
            let b = SymbolRc::new(&"foo".to_string());

            // Pointers differ
            assert!(!a.ptr_eq(&b));

            // But values do not
            assert_eq!(*a, *b);
        }
    }

    macro_rules! each_interner {
        ($($(#[$attr:meta])* fn $name:ident() $body:block)*) => {
            macro_rules! interner {
                ($mod:ident: $type:ty) => {
                    mod $mod {
                        use super::*;

                        type Sut = $type;

                        $(
                            $(#[$attr])*
                            fn $name() $body
                        )*
                    }
                }
            }

            interner!(common_hash_set: HashSetInterner<SymbolRc, RandomState>);
        }
    }

    each_interner! {
        #[test]
        fn recognizes_equal_strings() {
            let a = "foo";
            let b = a.to_string();
            let c = "bar";
            let d = c.to_string();

            let mut sut = Sut::new();

            let (ia, ib, ic, id) =
                (sut.intern(a), sut.intern(&b), sut.intern(c), sut.intern(&d));

            assert!(ia.ptr_eq(&ib));
            assert_eq!(ia, ib);
            assert_eq!(&ia, &ib);
            assert_eq!(*ia, *ib);

            assert!(ic.ptr_eq(&id));
            assert_eq!(ic, id);
            assert_eq!(&ic, &id);
            assert_eq!(*ic, *id);

            assert!(!ia.ptr_eq(&ic));
            assert_ne!(ia, ic);
            assert_ne!(&ia, &ic);
            assert_ne!(*ia, *ic);
        }

        #[test]
        fn length_increases_with_each_new_intern() {
            let mut sut = Sut::new();

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
            let mut sut = Sut::new();

            assert!(!sut.contains("foo"), "recognize missing value");
            sut.intern("foo");
            assert!(sut.contains("foo"), "recognize interned value");
        }

        #[test]
        fn intern_soft() {
            let mut sut = Sut::new();

            assert_eq!(None, sut.intern_soft("foo"));

            let foo = sut.intern("foo");
            assert_eq!(Some(foo), sut.intern_soft("foo"));
        }

        #[test]
        fn new_with_capacity() {
            let n = 512;
            let sut = Sut::with_capacity(n);

            // note that this is not publicly available
            assert!(sut.map.capacity() >= n);
        }
    }
}
