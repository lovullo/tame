// Functional primitives and conveniences
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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

//! Functional primitives and conveniences.
//!
//! TODO: More information on TAMER's stance on Rust,
//!   the architecture of the compiler,
//!   and how that squares with functional programming.
//!
//! The definitions in this module are _derived from needs in TAMER_.
//! This is _not_ intended to be a comprehensive functional programming
//!   library,
//!     nor is such a thing desirable.
//!
//! This module is named `f` rather than `fn` because the latter is a
//!   reserved word in Rust and makes for awkward (`r#`-prefixed) imports.

/// A type providing a `map` function from inner type `T` to `U`.
///
/// This used to be called `Functor`,
///   but was renamed because it was an abuse of terminology;
///   this is polymorphic over the entire trait,
///     rather than just the definition of `map`,
///     allowing implementations to provide multiple specialized `map`s
///       without having to define individual `map_*` methods.
/// Rust will often be able to infer the proper types and invoke the
///   intended `map` function within a given context,
///     but methods may still be explicitly disambiguated using the
///     turbofish notation if this is too confusing in context.
/// Strictly speaking,
///   if a functor requires a monomorphic function
///     (so `T = U`),
///   then it's not really a functor.
///
/// This trait also provides a number of convenience methods that can be
///   implemented in terms of [`Map::map`].
///
/// If a mapping can fail,
///   see [`TryMap`].
///
/// Why a primitive `map` instead of `fmap`?
/// ========================================
/// One of the methods of this trait is [`Map::fmap`],
///   which [is motivated by Haskell][haskell-functor].
/// This trait implements methods in terms of [`map`](Self::map) rather than
///   [`fmap`](Self::fmap) because `map` is a familiar idiom in Rust and
///   fits most naturally with surrounding idiomatic Rust code;
///     there is no loss in generality in doing so.
/// Furthermore,
///   implementing `fmap` requires the use of moved closures,
///   which is additional boilerplate relative to `map`.
///
/// [haskell-functor]: https://hackage.haskell.org/package/base/docs/Data-Functor.html
pub trait Map<T, U = T>: Sized {
    /// Type of object resulting from [`Map::map`] operation.
    ///
    /// The term "target" originates from category theory,
    ///   representing the codomain of the functor.
    type Target = Self;

    /// A structure-preserving map between types `T` and `U`.
    ///
    /// This unwraps any number of `T` from `Self` and applies the
    ///   function `f` to transform it into `U`,
    ///     wrapping the result back up into [`Self`].
    ///
    /// This is the only method that needs to be implemented on this trait;
    ///   all others are implemented in terms of it.
    fn map(self, f: impl FnOnce(T) -> U) -> Self::Target;

    /// Curried form of [`Map::map`],
    ///   with arguments reversed.
    ///
    /// `fmap` returns a unary closure that accepts an object of type
    ///   [`Self`].
    /// This is more amenable to function composition and a point-free style.
    fn fmap(f: impl FnOnce(T) -> U) -> impl FnOnce(Self) -> Self::Target {
        move |x| x.map(f)
    }

    /// Map over self,
    ///   replacing each mapped element with `value`.
    ///
    /// This is equivalent to mapping with an identity function.
    ///
    /// The name `overwrite` was chosen because `replace` is already used in
    ///   core Rust libraries to overwrite and then _return_ the original
    ///   value,
    ///     whereas this function overwrites and then returns
    ///     [`Self::Target`].
    ///
    /// This is intended for cases where there's a single element that will
    ///   be replaced,
    ///     taking advantage of [`Map`]'s trait-level polymorphism.
    fn overwrite(self, value: U) -> Self::Target {
        self.map(|_| value)
    }

    /// Curried form of [`Map::overwrite`],
    ///   with arguments reversed.
    fn foverwrite(value: U) -> impl FnOnce(Self) -> Self::Target {
        move |x| x.overwrite(value)
    }
}

impl<T, U> Map<T, U> for Option<T> {
    type Target = Option<U>;

    fn map(self, f: impl FnOnce(T) -> U) -> <Self as Map<T, U>>::Target {
        Option::map(self, f)
    }
}

/// A type providing a `try_map` function from inner type `T` to `U`.
///
/// This is a fallible version of [`Map`];
///   see that trait for more information.
pub trait TryMap<T, U = T>: Sized {
    /// Type of object resulting from [`TryMap::try_map`] operation.
    ///
    /// The term "target" originates from category theory,
    ///   representing the codomain of the functor.
    type Target = Self;

    /// Result of the mapping function.
    type FnResult<E> = Result<T, (T, E)>;

    /// Result of the entire map operation.
    type Result<E> = Result<Self, (Self, E)>;

    /// A structure-preserving map between types `T` and `U`.
    ///
    /// This unwraps any number of `T` from `Self` and applies the
    ///   function `f` to transform it into `U`,
    ///     wrapping the result back up into [`Self`].
    ///
    /// Since this method takes ownership over `self` rather than a mutable
    ///   reference,
    ///     [`Self::FnResult`] is expected to return some version of `T`
    ///     alongside the error `E`;
    ///       this is usually the original `self`,
    ///         but does not have to be.
    /// Similarly,
    ///   [`Self::Result`] will also return [`Self`] in the event of an
    ///   error.
    ///
    /// This is the only method that needs to be implemented on this trait;
    ///   all others are implemented in terms of it.
    fn try_map<E>(
        self,
        f: impl FnOnce(T) -> Self::FnResult<E>,
    ) -> Self::Result<E>;

    /// Curried form of [`TryMap::try_map`],
    ///   with arguments reversed.
    ///
    /// `try_fmap` returns a unary closure that accepts an object of type
    ///   [`Self`].
    /// This is more amenable to function composition and a point-free style.
    fn try_fmap<E>(
        f: impl FnOnce(T) -> Self::FnResult<E>,
    ) -> impl FnOnce(Self) -> Self::Result<E> {
        move |x| x.try_map(f)
    }
}

/// Generate monomorphic [`TryMap`] and [`Map`] implementations for the
///   provided type.
///
/// This macro is suitable for otherwise-boilerplate `impl`s for these
///   traits.
/// If you expect anything more than a generic `map` or `try_map` operation,
///   then you should implement the traits manually.
///
/// Only tuple structs are supported at present.
///
/// For example:
///
/// ```
/// # #[macro_use] extern crate tamer;
/// # use tamer::impl_mono_map;
/// # use tamer::f::Map;
/// # fn main() {
///   #[derive(Debug, PartialEq)]
///   struct Foo(u8, Bar);
///
///   #[derive(Debug, PartialEq)]
///   enum Bar { A, B };
///
///   impl_mono_map! {
///       u8 => Foo(@, bar),
///       Bar => Foo(n, @),
///   }
///
///   assert_eq!(Foo(5, Bar::A).overwrite(Bar::B), Foo(5, Bar::B));
/// # }
/// ```
///
/// Each line above generates a pair of `impl`s,
///   each for `Foo`,
///   where `@` represents the tuple item being mapped over.
#[macro_export] // for doc test above
macro_rules! impl_mono_map {
    ($($t:ty => $tup:ident( $($pre:ident,)* @ $(, $post:ident),* ),)+) => {
        $(
            impl $crate::f::TryMap<$t> for $tup {
                fn try_map<E>(
                    self,
                    f: impl FnOnce($t) -> Self::FnResult<E>,
                ) -> Self::Result<E> {
                    match self {
                        Self($($pre,)* x $(, $post),*) => match f(x) {
                            Ok(y) => Ok(Self($($pre,)* y $(, $post),*)),
                            Err((y, e)) => Err((
                                Self($($pre,)* y $(, $post),*),
                                e,
                            )),
                        },
                    }
                }
            }

            impl $crate::f::Map<$t> for $tup {
                fn map(self, f: impl FnOnce($t) -> $t) -> Self::Target {
                    use std::convert::Infallible;
                    use $crate::f::TryMap;

                    // `unwrap()` requires `E: Debug`,
                    //   so this avoids that bound.
                    match self.try_map::<Infallible>(|x| Ok(f(x))) {
                        Ok(y) => y,
                        // Verbosely emphasize unreachability
                        Err::<_, (_, Infallible)>(_) => unreachable!(),
                    }
                }
            }
        )+
    }
}

/// A nullary [`Fn`] delaying some computation.
///
/// For the history and usage of this term in computing,
///   see <https://en.wikipedia.org/wiki/Thunk>.
pub trait Thunk<T> = Fn() -> T;

/// Data represented either as a reference with a `'static` lifetime
///   (representing a computation already having been performed),
///   or a [`Thunk`] that will produce similar data when invoked.
///
/// The purpose of this trait is to force an API to defer potentially
///   expensive computations in situations where it may be too easy to
///   accidentally do too much work due to Rust's eager argument evaluation
///   strategy
///     (e.g. see Clippy's `expect_fun_call` lint).
///
/// This is sort of like a static [`std::borrow::Cow`],
///   in the sense that it can hold a reference or owned value;
///     a thunk can return a value of any type or lifetime
///       (including owned),
///       whereas non-thunks require static references.
/// _The burden is on the trait implementation to enforce these
///   constraints._
pub trait ThunkOrStaticRef<T: ?Sized> {
    type Output: AsRef<T>;

    /// Force the [`Thunk`] or return the static reference,
    ///   depending on the type of [`Self`].
    fn call(self) -> Self::Output;
}

impl<T: ?Sized, R: AsRef<T>, F: Fn() -> R> ThunkOrStaticRef<T> for F {
    type Output = R;

    fn call(self) -> R {
        self()
    }
}

impl ThunkOrStaticRef<str> for &'static str {
    type Output = &'static str;

    fn call(self) -> &'static str {
        self
    }
}
