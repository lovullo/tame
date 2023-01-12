// Functional primitives and conveniences
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
/// In an abuse of terminology,
///   this functor is polymorphic over the entire trait,
///     rather than just the definition of `map`,
///   allowing implementations to provide multiple specialized `map`s
///     without having to define individual `map_*` methods.
/// Rust will often be able to infer the proper types and invoke the
///   intended `map` function within a given context,
///     but methods may still be explicitly disambiguated using the
///     turbofish notation if this is too confusing in context.
/// Strictly speaking,
///   if a functor requires a monomorphic function
///     (so `T = U`),
///   then it's not really a functor.
/// We'll refer to these structures informally as monomorphic functors,
///   since they provide the same type of API as a functor,
///   but cannot change the underlying type.
///
/// This trait also provides a number of convenience methods that can be
///   implemented in terms of [`Functor::map`].
///
/// Why a primitive `map` instead of `fmap`?
/// ========================================
/// One of the methods of this trait is [`Functor::fmap`],
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
pub trait Functor<T, U = T>: Sized {
    /// Type of object resulting from [`Functor::map`] operation.
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

    /// Curried form of [`Functor::map`],
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
    ///     taking advantage of [`Functor`]'s trait-level polymorphism.
    fn overwrite(self, value: U) -> Self::Target {
        self.map(|_| value)
    }

    /// Curried form of [`Functor::overwrite`],
    ///   with arguments reversed.
    fn foverwrite(value: U) -> impl FnOnce(Self) -> Self::Target {
        move |x| x.overwrite(value)
    }
}

impl<T, U> Functor<T, U> for Option<T> {
    type Target = Option<U>;

    fn map(self, f: impl FnOnce(T) -> U) -> <Self as Functor<T, U>>::Target {
        Option::map(self, f)
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
