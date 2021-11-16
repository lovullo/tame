// Collecting and aggregating iterator values
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

//! A [`FromIterator`] that can fail,
//!   along with a [`try_collect`](TryCollect::try_collect) that is
//!   analogous to [`collect`](Iterator::collect).
//!
//! Rust's built-in [`Iterator::collect`] is a powerful abstraction that is
//!   able to remove a considerable amount of boilerplate and explicit
//!   composition,
//!     but it cannot handle failures.
//! These traits attempt to stay as faithful as possible to the Rust
//!   standard library while permitting (and expecting) failures to occur.
//!
//! See the [parent module](super) for more information.

use super::TrippableIterator;
use std::convert::Infallible;

/// Conversion from an [`Iterator`] that may fail in a controlled way.
///
/// If the conversion cannot fail,
///   use Rust's built-in [`FromIterator`] instead;
///     [`TryFromIterator`] is implemented automatically anything
///     implementing [`FromIterator`].
pub trait TryFromIterator<A>: Sized {
    /// Conversation failure.
    type Error;

    /// Attempts to create a value from an iterator,
    ///   failing in a controlled manner.
    ///
    /// This is generally called through [`TryCollect::try_collect`].
    ///
    /// See the [module-level documentation](super) for more information.
    fn try_from_iter<I: IntoIterator<Item = A>>(
        iter: I,
    ) -> Result<Self, Self::Error>;
}

impl<A, T> TryFromIterator<A> for T
where
    T: FromIterator<A>,
{
    type Error = Infallible;

    /// Create a value from an iterator.
    ///
    /// This is an automatic implementation for anything implementing
    ///   [`FromIterator`] and cannot fail;
    ///     it directly invokes [`FromIterator::from_iter`].
    fn try_from_iter<I: IntoIterator<Item = A>>(
        iter: I,
    ) -> Result<Self, Infallible> {
        Ok(FromIterator::from_iter(iter))
    }
}

/// Augment [`Iterator`]s with a [`try_collect`](TryCollect::try_collect)
///   method,
///     which is analogous to [`Iterator::collect`].
///
/// Where [`Iterator::collect`] uses [`FromIterator`],
///   `try_collect` uses [`TryFromIterator`].
pub trait TryCollect: Iterator + Sized {
    /// Attempts to transform an iterator into a collection,
    ///   which may fail in a controlled manner under certain
    ///   circumstances.
    ///
    /// If this operation is infailliable for all relevant types,
    ///   use [`Iterator::collect`] instead.
    ///
    /// See the [module-level documentation](super) for more information.
    #[inline]
    fn try_collect<B: TryFromIterator<Self::Item>>(
        self,
    ) -> Result<B, B::Error> {
        TryFromIterator::try_from_iter(self)
    }

    /// Attempts to transform a [`Result<T, E>`](Result) iterator into a
    ///   collection of [`TryFromIterator<T>`](TryFromIterator),
    ///     which may fail in a controlled manner on either the source
    ///     iterator or the transformation.
    ///
    /// Ideally, this method would not have to exist.
    /// However,
    ///   at the time of writing,
    ///   `iter.while_ok(TryCollect::try_collect)` fails to compile,
    ///     requiring instead a more verbose closure.
    /// This function exists purely to improve readability and reduce
    ///   boilerplate,
    ///     but at the expense of a somewhat confusing return type.
    ///
    /// The outer [`Result`] represents the the source iterator,
    ///   in the sense of [`TrippableIterator::while_ok`].
    /// The inner [`Result`] represents the result of the
    ///   [`try_collect`](TryCollect::try_collect) operation.
    /// Since these two errors types are expected to be unrelated to
    ///   one-another
    ///     (after all, [`TrippableIterator`] exists precisely to decouple
    ///       the downstream iterators from upstream failures),
    ///     there is no obvious and correct conversion,
    ///       and so it is left up to the caller.
    /// Often,
    ///   this call is simply suffixed with `??`,
    ///     leaving the containing function's return type to manage the
    ///     conversion via [`Into`].
    #[inline]
    fn try_collect_ok<T, E, B: TryFromIterator<T>>(
        mut self,
    ) -> Result<Result<B, B::Error>, E>
    where
        Self: Iterator<Item = Result<T, E>>,
    {
        // At the time of writing,
        //   `self.while_ok(TryCollect::try_collect)` does not compile,
        //     stating that `FnOnce` is "not general enough" and appearing
        //     immune to any attempts to generalize lifetimes using
        //     higher-rank trait bounds (HRTBs).
        self.while_ok(|iter| iter.try_collect())
    }
}

impl<I: Iterator> TryCollect for I {}
