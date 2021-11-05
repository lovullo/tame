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
pub trait TryCollect: Iterator {
    /// Attempts to transform an iterator into a collection,
    ///   which may fail in a controlled manner under certain
    ///   circumstances.
    ///
    /// If this operation is infailliable for all relevant types,
    ///   use [`Iterator::collect`] instead.
    ///
    /// See the [module-level documentation](super) for more information.
    fn try_collect<B: TryFromIterator<Self::Item>>(self)
        -> Result<B, B::Error>;
}

impl<I: Iterator> TryCollect for I {
    fn try_collect<B: TryFromIterator<Self::Item>>(
        self,
    ) -> Result<B, B::Error> {
        TryFromIterator::try_from_iter(self)
    }
}
