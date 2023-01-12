// Tripping iterator
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

//! An iterator that wraps [`ResultIterator`] and trips on the first
//!   [`Err`].
//!
//! This acts as a circuit breaker---when
//!   an error occurs,
//!     the iterator trips to prevent further damage,
//!     halting downstream processing while providing the opportunity to
//!       inspect and act upon the error.
//!
//! This iterator simplifies the management of [`ResultIterator`]s and
//!   allows downstram APIs of those iterators to remain simple without
//!   having to concern themselves with error conditions from a source
//!   iterator.
//!
//! _Support for immediate error recovery has not yet been added._
//! It will be once it is needed.
//!
//! See the [parent module](super) and [`TripIter`] for more information.

use super::ResultIterator;
use std::marker::PhantomData;

/// An iterator that trips when encountering an [`Err`] on an underling
///   iterator.
///
/// This iterator can be constructed using one of
///
///   1. [`with_iter_while_ok`] to retain ownership over the source
///        iterator; or
///   2. [`into_iter_while_ok`] for a more ergonomic API at the cost of
///        ceding ownership over the source iterator.
///
/// See those two functions and the [parent module](super) for more
///   information and examples.
pub struct TripIter<'a, I: ResultIterator<T, EI>, T, EI> {
    /// Iterator that will be unwrapped while its item is [`Ok`].
    inner: &'a mut I,

    /// The current state of the iterator,
    ///   including the most recently encountered error.
    ///
    /// If there is an error,
    ///   then we have been tripped and will return [`None`] until the error
    ///   condition has been resolved.
    state: Result<(), EI>,

    /// Our [`Iterator`] implementation will yield `T`,
    ///   but we don't store it.
    _phantom: PhantomData<T>,
}

impl<'a, I: ResultIterator<T, EI>, T, EI> TripIter<'a, I, T, EI> {
    /// Given a mutable reference to a
    ///   [`ResultIterator<T, E>`](ResultIterator),
    ///     yield a [`TripIter`] that yields the inner `T` value while the
    ///     iterator yields an [`Ok`] item.
    ///
    /// See the public [`with_iter_while_ok`] function for more
    ///   information.
    #[inline]
    fn with_iter_while_ok<F, U, E>(iter: &'a mut I, f: F) -> Result<U, E>
    where
        F: FnOnce(&mut Self) -> Result<U, E>,
        EI: Into<E>,
    {
        let mut biter = Self {
            inner: iter,
            state: Ok(()),
            _phantom: Default::default(),
        };

        // The TripIter will be available only for the duration of this
        //   call...
        let fret = f(&mut biter);

        // ...which ensures that we have the opportunity,
        //   after the caller is done with it,
        //   to check to see if we have tripped and force the caller to
        //   consider the error.
        biter.state.map_err(EI::into).and(fret)
    }

    /// Whether the iterator has been tripped by an [`Err`] on the
    ///   underlying iterator.
    ///
    /// Until a recovery mechanism is provided,
    ///   the [`Err`] is available through one of [`with_iter_while_ok`] or
    ///   [`into_iter_while_ok`].
    #[inline]
    pub fn is_tripped(&self) -> bool {
        self.state.is_err()
    }
}

impl<'a, I, T, EI> Iterator for TripIter<'a, I, T, EI>
where
    I: Iterator<Item = Result<T, EI>>,
{
    type Item = T;

    /// Unwrap and return the next [`Ok`] result from the underlying
    ///   iterator,
    ///     otherwise [`None`].
    ///
    /// Once an [`Err`] is encountered,
    ///   this iterator trips and will continue to return [`None`] until the
    ///   error state is resolved.
    /// See [`with_iter_while_ok`] for more information.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_tripped() {
            return None;
        }

        match self.inner.next() {
            Some(Ok(value)) => Some(value),
            Some(Err(err)) => {
                self.state = Err(err);
                None
            }
            None => None,
        }
    }
}

/// Given a mutable reference to a
///   [`ResultIterator<T, EI>`](ResultIterator),
///     yield a [`TripIter`] that yields the inner `T` value while the
///     iterator yields an [`Ok`] item.
///
/// Once an [`Err`] is encountered,
///   [`TripIter`] trips,
///   yielding [`None`] until the error condition is resolved.
/// This allows for a recovery mechanism that resumes computation,
///   provided that the system expects [`TripIter`] to be resumable.
///
/// The [`TripIter`] is provided as an argument to a callback `f`
///   and is valid only for its duration.
/// This allows us to return either the most recently encountered [`Err`],
///   otherwise the return value of `f`,
///   ensuring that the error causing the trip will not be lost.
///
/// The callback function `f` must return a [`Result`] whose error type is
///   [`Into<E>`](Into);
///     this allows [`Result`]s to be continuously flattened into a
///     consistent type,
///       which is especially useful when nesting [`TripIter`].
/// This puts the error type in control of the caller via the callback.
/// Since nested [`TripIter`] using this function must share the same error
///   type `E`,
///     this operation is monadic.
///
/// This function accepts a mutable reference to the underlying iterator,
///   allowing the caller to retain ownership for further processing after
///   iteration completes.
/// If this is not needed
///   (e.g. all processing will be performed in `f`),
///   [`into_iter_while_ok`] may provide a more ergonomic API at the cost of
///     ownership.
///
/// ```
/// use tamer::iter::with_iter_while_ok;
///
/// let mut values = [Ok(0), Err("trip"), Ok(1)].into_iter();
///
/// let result = with_iter_while_ok(&mut values, |iter| {
///   // First is `Ok`, so it yields.
///   assert_eq!(Some(0), iter.next());
///   assert!(!iter.is_tripped());
///
///   // But the next is an `Err`, so it trips and returns `None`
///   // from that point onward.
///   assert_eq!(None, iter.next());
///   assert_eq!(None, iter.next());
///   assert!(iter.is_tripped());
///
///   Ok(())
/// });
///
/// // The error that caused the trip is returned.
/// assert_eq!(Err("trip"), result);
///
/// // We still have access to the iterator where it left off.
/// assert_eq!(Some(Ok(1)), values.next());
/// ```
///
/// See the [parent module](super) for more information and examples.
#[inline]
pub fn with_iter_while_ok<'a, I, T, U, E, EI>(
    from: &'a mut I,
    f: impl FnOnce(&mut TripIter<'a, I, T, EI>) -> Result<U, E>,
) -> Result<U, E>
where
    I: ResultIterator<T, EI>,
    EI: Into<E>,
{
    TripIter::with_iter_while_ok(from, f)
}

/// Given an object capable of being converted into a
///   [`ResultIterator<T, EI>`](ResultIterator),
///     yield a [`TripIter`] that yields the inner `T` value while the
///     iterator yields an [`Ok`] item.
///
/// This is a more ergonomic form of [`with_iter_while_ok`] if ownership
///   over the `from` iterator can be ceded;
///     see that function for more information.
///
/// ```
/// use tamer::iter::into_iter_while_ok;
///
/// let values = [Ok(0), Err("trip"), Ok(1)];
///
/// let result = into_iter_while_ok(values, |iter| {
///   // First is `Ok`, so it yields.
///   assert_eq!(Some(0), iter.next());
///   assert!(!iter.is_tripped());
///
///   // But the next is an `Err`, so it trips and returns `None`
///   // from that point onward.
///   assert_eq!(None, iter.next());
///   assert_eq!(None, iter.next());
///   assert!(iter.is_tripped());
///
///   Ok(())
/// });
///
/// // The error that caused the trip is returned.
/// assert_eq!(Err("trip"), result);
///
/// // But we cannot access the remainder of the iterator, having
/// // lost ownership.  See `with_iter_while_ok` if this is needed.
/// ```
///
/// See the [parent module](super) for more information and examples.
#[inline]
pub fn into_iter_while_ok<I, T, U, E, EI>(
    from: I,
    f: impl FnOnce(&mut TripIter<I::IntoIter, T, EI>) -> Result<U, E>,
) -> Result<U, E>
where
    I: IntoIterator<Item = Result<T, EI>>,
    EI: Into<E>,
{
    with_iter_while_ok(&mut from.into_iter(), f)
}

/// An [`Iterator`] supporting the [`while_ok`](TrippableIterator::while_ok)
///   and [`into_while_ok`](TrippableIterator::into_while_ok) trip
///   operations.
///
/// For more information,
///   see the [module-level documentation](self).
pub trait TrippableIterator<T, EI>
where
    Self: Iterator<Item = Result<T, EI>> + Sized,
{
    /// Given a mutable reference to a
    ///   [`ResultIterator<T, E>`](ResultIterator),
    ///     yield a [`TripIter`] that yields the inner `T` value while the
    ///     iterator yields an [`Ok`] item.
    ///
    /// For more information,
    ///   see [`with_iter_while_ok`] and the
    ///   [module-level documentation](super).
    #[inline]
    fn while_ok<F, U, E>(&mut self, f: F) -> Result<U, E>
    where
        F: FnOnce(&mut TripIter<Self, T, EI>) -> Result<U, E>,
        EI: Into<E>,
    {
        TripIter::with_iter_while_ok(self, f)
    }

    /// Given an object capable of being converted into a
    ///   [`ResultIterator<T, E>`](ResultIterator),
    ///     yield a [`TripIter`] that yields the inner `T` value while the
    ///     iterator yields an [`Ok`] item.
    ///
    /// For more information,
    ///   see [`into_iter_while_ok`] and the [module-level documentation](super).
    #[inline]
    fn into_while_ok<F, U, E>(mut self, f: F) -> Result<U, E>
    where
        F: FnOnce(&mut TripIter<Self, T, EI>) -> Result<U, E>,
        EI: Into<E>,
    {
        self.while_ok(f)
    }
}

impl<T, E, I> TrippableIterator<T, E> for I where
    I: Iterator<Item = Result<T, E>> + Sized
{
}

#[cfg(test)]
mod test {
    use super::*;

    type TestResult<E = ()> = Result<(), E>;

    #[test]
    fn inner_none_yields_none() -> TestResult {
        let empty = Vec::<TestResult>::new();

        into_iter_while_ok(empty, |sut| {
            assert_eq!(None, sut.next());
            Ok(())
        })
    }

    #[test]
    fn ok_yields_unwrapped_values() -> TestResult {
        let value1 = "inner1";
        let value2 = "inner2";

        let iter = [Result::<_, ()>::Ok(value1), Ok(value2)];

        into_iter_while_ok(iter, |sut| {
            assert_eq!(Some(value1), sut.next());
            assert_eq!(Some(value2), sut.next());
            assert_eq!(None, sut.next());

            assert!(!sut.is_tripped());

            Ok(())
        })
    }

    #[test]
    fn err_yields_none_and_trips() {
        let value = "okay value";
        let err = "tripped";

        let result =
            into_iter_while_ok([Ok(value), Err(err), Ok(value)], |sut| {
                // The first value is `Ok`,
                //   but the second is `Err` which trips the breaker and does not
                //   yield any further elements.
                assert_eq!(Some(value), sut.next());
                assert!(!sut.is_tripped());

                // Trips here.
                assert_eq!(None, sut.next());
                assert!(sut.is_tripped());

                // Nor should it ever, while tripped.
                assert_eq!(None, sut.next());
                assert!(sut.is_tripped());

                Ok("")
            });

        assert_eq!(result, Err(err));
    }

    #[test]
    fn trip_stops_consuming_iter() {
        let mut values = [Err("trip"), Ok(1)].into_iter();

        assert_eq!(
            Err("trip"),
            with_iter_while_ok(&mut values, |sut| {
                // Try to consume everything.
                assert_eq!(None, sut.next());
                assert_eq!(None, sut.next());

                Ok(())
            }),
        );

        // Should have only consumed up to the `Err`.
        assert_eq!(Some(Ok(1)), values.next());
    }

    #[test]
    fn open_returns_f_ret_after_none() {
        let ret = "retval";
        assert_eq!(
            Result::<_, ()>::Ok(ret),
            into_iter_while_ok([Result::<_, ()>::Ok(())], |sut| {
                sut.next();
                Ok(ret)
            })
        );
    }

    #[test]
    fn tripped_ignores_f_ret_after_none() {
        let err = "tripped ignore ret";
        assert_eq!(
            Err(err),
            into_iter_while_ok([Result::<(), _>::Err(err)], |sut| {
                sut.next();
                Ok("not used")
            })
        );
    }

    // If we don't call `next`,
    //   then we haven't hit the error and so it should not trip.
    #[test]
    fn does_not_trip_before_err_is_encountered() {
        let ret = "no next";
        assert_eq!(
            Result::<_, &str>::Ok(ret),
            into_iter_while_ok(
                [Result::<(), _>::Err("will not be seen")],
                |_| { Ok("no next") }
            )
        );
    }
}
