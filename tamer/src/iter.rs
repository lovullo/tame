// TAMER iterators
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

//! Iterators for common problems.
//!
//! TAMER makes heavy use of iterators for data streams and lowering
//!   operations.
//!
//! [`Result`] Iterators
//! ====================
//! Iterators that can fail,
//!   such as XIR's
//!     [`TokenResultStream`](crate::xir::TokenResultStream),
//!   can be confounding and difficult to work with because
//!     [`Iterator::next`] wraps the [`Result`] within an [`Option`].
//! Further,
//!   if we were to directly pipe the results of one iterator to another,
//!   then downstream iterators would have to worry about handling failures
//!     of their source iterators just for the sake of propagation,
//!       forcing them to accept a [`Result`] iterator rather than an
//!       iterator producing the inner type that they are interested in.
//!
//! [`ResultIterator`] exists to slightly reduce cognitive load in code with
//!   complex types;
//!     it's simply an alias for `Iterator<Item = Result<T, E>>`.
//!
//! Managing Failures
//! -----------------
//! The [`TripIter`] iterator helps to simplify APIs and provides the
//!   opportunity for error correction by yielding the inner value of [`Ok`]
//!   on an underlying iterator,
//!     tripping if it encounters an [`Err`].
//! This is analogous to a circuit breaker,
//!   protecting downstream subsystems from faulty data.
//!
//! When a trip happens,
//!   the [`TripIter`] yields [`None`].
//! Downstream systems must determine for themselves whether receiving
//!   [`None`] should constitute an error,
//!     or whether they should wait around until the iterator can be resumed
//!     to continue processing.
//!
//! Put simply: we take an `Iterator<Item = Result<T, E>` and produce an
//!   `Iterator<Item = T>`,
//!     so that consumers of `T` needn't know or care that we could fail to
//!     produce a `T`.
//!
//! This iterator is constructed using either [`with_iter_while_ok`] or
//!   [`into_iter_while_ok`],
//!     depending on whether ownership of the source iterator needs to be
//!     retained.
//! The [`TrippableIterator`] trait also provides convenient methods
//!   directly on compatible [`Iterator`]s.
//! Each of those functions provide their own minimal examples,
//!   one of which is reproduced here:
//!
//! ```
//! use tamer::iter::TrippableIterator;
//!
//! let mut values = [Ok(0), Err("trip"), Ok(1)].into_iter();
//!
//! let result = values.while_ok(|iter| {
//!   // First is `Ok`, so it yields.  Note that the value is no longer
//!   // `Ok`, which liberates our system from handling others' errors.
//!   assert_eq!(Some(0), iter.next());
//!   assert!(!iter.is_tripped());
//!
//!   // But the next is an `Err`, so it trips and returns `None`
//!   // from that point onward.
//!   assert_eq!(None, iter.next());
//!   assert_eq!(None, iter.next());
//!   assert!(iter.is_tripped());
//! });
//!
//! // The error that caused the trip is returned.
//! assert_eq!(Err("trip"), result);
//!
//! // We still have access to the iterator where it left off.
//! assert_eq!(Some(Ok(1)), values.next());
//! ```
//!
//! Each of these functions take a callback;
//!   the [`TripIter`] is valid only for the duration of that function.
//! This allows us to know when the caller is finished with the iterator so
//!   that we can determine whether it tripped and,
//!     if so,
//!     yield the associated [`Result`] to force the caller to consider what
//!     happened.
//! This allows for out-of-band error processing---we
//!   still get to handle and propagate the error,
//!     but alleviate the system using [`TripIter`] from having to know that
//!     source failures were even possible.
//!
//! Alternative Failure Modes
//! -------------------------
//! [`TripIter`] is only needed for high-performance lazy processing of data
//!   streams where error recovery may be needed.
//! If that's not your case,
//!   Rust has built-in features that may suit your needs.
//!
//! For example,
//!   if you wish to collect iterator results but yield an [`Err`] if any of
//!   those fail,
//!     you can use [`Iterator::collect`]:
//!
//! ```
//! // Contains an `Err`, and so fails.
//! let values = [Ok(1), Err("bad")].into_iter();
//! assert_eq!(Err("bad"), values.collect::<Result<Vec<_>, _>>());
//!
//! // All values are `Ok`.
//! let values = [Ok(1), Ok(2)].into_iter();
//! assert_eq!(Ok(vec![1, 2]), values.collect::<Result<Vec<_>, ()>>());
//! ```
//!
//! But this allocates data on the heap,
//!   which is unacceptable for stream processing.
//!
//! Another option is to halt at the fist sign of trouble:
//!
//! ```
//! let values = [Ok(1), Ok(2), Err("bad")].into_iter();
//!
//! // Grab everything up to the first error.
//! assert_eq!(vec![1, 2], values.map_while(Result::ok).collect::<Vec<_>>());
//! ```
//!
//! Similar can be done with [`Iterator::take_while`].
//! But this places the responsibility of doing the right thing on the caller,
//!   and can generate unwieldy types that are difficult to store in structs
//!     (e.g. closure types that cannot be written out)
//!     without resorting to boxing,
//!       even with Rust's `impl Trait` feature.
//!
//! There are other options,
//!   but they also result in boilerplate that is handled for you by
//!   [`TripIter`].
//!
//!
//! Failliable Collecting
//! =====================
//! [`Iterator::collect`] provides a powerful interface that can remove a
//!   lot of boilerplate or manual composition,
//!     but is not designed to handle failures.
//! This becomes problematic when attempting to encapsulate failure logic
//!   within [`FromIterator`] implementations because of foreign traits.
//! [`TryFromIterator`] provides a [`Result`] return type for the collection
//!   itself.
//!
//! This is useful when an object aggregates data from an iterator in an
//!   all-or-nothing manner,
//!     or wishes to reject data it does not support.
//!
//! Consider this example where we have a set attributes from which we wish
//!   to generate a `Variable` object consisting of a name and value.
//! This object expects both of these attributes to be present,
//!   and further expects that no other attributes will be provided.
//! This effectively defines a basic schema.
//!
//! ```
//! use tamer::iter::{TryCollect, TryFromIterator};
//! use tamer::sym::{GlobalSymbolIntern, SymbolId};
//! use tamer::sym::st::raw::{L_NAME, L_VALUE};
//!
//! struct Attr(SymbolId, SymbolId);
//!
//! #[derive(Debug, PartialEq, Eq)]
//! struct Variable {
//!     name: SymbolId,
//!     value: SymbolId,
//! }
//!
//! #[derive(Debug, PartialEq, Eq)]
//! enum VariableError {
//!   UnknownAttr(SymbolId),
//!   MissingName,
//!   MissingValue,
//! }
//!
//! impl TryFromIterator<Attr> for Variable {
//!     type Error = VariableError;
//!
//!     fn try_from_iter<I: IntoIterator<Item = Attr>>(
//!         iter: I,
//!     ) -> Result<Self, Self::Error> {
//!         let mut name = None;
//!         let mut value = None;
//!
//!         for Attr(attr_name, attr_value) in iter.into_iter() {
//!             match attr_name {
//!                 L_NAME => { name.replace(attr_value); },
//!                 L_VALUE => { value.replace(attr_value); },
//!                 _ => return Err(VariableError::UnknownAttr(attr_name)),
//!             }
//!         }
//!
//!         Ok(Variable {
//!             name: name.ok_or(VariableError::MissingName)?,
//!             value: value.ok_or(VariableError::MissingValue)?,
//!         })
//!     }
//! }
//!
//! let name = "foo".intern();
//! let value = "bar".intern();
//!
//! assert_eq!(
//!   Ok(Variable { name, value }),
//!   vec![Attr(L_NAME, name), Attr(L_VALUE, value)].into_iter().try_collect(),
//! );
//!
//! assert_eq!(
//!   Err(VariableError::MissingName),
//!   vec![Attr(L_VALUE, value)].into_iter().try_collect::<Variable>(),
//! );
//! ```
//!
//! This cannot be done with [`FromIterator`] because it would require
//!   `impl FromIterator<Attr> for Result<Variable, VariableError>`,
//!     both of which are foreign traits.
//! Perhaps this restriction will be relaxed in Rust in the future,
//!   rendering [`TryFromIterator`] obsolete.
//!
//! [`TryFromIterator`] is implemented for all [`FromIterator`],
//!   allowing [`try_collect`] to accept all types that [`collect`] accepts.
//! Type inference maintains the powerful sentient illusion that [`collect`]
//!   provides:
//!
//! ```
//! use tamer::iter::TryCollect;
//!
//! // Wraps `Iterator::collect`.
//! assert_eq!(
//!   Ok(Some(vec![1, 2, 3])),
//!   vec![Some(1), Some(2), Some(3)].into_iter().try_collect()
//! );
//!
//! // Does _not_ unwrap `Result` from `Iterator::collect`.
//! assert_eq!(
//!   Ok(Err("fail")),
//!   vec![Ok(1), Err("fail")].into_iter().try_collect::<Result<Vec<_>, _>>()
//! );
//! ```
//!
//! Notably, [`try_collect`] _does not_ unwrap [`Result`] from [`collect`].
//! This makes sense from a type perspective,
//!   but may seem unintuitive at a glance.
//!
//! [`try_collect`]: TryCollect::try_collect
//! [`collect`]: Iterator::collect

mod collect;
mod trip;

/// An [`Iterator`] over [`Result`]s.
///
/// Since [`Iterator::next`] returns [`Option`],
///   this results in a return value of [`Option<Result<T, E>>`],
///   which is confusing and inconvenient to work with.
pub trait ResultIterator<T, E> = Iterator<Item = Result<T, E>>;

pub use collect::{TryCollect, TryFromIterator};
pub use trip::{
    into_iter_while_ok, with_iter_while_ok, TripIter, TrippableIterator,
};
