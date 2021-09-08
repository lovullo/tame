// Additional type conversion abstractions
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

//! TAMER opinionated traits for conversion between types.
//!
//! _This should not be used for error reporting to users!_
//! Panics in TAMER represent internal compiler errors.
//!
//! This module introduces two new traits:
//!
//!   - [`ExpectFrom`] implemented for all [`TryFrom`]; and
//!   - [`ExpectInto`] implemented for all [`TryInto`].
//!
//! These traits provide two new methods:
//!
//!   - `expect_{from,into}`, which is equivalent to calling
//!     [`Result::expect`] on the result; and
//!   - `unwrap_{from,into}`, which is equivalent to calling
//!     [`Result::unwrap`] on the result.
//!
//! These two traits are intended to eliminate boilerplate in situations
//!   where _it is expected that these will never fail_.
//! There may be better options;
//!   these are most useful in writing tests.

use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;

/// Safe type conversion that may panic under some circumstances.
///
/// This is implemented for every type implementing [`TryFrom`] and is
///   intended to be used in situations where we are certain that failure
///   cannot occur,
///     or where failure would otherwise be folowed by [`Result::expect`] or
///     [`Result::unwrap`].
///
/// See the [module-level documentation](self) for more information.
pub trait ExpectFrom<T>: TryFrom<T>
where
    <Self as TryFrom<T>>::Error: Debug,
{
    fn expect_from(value: T, msg: &str) -> T {
        T::try_from(value).expect(msg)
    }

    fn unwrap_from(value: T) -> T {
        T::try_from(value).unwrap()
    }
}

impl<T, U> ExpectFrom<U> for T
where
    T: TryFrom<U>,
    <T as TryFrom<U>>::Error: Debug,
{
}

/// An attempted conversion that consumes `self`,
///   which may or may not be expensive and will panic on failure.
///
/// This is implemented for every type implementing [`TryInto`] and is
///   intended to be used in situations where we are certain that failure
///   cannot occur,
///     or where failure would otherwise be folowed by [`Result::expect`] or
///     [`Result::unwrap`].
///
/// See the [module-level documentation](self) for more information.
pub trait ExpectInto<T>: TryInto<T>
where
    <Self as TryInto<T>>::Error: Debug,
{
    fn expect_into(self, msg: &str) -> T {
        self.try_into().expect(msg)
    }

    fn unwrap_into(self) -> T {
        self.try_into().unwrap()
    }
}

impl<T, U> ExpectInto<U> for T
where
    T: TryInto<U>,
    <T as TryInto<U>>::Error: Debug,
{
}
