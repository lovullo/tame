// Global constants across the entirety of TAMER
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

//! System-wide static configuration.
//!
//! This module provides a system-wide configuration.
//! Subsystems should reference these values rather than defining their own
//!   and risk incompatibilities or maintenance issues as requirements
//!   change.
//!
//! This package contains static assertions to ensure that certain sizes
//!   will play well with one-another and provide a clear direction if
//!   something goes wrong
//!     (beyond a slew of other type errors).
//!
//! By convention,
//!   import this entire module rather than individual members and reference
//!   them as `global::foo` to emphasize their nature and risk.

use std::{mem::size_of, num};

/// The initial capacity for global interners.
pub const INIT_GLOBAL_INTERNER_CAPACITY: usize = 1024;

/// A size capable of representing every interned string in a program.
pub type ProgSymSize = u32;

/// A non-zero equivalent of [`ProgSymSize`];
pub type NonZeroProgSymSize = num::NonZeroU32;

/// Size large enough to accommodate the number of bytes in a source file.
pub type SourceFileSize = u32;

/// Length of tokens derived from source files.
///
/// This size should be kept small,
///   since its principle use is as an alternative to a byte offset
///   ([`SourceFileSize`]) for struct packing.
/// It should, however, be large enough to handle comment and
///   documentation tokens.
/// Tokens that are too large can potentially be split into multiple
///   tokens.
pub type FrontendTokenLength = u16;
const_assert!(size_of::<FrontendTokenLength>() <= size_of::<SourceFileSize>());

/// A size capable of representing the union of every identifier of every
///   package used by an entire program.
///
/// This must be ≥ [`ProgSymSize`].
pub type ProgIdentSize = u32;
const_assert!(size_of::<ProgIdentSize>() >= size_of::<ProgSymSize>());

/// A size capable of representing the union of every identifier and every
///   expression of every package used by an entire program.
///
/// Note that,
///   since TAME is a metalanguage and can easily expand into a great
///     deal of code,
///   this must accommodate far more than the user's expectations
///     working within the provided level of abstraction.
///
/// This must be ≥ [`ProgSymSize`].
pub type ProgIdentExprSize = u32;
const_assert!(size_of::<ProgIdentExprSize>() >= size_of::<ProgSymSize>());
