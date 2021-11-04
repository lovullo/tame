// Intermediate representations (IRs)
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

//! Intermediate representations for TAME programs.
//!
//! _This module is being removed in favor of more specific locations for
//! the contained IRs._
//!
//! [Intermediate representations][ir] (IRs) are data structures used to
//!   represent source data in a manner most suitable for a particular phase
//!   of compilation.
//! A single IR may be used by multiple compilation phases,
//!   or by multiple systems (e.g. various compilers or [linkers][]).
//!
//! [ir]: https://en.wikipedia.org/wiki/Intermediate_representation
//! [linkers]: crate::ld

pub mod asg;
#[macro_use]
pub mod xir;
