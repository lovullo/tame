// TAME in Rust (TAMER)
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

//! An incremental rewrite of TAME in Rust.

// Constant functions are still in their infancy as of the time of writing
//   (October 2021).
// These two features are used by [`sym::prefill::st_as_sym`] to provide
//   polymorphic symbol types despite Rust's lack of support for constant
//   trait methods.
// See that function for more information.
#![feature(const_fn_trait_bound)]
#![feature(const_transmute_copy)]
// We build docs for private items
#![allow(rustdoc::private_intra_doc_links)]

pub mod global;

#[macro_use]
extern crate static_assertions;
#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[cfg(feature = "wip-frontends")]
pub mod frontend;

pub mod convert;
pub mod fs;
#[macro_use]
pub mod ir;
pub mod ld;
pub mod obj;
pub mod span;
pub mod sym;
pub mod tpwrap;

#[cfg(test)]
pub mod test;
