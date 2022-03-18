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
// This is used to unwrap const Option results rather than providing
//   panicing alternatives.
#![feature(const_option)]
// Trait aliases are convenient for reducing verbosity in situations where
//   type aliases cannot be used.
// To remove this feature if it is not stabalized,
//   simply replace each alias reference with its definition,
//   or possibly write a trait with a `Self` bound.
#![feature(trait_alias)]
// Can be replaced with `assert!(matches!(...))`,
//   but at a loss of a better error message.
#![feature(assert_matches)]
// Simplifies creating `Option` default values.
// To remove this feature,
//   this can be done more verbosely in the usual way,
//   or we can write our own version.
#![feature(option_get_or_insert_default)]
// This allows for e.g. `parse::<N>(foo)`,
//   where `fn parse<const N: T>(foo: impl Trait)`.
// Rust devs wanted more time for public testing as of the time of writing
//   (March 2022).
// We _could_ do without,
//   but this provides a nicer API.
#![feature(explicit_generic_args_with_impl_trait)]
// This simply removes a boilerplate `Default` impl;
//   we can do without if this does not get finalized.
#![feature(derive_default_enum)]
// We build docs for private items.
#![allow(rustdoc::private_intra_doc_links)]

pub mod global;

#[macro_use]
extern crate static_assertions;
#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[cfg(feature = "wip-frontends")]
pub mod frontend;

#[macro_use]
pub mod xir;

pub mod asg;
pub mod convert;
pub mod fs;
pub mod iter;
pub mod ld;
pub mod obj;
pub mod parse;
pub mod span;
pub mod sym;
pub mod tpwrap;

#[cfg(test)]
pub mod test;
