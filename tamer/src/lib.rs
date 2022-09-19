// TAME in Rust (TAMER)
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

//! An incremental rewrite of TAME in Rust.
//!
//! There are two entry points to this system:
//!
//!   - [`tamec`](../tamec), the TAME compiler; and
//!   - [`tameld`](../tameld), the TAME linker.

// Constant functions are still in their infancy as of the time of writing
//   (October 2021).
// These this feature is used by [`sym::prefill::st_as_sym`] to provide
//   polymorphic symbol types despite Rust's lack of support for constant
//   trait methods.
// See that function for more information.
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
// For `Try` and `FromResidual`,
//   allowing us to write our own `?`-compatible types.
#![feature(try_trait_v2)]
// Used primarily for convenience,
//   rather than having to create type constructors as type aliases that are
//   not associated with a trait.
// However,
//   this also allows for the associated type default to be overridden by
//   the implementer,
//     in which case this feature's only substitute is a type parameter.
#![feature(associated_type_defaults)]
// Convenience features that are easily replaced if not stabalized.
#![feature(nonzero_min_max)]
#![feature(nonzero_ops)]
// Note: this is the first time TAMER was hit by a change in an unstable
//   feature,
//     when `log10` et al. were renamed to `ilog10` et al:
//       <https://github.com/rust-lang/rust/pull/100332>
#![feature(int_log)]
// Enabled for qualified paths in `matches!`.
#![feature(more_qualified_paths)]
// Used for const params like `&'static str` in `crate::fmt`.
// If this is not stabalized,
//   then we can do without by changing the abstraction;
//     this is largely experimentation to see if it's useful.
#![allow(incomplete_features)]
#![feature(adt_const_params)]
// We build docs for private items.
#![allow(rustdoc::private_intra_doc_links)]
// For sym::prefill recursive macro `static_symbols!`.
#![recursion_limit = "512"]

pub mod global;

#[macro_use]
extern crate static_assertions;

#[macro_use]
pub mod xir;

pub mod asg;
pub mod convert;
pub mod diagnose;
pub mod fmt;
pub mod fs;
pub mod iter;
pub mod ld;
pub mod nir;
pub mod num;
pub mod obj;
pub mod parse;
pub mod span;
pub mod sym;

#[cfg(test)]
pub mod test;
