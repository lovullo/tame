// TAME in Rust (TAMER)
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
// Enabled for qualified paths in `matches!`.
#![feature(more_qualified_paths)]
// Used for const params like `&'static str` in `crate::fmt`.
// If this is not stabalized,
//   then we can do without by changing the abstraction;
//     this is largely experimentation to see if it's useful.
#![allow(incomplete_features)]
#![feature(adt_const_params)]
// Used for traits returning functions,
//   such as those in `crate::f`.
// Our use of this feature is fairly basic;
//   should it become too complex then we should re-evaluate what we ought
//   to be doing relative to the status of this feature.
#![feature(return_position_impl_trait_in_trait)]
// We build docs for private items.
#![allow(rustdoc::private_intra_doc_links)]
// For sym::prefill recursive macro `static_symbols!`.
#![recursion_limit = "512"]
//
// Clippy Lints
// ============
// This section contains rationale for deviating from standard lints.
// This reasoning applies to TAMER and may not be appropriate for other
//   projects,
//     or even other teams.
//
// These are presented in no particular order,
//   but if you do rearrange them,
//   be mindful of the comments that may reference preceding lints.
//
// Choosing not to inline format args sometimes adds to the clarity of the
//   format string by emphasizing structure more concisely.
// Use your judgment.
#![allow(clippy::uninlined_format_args)]
// The rationale for this lint is that it may catch accidental semicolons,
//   but the type system is plenty sufficient to catch unit types that were
//   unintended.
#![allow(clippy::unit_arg)]
// Use your judgment;
//   a `match` may be more clear within a given context.
// Or may simply be personal preference.
#![allow(clippy::single_match)]
// Same rationale as the previous,
//   but additionally this clearly scopes pattern bindings to an inner
//   block,
//     which is not the case with a sibling `let` binding.
// This pattern was originally taken from `rustc` itself.
#![allow(clippy::match_single_binding)]
// This lint also seems to apply when dereferencing a double reference,
//   for which the use of `cloned` would be far more confusing.
#![allow(clippy::map_clone)]
// Perhaps `is_empty` does not make sense for that particular trait/impl?
// We don't need a linter to guide these abstractions;
//   an `is-empty` method will be added if it is needed and actually
//   utilized.
#![allow(clippy::len_without_is_empty)]
// This is another case of a linter trying to guide abstractions.
// `Default` will be implemented if it both makes sense and is needed,
//   not needlessly,
//   as TAMER is not a library and its uses are statically known.
// Furthermore,
//   `Default` is sometimes explicitly omitted to disallow automatic
//   construction in various contexts.
#![allow(clippy::new_without_default)]
// When surrounding code uses `write!`,
//   switching to `writeln!` for the last line adds an inconsistency that
//   can make the code less clear,
//     or possibly even introduce bugs by having the reader miss the change
//     in pattern.
// `writeln!` also gives the impression that it's writing a line,
//   when in actuality it may simply be appending to a partially-written
//   line,
//     making it feel like an inappropriate abstraction.
// Choose the abstraction that's most appropriate within a given context.
#![allow(clippy::write_with_newline)]
// Calling this "obfuscation" is hyperbole.
// Furthermore,
//   `if` statements are expanded by `rustfmt` into something with a
//   significantly larger footprint than this form,
//     so this lint does _not_ suggest a suitable replacement.
#![allow(clippy::obfuscated_if_else)]

pub mod global;

#[macro_use]
extern crate static_assertions;

#[macro_use]
pub mod xir;

pub mod asg;
pub mod convert;
pub mod diagnose;
pub mod f;
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
