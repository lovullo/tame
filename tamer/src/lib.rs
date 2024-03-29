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
//!
//! The [`pipeline`] module contains declarative definitions and
//!   documentation for TAMER's _lowering pipelines_;
//!     you should start there if you are looking for how a particular
//!     component of parsing or code generation is integrated.

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
#![feature(nonzero_ops)]
// Enabled for qualified paths in `matches!`.
#![feature(more_qualified_paths)]
// Collecting interators into existing objects.
// Can be done manually in a more verbose way.
#![feature(iter_collect_into)]
// Concise and descriptive.
// Can be done manually in a more verbose way.
#![feature(str_split_remainder)]
// Concise and descriptive.
// Can be done manually in a more verbose way.
#![feature(iter_intersperse)]
// Used for const params like `&'static str` in `crate::fmt`.
// If this is not stabalized,
//   then we can do without by changing the abstraction;
//     this is largely experimentation to see if it's useful.
// See `rust-toolchain.toml` for information on how this blocks more recent
//   nightly versions as of 2023-06.
#![allow(incomplete_features)]
#![feature(adt_const_params)]
// Used for traits returning functions,
//   such as those in `crate::f`.
// Our use of this feature is fairly basic;
//   should it become too complex then we should re-evaluate what we ought
//   to be doing relative to the status of this feature.
#![feature(return_position_impl_trait_in_trait)]
// Added for use with `rustfmt::skip`,
//   so that we can ignore formatting more precisely.
#![feature(stmt_expr_attributes)]
// Allows using `impl Trait` for associated type bounds instead of having to
//   extract it into a more verbose `where` clause.
// This is not necessary,
//   and may not even be desirable,
//   but it's a nice option to have if `impl` would otherwise be used.
#![feature(associated_type_bounds)]
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
// Sometimes being explicit about lifetimes,
//   even if it's unnecessary,
//   can help a human to understand what bounds are in play,
//     which are hidden when they're elided.
// Sometimes doing such a thing is a bad idea and introduces complexity.
// We need to use our judgment.
// Further,
//   Clippy sometimes recommends eliding named bounds which does not
//   compile,
//     but then accepts introducing an anonymous lifetime bound (`'_`),
//       which can be inscrutable if you are not very familiar with Rust's
//       borrow checker.
#![allow(clippy::needless_lifetimes)]
// Uh oh.  Trait specialization, you say?
// This deserves its own section.
//
// Rust has two trait specialization feature flags:
//   - min_specialization; and
//   - specialization.
//
// Both are unstable,
//   but _the latter has soundness holes when it comes to lifetimes_.
// A viable subset of `specialization` was introduced for use in the Rust
//   compiler itself,
//     dubbed `min_specialization`.
// That hopefully-not-unsound subset is what has been adopted here.
//
// Here's the problem:
//   TAMER makes _heavy_ use of the type system for various guarantees,
//     operating as proofs.
// This static information means that we're able to determine a lot of
//   behavior statically.
// However,
//   we also have to support various operations dynamically,
//     and marry to the two together.
// The best example of this at the time of writing is AIR,
//   which uses static types for graph construction and manipulation
//   whenever it can,
//     but sometimes has to rely on runtime information to determine which
//     types are applicable.
// In that case,
//   we have to match on runtime type information and branch into various
//   static paths based on that information.
//
// Furthermore,
//   this type information often exhibits specialized behavior for certain
//   cases,
//     and fallback behavior for all others.
//
// This conversion back and fourth in various direction results in either a
//   maintenance burden
//     (e.g. any time new types or variants are introduced,
//       branching code has to be manually updated),
//     or complex macros that attempt to generate that code.
// It's all boilerplate,
//   and it's messy.
//
// Trait specialization allows for a simple and declarative approach to
//   solving these problems without all of the boilerplate;
//     the type system can be used to match on relevant types and will fall
//     back to specialization in situations where we are not concerned with
//     other types.
// In situations where we _do_ want to comprehensively match all types,
//   we still have that option in the traditional way.
//
// TAMER will begin to slowly and carefully utilize `min_specialization` in
//   isolated areas to experiment with the stability and soundness of the
//   system.
// You can search for its uses by searching for `default fn`.
//
// If it is decided to _not_ utilize this feature in the future,
//   then specialization must be replaced with burdensome branching code as
//   mentioned above.
// It is doable without sacrificing type safety,
//   but it makes many changes very time-consuming and therefore very
//   expensive.
//
// (At the time of writing,
//    there is no clear path to stabalization of this feature.)
#![feature(min_specialization)]

pub mod global;

#[macro_use]
extern crate static_assertions;

#[macro_use]
pub mod f;
#[macro_use]
pub mod diagnose;
#[macro_use]
pub mod xir;

pub mod asg;
pub mod convert;
pub mod fmt;
pub mod fs;
pub mod iter;
pub mod ld;
pub mod nir;
pub mod num;
pub mod obj;
pub mod parse;
pub mod pipeline;
pub mod span;
pub mod sym;

#[cfg(test)]
pub mod test;
