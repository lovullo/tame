// Abstract binding translation for NIR
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

//! Translation of non-interpolated abstract bindings for NIR.
//!
//! Metavariables can be used in the context of an identifier binding to
//!   produce identifiers dynamically via template expansion.
//! For example:
//!
//! ```xml
//! <classify as="@as@" yields="@yields@">
//! <!--          ^^^^          ^^^^^^^^
//!               Ident          Ident -->
//!               
//!   <match on="@match@" />
//!   <!--       ~~~~~~~
//!                Ref   -->
//! </classify>
//! ```
//!
//! In the above example,
//!   both `@as@` and `@yields@` represent identifier bindings,
//!     but the concrete names are not known until the expansion of the
//!     respective metavariables.
//! This is equivalently expressed using interpolation:
//!
//! ```xml
//! <classify as="{@as@}" yields="{@yields@}">
//! <!--          ^^^^^^          ^^^^^^^^^^ -->
//! ```
//!
//! The above interpolation would cause the generation of an abstract
//!   identifier via [`super::interp`].
//! However,
//!   because metavariables historically have an exclusive naming convention
//!   that requires a `@` prefix and suffix,
//!     the curly braces can be optionally omitted.
//! This works just fine for the `@match@` _reference_ above,
//!   because that reference is unambiguously referring to a metavariable of
//!   that name.
//!
//! But binding contexts are different,
//!   because they assert that the provided lexical symbol should serve as
//!   the name of an identifier.
//! We need an additional and explicit level of indirection,
//!   otherwise we run into the following ambiguity:
//!
//! ```xml
//! <template name="_foo_">
//!   <param name="@as@" />
//!   <!--         ^^^^
//!                  !
//!                 vvv         -->
//!   <classify as="@as@" />
//! </template>
//! ```
//!
//! In this case,
//!   if we interpret `@as@` in both contexts to be bindings,
//!   then there is a redefinition,
//!     which is an error.
//! We instead want the equivalent of this:
//!
//! ```xml
//! <template name="_foo_">
//!   <param name="@as@" desc="Name of classification" />
//!   <!--         ^^^^                               -->
//!
//!   <classify as="{@as@}" />
//!   <!--           ~~~~  -->
//! </template>
//! ```
//!
//! This creates an awkward ambiguity,
//!   because what if we instead want this?
//!
//! ```xml
//! <template name="_foo_">
//!   <param name="{@as@}" desc="Name of classification" />
//!   <!--          ~~~~                                -->
//!
//!   <classify as="{@as@}" />
//!   <!--           ~~~~  -->
//! </template>
//! ```
//!
//! This was not possible in the XSLT-based TAME.
//! TAMER instead adopts this awkward convention,
//!   implemented in this module:
//!
//!   1. Template parameters treat all symbols in binding position
//!        (`@name`)
//!        as concrete identifiers.
//!      This behavior can be overridden using curly braces to trigger
//!        interpolation.
//!
//!   2. All other bindings treat symbols matching the `@`-prefix-suffix
//!        metavariable naming convention as abstract bindings.
//!      This is equivalent to the interpolation behavior.
//!
//! This module is therefore an _optional_ syntactic feature of TAME.
//! If desired,
//!   this module could be omitted from the lowering pipeline in favor of
//!   explicitly utilizing interpolation for all abstract identifiers.
//! When utilized,
//!   this lowering operation may be intergrated into the lowering pipeline
//!   either before or after interpolation.
//!
//! TODO: Since this module acts upon a naming convention,
//!     shouldn't it also enforce it on definition so that we know that
//!     metavariables _will_ always follow that convention?
//!   At the time of writing,
//!     no part of TAMER yet enforces metavariable naming conventions.

use super::Nir;
use crate::{parse::prelude::*, sym::GlobalSymbolResolve};
use memchr::memchr;

use Nir::*;

#[derive(Debug, PartialEq, Default)]
pub struct AbstractBindTranslate;

impl Display for AbstractBindTranslate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "scanning for abstract binding via metavariable naming convention"
        )
    }
}

impl ParseState for AbstractBindTranslate {
    type Token = Nir;
    type Object = Nir;
    type Error = AbstractBindTranslateError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        match tok {
            BindIdent(name) if needs_translation(name) => {
                Transition(self).ok(BindIdentAbstract(name))
            }

            _ => Transition(self).ok(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        true
    }
}

/// Determine whether the given name requires translation into an abstract
///   identifier.
///
/// It's important to understand how the naming convention is utilized;
///   this assumes that:
///
///   1. Metavariables are `@`-prefixed.
///      The convention is actually to have a suffix too,
///        but since `@` is not permitted at the time of writing for any
///        other types of identifiers,
///          it should be the case that a prefix also implies a suffix,
///            otherwise some other portion of the system will fail.
///   2. This should not be consulted for metavariable definitions,
///        like template parameters.
fn needs_translation(name: SPair) -> bool {
    // Unlike the interpolation module which must check many symbols,
    //   we assume here that it's not necessary
    //     (and may be even be determental)
    //     for a "quick" check version given that this is invoked for
    //     bindings,
    //       and bindings will very likely introduce something new.
    // It'd be worth verifying this assumption at some point in the future,
    //   but is unlikely to make a significant different either way.
    #[rustfmt::skip]
    matches!(
        memchr(b'@', name.symbol().lookup_str().as_bytes()),
        Some(0),
    )
}

diagnostic_infallible! {
    pub enum AbstractBindTranslateError {}
}

#[cfg(test)]
mod test;
