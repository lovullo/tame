// Objects represented on ASG
//
//  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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

//! Objects represented by the ASG.
//!
//! _This is a private module.
//!  See [`super`] for available exports._

use super::ident::IdentKind;
use crate::sym::Symbol;

/// Type of object.
///
/// These types represent object states:
///
/// ```text
/// ((Empty)) -> (Extern) -> ((Ident)) -> ((IdentFragment)).
///     \                        ^
///      \                      /
///       `--------------------`
/// ```
///
/// The [`Empty`][Object::Empty] state is never directly accessable
///   through [`Asg`][super::Asg]'s public API,
///     as it represents the absence of an object at that node within the
///     ASG.
///
/// TODO: Source location (span; see Rustc).
#[derive(Debug, PartialEq)]
pub enum Object<'i> {
    /// A resolved identifier.
    ///
    /// This represents an identifier that has been declared with certain
    ///   type information.
    Ident(&'i Symbol<'i>, IdentKind),

    /// An identifier that has not yet been resolved.
    ///
    /// Externs are upgraded to [`Object::Ident`] once an identifier of
    ///   the same name is loaded.
    /// It is an error if the loaded identifier does not have a compatible
    ///   [`IdentKind`].
    Extern(&'i Symbol<'i>, IdentKind),

    /// Identifier with associated text.
    ///
    /// Code fragments are portions of the target language associated with
    ///   an identifier.
    /// They are produced by the compiler and it is the job of the
    ///   [linker][crate::ld] to put them into the correct order for the
    ///   final executable.
    IdentFragment(&'i Symbol<'i>, IdentKind, FragmentText),

    /// The empty node (default value for indexer).
    ///
    /// This is not a valid state accessible via [`Asg`][super::Asg].
    Empty,
}

/// Compiled fragment for identifier.
///
/// This represents the text associated with an identifier.
pub type FragmentText = String;
