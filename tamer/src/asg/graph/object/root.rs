// Root of the ASG.
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

//! Root node of the ASG.

use std::fmt::Display;

use super::{prelude::*, Ident, Pkg};

#[cfg(doc)]
use super::ObjectKind;

/// A unit [`Object`] type representing the root node.
///
/// This exists for consistency with the rest of the object API,
///   and for use with [`ObjectIndex`].
#[derive(Debug, PartialEq)]
pub struct Root;

impl Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ASG root")
    }
}

object_rel! {
    /// The root of the graph by definition has no cross edges.
    Root -> {
        tree Pkg,
        tree Ident,
    }
}
