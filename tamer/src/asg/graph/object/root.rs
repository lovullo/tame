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

use super::{prelude::*, Ident, Pkg};
use std::fmt::Display;

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
        // Packages are always rooted since they are the toplevel
        //   collection.
        tree Pkg,

        // Identifiers may optionally be explicitly rooted in contexts where
        //   the system cares only about particular identifiers and their
        //   dependencies,
        //     with `Pkg`s being only incidental.
        // For example,
        //   `tameld` only links objects that are reachable from identifiers
        //   in the return map.
        cross Ident,
    }
}

impl ObjectIndex<Root> {
    /// Root an identifier in the graph without a parent [`Pkg`].
    ///
    /// An identifier ought to be rooted by a package that defines it;
    ///   this is intended as a legacy operation for `tameld` that will be
    ///   removed in the future.
    pub fn root_ident(
        &self,
        asg: &mut Asg,
        oi: ObjectIndex<Ident>,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        oi.add_edge_from(asg, *self, None)
    }
}
