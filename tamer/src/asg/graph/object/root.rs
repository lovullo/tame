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
use crate::{
    asg::{air::EnvScopeKind, IdentKind, Source},
    parse::util::SPair,
    span::Span,
};
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
    pub fn root_ident(&self, asg: &mut Asg, name: SPair) -> ObjectIndex<Ident> {
        asg.lookup_or_missing(*self, name)
            .add_edge_from(asg, *self, None)
    }

    /// Attempt to retrieve an indexed [`Ident`] owned by `self`.
    ///
    /// See [`Self::root_ident`].
    pub fn lookup_or_missing(
        &self,
        asg: &mut Asg,
        name: SPair,
    ) -> ObjectIndex<Ident> {
        asg.lookup_or_missing(*self, name)
    }

    /// Declare a concrete identifier.
    ///
    /// See [`ObjectIndex::<Ident>::declare`] for more information.
    pub fn declare(
        &self,
        asg: &mut Asg,
        name: SPair,
        kind: IdentKind,
        src: Source,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        self.lookup_or_missing(asg, name)
            .declare(asg, name, kind, src)
    }

    /// Attempt to declare a package with the given canonical name.
    ///
    /// A canonical package name is a path relative to the project root.
    ///
    /// This assignment will fail if the package either already has a name
    ///   or if a package of the same name has already been declared.
    pub fn create_pkg(
        self,
        asg: &mut Asg,
        start: Span,
        name: SPair,
    ) -> Result<ObjectIndex<Pkg>, AsgError> {
        let oi_pkg = asg.create(Pkg::new_canonical(start, name)?);

        // TODO: We shouldn't be responsible for this
        let eoi_pkg = EnvScopeKind::Visible(oi_pkg);

        asg.try_index(self, name, eoi_pkg).map_err(|oi_prev| {
            let prev = oi_prev.resolve(asg);

            // unwrap note: a canonical name must exist for this error to
            //   have been thrown,
            //     but this will at least not blow up if something really
            //     odd happens.
            AsgError::PkgRedeclare(prev.canonical_name(), name)
        })?;

        Ok(oi_pkg.root(asg))
    }
}
