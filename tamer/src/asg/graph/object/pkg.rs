// Packages represented on ASG
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

//! Package object on the ASG.

use super::{prelude::*, Doc, Ident, Tpl};
use crate::{
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{util::SPair, Token},
    span::Span,
};
use std::fmt::Display;

#[cfg(doc)]
use super::ObjectKind;

mod name;

pub use name::{CanonicalName, CanonicalNameError};

#[derive(Debug, PartialEq, Eq)]
pub struct Pkg(Span, CanonicalName);

impl Pkg {
    /// Create a new package with a canonicalized name.
    ///
    /// A canonical package name is a path relative to the project root.
    pub(super) fn new_canonical<S: Into<Span>>(
        start: S,
        name: SPair,
    ) -> Result<Self, AsgError> {
        Ok(Self(start.into(), CanonicalName::from_canonical(name)?))
    }

    /// Import a package with a namespec canonicalized against a reference
    ///   parent package.
    ///
    /// The parent package should be the package containing the namespec.
    pub fn new_imported(
        Pkg(_, parent_name): &Pkg,
        namespec: SPair,
    ) -> Result<Self, AsgError> {
        Ok(Self(
            namespec.span(),
            CanonicalName::resolve_namespec_rel(parent_name, namespec)?,
        ))
    }

    pub fn span(&self) -> Span {
        match self {
            Self(span, _) => *span,
        }
    }

    /// The canonical name for this package assigned by
    ///   [`Self::new_canonical`],
    ///     if any.
    pub fn canonical_name(&self) -> SPair {
        match self {
            Self(_, name) => (*name).into(),
        }
    }
}

impl Display for Pkg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(_, name) => write!(f, "package {}", TtQuote::wrap(name)),
        }
    }
}

impl Functor<Span> for Pkg {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self(span, path) => Self(f(span), path),
        }
    }
}

object_rel! {
    /// Packages serve as a root for all identifiers defined therein,
    ///   and so an edge to [`Ident`] will never be a cross edge.
    ///
    /// Imported [`Ident`]s do not have edges from this package.
    Pkg -> {
        // Package import
        cross Pkg,

        // Identified objects owned by this package.
        tree Ident,

        // Anonymous templates are used for expansion.
        tree Tpl,

        // Arbitrary blocks of text serving as documentation.
        tree Doc,
    }
}

impl ObjectIndex<Pkg> {
    /// Complete the definition of a package.
    pub fn close(self, asg: &mut Asg, span: Span) -> Self {
        self.map_obj(asg, Pkg::fmap(|open| open.merge(span).unwrap_or(open)))
    }

    /// Indicate that a package should be imported at the provided
    ///   pathspec.
    ///
    /// This simply adds the import to the graph;
    ///   package loading must be performed by another subsystem.
    pub fn import(
        self,
        asg: &mut Asg,
        namespec: SPair,
    ) -> Result<Self, AsgError> {
        let parent = self.resolve(asg);
        let oi_import = asg.create(Pkg::new_imported(parent, namespec)?);

        Ok(self.add_edge_to(asg, oi_import, Some(namespec.span())))
    }

    /// Arbitrary text serving as documentation in a literate style.
    pub fn append_doc_text(&self, asg: &mut Asg, text: SPair) -> Self {
        let oi_doc = asg.create(Doc::new_text(text));
        self.add_edge_to(asg, oi_doc, None)
    }
}
