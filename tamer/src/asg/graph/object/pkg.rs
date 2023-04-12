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
    parse::{util::SPair, Token},
    span::Span,
    sym::st::raw::WS_EMPTY,
};
use std::fmt::Display;

#[cfg(doc)]
use super::ObjectKind;

#[derive(Debug, PartialEq, Eq)]
pub struct Pkg(Span, PathSpec);

/// Package path specification used to import this package.
///
/// TODO: This is simply punting on handling of imports for now.
type PathSpec = SPair;

impl Pkg {
    /// Create a new package intended to serve as the compilation unit,
    ///   with an empty pathspec.
    pub fn new<S: Into<Span>>(span: S) -> Self {
        let s = span.into();
        Self(s, SPair(WS_EMPTY, s))
    }

    /// Represent a package imported according to the provided
    ///   [`PathSpec`].
    pub fn new_imported(pathspec: PathSpec) -> Self {
        Self(pathspec.span(), pathspec)
    }

    pub fn span(&self) -> Span {
        match self {
            Self(span, _) => *span,
        }
    }

    pub fn pathspec(&self) -> PathSpec {
        match self {
            Self(_, pathspec) => *pathspec,
        }
    }
}

impl Display for Pkg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "package")
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
    pub fn import(self, asg: &mut Asg, pathspec: SPair) -> Self {
        let oi_import = asg.create(Pkg::new_imported(pathspec));
        self.add_edge_to(asg, oi_import, Some(pathspec.span()))
    }

    /// Arbitrary text serving as documentation in a literate style.
    pub fn append_doc_text(&self, asg: &mut Asg, text: SPair) -> Self {
        let oi_doc = asg.create(Doc::new_text(text));
        self.add_edge_to(asg, oi_doc, None)
    }
}
