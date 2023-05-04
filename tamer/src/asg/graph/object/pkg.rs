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

use super::{prelude::*, Doc, Ident, NameableMissingObject, Tpl};
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

#[derive(Debug, PartialEq, Eq)]
pub struct Pkg(Span, PathSpec);

impl Pkg {
    /// Create a new package with a canonicalized name.
    ///
    /// A canonical package name is a path relative to the project root.
    pub(super) fn new_canonical<S: Into<Span>>(start: S, name: SPair) -> Self {
        Self(start.into(), PathSpec::Canonical(name))
    }

    /// Represent a package imported according to the provided
    ///   [`PathSpec`].
    pub fn new_imported(pathspec: SPair) -> Self {
        Self(pathspec.span(), PathSpec::TodoImport(pathspec))
    }

    pub fn span(&self) -> Span {
        match self {
            Self(span, _) => *span,
        }
    }

    /// The canonical name for this package assigned by
    ///   [`Self::new_canonical`],
    ///     if any.
    pub fn canonical_name(&self) -> Option<SPair> {
        match self {
            Self(_, pathspec) => pathspec.canonical_name(),
        }
    }

    /// The import path to this package as provided by
    ///   [`Self::new_imported`],
    ///     if any.
    pub fn import_path(&self) -> Option<SPair> {
        match self {
            Self(_, pathspec) => pathspec.import_path(),
        }
    }
}

impl Display for Pkg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "package")
    }
}

impl NameableMissingObject for Pkg {
    fn missing(pathspec: SPair) -> Self {
        Self::new_imported(pathspec)
    }
}

impl Functor<Span> for Pkg {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self(span, path) => Self(f(span), path),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum PathSpec {
    /// Canonical package name.
    ///
    /// This is the name of the package relative to the project root.
    /// This is like the module name after `crate::` in Rust,
    ///   but with `/` package separators in place of `::`.
    Canonical(SPair),

    /// Import path relative to the current package
    ///   (which is likely the compilation unit).
    ///
    /// TODO: This will be replaced with [`Self::Canonical`] once that is
    ///   working and relative paths can be resolved against the active
    ///   package.
    TodoImport(SPair),
}

impl PathSpec {
    fn canonical_name(&self) -> Option<SPair> {
        use PathSpec::*;

        match self {
            Canonical(spair) => Some(*spair),
            TodoImport(_) => None,
        }
    }

    fn import_path(&self) -> Option<SPair> {
        use PathSpec::*;

        match self {
            // TODO: Let's wait to see if we actually need this,
            //   since we'll need to allocate and intern a `/`-prefixed
            //   symbol.
            Canonical(_) => None,
            TodoImport(spair) => Some(*spair),
        }
    }
}

impl Display for PathSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use PathSpec::*;

        match self {
            Canonical(spair) => write!(f, "package {}", TtQuote::wrap(spair)),
            TodoImport(spair) => write!(
                f,
                "package import {} relative to compilation unit",
                TtQuote::wrap(spair)
            ),
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
