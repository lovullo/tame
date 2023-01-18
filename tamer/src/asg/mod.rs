// Abstract semantic graph (ASG) intermediate representation (IR)
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

//! Abstract semantic graph.
//!
//! The [abstract semantic graph][asg] (ASG) is an IR representing the
//!   relationship between objects using a directed [graph][].
//! An _object_ is an identifier or expression.
//!
//! Since TAME is a declarative language,
//!   the ASG does not represent control flow;
//!     instead, it represents the relationship between objects and their
//!     dependencies.
//! Control flow is determined solely by the [linker][crate::ld] based on
//!   these dependencies.
//!
//! See [`crate::global`] for available index sizes depending on context.
//! For example,
//!   a linker may choose to use [`crate::global::ProgIdentSize`];
//!
//!
//! Graph Structure
//! ===============
//! Each node (vector) in the graph represents an [`Object`],
//!   such as an identifier or an expression.
//! Each directed edge `(A->B)` represents that `A` depends upon `B`.
//!
//! Graphs may contain cycles for recursive functions—that is,
//!   TAME's ASG is _not_ a DAG.
//! Mutually recursive functions are therefore represented as
//!   [strongly connected components][scc].
//!
//! [asg]: https://en.wikipedia.org/wiki/Abstract_semantic_graph
//! [graph]: https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)
//! [scc]: https://en.wikipedia.org/wiki/Strongly_connected_component
//!
//! Each object may have a number of valid states;
//!   see [`Ident`] for valid object states and transitions.
//!
//! Missing Identifiers
//! -------------------
//! Since identifiers in TAME can be defined in any order relative to their
//!   dependencies within a source file,
//!     it is often the case that a dependency will have to be added to the
//!     graph before it is resolved.
//! For example,
//!   [`Asg::add_dep_lookup`] will add an [`Ident::Missing`] to the graph
//!     if either identifier has not yet been declared.

mod error;
mod graph;

pub mod air;

pub use error::AsgError;
pub use graph::{
    object::{
        expr::{Expr, ExprDim, ExprOp},
        ident::{
            FragmentText, Ident, IdentKind, Source, TransitionError,
            TransitionResult, UnresolvedError,
        },
        Object, ObjectIndex, ObjectKind,
    },
    Asg, AsgResult, IndexType,
};

/// Default concrete ASG implementation.
pub type DefaultAsg = graph::Asg;
