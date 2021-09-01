// Abstract semantic graph (ASG) intermediate representation (IR)
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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
//! Each node (vector) in the graph represents an [object][IdentObject],
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
//!   see [`IdentObject`] for valid object states and transitions.
//!
//!
//! How To Use
//! ==========
//! A suitable concrete [`Asg`] implementation is provided by
//!   [`DefaultAsg`].
//!
//! ```
//! use tamer::global;
//! use tamer::ir::asg::{Asg, DefaultAsg, IdentKind, IdentObject, Source};
//! use tamer::sym::{Interner, DefaultPkgInterner};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Be sure to choose size and initial capacities appropriate for your
//! // situation.
//! let mut asg = DefaultAsg::<IdentObject<_>, global::PkgIdentSize>::with_capacity(
//!     1024,
//!     1024,
//! );
//!
//! let interner = DefaultPkgInterner::new();
//! let identa_sym = interner.intern("identa");
//! let identb_sym = interner.intern("identb");
//!
//! let identa = asg.declare(identa_sym, IdentKind::Meta, Source::default())?;
//! let identb = asg.declare_extern(identb_sym, IdentKind::Meta, Source::default())?;
//!
//! assert_eq!(
//!     Some(&IdentObject::Extern(identb_sym, IdentKind::Meta, Source::default())),
//!     asg.get(identb),
//! );
//!
//! // Dependencies can be declared even if an identifier is
//! // unresolved.  This declares `(identa)->(identb)`.
//! asg.add_dep(identa, identb);
//! assert!(asg.has_dep(identa, identb));
//!
//! // TODO: extern resolution
//!
//! // Identifiers are indexed by symbol name.
//! assert_eq!(Some(identa), asg.lookup(identa_sym));
//! #
//! # Ok(()) // main
//! # }
//! ```
//!
//! Missing Identifiers
//! -------------------
//! Since identifiers in TAME can be defined in any order relative to their
//!   dependencies within a source file,
//!     it is often the case that a dependency will have to be added to the
//!     graph before it is resolved.
//! For example,
//!   [`Asg::add_dep_lookup`] will add an [`IdentObject::Missing`] to the graph
//!     if either identifier has not yet been declared.
//!
//! ```
//! # use tamer::global;
//! # use tamer::ir::asg::{Asg, DefaultAsg, IdentKind, IdentObject, FragmentText, Source};
//! # use tamer::sym::{Interner, DefaultPkgInterner};
//! #
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let mut asg = DefaultAsg::<IdentObject<_>, global::PkgIdentSize>::with_capacity(
//! #     1024,
//! #     1024,
//! # );
//! # let interner = DefaultPkgInterner::new();
//! #
//! let identa_sym = interner.intern("identa");
//! let identb_sym = interner.intern("identb");
//! let (identa, identb) = asg.add_dep_lookup(identa_sym, identb_sym);
//!
//! assert_eq!(Some(&IdentObject::Missing(identa_sym)), asg.get(identa));
//! assert_eq!(Some(&IdentObject::Missing(identb_sym)), asg.get(identb));
//!
//! // The identifiers returned above are proper objects on the graph.
//! assert_eq!(Some(identa), asg.lookup(identa_sym));
//! assert_eq!(Some(identb), asg.lookup(identb_sym));
//!
//! // Once declared, the missing identifier changes state and dependencies
//! // are retained.
//! asg.declare(identa_sym, IdentKind::Meta, Source::default())?;
//!
//! assert_eq!(
//!     Some(&IdentObject::Ident(identa_sym, IdentKind::Meta, Source::default())),
//!     asg.get(identa),
//! );
//!
//! assert!(asg.has_dep(identa, identb));
//! #
//! # Ok(()) // main
//! # }
//! ```
//!
//! Fragments
//! ---------
//! A compiled fragment can be attached to any resolved identifier (see
//!   [`IdentObject::Ident`]) using [`Asg::set_fragment`].
//! Doing so changes the state of the identifier to [`IdentObject::IdentFragment`],
//!   and it is an error to attempt to overwrite that fragment once it is
//!   set.
//!
//! ```
//! # use tamer::global;
//! # use tamer::ir::asg::{Asg, DefaultAsg, IdentKind, IdentObject, FragmentText, Source};
//! # use tamer::sym::{Interner, DefaultPkgInterner};
//! #
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let mut asg = DefaultAsg::<IdentObject<_>, global::PkgIdentSize>::with_capacity(
//! #     1024,
//! #     1024,
//! # );
//! # let interner = DefaultPkgInterner::new();
//! #
//! // Fragments can be attached to resolved identifiers.
//! let ident = asg.declare(
//!     interner.intern("ident"), IdentKind::Meta, Source::default()
//! )?;
//! asg.set_fragment(ident, FragmentText::from("test fragment"))?;
//!
//! assert_eq!(
//!     Some(&IdentObject::IdentFragment(
//!         interner.intern("ident"),
//!         IdentKind::Meta,
//!         Source::default(),
//!         FragmentText::from("test fragment"),
//!     )),
//!     asg.get(ident),
//! );
//!
//! // But overwriting will fail
//! let bad = asg.set_fragment(ident, FragmentText::from("overwrite"));
//! assert!(bad.is_err());
//! #
//! # Ok(()) // main
//! # }
//! ```

mod base;
mod graph;
mod ident;
mod object;
mod section;

pub use graph::{
    Asg, AsgError, AsgResult, IndexType, ObjectRef, SortableAsg,
    SortableAsgError,
};
pub use ident::{DataType, Dim, IdentKind, IdentKindError};
pub use object::{
    FragmentText, IdentObject, IdentObjectData, IdentObjectState, Source,
    TransitionError, TransitionResult, UnresolvedError,
};
pub use section::{Section, SectionIter, Sections};

/// Default concrete ASG implementation.
pub type DefaultAsg<O, Ix> = base::BaseAsg<O, Ix>;
