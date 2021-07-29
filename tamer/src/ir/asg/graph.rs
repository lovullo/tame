// Graph abstraction
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

//! Abstract graph as the basis for concrete ASGs.

use super::ident::IdentKind;
use super::object::{
    FragmentText, IdentObjectData, IdentObjectState, Source, TransitionError,
    UnresolvedError,
};
use super::Sections;
use crate::sym::{Symbol, SymbolIndexSize};
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use std::result::Result;

/// Datatype representing node and edge indexes.
pub trait IndexType: petgraph::graph::IndexType {}
impl<T: petgraph::graph::IndexType> IndexType for T {}

/// An abstract semantic graph of [objects][super::object].
///
/// This IR focuses on the definition and manipulation of objects and their
///   dependencies.
/// See [`IdentObject`](super::object::IdentObject) for a summary of valid
///   identifier object state transitions.
///
/// Objects are never deleted from the graph,
///   so [`ObjectRef`]s will remain valid for the lifetime of the ASG.
///
/// For more information,
///   see the [module-level documentation][self].
pub trait Asg<'i, O, Ix>
where
    Ix: IndexType + SymbolIndexSize,
    O: IdentObjectState<'i, Ix, O>,
{
    /// Declare a concrete identifier.
    ///
    /// An identifier declaration is similar to a declaration in a header
    ///   file in a language like C,
    ///     describing the structure of the identifier.
    /// Once declared,
    ///   this information cannot be changed.
    ///
    /// Identifiers are uniquely identified by a [`Symbol`] `name`.
    /// If an identifier of the same `name` already exists,
    ///   then the provided declaration is compared against the existing
    ///   declaration---should
    ///     they be incompatible,
    ///       then the operation will fail;
    ///     otherwise,
    ///       the existing identifier will be returned.
    ///
    /// If a concrete identifier has already been declared (see
    ///   [`Asg::declare`]),
    ///     then extern declarations will be compared and,
    ///       if compatible,
    ///       the identifier will be immediately _resolved_ and the object
    ///         on the graph will not be altered.
    /// Resolution will otherwise fail in error.
    ///
    /// For more information on state transitions that can occur when
    ///   redeclaring an identifier that already exists,
    ///     see [`IdentObjectState::resolve`].
    ///
    /// A successful declaration will add an identifier to the graph
    ///   and return an [`ObjectRef`] reference.
    fn declare(
        &mut self,
        name: &'i Symbol<'i, Ix>,
        kind: IdentKind,
        src: Source<'i, Ix>,
    ) -> AsgResult<ObjectRef<Ix>>;

    /// Declare an abstract identifier.
    ///
    /// An _extern_ declaration declares an identifier the same as
    ///   [`Asg::declare`],
    ///     but omits source information.
    /// Externs are identifiers that are expected to be defined somewhere
    ///   else ("externally"),
    ///     and are resolved at [link-time][crate::ld].
    ///
    /// If a concrete identifier has already been declared (see
    ///   [`Asg::declare`]),
    ///     then the declarations will be compared and,
    ///       if compatible,
    ///       the identifier will be immediately _resolved_ and the object
    ///         on the graph will not be altered.
    /// Resolution will otherwise fail in error.
    ///
    /// See [`IdentObjectState::extern_`] and
    ///   [`IdentObjectState::resolve`] for more information on
    ///   compatibility related to extern resolution.
    fn declare_extern(
        &mut self,
        name: &'i Symbol<'i, Ix>,
        kind: IdentKind,
        src: Source<'i, Ix>,
    ) -> AsgResult<ObjectRef<Ix>>;

    /// Set the fragment associated with a concrete identifier.
    ///
    /// Fragments are intended for use by the [linker][crate::ld].
    /// For more information,
    ///   see [`IdentObjectState::set_fragment`].
    fn set_fragment(
        &mut self,
        identi: ObjectRef<Ix>,
        text: FragmentText,
    ) -> AsgResult<ObjectRef<Ix>>;

    /// Retrieve an object from the graph by [`ObjectRef`].
    ///
    /// Since an [`ObjectRef`] should only be produced by an [`Asg`],
    ///   and since objects are never deleted from the graph,
    ///   this should never fail so long as references are not shared
    ///   between multiple graphs.
    /// It is nevertheless wrapped in an [`Option`] just in case.
    fn get<I: Into<ObjectRef<Ix>>>(&self, index: I) -> Option<&O>;

    /// Attempt to retrieve an identifier from the graph by name.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    fn lookup(&self, name: &'i Symbol<'i, Ix>) -> Option<ObjectRef<Ix>>;

    /// Declare that `dep` is a dependency of `ident`.
    ///
    /// An object must be declared as a dependency if its value must be
    ///   computed before computing the value of `ident`.
    /// The [linker][crate::ld] will ensure this ordering.
    ///
    /// See [`add_dep_lookup`][Asg::add_dep_lookup] if identifiers have to
    ///   be looked up by [`Symbol`] or if they may not yet have been
    ///   declared.
    fn add_dep(&mut self, ident: ObjectRef<Ix>, dep: ObjectRef<Ix>);

    /// Check whether `dep` is a dependency of `ident`.
    fn has_dep(&self, ident: ObjectRef<Ix>, dep: ObjectRef<Ix>) -> bool;

    /// Declare that `dep` is a dependency of `ident`,
    ///   regardless of whether they are known.
    ///
    /// In contrast to [`add_dep`][Asg::add_dep],
    ///   this method will add the dependency even if one or both of `ident`
    ///   or `dep` have not yet been declared.
    /// In such a case,
    ///   a missing identifier will be added as a placeholder,
    ///     allowing the ASG to be built with partial information as
    ///     identifiers continue to be discovered.
    /// See [`IdentObjectState::declare`] for more information.
    ///
    /// References to both identifiers are returned in argument order.
    fn add_dep_lookup(
        &mut self,
        ident: &'i Symbol<'i, Ix>,
        dep: &'i Symbol<'i, Ix>,
    ) -> (ObjectRef<Ix>, ObjectRef<Ix>);
}

/// Sort a graph into different [`Sections`]
///
/// Allow a graph to be partitioned into different [`Sections`] that can be
///   used as an `Intermediate Representation`.
pub trait SortableAsg<'i, O, Ix>
where
    O: IdentObjectData<'i, Ix>,
    Ix: IndexType + SymbolIndexSize,
{
    /// Sort graph into [`Sections`].
    ///
    /// Sorting will fail if the graph contains unresolved objects,
    ///   or identifiers whose kind cannot be determined
    ///   (see [`UnresolvedError`]).
    fn sort(
        &'i self,
        roots: &[ObjectRef<Ix>],
    ) -> SortableAsgResult<Sections<'i, O>, Ix>;
}

/// A [`Result`] with a hard-coded [`AsgError`] error type.
///
/// This is the result of every [`Asg`] operation that could potentially
///   fail in error.
pub type AsgResult<T> = Result<T, AsgError>;

/// A [`Result`] with a hard-coded [`SortableAsgError`] error type.
///
/// This is the result of every [`SortableAsg`] operation that could
///   potentially fail in error.
pub type SortableAsgResult<T, Ix> = Result<T, SortableAsgError<Ix>>;

/// Reference to an [object][super::object] stored within the [`Asg`].
///
/// IdentObject references are integer offsets,
///   not pointers.
/// See the [module-level documentation][self] for more information.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct ObjectRef<Ix>(NodeIndex<Ix>);

impl<Ix: IndexType> ObjectRef<Ix> {
    pub fn new(index: NodeIndex<Ix>) -> Self {
        Self(index)
    }
}

impl<Ix: IndexType> From<NodeIndex<Ix>> for ObjectRef<Ix> {
    fn from(index: NodeIndex<Ix>) -> Self {
        Self(index)
    }
}

impl<Ix: IndexType> From<ObjectRef<Ix>> for NodeIndex<Ix> {
    fn from(objref: ObjectRef<Ix>) -> Self {
        objref.0
    }
}

/// There are currently no data stored on edges ("edge weights").
pub type AsgEdge = ();

/// Each node of the graph represents an object.
///
/// Enclosed in an [`Option`] to permit moving owned values out of the
///   graph.
pub type Node<O> = Option<O>;

/// An error from an ASG operation.
///
/// Storing [`Symbol`] would require that this have a lifetime,
///   which is very inconvenient when chaining [`Result`],
///   so this stores only owned values.
/// The caller will know the problem values.
#[derive(Debug, PartialEq)]
pub enum AsgError {
    /// An object could not change state in the manner requested.
    ///
    /// See [`Asg::declare`] and [`Asg::set_fragment`] for more
    ///   information.
    /// See also [`TransitionError`].
    ObjectTransition(TransitionError),

    /// The node was not expected in the current context
    UnexpectedNode(String),
}

impl std::fmt::Display for AsgError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ObjectTransition(err) => std::fmt::Display::fmt(&err, fmt),
            Self::UnexpectedNode(msg) => {
                write!(fmt, "unexpected node: {}", msg)
            }
        }
    }
}

impl std::error::Error for AsgError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ObjectTransition(err) => err.source(),
            _ => None,
        }
    }
}

impl From<TransitionError> for AsgError {
    fn from(err: TransitionError) -> Self {
        Self::ObjectTransition(err)
    }
}

/// Error during graph sorting.
///
/// These errors reflect barriers to meaningfully understanding the
///   properties of the data in the graph with respect to sorting.
/// It does not represent bad underlying data that does not affect the
///   sorting process.
#[derive(Debug, PartialEq)]
pub enum SortableAsgError<Ix: IndexType> {
    /// An unresolved object was encountered during sorting.
    ///
    /// An unresolved object means that the graph has an incomplete picture
    ///   of the program,
    ///     and so sorting cannot be reliably performed.
    /// Since all objects are supposed to be resolved prior to sorting,
    ///   this represents either a problem with the program being compiled
    ///   or a bug in the compiler itself.
    UnresolvedObject(UnresolvedError),

    /// The kind of an object encountered during sorting could not be
    ///   determined.
    ///
    /// Sorting uses the object kind to place objects into their appropriate
    ///   sections.
    /// It should never be the case that a resolved object has no kind,
    ///   so this likely represents a compiler bug.
    MissingObjectKind(String),

    /// The graph has a cyclic dependency.
    Cycles(Vec<Vec<ObjectRef<Ix>>>),
}

impl<Ix: IndexType> From<UnresolvedError> for SortableAsgError<Ix> {
    fn from(err: UnresolvedError) -> Self {
        Self::UnresolvedObject(err)
    }
}

impl<Ix: IndexType> std::fmt::Display for SortableAsgError<Ix> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnresolvedObject(err) => std::fmt::Display::fmt(err, fmt),
            Self::MissingObjectKind(name) => write!(
                fmt,
                "internal error: missing object kind for object `{}` (this may be a compiler bug!)",
                name,
            ),
            Self::Cycles(_) => write!(fmt, "cyclic dependencies"),
        }
    }
}

impl<Ix: IndexType> std::error::Error for SortableAsgError<Ix> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::UnresolvedObject(err) => Some(err),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod objref {
        use super::*;

        #[test]
        fn to_from_nodeindex() {
            let index = NodeIndex::<u32>::new(5);
            let objref: ObjectRef<u32> = ObjectRef::from(index);

            assert_eq!(index, objref.0);
            assert_eq!(index, objref.into());
        }
    }
}
