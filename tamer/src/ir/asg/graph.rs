// Graph abstraction
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
use super::object::{FragmentText, Object, Source};
use super::Sections;
use crate::sym::Symbol;
use petgraph::graph::{IndexType, NodeIndex};
use std::result::Result;

/// An abstract semantic graph of [objects][Object].
///
/// This IR focuses on the definition and manipulation of objects and their
///   dependencies.
/// See [`Object`] for a summary of valid object state transitions.
///
/// Objects are never deleted from the graph,
///   so [`ObjectRef`]s will remain valid for the lifetime of the ASG.
///
/// For more information,
///   see the [module-level documentation][self].
pub trait Asg<'i, Ix: IndexType> {
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
    /// A successful declaration will add a [`Object::Ident`] to the graph
    ///   and return an [`ObjectRef`] reference.
    ///
    /// If an existing identifier is an extern (see
    ///   [`Asg::declare_extern`]),
    ///     then the declaration will be compared just the same,
    ///       but the identifier will be converted from a
    ///       [`Object::Extern`] into a [`Object::Ident`].
    /// When this happens,
    ///   the extern is said to be _resolved_.
    ///
    /// If a virtual identifier of type [`Object::IdentFragment`] is
    ///   overridden,
    ///     then its fragment is cleared
    ///     (it returns to a [`Object::Ident`])
    ///     to make way for the fragment of the override.
    fn declare(
        &mut self,
        name: &'i Symbol<'i>,
        kind: IdentKind,
        src: Source<'i>,
    ) -> AsgResult<ObjectRef<Ix>>;

    /// Declare an abstract identifier.
    ///
    /// An _extern_ declaration declares an identifier the same as
    ///   [`Asg::declare`],
    ///     but instead as [`Object::Extern`].
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
    fn declare_extern(
        &mut self,
        name: &'i Symbol<'i>,
        expected_kind: IdentKind,
    ) -> AsgResult<ObjectRef<Ix>>;

    /// Set the fragment associated with a concrete identifier.
    ///
    /// This changes the type of the identifier from [`Object::Ident`]
    ///   into [`Object::IdentFragment`],
    ///     which is intended for use by the [linker][crate::ld].
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
    fn get<I: Into<ObjectRef<Ix>>>(&self, index: I) -> Option<&Object<'i>>;

    /// Attempt to retrieve an identifier from the graph by name.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    fn lookup(&self, name: &'i Symbol<'i>) -> Option<ObjectRef<Ix>>;

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
    ///   an [`Object::Missing`] will be added as a placeholder for the
    ///   missing identifier,
    ///     allowing the ASG to be built with partial information as
    ///     identifiers continue to be discovered.
    ///
    /// References to both identifiers are returned in argument order.
    fn add_dep_lookup(
        &mut self,
        ident: &'i Symbol<'i>,
        dep: &'i Symbol<'i>,
    ) -> (ObjectRef<Ix>, ObjectRef<Ix>);
}

/// Sort a graph into different [`Sections`]
///
/// Allow a graph to be partitioned into different [`Sections`] that can be
///   used as an `Intermediate Representation`.
pub trait SortableAsg<'a, 'i, Ix: IndexType> {
    fn sort(&'a self, roots: &[ObjectRef<Ix>]) -> AsgResult<Sections<'a, 'i>>;
}

/// A [`Result`] with a hard-coded [`AsgError`] error type.
///
/// This is the result of every [`Asg`] operation that could potentially
///   fail in error.
pub type AsgResult<T> = Result<T, AsgError>;

/// Reference to an [object][Object] stored within the [`Asg`].
///
/// Object references are integer offsets,
///   not pointers.
/// See the [module-level documentation][self] for more information.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct ObjectRef<Ix>(pub NodeIndex<Ix>);

impl<Ix> From<NodeIndex<Ix>> for ObjectRef<Ix>
where
    Ix: IndexType,
{
    fn from(index: NodeIndex<Ix>) -> Self {
        Self(index)
    }
}

impl<Ix> From<ObjectRef<Ix>> for NodeIndex<Ix>
where
    Ix: IndexType,
{
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
pub type Node<'i> = Option<Object<'i>>;

/// An error from an ASG operation.
///
/// Storing [`Symbol`] would require that this have a lifetime,
///   which is very inconvenient when chaining [`Result`],
///   so this stores only owned values.
/// The caller will know the problem values.
#[derive(Debug, PartialEq)]
pub enum AsgError {
    /// The provided identifier is not in a state that is permitted to
    ///   receive a fragment.
    ///
    /// See [`Asg::set_fragment`] for more information.
    BadFragmentDest(String),
    /// The node was not expected in the current context
    UnexpectedNode(String),
}

impl std::fmt::Display for AsgError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BadFragmentDest(msg) => {
                write!(fmt, "bad fragment destination: {}", msg)
            }
            Self::UnexpectedNode(msg) => {
                write!(fmt, "unexpected node: {}", msg)
            }
        }
    }
}

impl std::error::Error for AsgError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
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
