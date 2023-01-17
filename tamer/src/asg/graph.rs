// Graph abstraction
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

//! Abstract graph as the basis for concrete ASGs.

use super::object::{ObjectContainer, ObjectRelTo};
use super::{
    AsgError, FragmentText, Ident, IdentKind, Object, ObjectIndex, ObjectKind,
    Source, TransitionResult,
};
use crate::diagnose::panic::DiagnosticPanic;
use crate::diagnose::{Annotate, AnnotatedSpan};
use crate::f::Functor;
use crate::fmt::{DisplayWrapper, TtQuote};
use crate::global;
use crate::parse::util::SPair;
use crate::parse::Token;
use crate::sym::SymbolId;
use petgraph::{
    graph::{DiGraph, Graph, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use std::fmt::Debug;
use std::result::Result;

/// Datatype representing node and edge indexes.
pub trait IndexType = petgraph::graph::IndexType;

/// A [`Result`] with a hard-coded [`AsgError`] error type.
///
/// This is the result of every [`Asg`] operation that could potentially
///   fail in error.
pub type AsgResult<T> = Result<T, AsgError>;

/// There are currently no data stored on edges ("edge weights").
pub type AsgEdge = ();

/// Each node of the graph.
pub type Node = ObjectContainer;

/// Index size for Graph nodes and edges.
type Ix = global::ProgSymSize;

/// An abstract semantic graph (ASG) of [objects][super::object].
///
/// This implementation is currently based on [`petgraph`].
///
/// Identifiers are cached by name for `O(1)` lookup.
/// Since [`SymbolId`][crate::sym::SymbolId] is used for this purpose,
///   the index may contain more entries than nodes and may contain gaps.
///
/// This IR focuses on the definition and manipulation of objects and their
///   dependencies.
/// See [`Ident`]for a summary of valid identifier object state
///   transitions.
///
/// Objects are never deleted from the graph,
///   so [`ObjectIndex`]s will remain valid for the lifetime of the ASG.
///
/// For more information,
///   see the [module-level documentation][self].
#[derive(Debug)]
pub struct Asg {
    // TODO: private; see `ld::xmle::lower`.
    /// Directed graph on which objects are stored.
    pub graph: DiGraph<Node, AsgEdge, Ix>,

    /// Map of [`SymbolId`][crate::sym::SymbolId] to node indexes.
    ///
    /// This allows for `O(1)` lookup of identifiers in the graph.
    /// Note that,
    ///   while we store [`NodeIndex`] internally,
    ///   the public API encapsulates it within an [`ObjectIndex`].
    index: Vec<NodeIndex<Ix>>,

    /// Empty node indicating that no object exists for a given index.
    empty_node: NodeIndex<Ix>,

    /// The root node used for reachability analysis and topological
    ///   sorting.
    root_node: NodeIndex<Ix>,
}

impl Default for Asg {
    fn default() -> Self {
        Self::new()
    }
}

impl Asg {
    /// Create a new ASG.
    ///
    /// See also [`with_capacity`](Asg::with_capacity).
    pub fn new() -> Self {
        // TODO: Determine a proper initial capacity.
        Self::with_capacity(0, 0)
    }

    /// Create an ASG with the provided initial capacity.
    ///
    /// The value for `objects` will be used as the capacity for the nodes
    ///   in the graph,
    ///     as well as the initial index capacity.
    /// The value for `edges` may be more difficult to consider,
    ///   since edges are used to represent various relationships between
    ///   different types of objects,
    ///     but it's safe to say that each object will have at least one
    ///     edge to another object.
    pub fn with_capacity(objects: usize, edges: usize) -> Self {
        let mut graph = Graph::with_capacity(objects, edges);
        let mut index = Vec::with_capacity(objects);

        // Exhaust the first index to be used as a placeholder
        //   (its value does not matter).
        let empty_node = graph.add_node(Object::Root.into());
        index.push(empty_node);

        // Automatically add the root which will be used to determine what
        //   identifiers ought to be retained by the final program.
        // This is not indexed and is not accessable by name.
        let root_node = graph.add_node(Object::Root.into());

        Self {
            graph,
            index,
            empty_node,
            root_node,
        }
    }

    /// Get the underlying Graph
    pub fn into_inner(self) -> DiGraph<Node, AsgEdge, Ix> {
        self.graph
    }

    /// Index the provided symbol `name` as representing the identifier `node`.
    ///
    /// This index permits `O(1)` identifier lookups.
    ///
    /// After an identifier is indexed it is not expected to be reassigned
    ///   to another node.
    /// Debug builds contain an assertion that will panic in this instance.
    ///
    /// Panics
    /// ======
    /// Will panic if unable to allocate more space for the index.
    fn index_identifier(&mut self, name: SymbolId, node: NodeIndex<Ix>) {
        let i = name.as_usize();

        if i >= self.index.len() {
            // If this is ever a problem we can fall back to usize max and
            // re-compare before panicing
            let new_size = (i + 1)
                .checked_next_power_of_two()
                .expect("internal error: cannot allocate space for ASG index");

            self.index.resize(new_size, self.empty_node);
        }

        // We should never overwrite indexes
        debug_assert!(self.index[i] == self.empty_node);

        self.index[i] = node;
    }

    /// Lookup `ident` or add a missing identifier to the graph and return a
    ///   reference to it.
    ///
    /// The provided span is necessary to seed the missing identifier with
    ///   some sort of context to aid in debugging why a missing identifier
    ///   was introduced to the graph.
    ///
    /// See [`Ident::declare`] for more information.
    pub(super) fn lookup_or_missing(
        &mut self,
        ident: SPair,
    ) -> ObjectIndex<Ident> {
        self.lookup(ident).unwrap_or_else(|| {
            let index = self.graph.add_node(Ident::declare(ident).into());

            self.index_identifier(ident.symbol(), index);
            ObjectIndex::new(index, ident.span())
        })
    }

    /// Perform a state transition on an identifier by name.
    ///
    /// Look up `ident` or add a missing identifier if it does not yet exist
    ///   (see [`Self::lookup_or_missing`]).
    /// Then invoke `f` with the located identifier and replace the
    ///   identifier on the graph with the result.
    ///
    /// This will safely restore graph state to the original identifier
    ///   value on transition failure.
    fn with_ident_lookup<F>(
        &mut self,
        name: SPair,
        f: F,
    ) -> AsgResult<ObjectIndex<Ident>>
    where
        F: FnOnce(Ident) -> TransitionResult<Ident>,
    {
        let identi = self.lookup_or_missing(name);
        self.with_ident(identi, f)
    }

    /// Perform a state transition on an identifier by [`ObjectIndex`].
    ///
    /// Invoke `f` with the located identifier and replace the identifier on
    ///   the graph with the result.
    ///
    /// This will safely restore graph state to the original identifier
    ///   value on transition failure.
    fn with_ident<F>(
        &mut self,
        identi: ObjectIndex<Ident>,
        f: F,
    ) -> AsgResult<ObjectIndex<Ident>>
    where
        F: FnOnce(Ident) -> TransitionResult<Ident>,
    {
        let container = self.graph.node_weight_mut(identi.into()).unwrap();

        container
            .try_replace_with(f)
            .map(|()| identi)
            .map_err(Into::into)
    }

    // TODO: This is transitional;
    //   remove once [`crate::xmlo::asg_builder`] is removed.
    pub fn root(&self) -> NodeIndex<Ix> {
        self.root_node
    }

    /// Add an object as a root.
    ///
    /// Roots are always included during a topological sort and any
    ///   reachability analysis.
    ///
    /// Ideally,
    ///   roots would be minimal and dependencies properly organized such
    ///   that objects will be included if they are a transitive dependency
    ///   of some included subsystem.
    ///
    /// See also [`IdentKind::is_auto_root`].
    pub fn add_root(&mut self, identi: ObjectIndex<Ident>) {
        self.graph
            .add_edge(self.root_node, identi.into(), Default::default());
    }

    /// Whether an object is rooted.
    ///
    /// See [`Asg::add_root`] for more information about roots.
    #[cfg(test)]
    pub(super) fn is_rooted(&self, identi: ObjectIndex<Ident>) -> bool {
        self.graph.contains_edge(self.root_node, identi.into())
    }

    /// Declare a concrete identifier.
    ///
    /// An identifier declaration is similar to a declaration in a header
    ///   file in a language like C,
    ///     describing the structure of the identifier.
    /// Once declared,
    ///   this information cannot be changed.
    ///
    /// Identifiers are uniquely identified by a [`SymbolId`] `name`.
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
    ///     see [`Ident::resolve`].
    ///
    /// A successful declaration will add an identifier to the graph
    ///   and return an [`ObjectIndex`] reference.
    pub fn declare(
        &mut self,
        name: SPair,
        kind: IdentKind,
        src: Source,
    ) -> AsgResult<ObjectIndex<Ident>> {
        let is_auto_root = kind.is_auto_root();

        self.with_ident_lookup(name, |obj| obj.resolve(name.span(), kind, src))
            .map(|node| {
                is_auto_root.then(|| self.add_root(node));
                node
            })
    }

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
    /// See [`Ident::extern_`] and
    ///   [`Ident::resolve`] for more information on
    ///   compatibility related to extern resolution.
    pub fn declare_extern(
        &mut self,
        name: SPair,
        kind: IdentKind,
        src: Source,
    ) -> AsgResult<ObjectIndex<Ident>> {
        self.with_ident_lookup(name, |obj| obj.extern_(name.span(), kind, src))
    }

    /// Set the fragment associated with a concrete identifier.
    ///
    /// Fragments are intended for use by the [linker][crate::ld].
    /// For more information,
    ///   see [`Ident::set_fragment`].
    pub fn set_fragment(
        &mut self,
        name: SPair,
        text: FragmentText,
    ) -> AsgResult<ObjectIndex<Ident>> {
        self.with_ident_lookup(name, |obj| obj.set_fragment(text))
    }

    /// Create a new object on the graph.
    ///
    /// The provided [`ObjectIndex`] will be augmented with the span
    ///   of `obj`.
    pub(super) fn create<O: ObjectKind>(&mut self, obj: O) -> ObjectIndex<O> {
        let o = obj.into();
        let span = o.span();
        let node_id = self.graph.add_node(ObjectContainer::from(o.into()));

        ObjectIndex::new(node_id, span)
    }

    /// Add an edge from the [`Object`] represented by the
    ///   [`ObjectIndex`] `from_oi` to the object represented by `to_oi`.
    ///
    /// For more information on how the ASG's ontology is enforced statically,
    ///   see [`ObjectRelTo`].
    pub(super) fn add_edge<OA: ObjectKind, OB: ObjectKind>(
        &mut self,
        from_oi: ObjectIndex<OA>,
        to_oi: ObjectIndex<OB>,
    ) where
        OA: ObjectRelTo<OB>,
    {
        self.graph.add_edge(from_oi.into(), to_oi.into(), ());
    }

    /// Retrieve an object from the graph by [`ObjectIndex`].
    ///
    /// Since an [`ObjectIndex`] should only be produced by an [`Asg`],
    ///   and since objects are never deleted from the graph,
    ///   this should never fail so long as references are not shared
    ///   between multiple graphs.
    /// It is nevertheless wrapped in an [`Option`] just in case.
    #[inline]
    pub fn get<O: ObjectKind>(&self, index: ObjectIndex<O>) -> Option<&O> {
        self.graph
            .node_weight(index.into())
            .map(ObjectContainer::get)
    }

    /// Map over an inner [`Object`] referenced by [`ObjectIndex`].
    ///
    /// The type `O` is the expected type of the [`Object`],
    ///   which should be known to the caller based on the provied
    ///   [`ObjectIndex`].
    /// This method will attempt to narrow to that object type,
    ///   panicing if there is a mismatch;
    ///     see the [`object` module documentation](super::object) for more
    ///     information and rationale on this behavior.
    ///
    /// The `mut_` prefix of this method is intended to emphasize that,
    ///   unlike traditional `map` methods,
    ///   this does not take and return ownership;
    ///     the ASG is most often interacted with via mutable reference.
    ///
    /// Panics
    /// ======
    /// This method chooses to simplify the API by choosing panics for
    ///   situations that ought never to occur and represent significant bugs
    ///   in the compiler.
    /// Those situations are:
    ///
    ///   1. If the provided [`ObjectIndex`] references a node index that is
    ///        not present on the graph;
    ///   2. If the node referenced by [`ObjectIndex`] exists but its container
    ///        is empty because an object was taken but never returned; and
    ///   3. If an object cannot be narrowed (downcast) to type `O`,
    ///        representing a type mismatch between what the caller thinks
    ///        this object represents and what the object actually is.
    #[must_use = "returned ObjectIndex has a possibly-updated and more relevant span"]
    pub fn mut_map_obj<O: ObjectKind>(
        &mut self,
        index: ObjectIndex<O>,
        f: impl FnOnce(O) -> O,
    ) -> ObjectIndex<O> {
        let obj_container =
            self.graph.node_weight_mut(index.into()).diagnostic_expect(
                || diagnostic_node_missing_desc(index),
                "invalid ObjectIndex: data are missing from the ASG",
            );

        obj_container.replace_with(f);

        index.overwrite(obj_container.get::<Object>().span())
    }

    /// Create an iterator over the [`ObjectIndex`]es of the outgoing edges
    ///   of `self`.
    ///
    /// This is a generic method that simply returns an [`ObjectKind`] of
    ///   [`Object`] for each [`ObjectIndex`];
    ///     it is the responsibility of the caller to narrow the type to
    ///     what is intended.
    /// This is sufficient in practice,
    ///   since the graph cannot be constructed without adhering to the edge
    ///   ontology defined by [`ObjectRelTo`],
    ///     but this API is not helpful for catching problems at
    ///     compile-time.
    ///
    /// The reason for providing a generic index to [`Object`] is that it
    ///   allows the caller to determine how strict it wants to be with
    ///   reading from the graph;
    ///     for example,
    ///       it may prefer to filter unwanted objects rather than panicing
    ///       if they do not match a given [`ObjectKind`],
    ///         depending on its ontology.
    ///
    /// You should prefer methods on [`ObjectIndex`] instead,
    ///   with this method expected to be used only in those
    ///   implementations.
    pub(super) fn edges<'a, O: ObjectKind + 'a>(
        &'a self,
        oi: ObjectIndex<O>,
    ) -> impl Iterator<Item = ObjectIndex<Object>> + 'a {
        self.graph
            .edges(oi.into())
            .map(move |edge| ObjectIndex::new(edge.target(), oi))
    }

    /// Retrieve the [`ObjectIndex`] to which the given `ident` is bound,
    ///   if any.
    ///
    /// The type parameter `O` indicates the _expected_ [`ObjectKind`] to be
    ///   bound to the returned [`ObjectIndex`],
    ///     which will be used for narrowing (downcasting) the object after
    ///     lookup.
    /// An incorrect kind will not cause any failures until such a lookup
    ///   occurs.
    ///
    /// This will return [`None`] if the identifier is either opaque or does
    ///   not exist.
    fn get_ident_oi<O: ObjectKind>(
        &self,
        ident: SPair,
    ) -> Option<ObjectIndex<O>> {
        self.lookup(ident)
            .and_then(|identi| {
                self.graph
                    .neighbors_directed(identi.into(), Direction::Outgoing)
                    .next()
            })
            // Note that this use of `O` for `ObjectIndex` here means "I
            //   _expect_ this to `O`";
            //     the type will be verified during narrowing but will panic
            //     if this expectation is not met.
            .map(|ni| ObjectIndex::<O>::new(ni, ident.span()))
    }

    /// Retrieve the [`ObjectIndex`] to which the given `ident` is bound,
    ///   panicing if the identifier is either opaque or does not exist.
    ///
    /// Panics
    /// ======
    /// This method will panic if the identifier is opaque
    ///   (has no edge to the object to which it is bound)
    ///   or does not exist on the graph.
    pub fn expect_ident_oi<O: ObjectKind>(
        &self,
        ident: SPair,
    ) -> ObjectIndex<O> {
        self.get_ident_oi(ident).diagnostic_expect(
            || diagnostic_opaque_ident_desc(ident),
            || {
                format!(
                    "opaque identifier: {} has no object binding",
                    TtQuote::wrap(ident),
                )
            },
        )
    }

    /// Attempt to retrieve the [`Object`] to which the given `ident` is bound.
    ///
    /// If the identifier either does not exist on the graph or is opaque
    ///   (is not bound to any expression),
    ///   then [`None`] will be returned.
    ///
    /// If the system expects that the identifier must exist and would
    ///   otherwise represent a bug in the compiler,
    ///     see [`Self::expect_ident_obj`].
    ///
    /// Panics
    /// ======
    /// This method will panic if certain graph invariants are not met,
    ///   representing an invalid system state that should not be able to
    ///   occur through this API.
    /// Violations of these invariants represent either a bug in the API
    ///   (that allows for the invariant to be violated)
    ///   or direct manipulation of the underlying graph.
    pub fn get_ident_obj<O: ObjectKind>(&self, ident: SPair) -> Option<&O> {
        self.get_ident_oi::<O>(ident).map(|oi| self.expect_obj(oi))
    }

    pub(super) fn expect_obj<O: ObjectKind>(&self, oi: ObjectIndex<O>) -> &O {
        let obj_container =
            self.graph.node_weight(oi.into()).diagnostic_expect(
                || diagnostic_node_missing_desc(oi),
                "invalid ObjectIndex: data are missing from the ASG",
            );

        obj_container.get()
    }

    /// Attempt to retrieve the [`Object`] to which the given `ident` is bound,
    ///   panicing if the identifier is opaque or does not exist.
    ///
    /// This method represents a compiler invariant;
    ///   it should _only_ be used when the identifier _must_ exist,
    ///     otherwise there is a bug in the compiler.
    /// If this is _not_ the case,
    ///   use [`Self::get_ident_obj`] to get [`None`] in place of a panic.
    ///
    /// Panics
    /// ======
    /// This method will panic if
    ///
    ///   1. The identifier does not exist on the graph; or
    ///   2. The identifier is opaque (has no edge to any object on the
    ///        graph).
    pub fn expect_ident_obj<O: ObjectKind>(&self, ident: SPair) -> &O {
        self.get_ident_obj(ident).diagnostic_expect(
            || diagnostic_opaque_ident_desc(ident),
            || {
                format!(
                    "opaque identifier: {} has no object binding",
                    TtQuote::wrap(ident),
                )
            },
        )
    }

    /// Retrieve an identifier from the graph by [`ObjectIndex`].
    ///
    /// If the object exists but is not an identifier,
    ///   [`None`] will be returned.
    #[inline]
    pub fn get_ident(&self, index: ObjectIndex<Ident>) -> Option<&Ident> {
        self.get(index)
    }

    /// Attempt to retrieve an identifier from the graph by name.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    #[inline]
    pub fn lookup(&self, id: SPair) -> Option<ObjectIndex<Ident>> {
        let i = id.symbol().as_usize();

        self.index
            .get(i)
            .filter(|ni| ni.index() > 0)
            .map(|ni| ObjectIndex::new(*ni, id.span()))
    }

    /// Declare that `dep` is a dependency of `ident`.
    ///
    /// An object must be declared as a dependency if its value must be
    ///   computed before computing the value of `ident`.
    /// The [linker][crate::ld] will ensure this ordering.
    ///
    /// See [`add_dep_lookup`][Asg::add_dep_lookup] if identifiers have to
    ///   be looked up by [`SymbolId`] or if they may not yet have been
    ///   declared.
    pub fn add_dep<O: ObjectKind>(
        &mut self,
        identi: ObjectIndex<Ident>,
        depi: ObjectIndex<O>,
    ) {
        self.graph
            .update_edge(identi.into(), depi.into(), Default::default());
    }

    /// Check whether `dep` is a dependency of `ident`.
    #[inline]
    pub fn has_dep(
        &self,
        ident: ObjectIndex<Ident>,
        dep: ObjectIndex<Ident>,
    ) -> bool {
        self.graph.contains_edge(ident.into(), dep.into())
    }

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
    /// See [`Ident::declare`] for more information.
    ///
    /// References to both identifiers are returned in argument order.
    pub fn add_dep_lookup(
        &mut self,
        ident: SPair,
        dep: SPair,
    ) -> (ObjectIndex<Ident>, ObjectIndex<Ident>) {
        let identi = self.lookup_or_missing(ident);
        let depi = self.lookup_or_missing(dep);

        self.graph
            .update_edge(identi.into(), depi.into(), Default::default());

        (identi, depi)
    }
}

fn diagnostic_node_missing_desc<O: ObjectKind>(
    index: ObjectIndex<O>,
) -> Vec<AnnotatedSpan<'static>> {
    vec![
        index.internal_error("this object is missing from the ASG"),
        index.help("this means that either an ObjectIndex was malformed, or"),
        index.help("  the object no longer exists on the graph, both of"),
        index.help("  which are unexpected and possibly represent data"),
        index.help("  corruption."),
        index.help("The system cannot proceed with confidence."),
    ]
}

fn diagnostic_opaque_ident_desc(ident: SPair) -> Vec<AnnotatedSpan<'static>> {
    vec![
        ident.internal_error(
            "this identifier is not bound to any object on the ASG",
        ),
        ident.help("the system expects to be able to reach the object that"),
        ident.help("  this identifies, but this identifier has no"),
        ident.help("  corresponding object present on the graph."),
    ]
}

#[cfg(test)]
mod test {
    use super::super::error::AsgError;
    use super::*;
    use crate::{num::Dim, span::dummy::*, sym::GlobalSymbolIntern};
    use std::assert_matches::assert_matches;

    type Sut = Asg;

    #[test]
    fn create_with_capacity() {
        let node_capacity = 100;
        let edge_capacity = 300;
        let sut = Sut::with_capacity(node_capacity, edge_capacity);

        let (nc, ec) = sut.graph.capacity();
        assert!(nc >= node_capacity);
        assert!(ec >= edge_capacity);
        assert!(sut.index.capacity() >= node_capacity);
    }

    #[test]
    fn declare_new_unique_idents() -> AsgResult<()> {
        let mut sut = Sut::new();

        // NB: The index ordering is important!  We first use a larger
        // index to create a gap, and then use an index within that gap
        // to ensure that it's not considered an already-defined
        // identifier.
        let syma = "syma".into();
        let symb = "symab".into();

        let nodea = sut.declare(
            SPair(syma, S1),
            IdentKind::Meta,
            Source {
                desc: Some("a".into()),
                ..Default::default()
            },
        )?;

        let nodeb = sut.declare(
            SPair(symb, S2),
            IdentKind::Worksheet,
            Source {
                desc: Some("b".into()),
                ..Default::default()
            },
        )?;

        assert_ne!(nodea, nodeb);

        let givena = sut.get_ident(nodea).unwrap();
        assert_eq!(SPair(syma, S1), givena.name());
        assert_eq!(Some(&IdentKind::Meta), givena.kind());
        assert_eq!(
            Some(&Source {
                desc: Some("a".into()),
                ..Default::default()
            },),
            givena.src()
        );

        let givenb = sut.get_ident(nodeb).unwrap();
        assert_eq!(SPair(symb, S2), givenb.name());
        assert_eq!(Some(&IdentKind::Worksheet), givenb.kind());
        assert_eq!(
            Some(&Source {
                desc: Some("b".into()),
                ..Default::default()
            }),
            givenb.src()
        );

        Ok(())
    }

    #[test]
    fn declare_kind_auto_root() -> AsgResult<()> {
        let mut sut = Sut::new();

        let auto_kind = IdentKind::Worksheet;
        // Sanity check, in case this changes.
        assert!(auto_kind.is_auto_root());

        let auto_root_node = sut.declare(
            SPair("auto_root".into(), S1),
            auto_kind,
            Default::default(),
        )?;

        // Should have been automatically added as a root.
        assert!(sut
            .graph
            .contains_edge(sut.root_node, auto_root_node.into()));

        let no_auto_kind = IdentKind::Tpl;
        assert!(!no_auto_kind.is_auto_root());

        let no_auto_root_node = sut.declare(
            SPair("no_auto_root".into(), S2),
            no_auto_kind,
            Default::default(),
        )?;

        // Non-auto-roots should _not_ be added as roots automatically.
        assert!(!sut
            .graph
            .contains_edge(sut.root_node, no_auto_root_node.into()));

        Ok(())
    }

    #[test]
    fn lookup_by_symbol() -> AsgResult<()> {
        let mut sut = Sut::new();

        let id = SPair("lookup".into(), S1);
        let node = sut.declare(
            id,
            IdentKind::Meta,
            Source {
                generated: true,
                ..Default::default()
            },
        )?;

        assert_eq!(Some(node), sut.lookup(id));

        Ok(())
    }

    #[test]
    fn declare_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "symdup".into();
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        // Set up an object to fail redeclaration.
        let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;
        let result =
            sut.declare(SPair(sym, S2), IdentKind::Meta, Source::default());

        assert_matches!(result, Err(AsgError::IdentTransition(..)));

        // The node should have been restored.
        assert_eq!(Some(&src), sut.get_ident(node).unwrap().src());

        Ok(())
    }

    #[test]
    fn declare_extern_returns_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "symext".into();
        let src = Source::default();
        let kind = IdentKind::Class(Dim::Matrix);
        let node =
            sut.declare_extern(SPair(sym, S1), kind.clone(), src.clone())?;

        let resrc = Source {
            desc: Some("redeclare".into()),
            ..Default::default()
        };
        let redeclare =
            sut.declare_extern(SPair(sym, S2), kind.clone(), resrc.clone())?;

        assert_eq!(node, redeclare);

        Ok(())
    }

    // Builds upon declare_returns_existing.
    #[test]
    fn declare_extern_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "symdup".into();
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

        // Changes kind, which is invalid.
        let result = sut.declare_extern(
            SPair(sym, S2),
            IdentKind::Worksheet,
            Source::default(),
        );

        assert_matches!(result, Err(AsgError::IdentTransition(..)));

        // The node should have been restored.
        assert_eq!(Some(&src), sut.get_ident(node).unwrap().src());

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "tofrag".into();
        let src = Source {
            generated: true,
            ..Default::default()
        };
        let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

        let fragment = "a fragment".intern();
        let node_with_frag = sut.set_fragment(SPair(sym, S2), fragment)?;

        // Attaching a fragment should _replace_ the node, not create a
        // new one
        assert_eq!(
            node, node_with_frag,
            "fragment node does not match original node"
        );

        let obj = sut.get_ident(node).unwrap();

        assert_eq!(SPair(sym, S1), obj.name());
        assert_eq!(Some(&IdentKind::Meta), obj.kind());
        assert_eq!(Some(&src), obj.src());
        assert_eq!(Some(fragment), obj.fragment());

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "failfrag".into();
        let src = Source {
            generated: true,
            ..Default::default()
        };

        // The failure will come from terr below, not this.
        let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

        // The first set will succeed.
        sut.set_fragment(SPair(sym, S2), "".into())?;

        // This will fail.
        let result = sut.set_fragment(SPair(sym, S3), "".into());

        // The node should have been restored.
        let obj = sut.get_ident(node).unwrap();

        assert_eq!(SPair(sym, S1), obj.name());
        assert_matches!(result, Err(AsgError::IdentTransition(..)));

        Ok(())
    }

    #[test]
    fn add_ident_dep_to_ident() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "sym".into();
        let dep = "dep".into();

        let symnode =
            sut.declare(SPair(sym, S1), IdentKind::Meta, Source::default())?;
        let depnode =
            sut.declare(SPair(dep, S2), IdentKind::Meta, Source::default())?;

        sut.add_dep(symnode, depnode);
        assert!(sut.has_dep(symnode, depnode));

        // sanity check if we re-add a dep
        sut.add_dep(symnode, depnode);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    // same as above test
    #[test]
    fn add_dep_lookup_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = SPair("sym".into(), S1);
        let dep = SPair("dep".into(), S2);

        let _ = sut.declare(sym, IdentKind::Meta, Source::default())?;
        let _ = sut.declare(dep, IdentKind::Meta, Source::default())?;

        let (symnode, depnode) = sut.add_dep_lookup(sym, dep);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    #[test]
    fn add_dep_lookup_missing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = SPair("sym".into(), S1);
        let dep = SPair("dep".into(), S2);

        // both of these are missing
        let (symnode, depnode) = sut.add_dep_lookup(sym, dep);
        assert!(sut.has_dep(symnode, depnode));

        assert_eq!(sym, sut.get_ident(symnode).unwrap().name());
        assert_eq!(dep, sut.get_ident(depnode).unwrap().name());

        Ok(())
    }

    #[test]
    fn declare_return_missing_symbol() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = SPair("sym".into(), S1);
        let dep = SPair("dep".into(), S2);

        // both of these are missing, see add_dep_lookup_missing
        let (symnode, _) = sut.add_dep_lookup(sym, dep);

        let src = Source {
            desc: Some("redeclare missing".into()),
            ..Default::default()
        };

        // Check with a declared value
        let declared = sut.declare(sym, IdentKind::Meta, src.clone())?;

        assert_eq!(symnode, declared);

        let obj = sut.get_ident(declared).unwrap();

        assert_eq!(sym, obj.name());
        assert_eq!(Some(&IdentKind::Meta), obj.kind());
        assert_eq!(Some(&src), obj.src());

        Ok(())
    }

    #[test]
    fn mut_map_narrows_and_modifies() {
        let mut sut = Sut::new();

        let id_a = SPair("foo".into(), S1);
        let id_b = SPair("bar".into(), S2);

        let oi = sut.create(Ident::Missing(id_a));

        // This is the method under test.
        // It should narrow to an `Ident` because `oi` was `create`'d with
        //   an `Ident`.
        let oi_new = sut.mut_map_obj(oi, |ident| {
            assert_eq!(ident, Ident::Missing(id_a));

            // Replace the identifier
            Ident::Missing(id_b)
        });

        // These would not typically be checked by the caller;
        //   they are intended for debugging.
        assert_eq!(S1, oi.into());
        assert_eq!(S2, oi_new.into());

        // A change in span does not change its equivalence.
        assert_eq!(oi_new, oi);

        // Ensure that the graph was updated with the new object from the
        //   above method.
        assert_eq!(&Ident::Missing(id_b), sut.get(oi).unwrap(),);
    }
}
