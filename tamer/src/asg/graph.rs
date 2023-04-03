// Graph abstraction
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

//! Abstract semantic graph.
//!
//! ![Visualization of ASG ontology](../ontviz.svg)

use self::object::{
    DynObjectRel, ObjectIndexRelTo, ObjectRelFrom, ObjectRelTy,
    ObjectRelatable, Root,
};

use super::{
    AsgError, FragmentText, Ident, IdentKind, Object, ObjectIndex, ObjectKind,
    Source, TransitionResult,
};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    global,
    parse::{util::SPair, Token},
    span::Span,
    sym::SymbolId,
};
use fxhash::FxHashMap;
use petgraph::{
    graph::{DiGraph, Graph, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use std::{fmt::Debug, result::Result};

pub mod object;
pub mod visit;
pub mod xmli;

use object::{ObjectContainer, ObjectRelTo};

/// Datatype representing node and edge indexes.
pub trait IndexType = petgraph::graph::IndexType;

/// A [`Result`] with a hard-coded [`AsgError`] error type.
///
/// This is the result of every [`Asg`] operation that could potentially
///   fail in error.
pub type AsgResult<T> = Result<T, AsgError>;

/// The [`ObjectRelTy`] (representing the [`ObjectKind`]) of the source and
///   destination [`Node`]s respectively.
///
/// This small memory expense allows for bidirectional edge filtering
///   and [`ObjectIndex`] [`ObjectKind`] resolution without an extra layer
///   of indirection to look up the source/target [`Node`].
///
/// The edge may also optionally contain a [`Span`] that provides additional
///   context in situations where the distinction between the span of the
///   target object and the span of the _reference_ to that object is
///   important.
type AsgEdge = (ObjectRelTy, ObjectRelTy, Option<Span>);

/// Each node of the graph.
type Node = ObjectContainer;

/// Index size for Graph nodes and edges.
type Ix = global::ProgSymSize;

/// An abstract semantic graph (ASG) of [objects](object).
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
pub struct Asg {
    // TODO: private; see `ld::xmle::lower`.
    /// Directed graph on which objects are stored.
    pub graph: DiGraph<Node, AsgEdge, Ix>,

    /// Scoped map of [`SymbolId`][crate::sym::SymbolId] to node indexes.
    ///
    /// This maps a `(SymbolId, NodeIndex)` pair to a node on the graph,
    ///   if any,
    ///   such that the provided [`SymbolId`] is part of the environment at
    ///   the paired [`NodeIndex`].
    /// That is:
    ///   an identifier must be indexed for _each container_ in its
    ///     environment,
    ///       which in turn determines the scope of that identifier.
    ///
    /// _An index does not imply the existence of an edge._
    /// This index serves as a shortcut for finding nodes on a graph,
    ///   _but makes no claims about the structure of the graph_.
    ///
    /// This allows for `O(1)` lookup of identifiers in the graph relative
    ///   to the environment of a given node.
    /// Note that,
    ///   while we store [`NodeIndex`] internally,
    ///   the public API encapsulates it within an [`ObjectIndex`].
    index: FxHashMap<(SymbolId, NodeIndex<Ix>), NodeIndex<Ix>>,

    /// The root node used for reachability analysis and topological
    ///   sorting.
    root_node: NodeIndex<Ix>,
}

impl Debug for Asg {
    /// Trimmed-down Asg [`Debug`] output.
    ///
    /// This primarily hides the large `self.index` that takes up so much
    ///   space in parser traces,
    ///     but also hides irrelevant information.
    ///
    /// The better option in the future may be to create a newtype for
    ///   `index` if it sticks around in its current form,
    ///     which in turn can encapsulate `self.empty_node`.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Asg")
            .field("root_node", &self.root_node)
            .field("graph", &self.graph)
            .finish_non_exhaustive()
    }
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
        let index =
            FxHashMap::with_capacity_and_hasher(objects, Default::default());

        // Automatically add the root which will be used to determine what
        //   identifiers ought to be retained by the final program.
        // This is not indexed and is not accessable by name.
        let root_node = graph.add_node(Object::Root(Root).into());

        Self {
            graph,
            index,
            root_node,
        }
    }

    /// Get the underlying Graph
    pub fn into_inner(self) -> DiGraph<Node, AsgEdge, Ix> {
        self.graph
    }

    /// Index the provided symbol `name` as representing the identifier
    ///   `node` in the immediate environment `imm_env`.
    ///
    /// This index permits `O(1)` identifier lookups.
    /// The term "immediate environment" is meant to convey that this index
    ///   applies only to the provided `imm_env` node and does not
    ///   propagate to any other nodes that share this environment.
    ///
    /// After an identifier is indexed it is not expected to be reassigned
    ///   to another node.
    /// Debug builds contain an assertion that will panic in this instance.
    fn index_identifier(
        &mut self,
        imm_env: NodeIndex<Ix>,
        name: SymbolId,
        node: NodeIndex<Ix>,
    ) {
        let prev = self.index.insert((name, imm_env), node);

        // We should never overwrite indexes
        debug_assert!(prev.is_none());
    }

    /// Lookup `ident` or add a missing identifier to the graph relative to
    ///   the immediate environment `imm_env` and return a reference to it.
    ///
    /// The provided span is necessary to seed the missing identifier with
    ///   some sort of context to aid in debugging why a missing identifier
    ///   was introduced to the graph.
    /// The provided span will be used even if an identifier exists on the
    ///   graph,
    ///     which can be used for retaining information on the location that
    ///     requested the identifier.
    /// To retrieve the span of a previously declared identifier,
    ///   you must resolve the [`Ident`] object and inspect it.
    ///
    /// See [`Ident::declare`] for more information.
    pub(super) fn lookup_or_missing<OS: ObjectIndexRelTo<Ident>>(
        &mut self,
        imm_env: OS,
        name: SPair,
    ) -> ObjectIndex<Ident> {
        self.lookup(imm_env, name).unwrap_or_else(|| {
            let index = self.graph.add_node(Ident::declare(name).into());

            self.index_identifier(self.root_node, name.symbol(), index);
            ObjectIndex::new(index, name.span())
        })
    }

    /// Lookup `ident` or add a missing identifier to the graph relative to
    ///   the global scope and return a reference to it.
    ///
    /// See [`Self::lookup_or_missing`] for more information.
    pub(super) fn lookup_global_or_missing(
        &mut self,
        name: SPair,
    ) -> ObjectIndex<Ident> {
        self.lookup_or_missing(
            ObjectIndex::<Root>::new(self.root_node, name),
            name,
        )
    }

    /// Perform a state transition on an identifier by name.
    ///
    /// Look up `ident` or add a missing identifier if it does not yet exist
    ///   (see [`Self::lookup_global_or_missing`]).
    /// Then invoke `f` with the located identifier and replace the
    ///   identifier on the graph with the result.
    ///
    /// This will safely restore graph state to the original identifier
    ///   value on transition failure.
    fn with_ident_lookup_global<F>(
        &mut self,
        name: SPair,
        f: F,
    ) -> AsgResult<ObjectIndex<Ident>>
    where
        F: FnOnce(Ident) -> TransitionResult<Ident>,
    {
        let identi = self.lookup_global_or_missing(name);
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

    /// Root object.
    ///
    /// All [`Object`]s reachable from the root will be included in the
    ///   compilation unit or linked executable.
    ///
    /// The `witness` is used in the returned [`ObjectIndex`] and is
    ///   intended for diagnostic purposes to highlight the source entity that
    ///   triggered the request of the root.
    pub fn root(&self, witness: Span) -> ObjectIndex<Root> {
        ObjectIndex::new(self.root_node, witness)
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
        self.graph.add_edge(
            self.root_node,
            identi.into(),
            (ObjectRelTy::Root, ObjectRelTy::Ident, None),
        );
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

        self.with_ident_lookup_global(name, |obj| {
            obj.resolve(name.span(), kind, src)
        })
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
        self.with_ident_lookup_global(name, |obj| {
            obj.extern_(name.span(), kind, src)
        })
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
        self.with_ident_lookup_global(name, |obj| obj.set_fragment(text))
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
    /// The edge may optionally contain a _contextual [`Span`]_,
    ///   in cases where it is important to distinguish between the span
    ///   associated with the target and the span associated with the
    ///   _reference_ to the target.
    ///
    /// For more information on how the ASG's ontology is enforced statically,
    ///   see [`ObjectRelTo`].
    fn add_edge<OB: ObjectKind + ObjectRelatable>(
        &mut self,
        from_oi: impl ObjectIndexRelTo<OB>,
        to_oi: ObjectIndex<OB>,
        ctx_span: Option<Span>,
    ) {
        self.graph.add_edge(
            from_oi.widen().into(),
            to_oi.into(),
            (from_oi.src_rel_ty(), OB::rel_ty(), ctx_span),
        );
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

    /// Attempt to map over an inner [`Object`] referenced by
    ///   [`ObjectIndex`].
    ///
    /// The type `O` is the expected type of the [`Object`],
    ///   which should be known to the caller based on the provied
    ///   [`ObjectIndex`].
    /// This method will attempt to narrow to that object type,
    ///   panicing if there is a mismatch;
    ///     see the [`object` module documentation](object) for more
    ///     information and rationale on this behavior.
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
    pub(super) fn try_map_obj<O: ObjectKind, E>(
        &mut self,
        index: ObjectIndex<O>,
        f: impl FnOnce(O) -> Result<O, (O, E)>,
    ) -> Result<ObjectIndex<O>, E> {
        let obj_container =
            self.graph.node_weight_mut(index.into()).diagnostic_expect(
                || diagnostic_node_missing_desc(index),
                "invalid ObjectIndex: data are missing from the ASG",
            );

        obj_container
            .try_replace_with(f)
            .map(|()| index.overwrite(obj_container.get::<Object>().span()))
    }

    /// Create an iterator over the [`ObjectIndex`]es of the outgoing edges
    ///   of `oi`.
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
    fn edges<'a, O: ObjectKind + ObjectRelatable + 'a>(
        &'a self,
        oi: ObjectIndex<O>,
    ) -> impl Iterator<Item = O::Rel> + 'a {
        self.edges_dyn(oi.widen()).map(move |dyn_rel| {
            let target_ty = dyn_rel.target_ty();

            dyn_rel.narrow_target::<O>().diagnostic_unwrap(|| {
                vec![
                    oi.internal_error(format!(
                        "encountered invalid outgoing edge type {:?}",
                        target_ty,
                    )),
                    oi.help(
                        "this means that Asg did not enforce edge invariants \
                            during construction, which is a significant bug",
                    ),
                ]
            })
        })
    }

    /// Create an iterator over the [`ObjectIndex`]es of the outgoing edges
    ///   of `oi` in a dynamic context.
    ///
    /// _This method should be used only when the types of objects cannot be
    ///   statically known,_
    ///     which is generally true only for code paths operating on
    ///     significant portions of
    ///       (or the entirety of)
    ///       the graph without distinction.
    /// See [`Self::edges`] for more information.
    fn edges_dyn<'a>(
        &'a self,
        oi: ObjectIndex<Object>,
    ) -> impl Iterator<Item = DynObjectRel> + 'a {
        self.graph.edges(oi.into()).map(move |edge| {
            let (src_ty, target_ty, ctx_span) = edge.weight();

            DynObjectRel::new(
                *src_ty,
                *target_ty,
                oi,
                ObjectIndex::<Object>::new(edge.target(), oi),
                *ctx_span,
            )
        })
    }

    /// Incoming edges to `oi` filtered by [`ObjectKind`] `OI`.
    ///
    /// The rationale behind the filtering is that objects ought to focus
    ///   primarily on what they _relate to_,
    ///     which is what the ontology is designed around.
    /// If an object cares about what has an edge _to_ it,
    ///   it should have good reason and a specific use case in mind.
    fn incoming_edges_filtered<'a, OI: ObjectKind + ObjectRelatable + 'a>(
        &'a self,
        oi: ObjectIndex<impl ObjectKind + ObjectRelFrom<OI> + 'a>,
    ) -> impl Iterator<Item = ObjectIndex<OI>> + 'a {
        self.graph
            .edges_directed(oi.into(), Direction::Incoming)
            .filter(|edge| edge.weight().0 == OI::rel_ty())
            .map(move |edge| ObjectIndex::<OI>::new(edge.source(), oi))
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
        self.lookup_global(ident)
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
            || diagnostic_unknown_ident_desc(ident),
            || format!("unknown identifier {}", TtQuote::wrap(ident),),
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
                    "identifier was not expected to be opaque: \
                        {} has no object binding",
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

    /// Attempt to retrieve an identifier from the graph by name relative to
    ///   the immediate environment `imm_env`.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    ///
    /// The global environment is defined as the environment of the current
    ///   compilation unit,
    ///     which is a package.
    #[inline]
    pub(super) fn lookup<OS: ObjectIndexRelTo<Ident>>(
        &self,
        imm_env: OS,
        id: SPair,
    ) -> Option<ObjectIndex<Ident>> {
        self.index
            .get(&(id.symbol(), imm_env.widen().into()))
            .map(|&ni| ObjectIndex::new(ni, id.span()))
    }

    /// Attempt to retrieve an identifier from the graph by name utilizing
    ///   the global environment.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    ///
    /// The global environment is defined as the environment of the current
    ///   compilation unit,
    ///     which is a package.
    #[inline]
    pub fn lookup_global(&self, name: SPair) -> Option<ObjectIndex<Ident>> {
        self.lookup(ObjectIndex::<Root>::new(self.root_node, name), name)
    }

    /// Declare that `dep` is a dependency of `ident`.
    ///
    /// An object must be declared as a dependency if its value must be
    ///   computed before computing the value of `ident`.
    /// The [linker][crate::ld] will ensure this ordering.
    ///
    /// See [`Self::add_dep_lookup_global`] if identifiers have to
    ///   be looked up by [`SymbolId`] or if they may not yet have been
    ///   declared.
    pub fn add_dep<O: ObjectKind>(
        &mut self,
        identi: ObjectIndex<Ident>,
        depi: ObjectIndex<O>,
    ) where
        Ident: ObjectRelTo<O>,
    {
        self.graph.update_edge(
            identi.into(),
            depi.into(),
            (Ident::rel_ty(), O::rel_ty(), None),
        );
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
    pub fn add_dep_lookup_global(
        &mut self,
        ident: SPair,
        dep: SPair,
    ) -> (ObjectIndex<Ident>, ObjectIndex<Ident>) {
        let identi = self.lookup_global_or_missing(ident);
        let depi = self.lookup_global_or_missing(dep);

        self.graph.update_edge(
            identi.into(),
            depi.into(),
            (Ident::rel_ty(), Ident::rel_ty(), None),
        );

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

fn diagnostic_unknown_ident_desc(ident: SPair) -> Vec<AnnotatedSpan<'static>> {
    vec![
        ident.internal_error("reference to an unknown identifier"),
        ident.help(
            "the system expects this identifier to be known, \
                but it could not be found.",
        ),
    ]
}

#[cfg(test)]
mod test;
