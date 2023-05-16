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
    DynObjectRel, NameableMissingObject, ObjectIndexRelTo, ObjectRelFrom,
    ObjectRelTy, ObjectRelatable, Root,
};

use super::{air::EnvScopeKind, AsgError, Object, ObjectIndex, ObjectKind};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    f::Functor,
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

use object::ObjectContainer;

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
/// Objects are never deleted from the graph,
///   so [`ObjectIndex`]s will remain valid for the lifetime of the ASG.
///
/// For more information,
///   see the [module-level documentation][self].
pub struct Asg {
    /// Directed graph on which objects are stored.
    graph: DiGraph<Node, AsgEdge, Ix>,

    /// Environment cache of [`SymbolId`][crate::sym::SymbolId] to
    ///   [`ObjectIndex`]es.
    ///
    /// This maps a `(SymbolId, NodeIndex)` pair to a node on the graph for
    ///   a given [`ObjectRelTy`].
    /// _This indexing is not automatic_;
    ///   it must be explicitly performed using [`Self::index`].
    ///
    /// This index serves as a shortcut for finding nodes on a graph,
    ///   _but makes no claims about the structure of the graph_.
    ///
    /// This allows for `O(1)` lookup of identifiers in the graph relative
    ///   to a given node.
    /// Note that,
    ///   while we store [`NodeIndex`] internally,
    ///   the public API encapsulates it within an [`ObjectIndex`].
    index: FxHashMap<
        (ObjectRelTy, SymbolId, ObjectIndex<Object>),
        EnvScopeKind<ObjectIndex<Object>>,
    >,

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

    /// Number of [`Object`]s on the graph.
    ///
    /// This is equivalent to the number of nodes on the graph at the time
    ///   of writing,
    ///     but that may not always be the case.
    fn object_count(&self) -> usize {
        self.graph.node_count()
    }

    pub(super) fn try_index<
        O: ObjectRelatable,
        OS: ObjectIndexRelTo<O>,
        S: Into<SymbolId>,
    >(
        &mut self,
        imm_env: OS,
        name: S,
        eoi: EnvScopeKind<ObjectIndex<O>>,
    ) -> Result<(), ObjectIndex<O>> {
        let sym = name.into();
        let prev = self.index.insert(
            (O::rel_ty(), sym, imm_env.widen()),
            eoi.map(ObjectIndex::widen),
        );

        match prev {
            None => Ok(()),
            Some(eoi) => Err(eoi.into_inner().must_narrow_into::<O>()),
        }
    }

    /// Index the provided symbol `name` as representing the
    ///   [`ObjectIndex`] in the immediate environment `imm_env`.
    ///
    /// An index does not require the existence of an edge,
    ///   but an index may only be created if an edge `imm_env->oi` _could_
    ///   be constructed.
    ///
    /// This index permits `O(1)` object lookups.
    /// The term "immediate environment" is meant to convey that this index
    ///   applies only to the provided `imm_env` node and does not
    ///   propagate to any other objects that share this environment.
    ///
    /// After an object is indexed it is not expected to be re-indexed
    ///   to another node.
    /// Debug builds contain an assertion that will panic in this instance.
    pub(super) fn index<
        O: ObjectRelatable,
        OS: ObjectIndexRelTo<O>,
        S: Into<SymbolId>,
    >(
        &mut self,
        imm_env: OS,
        name: S,
        eoi: EnvScopeKind<ObjectIndex<O>>,
    ) {
        let sym = name.into();
        let prev = self.try_index(imm_env, sym, eoi);

        // We should never overwrite indexes
        #[allow(unused_variables)] // used only for debug
        #[allow(unused_imports)]
        if let Err(prev_oi) = prev {
            use crate::fmt::{DisplayWrapper, TtQuote};
            crate::debug_diagnostic_panic!(
                vec![
                    imm_env.widen().note("at this scope boundary"),
                    prev_oi.note("previously indexed identifier was here"),
                    eoi.internal_error(
                        "this identifier has already been indexed at the above scope boundary"
                    ),
                    eoi.help(
                        "this is a bug in the system responsible for analyzing \
                            identifier scope;"
                    ),
                    eoi.help(
                        "  you can try to work around it by duplicating the definition of "
                    ),
                    eoi.help(
                        format!(
                            "  {} as a _new_ identifier with a different name.",
                            TtQuote::wrap(sym),
                        )
                    ),
                ],
                "re-indexing of identifier at scope boundary",
            );
        }
    }

    /// Lookup `name or add a missing object to the graph relative to
    ///   the immediate environment `imm_env` and return a reference to it.
    ///
    /// The provided span is necessary to seed the missing object with
    ///   some sort of context to aid in debugging why a missing object
    ///   was introduced to the graph.
    /// The provided span will be used by the returned [`ObjectIndex`] even
    ///   if an object exists on the graph,
    ///     which can be used for retaining information on the location that
    ///     requested the object.
    /// To retrieve the span of a previously declared object,
    ///   you must resolve the [`ObjectIndex`] and inspect it.
    ///
    /// See [`Self::index`] for more information.
    pub(super) fn lookup_or_missing<O: ObjectRelatable>(
        &mut self,
        imm_env: impl ObjectIndexRelTo<O>,
        name: SPair,
    ) -> ObjectIndex<O>
    where
        O: NameableMissingObject,
    {
        self.lookup(imm_env, name).unwrap_or_else(|| {
            let oi = self.create(O::missing(name));

            // TODO: This responsibility is split between `Asg` and
            //   `AirAggregateCtx`!
            let eoi = EnvScopeKind::Visible(oi);

            self.index(imm_env, name.symbol(), eoi);
            oi
        })
    }

    /// Root object.
    ///
    /// All [`Object`]s reachable from the root will be included in the
    ///   compilation unit or linked executable.
    ///
    /// The `witness` is used in the returned [`ObjectIndex`] and is
    ///   intended for diagnostic purposes to highlight the source entity that
    ///   triggered the request of the root.
    pub fn root<S: Into<Span>>(&self, witness: S) -> ObjectIndex<Root> {
        ObjectIndex::new(self.root_node, witness.into())
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
    ///   see [`ObjectRelTo`](object::ObjectRelTo).
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
    ///   ontology defined by [`ObjectRelTo`](object::ObjectRelTo),
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

    /// Check whether an edge exists from `from` to `to.
    #[inline]
    pub fn has_edge<OB: ObjectRelatable>(
        &self,
        from: impl ObjectIndexRelTo<OB>,
        to: ObjectIndex<OB>,
    ) -> bool {
        self.graph.contains_edge(from.widen().into(), to.into())
    }

    pub(super) fn expect_obj<O: ObjectKind>(&self, oi: ObjectIndex<O>) -> &O {
        let obj_container =
            self.graph.node_weight(oi.into()).diagnostic_expect(
                || diagnostic_node_missing_desc(oi),
                "invalid ObjectIndex: data are missing from the ASG",
            );

        obj_container.get()
    }

    /// Attempt to retrieve an identifier from the graph by name relative to
    ///   the immediate environment `imm_env`.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    #[inline]
    pub fn lookup<O: ObjectRelatable>(
        &self,
        imm_env: impl ObjectIndexRelTo<O>,
        id: SPair,
    ) -> Option<ObjectIndex<O>> {
        self.lookup_raw(imm_env, id)
            .and_then(EnvScopeKind::in_scope)
            .map(EnvScopeKind::into_inner)
    }

    /// Attempt to retrieve an identifier and its scope information from the
    ///   graph by name relative to the immediate environment `imm_env`.
    ///
    /// See [`Self::lookup`] for more information.
    #[inline]
    pub(super) fn lookup_raw<O: ObjectRelatable>(
        &self,
        imm_env: impl ObjectIndexRelTo<O>,
        id: SPair,
    ) -> Option<EnvScopeKind<ObjectIndex<O>>> {
        // The type `O` is encoded into the index on [`Self::index`] and so
        //   should always be able to be narrowed into the expected type.
        // If this invariant somehow does not hold,
        //   then the system will panic when the object is resolved.
        // Maybe future Rust will have dependent types that allow for better
        //   static assurances.
        self.index
            .get(&(O::rel_ty(), id.symbol(), imm_env.widen()))
            .map(|&eoi| {
                eoi.map(|oi| oi.overwrite(id.span()).must_narrow_into::<O>())
            })
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

#[cfg(test)]
mod test;
