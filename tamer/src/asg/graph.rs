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

use super::{AsgError, Object, ObjectIndex, ObjectKind};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    f::Map,
    global,
    span::Span,
};
use petgraph::{
    graph::{DiGraph, Graph, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use std::{fmt::Debug, result::Result};

#[cfg(doc)]
use object::{ObjectIndexTo, Tpl};

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

    /// The root node used for reachability analysis and topological
    ///   sorting.
    root_node: NodeIndex<Ix>,
}

impl Default for Asg {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Asg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The ASG provides far too much information even on modestly size
        //   tests,
        //     letalone real-world graphs with tens to hundreds of thousands
        //     of nodes and edges.
        // Outputting the graph also brings Parser trace generation to a
        //   crawl,
        //     making tracing  half-useless.
        // So this provides a simple summary.
        // If we need a graph representation,
        //   a visualization or ability to query it is far more appropriate.
        write!(
            f,
            "[ASG: {} objects, {} edges]",
            self.object_count(),
            self.graph.edge_count(),
        )
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

        // Automatically add the root which will be used to determine what
        //   identifiers ought to be retained by the final program.
        // This is not indexed and is not accessable by name.
        let root_node = graph.add_node(Object::Root(Root).into());

        Self { graph, root_node }
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
    ///
    /// Callers external to this module should use [`ObjectIndex`] APIs to
    ///   manipulate the graph;
    ///     this allows those objects to uphold their own invariants
    ///     relative to the state of the graph.
    fn add_edge<OA: ObjectIndexRelTo<OB>, OB: ObjectKind + ObjectRelatable>(
        &mut self,
        from_oi: OA,
        to_oi: ObjectIndex<OB>,
        ctx_span: Option<Span>,
    ) -> Result<(), AsgError> {
        from_oi.pre_add_edge(self, to_oi, ctx_span).map(|()| {
            self.graph.add_edge(
                from_oi.widen().into(),
                to_oi.into(),
                (from_oi.src_rel_ty(), OB::rel_ty(), ctx_span),
            );
        })
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

/// Mutation of an [`Object`]'s relationships (edges) on the [`Asg`].
///
/// This trait is intended to delegate certain responsibilities to
///   [`ObjectKind`]s so that they may enforce their own invariants with
///   respect to their relationships to other objects on the graph.
///
/// TODO: It'd be nice if only [`Asg`] were able to invoke methods on this
///   trait,
///     but the current module structure together with Rust's visibility
///     with sibling modules doesn't seem to make that possible.
///
/// How Does This Work With Trait Specialization?
/// =============================================
/// [`Asg::add_edge`] is provided a [`ObjectIndexRelTo`],
///   which needs narrowing to an appropriate source [`ObjectKind`] so that
///   we can invoke [`<O as AsgRelMut>::pre_add_edge`](AsgRelMut::pre_add_edge).
///
/// At the time of writing,
///   there are two implementors of [`ObjectIndexRelTo`]:
///
///   - [`ObjectIndex<O>`],
///       for which we will know `O: ObjectKind`.
///   - [`ObjectIndexTo<OB>`],
///       for which we only know the _target_ `OB: ObjectKind`.
///
/// The entire purpose of [`ObjectIndexTo`] is to allow for a dynamic
///   source [`ObjectKind`];
///     we do not know what it is statically.
/// So [`ObjectIndexTo::pre_add_edge`] is a method that will dynamically
///   branch to an appropriate static path to invoke the correct
///   [`AsgRelMut::pre_add_edge`].
///
/// And there's the problem.
///
/// We match on each [`ObjectRelTy`] based on
///   [`ObjectIndexTo::src_rel_ty`],
///     and invoke the appropriate [`AsgRelMut::pre_add_edge`].
/// But the trait bound on `OB` for the `ObjectIndexRelTo` `impl` is
///   [`ObjectRelatable`].
/// So it resolves as `AsgRelMut<OB: ObjectRelatable>`.
///
/// But we don't have that implementation.
/// We have implementations for _individual target [`ObjectRelatable`]s,
///   e.g. `impl AsgRelMut<Expr> for Tpl`.
/// So Rust rightfully complains that `AsgRelMut<OB: ObjectRelatable>`
///   is not implemented for [`Tpl`].
/// (Go ahead and remove the generic `impl` block containing `default fn`
///    and see what happens.)
///
/// Of course,
///   _we_ know that there's a trait implemented for every possible
///   [`ObjectRelFrom<Tpl>`],
///     because `object_rel!` does that for us based on the same
///     definition that generates those other types.
/// But Rust does not perform that type of analysis---​
///   it does not know that we've accounted for every type.
/// So the `default fn`` uses the unstable `min_specialization` feature to
///   satisfy those more generic trait bounds,
///     making the compiler happy.
///
/// But if Rust is seeing `OB: ObjectRelatable`,
///   why is it not monomorphizing to _this_ one rather than the more
///   specialized implementation?
///
/// That type error described above is contemplating bounds for _any
///   potential caller_.
/// But when we're about to add an edge,
///   we're invoking with a specific type of `OB`.
/// Monomorphization takes place at that point,
///   with the expected type,
///   and uses the appropriate specialization.
///
/// Because of other trait bounds leading up to this point,
///   including those on [`Asg::add_edge`] and [`ObjectIndexRelTo`],
///   this cannot be invoked for any `to_oi` that is not a valid target
///     for `Self`.
/// But we cannot be too strict on that bound _here_,
///   because otherwise it's not general enough for
///   [`ObjectIndexTo::pre_add_edge`].
/// We could do more runtime verification and further refine types,
///   but that is a lot of work for no additional practical benefit,
///     at least at this time.
pub trait AsgRelMut<OB: ObjectRelatable>: ObjectRelatable {
    /// Allow an object to handle or reject the creation of an edge from it
    ///   to another object.
    ///
    /// Objects own both their node on the graph and the edges _from_ that
    ///   node to another object.
    /// Phrased another way:
    ///   they own their data and their relationships.
    ///
    /// This gives an opportunity for the [`ObjectKind`] associated with the
    ///   source object to evaluate the proposed relationship.
    /// This guarantee allows objects to cache information about these
    ///   relationships and enforce any associated invariants without
    ///   worrying about how the object may change out from underneath
    ///   them.
    /// In some cases,
    ///   this is the only way that an object will know whether an edge has
    ///   been added,
    ///     since the [`ObjectIndex`] APIs may not be utilized
    ///       (e.g. in the case of [`ObjectIndexRelTo`].
    ///
    /// This is invoked by [`Asg::add_edge`].
    /// The provided `commit` callback will complete the addition of the
    ///   edge if provided [`Ok`],
    ///     and the commit cannot fail.
    /// If [`Err`] is provided to `commit`,
    ///   then [`Asg::add_edge`] will fail with that error.
    ///
    /// Unlike the type of [`Asg::add_edge`],
    ///   the source [`ObjectIndex`] has been narrowed to the appropriate
    ///   type for you.
    fn pre_add_edge(
        asg: &mut Asg,
        rel: ProposedRel<Self, OB>,
    ) -> Result<(), AsgError>;
}

impl<OA: ObjectRelatable, OB: ObjectRelatable> AsgRelMut<OB> for OA {
    /// Default edge creation method for all [`ObjectKind`]s.
    ///
    /// This takes the place of a default implementation on the trait itself
    ///   above.
    /// It will be invoked any time there is not a more specialized
    ///   implementation.
    /// Note that `object_rel!` doesn't provide method
    ///   definitions unless explicitly specified by the user,
    ///     so this is effective the method called for all edges _unless_
    ///     overridden for a particular edge for a particular object
    ///       (see [`object::tpl`] as an example).
    default fn pre_add_edge(
        _asg: &mut Asg,
        _rel: ProposedRel<Self, OB>,
    ) -> Result<(), AsgError> {
        let _ = _rel.ctx_span; // TODO: remove when used (dead_code)
        Ok(())
    }
}

/// The relationship proposed by [`Asg::add_edge`],
///   requiring approval from [`AsgRelMut::pre_add_edge`].
pub struct ProposedRel<OA: ObjectKind, OB: ObjectKind> {
    from_oi: ObjectIndex<OA>,
    to_oi: ObjectIndex<OB>,
    ctx_span: Option<Span>,
}

#[cfg(test)]
mod test;
