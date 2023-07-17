// Ontological tree preorder ASG traversal
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

//! Preorder traversal of ontological tree.
//!
//! This traversal is instantiated by [`tree_reconstruction`].
//!
//! This traversal should be used to reconstruct a source representation of
//!   the compilation unit from the current state of the [`Asg`].
//!
//! Traversal Properties
//! ====================
//! This is a [depth-first search][w-depth-first-search]
//!   visiting all nodes that are _reachable_ from the graph root
//!     (see [`Asg::root`]).
//! [`TreeWalkRel`]s are emitted in pre-order during the traversal,
//!   and may be emitted more than once if
//!     (a) they are the destination of cross edges or
//!     (b) they are shared between trees
//!           (most likely due to compiler optimizations).
//!
//! The tree is defined by the graph ontology,
//!   not an arbitrary graph traversal.
//! This traversal is initialized by pushing each target [`ObjectIndex`] of
//!   the ASG root
//!     (see [`Asg::root`])
//!   onto the stack.
//! Each iteration pops a single node off of the stack and visits it,
//!   until no more nodes remain on the stack,
//!   after which the traversal completes and the iterator is exhausted.
//! If the node was reached via a tree edge,
//!   its edge targets are pushed onto the stack.
//! If a node is a target of a cross edge,
//!   its edges targets are _not_ added to the stack for later traversal.
//!
//! Targets of a cross edge
//!   (see [`ObjectRel::is_cross_edge`])
//!   will be emitted multiple times:
//!
//!   1. The target of a cross edge is emitted each time a cross edge is
//!        followed; and
//!   2. When the node is encountered on a tree edge.
//!
//! The traversal relies on the ontology to enforce a tree-like structure
//!   and to properly define cross edges via `ObjectRel::is_cross_edge`.
//! A _tree edge_ is an edge that is not a cross edge.
//! Consequently,
//!   if a cross edge is replaced by a tree edge,
//!   then this traversal interprets that edge as part of _multiple_ trees,
//!     effectively inlining it as if the user had entered the exact same
//!     code in both locations.
//! You should choose carefully where in the lowering pipeline you wish
//!   for this traversal to take place so that the tree reconstruction has
//!   the desired properties.
//!
//! Because the graph is expected to be a DAG
//!   (directed acyclic graph),
//!     this traversal _does not track visited nodes_;
//!       this ensures that nodes shared by trees due to optimizations like
//!       common subexpression elimination will have proper trees
//!       reconstructed.
//! If there are exceptional subgraphs where cycles do appear,
//!   this traversal's implementation must be modified to take them into
//!     account,
//!   otherwise it will iterate indefinitely.
//!
//! Edges are visited in the same order that they were added to the graph,
//!   so the tree reconstruction should match closely the order of the
//!   source file.
//! However,
//!   note that compiler passes,
//!     if present,
//!     may modify the graph beyond recognition,
//!       though they should retain ordering where it is important.
//!
//! _Objects that do not have a path from the root will not be visited by
//!   this traversal._
//! These objects are expected to act as additional metadata,
//!   and must be queried for explicitly.
//! Such querying can be done during the traversal since this visitor holds
//!   only a shared immutable reference to the [`Asg`].
//!
//! For more information,
//!   see [`ObjectRel::is_cross_edge`].
//!
//! [w-depth-first-search]: https://en.wikipedia.org/wiki/Depth-first_search
//!
//! Depth Tracking
//! ==============
//! Each [`TreeWalkRel`] emitted by this traversal is accompanied by a
//!   [`Depth`] representing the length of the current path relative to the
//!   [`Asg`] root.
//! Since the ASG root is never emitted,
//!   the [`Depth`] value will always be ≥1.
//! Because nodes are always visited when an edge is followed,
//!   a lower [`Depth`] will always be emitted prior to switching tree
//!   branches.
//!
//! Let _S_ be an undirected spanning tree formed from the ontological tree.
//! At each iteration,
//!   one of the following will be true:
//!
//!   1. [`Depth`] will increase by 1,
//!        representing a tree edge or a cross edge;
//!   2. [`Depth`] will remain unchanged from the previous iteration,
//!        representing a sibling node in the tree; or
//!   3. [`Depth`] will decrease by ≥1,
//!        representing a back edge to an ancestor node on _S_.
//!
//! This depth information is the only means by which to reconstruct the
//!   structure of the tree from the emitted [`ObjectIndex`]es.
//! For example,
//!   if you are producing output in a nested format like XML,
//!     an unchanged depth means that the current element should be closed
//!       and a new one opened,
//!     and you will close _one or more_ elements on a back edge.
//!
//! Note that,
//!   because the [`Depth`] represents the current _path_,
//!   the same [`ObjectIndex`] may be emitted multiple times with different
//!   [`Depth`]s.
//!
//!
//! Edge Order
//! ==========
//! The order of edges in the tree is important,
//!   since there are a number of non-commutative operations in TAME.
//! Ordering is determined by a [`TreeEdgeOrder`] strategy:
//!
//!   1. [`NaturalTreeEdgeOrder`] will traverse in the same order that edges
//!        were added to the graph.
//!      This ordering is fine for most internal operations,
//!        but is not suitable for [`tree_reconstruction`].
//!
//!   2. [`SourceCompatibleTreeEdgeOrder`] traverses edges in an order that
//!        will produce a valid source file for [NIR XML](crate::nir).
//!      For example,
//!        templates require a header and body section,
//!          where [`Asg`] permits mixing them.
//!      This maintains natural order in all other cases.

use std::{fmt::Display, marker::PhantomData};

use super::super::{
    object::{self, DynObjectRel},
    Asg,
};
use crate::{
    asg::graph::object::ObjectTy,
    parse::{self, Token},
    span::{Span, UNKNOWN_SPAN},
};

// Re-export so that users of this API can avoid an awkward import from a
//   completely different module hierarchy.
pub use crate::xir::flat::Depth;

#[cfg(doc)]
use super::super::object::{ObjectIndex, ObjectRel};

pub use order::*;

/// Produce an iterator suitable for reconstructing a source tree based on
///   the contents of the [`Asg`].
///
/// The implementation of this traversal is exceedingly simple because of
///   its reliance on important graph invariants,
///     but it embodies a number of important and subtle properties.
///
/// See the [module-level documentation](super) for important information
///   about this traversal.
pub fn tree_reconstruction(
    asg: &Asg,
) -> TreePreOrderDfs<SourceCompatibleTreeEdgeOrder> {
    TreePreOrderDfs::new(asg)
}

/// Pre-order depth-first search (DFS) using the ontological tree.
///
/// This DFS has an interesting property:
///   _it does not track visited nodes_,
///     relying instead on the ontology and recognition of cross edges to
///     produce the intended spanning tree.
/// An object that is the target of a cross edge will be output
///   more than once.
///
/// See [`tree_reconstruction`] for more information.
pub struct TreePreOrderDfs<'a, O: TreeEdgeOrder> {
    /// Reference [`Asg`].
    ///
    /// Holding a reference to the [`Asg`] allows us to serve conveniently
    ///   as an iterator.
    asg: &'a Asg,

    /// DFS stack.
    ///
    /// As objects (nodes/vertices) are visited,
    ///   its relationships (edges) are pushed onto the stack.
    /// Each iterator pops a relationship off the stack and visits it.
    ///
    /// The traversal ends once the stack becomes empty.
    stack: Vec<(DynObjectRel, Depth)>,

    _phantom: PhantomData<O>,
}

/// Initial size of the DFS stack for [`TreePreOrderDfs`].
///
/// TODO: Derive a heuristic from our systems.
const TREE_INITIAL_STACK_SIZE: usize = 8;

impl<'a, O: TreeEdgeOrder> TreePreOrderDfs<'a, O> {
    fn new(asg: &'a Asg) -> Self {
        let span = UNKNOWN_SPAN;

        let mut dfs = Self {
            asg,
            stack: Vec::with_capacity(TREE_INITIAL_STACK_SIZE),
            _phantom: PhantomData,
        };

        let root = asg.root(span);
        let root_rel = DynObjectRel::new(
            root.rel_ty(),
            root.rel_ty(),
            root.widen(),
            root.widen(),
            None,
        );

        dfs.push_edges_of(&root_rel, Depth::root());
        dfs
    }

    fn push_edges_of(&mut self, rel: &DynObjectRel, depth: Depth) {
        O::push_edges_of(self.asg, rel, depth, &mut self.stack)
    }
}

impl<'a, O: TreeEdgeOrder> Iterator for TreePreOrderDfs<'a, O> {
    type Item = TreeWalkRel;

    /// Produce the next [`TreeWalkRel`] from the traversal in pre-order.
    ///
    /// An object may be emitted more than once;
    ///   see [`tree_reconstruction`] for more information.
    ///
    /// Each item contains a corresponding [`Depth`],
    ///   which represents the depth of the tree derived from the ASG,
    ///     _not_ the level of nesting of the source language used to
    ///     populate the graph.
    /// This depth is the only way to derive the tree structure from this
    ///   iterator.
    fn next(&mut self) -> Option<Self::Item> {
        // Note that we pushed edges in the order that `Asg` provided,
        //   and now pop them,
        //   which causes us to visit the edges in reverse.
        // Because of implementation details (Petgraph),
        //   this reversal ends up giving us the correct ordering.
        let (rel, depth) = self.stack.pop()?;

        // We want to output information about references to other trees,
        //   but we must not traverse into them.
        if !rel.is_cross_edge() {
            self.push_edges_of(&rel, depth);
        }

        Some(TreeWalkRel(rel, depth))
    }
}

#[derive(Debug, PartialEq)]
pub struct TreeWalkRel(pub DynObjectRel, pub Depth);

impl Display for TreeWalkRel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(dyn_rel, depth) => {
                write!(f, "{dyn_rel} at tree depth {depth}")
            }
        }
    }
}

impl Token for TreeWalkRel {
    fn ir_name() -> &'static str {
        "ASG ontological tree pre-order DFS walk"
    }

    /// Token context span.
    ///
    /// Note that this is _not_ the same span as other token
    ///   implementations,
    ///     and may default to [`UNKNOWN_SPAN`].
    /// This is because the token is derived from the relationships on the
    ///   graph,
    ///     while concrete spans are stored on the objects that those
    ///     relationships reference.
    /// This will return a potentially-useful span only if the inner
    ///   [`DynObjectRel::ctx_span`] does.
    fn span(&self) -> Span {
        match self {
            Self(dyn_rel, _) => dyn_rel.ctx_span().unwrap_or(UNKNOWN_SPAN),
        }
    }
}

impl parse::Object for TreeWalkRel {}

mod order {
    use super::*;

    /// Emit edges in the same order that they were added to the graph.
    ///
    /// Various parts of the system take care in what order edges are
    ///   added.
    /// This ordering is important for operations that are not commutative.
    pub struct NaturalTreeEdgeOrder;

    /// Emit edges in as close to [`NaturalTreeEdgeOrder`] as possible,
    ///   sorting only where object ordering would otherwise be
    ///   syntactically or grammatically invalid for streaming source
    ///   generation.
    ///
    /// Unless otherwise mentioned below,
    ///   ordering for objects will be the same as
    ///   [`NaturalTreeEdgeOrder`].
    ///
    /// Template Headers
    /// ----------------
    /// For [NIR XML](crate::nir) sources for TAME,
    ///   templates require that parameters be placed in a header,
    ///     before all objects representing the body of the template.
    /// This is necessary to disambiguate `<param>` nodes in sources,
    ///   even though no such ambiguity exists on the [`Asg`].
    ///
    /// All metavariables representing template params will be hoisted into
    ///   the header,
    ///     immediately after any template description [`Doc`](object::Doc)
    ///     node.
    /// This is a stable partial ordering:
    ///   the ordering of parameters relative to one-another will not change,
    ///   nor will the order of any objects in the body of the template.
    /// See [`TplOrder`].
    pub struct SourceCompatibleTreeEdgeOrder;

    /// Order in which tree edges are emitted.
    ///
    /// TAME is sensitive to edge ordering for non-commutative operations.
    /// For source lk
    pub trait TreeEdgeOrder {
        /// Push the edges of `rel` onto the `target` stack.
        ///
        /// The system will pop edges off of `target` to determine what edge
        ///   to traverse next.
        /// This means that edges will be visited in an order that is the
        ///   reverse of the elements of `target`.
        fn push_edges_of(
            asg: &Asg,
            rel: &DynObjectRel,
            depth: Depth,
            target: &mut Vec<(DynObjectRel, Depth)>,
        );
    }

    impl TreeEdgeOrder for NaturalTreeEdgeOrder {
        fn push_edges_of(
            asg: &Asg,
            rel: &DynObjectRel,
            depth: Depth,
            stack: &mut Vec<(DynObjectRel, Depth)>,
        ) {
            let oi = *rel.target();

            asg.edges_dyn(oi)
                .map(|rel| (rel, depth.child_depth()))
                .collect_into(stack);
        }
    }

    /// Ordering of template children for [`SourceCompatibleTreeEdgeOrder`].
    ///
    /// Since these edges are added to a _stack_,
    ///   larger values will be `pop`'d _before_ smaller ones,
    ///   as demonstrated by the variant order.
    /// That is:
    ///   we sort in the reverse order that we will visit them.
    ///
    /// ```text
    ///   ------> Sort direction
    ///   [ Body, Body, Param, Param, Desc ]
    ///              <------ visit direction
    ///                      (pop from stack)
    /// ```
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
    enum TplOrder {
        /// Forcing the template description to come first mimics
        ///   the expected [`NaturalTreeEdgeOrder`].
        ///
        /// We don't want to re-order things before it,
        ///   since we want to be able to stream source output,
        ///     e.g. `template/@desc` in XML.
        TplDesc = 2,
        /// Template parameters must appear in the template
        ///   "header",
        ///     which is all the nodes before the body that is to be
        ///     expanded on application.
        Param = 1,
        /// The body of the template is anything that is not part of
        ///   the header.
        ///
        /// The body represents the contents of the template that
        ///   will be expanded in place of any template application.
        Body = 0,
    }

    impl TreeEdgeOrder for SourceCompatibleTreeEdgeOrder {
        fn push_edges_of(
            asg: &Asg,
            rel: &DynObjectRel,
            depth: Depth,
            stack: &mut Vec<(DynObjectRel, Depth)>,
        ) {
            use ObjectTy::*;

            // We start by adding edges to the stack in natural order,
            //   remembering the original stack offset so that we can sort
            //   just the portion that we added.
            let offset = stack.len();
            NaturalTreeEdgeOrder::push_edges_of(asg, rel, depth, stack);

            match rel.target_ty() {
                // Templates require partial ordering into a header and a body.
                Tpl => {
                    // This represents the portion of the stack that we just
                    //   contributed to via [`NaturalTreeEdgeOrder`] above.
                    let part = &mut stack[offset..];

                    // TODO: Ideally we'd have metadata on the edge itself
                    //     about what type of object an `Ident` points to,
                    //       so that we can use the faster `sort_by_key`.
                    //   With that said,
                    //     initial profiling on large packages with many
                    //     template applications did not yield a
                    //     significant difference between the two methods on
                    //     system-level tests,
                    //       and given the small number of template
                    //       children,
                    //         this consideration may be a micro-optimization.
                    //   An unstable sort is avoided because we wish to
                    //     retain natural ordering as much as possible.
                    //
                    // TODO: In practice,
                    //     most of these are template _applications_ resulting
                    //     from `tplshort` desugaring.
                    //   At the time of writing,
                    //     _all_ need sorting because `Ref` is output before
                    //     the params.
                    //   We could recognize them as template applications at
                    //     some point and leave their order alone,
                    //       but at the time of writing we have no imports,
                    //         and so most refs are `Ident::Missing` in
                    //         practice.
                    //   Once template imports are taken care of,
                    //     then _nearly every single `Tpl` in practice` will
                    //     already be ordered and this will rarely have any
                    //     reordering to do
                    //       (just hoisting for interpolation in template
                    //          definitions,
                    //            which are not all that common relative to
                    //            everything else).
                    part.sort_by_cached_key(|(child_rel, _)| {
                        if let Some(ident) =
                            child_rel.filter_into_target::<object::Ident>()
                        {
                            // This is the (comparatively) expensive lookup,
                            //   requiring a small graph traversal.
                            match ident.definition::<object::Meta>(asg) {
                                Some(_) => TplOrder::Param,
                                None => TplOrder::Body,
                            }
                        } else if child_rel.target_ty() == Doc {
                            TplOrder::TplDesc
                        } else {
                            TplOrder::Body
                        }
                    });
                }

                // Leave natural (graph) ordering for everything else.
                Root | Pkg | Ident | Expr | Meta | Doc => (),
            }
        }
    }
}

#[cfg(test)]
mod test;
