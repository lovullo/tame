// ASG traversals
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

//! Graph traversals.
//!
//! The traversal [`tree_reconstruction`] should be used if the intent is to
//!   reconstruct a source representation of the program from the current
//!   state of [`Asg`].

use std::fmt::Display;

use super::{object::DynObjectRel, Asg, Object, ObjectIndex};
use crate::{
    parse::Token,
    span::{Span, UNKNOWN_SPAN},
};

// Re-export so that users of this API can avoid an awkward import from a
//   completely different module hierarchy.
pub use crate::xir::flat::Depth;

#[cfg(doc)]
use super::object::ObjectRel;

/// Produce an iterator suitable for reconstructing a source tree based on
///   the contents of the [`Asg`].
///
/// The implementation of this traversal is exceedingly simple because of
///   its reliance on important graph invariants,
///     but it embodies a number of important and subtle properties.
///
/// Traversal Properties
/// ====================
/// This is a [depth-first search][w-depth-first-search]
///   visiting all nodes that are _reachable_ from the graph root
///     (see [`Asg::root`]).
/// [`ObjectIndex`]es are emitted in pre-order during the traversal,
///   and may be emitted more than once if
///     (a) they are the destination of cross edges or
///     (b) they are shared between trees
///           (most likely due to compiler optimizations).
///
/// The tree is defined by the graph ontology,
///   not an arbitrary graph traversal.
/// This traversal is initialized by pushing each target [`ObjectIndex`] of
///   the ASG root
///     (see [`Asg::root`])
///   onto the stack.
/// Each iteration pops a single node off of the stack and visits it,
///   until no more nodes remain on the stack,
///   after which the traversal completes and the iterator is exhausted.
/// If the node was reached via a tree edge,
///   its edge targets are pushed onto the stack.
/// If a node is a target of a cross edge,
///   its edges targets are _not_ added to the stack for later traversal.
///
/// Targets of a cross edge
///   (see [`ObjectRel::is_cross_edge`])
///   will be emitted multiple times:
///
///   1. The target of a cross edge is emitted each time a cross edge is
///        followed; and
///   2. When the node is encountered on a tree edge.
///
/// The traversal relies on the ontology to enforce a tree-like structure
///   and to properly define cross edges via `ObjectRel::is_cross_edge`.
/// A _tree edge_ is an edge that is not a cross edge.
/// Consequently,
///   if a cross edge is replaced by a tree edge,
///   then this traversal interprets that edge as part of _multiple_ trees,
///     effectively inlining it as if the user had entered the exact same
///     code in both locations.
/// You should choose carefully where in the lowering pipeline you wish
///   for this traversal to take place so that the tree reconstruction has
///   the desired properties.
///
/// Because the graph is expected to be a DAG
///   (directed acyclic graph),
///     this traversal _does not track visited nodes_;
///       this ensures that nodes shared by trees due to optimizations like
///       common subexpression elimination will have proper trees
///       reconstructed.
/// If there are exceptional subgraphs where cycles do appear,
///   this traversal's implementation must be modified to take them into
///     account,
///   otherwise it will iterate indefinitely.
///
/// Edges are visited in the same order that they were added to the graph,
///   so the tree reconstruction should match closely the order of the
///   source file.
/// However,
///   note that compiler passes,
///     if present,
///     may modify the graph beyond recognition,
///       though they should retain ordering where it is important.
///
/// For more information,
///   see [`ObjectRel::is_cross_edge`].
///
/// [w-depth-first-search]: https://en.wikipedia.org/wiki/Depth-first_search
///
/// Depth Tracking
/// ==============
/// Each [`ObjectIndex`] emitted by this traversal is accompanied by a
///   [`Depth`] representing the length of the current path relative to the
///   [`Asg`] root.
/// Since the ASG root is never emitted,
///   the [`Depth`] value will always be ≥1.
/// Because nodes are always visited when an edge is followed,
///   a lower [`Depth`] will always be emitted prior to switching tree
///   branches.
///
/// Let _S_ be an undirected spanning tree formed from the ontological tree.
/// At each iteration,
///   one of the following will be true:
///
///   1. [`Depth`] will increase by 1,
///        representing a tree edge or a cross edge;
///   2. [`Depth`] will remain unchanged from the previous iteration,
///        representing a sibling node in the tree; or
///   3. [`Depth`] will decrease by ≥1,
///        representing a back edge to an ancestor node on _S_.
///
/// This depth information is the only means by which to reconstruct the
///   structure of the tree from the emitted [`ObjectIndex`]es.
/// For example,
///   if you are producing output in a nested format like XML,
///     an unchanged depth means that the current element should be closed
///       and a new one opened,
///     and you will close _one or more_ elements on a back edge.
///
/// Note that,
///   because the [`Depth`] represents the current _path_,
///   the same [`ObjectIndex`] may be emitted multiple times with different
///   [`Depth`]s.
pub fn tree_reconstruction(asg: &Asg) -> TreePreOrderDfs {
    TreePreOrderDfs::new(asg)
}

/// Pre-order depth-first search (DFS) using the ontological tree.
///
/// This DFS has an interesting property:
///   _it does not track visited nodes_,
///     relying instead on the ontology and recognition of cross edges to
///     produce the intended spanning tree.
/// An [`ObjectIndex`] that is the target of a cross edge will be output
///   more than once.
///
/// See [`tree_reconstruction`] for more information.
pub struct TreePreOrderDfs<'a> {
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
}

/// Initial size of the DFS stack for [`TreePreOrderDfs`].
///
/// TODO: Derive a heuristic from our systems.
const TREE_INITIAL_STACK_SIZE: usize = 8;

impl<'a> TreePreOrderDfs<'a> {
    fn new(asg: &'a Asg) -> Self {
        let span = UNKNOWN_SPAN;

        let mut dfs = Self {
            asg,
            stack: Vec::with_capacity(TREE_INITIAL_STACK_SIZE),
        };

        let root = asg.root(span);
        dfs.push_edges_of(root.widen(), Depth::root());
        dfs
    }

    fn push_edges_of(&mut self, oi: ObjectIndex<Object>, depth: Depth) {
        self.asg
            .edges_dyn(oi)
            .map(|rel| (rel, depth.child_depth()))
            .collect_into(&mut self.stack);
    }
}

impl<'a> Iterator for TreePreOrderDfs<'a> {
    type Item = TreeWalkRel;

    /// Produce the next [`ObjectIndex`] from the traversal in pre-order.
    ///
    /// An [`ObjectIndex`] may be emitted more than once;
    ///   see [`tree_reconstruction`] for more information.
    ///
    /// Each item contains a corresponding [`Depth`],
    ///   which represents the depth of the tree derived from the ASG,
    ///     _not_ the level of nesting of the source language used to
    ///     populate the graph.
    /// This depth is the only way to derive the tree structure from this
    ///   iterator.
    fn next(&mut self) -> Option<Self::Item> {
        let (rel, depth) = self.stack.pop()?;

        // We want to output information about references to other trees,
        //   but we must not traverse into them.
        if !rel.is_cross_edge() {
            self.push_edges_of(*rel.target(), depth);
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

#[cfg(test)]
mod test;
