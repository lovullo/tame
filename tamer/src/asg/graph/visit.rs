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

use super::{
    object::{is_dyn_cross_edge, ObjectRelTy},
    Asg, Object, ObjectIndex,
};
use crate::span::UNKNOWN_SPAN;

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
    /// The tuple represents the source and target edge [`ObjectRelTy`]s
    ///   respectively,
    ///     along with the [`ObjectIndex`] to be visited.
    /// As nodes are visited,
    ///   its edges are pushed onto the stack.
    /// Each iterator pops a tuple off the stack and visits that node.
    ///
    /// The traversal ends once the stack becomes empty.
    stack: Vec<(ObjectRelTy, ObjectRelTy, ObjectIndex<Object>)>,
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
        dfs.push_edges_of(root.rel_ty(), root.widen());
        dfs
    }

    fn push_edges_of(&mut self, from_ty: ObjectRelTy, oi: ObjectIndex<Object>) {
        self.asg
            .edges_dyn(oi)
            .map(|(rel_ty, oi)| (from_ty, rel_ty, oi))
            .collect_into(&mut self.stack);
    }
}

impl<'a> Iterator for TreePreOrderDfs<'a> {
    type Item = ObjectIndex<Object>;

    /// Produce the next [`ObjectIndex`] from the traversal in pre-order.
    ///
    /// An [`ObjectIndex`] may be emitted more than once;
    ///   see [`tree_reconstruction`] for more information.
    fn next(&mut self) -> Option<Self::Item> {
        let (from_ty, next_ty, next) = self.stack.pop()?;

        // We want to output information about references to other trees,
        //   but we must not traverse into them.
        if !is_dyn_cross_edge(from_ty, next_ty, next) {
            self.push_edges_of(next_ty, next);
        }

        Some(next)
    }
}

#[cfg(test)]
mod test;
