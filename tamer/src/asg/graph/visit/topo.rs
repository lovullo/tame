// Topological sort ASG traversal
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

//! Topological sort of [`Asg`] with ontological consideration.
//!
//! This toplogical sort is a depth-first search (DFS) that emits nodes in
//!   post-order.
//! Intuitively,
//!   it emits objects sorted in such a way that they appear before each of
//!   their dependencies.
//!
//! The ordering is deterministic between runs on the same graph,
//!   but it is only one of potentially many orderings.
//!
//! The only information provided by this sort is a stream of
//!   [`ObjectIndex`]es ordered linearly.
//! No information about the edge or source object is provided,
//!   nor is information about the length of the current path,
//!   since an object may be visited any number of different ways and the
//!   caller ought not rely on the particular path taken.
//! Furthermore,
//!   an object may be visited any number of times from any number of paths,
//!   but only the first visit is emitted,
//!     so any additional information would provide an incomplete picture;
//!       this sort is _not_ intended to provide information about all paths
//!       to a particular object and cannot be used in that way.

use super::super::{Asg, ObjectIndex};
use crate::{
    asg::{graph::object::DynObjectRel, Object, ObjectIndexResolvedSpan},
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
};
use fixedbitset::FixedBitSet;
use std::{error::Error, fmt::Display, iter::once};

#[cfg(doc)]
use crate::span::Span;

pub fn topo_sort(
    asg: &Asg,
    init: impl Iterator<Item = ObjectIndex<Object>>,
) -> TopoPostOrderDfs {
    TopoPostOrderDfs::new(asg, init)
}

/// Topological sort implemented as a post-order depth-first search (DFS).
///
/// See the [module-level documentation](self) for important information
///   about this traversal.
pub struct TopoPostOrderDfs<'a> {
    /// Reference [`Asg`].
    ///
    /// Holding a reference to the [`Asg`] allows this object to serve
    ///   conveniently as an iterator.
    asg: &'a Asg,

    /// DFS stack.
    ///
    /// As objects (nodes/vertices) are visited,
    ///   its relationships (edge targets) are pushed onto the stack.
    /// Each iterator pops a relationship off the stack and visits it.
    ///
    /// The inner [`Result`] serves as a cycle flag set by
    ///   [`Self::flag_if_cycle`].
    /// Computing the proper [`Cycle`] error before placing it on the stack
    ///   would not only bloat the size of each element of this stack,
    ///     but also use unnecessary memory on the heap.
    /// The proper [`Cycle`] error will be computed when this element is
    ///   retrieved by [`Self::next_oi`].
    ///
    /// _This may contain duplicate [`ObjectIndex`]es even if the graph
    ///   contains no cycles;_
    ///     see [`Self::push_neighbors`] for an explanation.
    ///
    /// The traversal ends once the stack becomes empty.
    /// It is expected the stack is initialized with at least one initial
    ///   object prior to beginning the traversal.
    stack: Vec<Result<ObjectIndex<Object>, ObjectIndex<Object>>>,

    /// Objects that have already been added to [`Self::stack`].
    ///
    /// An object that has already been visited will _not_ be visited
    ///   again.
    /// A visited object is only present in [`Self::stack`] until it is
    ///   finished,
    ///     after which it appears in [`Self::finished`].
    visited: FixedBitSet,

    /// Objects that have been emitted and pop'd from [`Self::stack`].
    ///
    /// This is used for cycle detection.
    /// Before pushing an object onto [`Self::stack`],
    ///   the system first checks [`Self::visited`].
    /// If an object has been visited,
    ///   but has not yet been finished,
    ///   then it must still be present on the stack and must therefore
    ///   be part of a cycle.
    finished: FixedBitSet,
}

pub trait ObjectRelFilter = Fn(DynObjectRel) -> bool;

/// Initial capacity of the [`TopoPostOrderDfs`] stack.
///
/// The stack will need to be able to accommodate all nodes and their
///   siblings within the longest path taken by the DFS.
/// If there are many rooted objects
///   (e.g. for `tameld`),
///     this may be quite large.
///
/// The current number is arbitrary and only intended to reduce initial
///   small re-allocations;
///     it is too small for linking and too large for individual packages.
const INIT_STACK_CAP: usize = 32;

impl<'a> TopoPostOrderDfs<'a> {
    fn new(
        asg: &'a Asg,
        init: impl Iterator<Item = ObjectIndex<Object>>,
    ) -> Self {
        let set_cap = asg.object_count();

        let mut stack = Vec::with_capacity(INIT_STACK_CAP);
        init.map(Ok).collect_into(&mut stack);

        Self {
            asg,
            stack,
            visited: FixedBitSet::with_capacity(set_cap),
            finished: FixedBitSet::with_capacity(set_cap),
        }
    }

    /// Push the neighbors of the given [`ObjectIndex`] onto [`Self::stack`]
    ///   for later processing.
    ///
    /// Placing neighbors on the stack allows us to yield elements from the
    ///   iterator without having to keep track of where we are on the graph
    ///   for each node in the path.
    ///
    /// When visiting a node for the first time,
    ///   its neighbors
    ///     (objects to which `src_oi` has an edge)
    ///     are pushed onto [`Self::stack`].
    /// It is expected that `src_oi` is left on the stack,
    ///   ensuring that its neighbors are processed before `src_oi` is,
    ///     leading to a post-order traversal.
    ///
    /// Objects that have already been emitted will _not_ be pushed onto the
    ///   stack;
    ///     this determination is made by consulting [`Self::finished`].
    ///
    /// Each object that is pushed onto the stack will be checked by
    ///   [`Self::flag_if_cycle`];
    ///     see that function for more information.
    /// It is important that each cycle be flagged individually,
    ///   rather than returning an error from this function,
    ///   otherwise only one cycle per object would be found.
    ///
    /// Duplicate Stack Entries Without Cycles
    /// ======================================
    /// [`Self::stack`] may contain duplicate [`ObjectIndex`]es even if
    ///   there is no cycle.
    ///
    /// The reason for this is that a cycle only occurs when an
    ///   [`ObjectIndex`] is part of the path currently being visited.
    /// But [`Self::stack`] contains objects that have _not yet been visited_;
    ///   they've been placed onto the stack by this method to be visited at
    ///   a future point.
    ///
    /// Consider this graph:
    ///
    /// ```text
    ///   (A) -> (B) -> (D)
    ///    '---> (C) <---'
    /// ```
    ///
    /// A traversal might yield this stack if `C` is visited before `B`:
    ///
    /// ```text
    ///   [A]              // root
    ///   [A, C, B]        // self.push_neighbors(A)
    ///   [A, C, B, D]     // self.push_neighbors(B)
    ///   [A, C, B, D, C]  // self.push_neighbors(D)
    /// ```
    ///
    /// Since `C` does not contain an edge _to_ any previous object,
    ///   there is no cycle.
    ///
    /// For this reason,
    ///   it is important for the implementation to check [`Self::finished`]
    ///   when removing objects from the stack to ensure that they have not
    ///   already been emitted.
    fn push_neighbors(&mut self, src_oi: ObjectIndex<Object>) {
        self.asg
            .edges_dyn(src_oi)
            .map(|dyn_oi| *dyn_oi.target())
            .filter(|&oi| !self.finished.contains(oi.into()))
            .map(|oi| Self::flag_if_cycle(&self.visited, oi))
            .collect_into(&mut self.stack);
    }

    /// Determine if the provided [`ObjectIndex`] would introduce a cycle if
    ///   appended to the current path and flag it if so.
    ///
    /// This should be called only after having checked [`Self::finished`],
    ///   which means that a node is _not_ in the path because it has
    ///   already been emitted.
    ///
    /// With [`Self::finished`] having been ruled out,
    ///   this uses [`Self::visited`] to determine if a node must be part of
    ///   the active path of the DFS.
    /// If so,
    ///   then introducing it again would produce a cycle.
    ///
    /// We use [`Result`] where `E` is [`ObjectIndex`] to simply flag the
    ///   object as containing a cycle;
    ///     this allows us to defer computation of the cycle and allocation
    ///     of memory for that path until we actually visit the node on
    ///     [`Self::stack`].
    /// This allows the element size of [`Self::stack`] to remain small.
    ///
    /// See [`Self::find_cycle_path`] for the actual cycle computation that
    ///   will eventually be performed.
    fn flag_if_cycle(
        visited: &FixedBitSet,
        oi: ObjectIndex<Object>,
    ) -> Result<ObjectIndex<Object>, ObjectIndex<Object>> {
        if visited.contains(oi.into()) {
            Err(oi)
        } else {
            Ok(oi)
        }
    }

    /// Attempt to retrieve the next [`ObjectIndex`] from the stack for
    ///   processing,
    ///     leaving it on the stack.
    ///
    /// If the object atop of the stack has been flagged as a cycle by
    ///   [`Self::flag_if_cycle`],
    ///     then the actual path associated with the cycle will be computed
    ///     by [`Self::find_cycle_path`] and an a [`Cycle`] returned.
    ///
    /// See also [`Self::pop_next_oi`].
    fn next_oi(&self) -> Option<Result<ObjectIndex<Object>, Cycle>> {
        self.stack
            .last()
            .map(|result| result.map_err(|oi| self.find_cycle_path(oi)))
    }

    /// Remove an item from [`Self::stack`].
    ///
    /// A better API for the future would take ownership over the stack and
    ///   know for certain that the element being removed is the element
    ///   previously returned.
    ///
    /// See also [`Self::next_oi`].
    fn pop_next_oi(&mut self) {
        self.stack.pop();
    }

    /// Knowing that the provided [`ObjectIndex`] would produce a cycle if
    ///   added to the current path,
    ///     calculate the path representing the cycle.
    ///
    /// This is a linear-time (`O(n)`) operation that performs a new heap
    ///   allocation.
    /// Since cycles are an error case,
    ///   it is expected that they will not often occur and so the DFS
    ///   algorithm is optimized for the most common case;
    ///     it is not worth computing the path during the course of the
    ///     search since that path would almost always be discarded.
    ///
    /// Deriving a path relies on understanding that:
    ///
    ///   1. An [`ObjectIndex`] in [`Self::stack`] is either awaiting
    ///        processing or is _currently_ being processed.
    ///      This means that it contains the path,
    ///        but it also contains neighbors of objects in the path.
    ///      We must filter out those neighbors.
    ///
    ///   2. The [`Result`] in [`Self::stack`] indicates whether the object
    ///        causes a cycle.
    ///      A previous object in the path must therefore be [`Ok`],
    ///        otherwise it would not have been traversed,
    ///        and so we must filter all [`Err`]s.
    ///      In doing so,
    ///        we also filter out `next` at the top of the stack,
    ///          and so _this function works correctly regardless of whether
    ///          `next` has already been `pop`'d from the stack_.
    ///
    ///   3. [`Self::visited`] is set just before neighbors of an object are
    ///        pushed onto [`Self::stack`].
    ///      Therefore,
    ///        only objects marked as visited are part of the active path,
    ///          and so to discover that path we need only filter out
    ///          non-visited objects.
    ///
    ///   4. [`Self::stack`] contains a path from a provided root.
    ///      We want to cut off the path at the beginning of the cycle.
    ///      The easiest way to do this is to iterate through the stack in
    ///        reverse,
    ///          stopping as soon as we encounter an [`ObjectIndex`]
    ///          matching `next`.
    ///      This has the effect of producing a cycle path in post-order,
    ///        which is consistent with the ordering of [`Self`]'s
    ///        traversal.
    ///
    ///   5. The [`ObjectIndex`]es sourced from the [`Asg`] do not contain
    ///        the spans of the target objects.
    ///      Cycles will almost certainly result in diagnostic messages,
    ///        which require accurate spans,
    ///        and so we must resolve the [`ObjectIndex`] to retrieve the
    ///        target [`Span`].
    ///
    /// The path produced will therefore be reversed,
    ///   with `next` as the last element.
    /// `next` will _not_ be duplicated as the first element,
    ///   which means that if you were to repeat the returned path
    ///   indefinitely end-to-end
    ///     (e.g. using [`Iterator::cycle`]),
    ///     you would have precisely this cycle.
    ///
    /// With all of that said,
    ///   the implementation is fairly straightforward and concise.
    fn find_cycle_path(&self, next: ObjectIndex<Object>) -> Cycle {
        let mut path = self
            .stack
            .iter()
            .rev()
            .copied()
            .filter_map(Result::ok)
            .take_while(|&oi| oi != next)
            .filter(|&oi| self.visited.contains(oi.into()))
            .map(|oi| oi.resolve_span(self.asg))
            .collect::<Vec<_>>();

        // We stopped _at_ `next`,
        //   so we need to manually add it to the path.
        path.push(next.resolve_span(self.asg));

        Cycle { path }
    }
}

impl<'a> Iterator for TopoPostOrderDfs<'a> {
    type Item = Result<ObjectIndex<Object>, Cycle>;

    fn next(&mut self) -> Option<Self::Item> {
        // Rust doesn't have guaranteed TCO as of 2023-04
        loop {
            match self.next_oi()? {
                Ok(next) => {
                    if self.visited.put(next.into()) {
                        self.pop_next_oi();

                        // See `Self::push_neighbors` for explanation.
                        if !self.finished.put(next.into()) {
                            break Some(Ok(next));
                        }
                    } else {
                        self.push_neighbors(next);
                    }
                }

                Err(cycle) => {
                    self.pop_next_oi();
                    return Some(Err(cycle));
                }
            };
        }
    }
}

/// A graph cycle.
///
/// A cycle means that a path contains a duplicate node,
///   as if it looped back on itself.
/// In terms of TAME,
///   a cycle implies a circular dependency.
///
/// Identifying Cycle Objects
/// =========================
/// TODO: Object names need to be derived from the cycle to display
///   concisely to the user.
/// The cycle very likely contains identifiers that can be used to describe
///   the cycle in more concise terms.
///
/// It used to be the case that cycles contained identifier names,
///   but that was before the topological sort was generalized to include
///   all graph objects;
///     see the commit that introduced this message for more information.
///
/// TODO: We also ought to represent the spans associated with _references_,
///   _in addition to_ just the referenced object.
#[derive(Debug, PartialEq)]
pub struct Cycle {
    /// The path representing the cycle in post-order (reversed).
    ///
    /// It is expected that [`ObjectIndex`]'s associated [`Span`] has been
    ///   resolved to that of the target object
    ///     (e.g. using [`ObjectIndex::resolve_span`]).
    /// This allows the indexes to be useful in a diagnostic context.
    ///
    /// See [`Self::path_rev`] for more information.
    path: Vec<ObjectIndexResolvedSpan<Object>>,
}

impl Cycle {
    /// The path representing the cycle in post-order (reversed).
    ///
    /// The path is truncated such that the first node in the path is the
    ///   beginning of the cycle.
    /// The final node in the cycle is omitted,
    ///   since it is the same as the first;
    ///     if you repeated this path indefinitely
    ///     (e.g. with [`Iterator::cycle`])
    ///     then you would have precisely the cycle.
    ///
    /// The [`ObjectIndex`]es should have [`Span`]s that are resolved
    ///   against the target so that they are useful in a diagnostic
    ///   context.
    pub fn path_rev(&self) -> &Vec<ObjectIndexResolvedSpan<Object>> {
        &self.path
    }
}

impl Display for Cycle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: See note on [`Cycle`] about deriving names.
        write!(f, "[...]")
    }
}

impl Error for Cycle {}

impl Diagnostic for Cycle {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        let path = &self.path;
        let n = path.len();
        let ident = path.last().unwrap();

        // TODO: See note on [`Cycle`] about deriving names.
        path.iter()
            .rev()
            .enumerate()
            .map(|(i, oi)| {
                oi.note(match i {
                    0 => format!(
                        "[0/{n}] the cycle begins here, depending on..."
                    ),
                    // TODO: s/object/<TYPE OF OBJECT>/
                    _ => {
                        format!("[{i}/{n}] ...this object, which depends on...")
                    }
                })
            })
            .chain(once(ident.error(format!(
                "[{n}/{n}] ...the object once again, \
                    creating the cycle"
            ))))
            .collect::<Vec<_>>()
    }
}

#[cfg(test)]
mod test;
