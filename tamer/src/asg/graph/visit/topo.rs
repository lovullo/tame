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
use crate::asg::{graph::object::DynObjectRel, AsgError, Object};
use fixedbitset::FixedBitSet;

pub fn topo_sort(
    asg: &Asg,
    init: impl Iterator<Item = ObjectIndex<Object>>,
) -> TopoPostOrderDfs {
    TopoPostOrderDfs::new(asg, init)
}

/// Topological sort implemented as a post-order depth-first search (DFS).
///
/// See the [module-level documentation](super) for important information
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
    /// The traversal ends once the stack becomes empty.
    /// It is expected the stack is initialized with at least one initial
    ///   object prior to beginning the traversal.
    stack: Vec<ObjectIndex<Object>>,

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
        init.collect_into(&mut stack);

        Self {
            asg,
            stack,
            visited: FixedBitSet::with_capacity(set_cap),
            finished: FixedBitSet::with_capacity(set_cap),
        }
    }
}

impl<'a> Iterator for TopoPostOrderDfs<'a> {
    type Item = Result<ObjectIndex<Object>, AsgError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Rust doesn't have guaranteed TCO as of 2023-04
        loop {
            let next = *self.stack.last()?;

            if self.visited.put(next.into()) {
                self.stack.pop(); // next

                if !self.finished.put(next.into()) {
                    break Some(Ok(next));
                } else {
                    // Must have been visited by another path.
                    continue;
                };
            }

            self.asg
                .edges_dyn(next)
                .map(|dyn_oi| *dyn_oi.target())
                .filter(|&oi| {
                    let finished = self.finished.contains(oi.into());

                    // TODO:
                    let _is_cycle =
                        !finished && self.visited.contains(oi.into());

                    !finished
                })
                .collect_into(&mut self.stack);
        }
    }
}

#[cfg(test)]
mod test;
