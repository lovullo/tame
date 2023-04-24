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
//!   reconstruct a source representation of a compilation unit from the current
//!   state of the [`Asg`](super::Asg).

mod ontree;

pub use ontree::{tree_reconstruction, Depth, TreePreOrderDfs, TreeWalkRel};
