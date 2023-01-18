// XIR parser generators
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

//! Parser generators for parsing of [XIRF](super::flat).
//!
//! XIRF is chosen as the input IR because it handles some initial
//!   processing of the input XML to ensure well-formedness.

mod attr;
mod ele;
mod error;

pub use attr::{parse_attrs, AttrParseState};
pub use ele::{
    EleParseState, NodeMatcher, Nt, NtState, StateStack, SumNt, SumNtState,
    SuperState, SuperStateContext,
};
pub use error::{AttrParseError, NtError, SumNtError};
