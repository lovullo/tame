// General numeric types for TAMER
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

//! General numeric types used throughout the system.

use crate::sym::{st, SymbolId};

/// Value dimensionality.
///
/// This indicates the number of subscripts needed to access a scalar
///   value.
/// This the value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Dim {
    Scalar = 0,
    Vector = 1,
    Matrix = 2,
}

assert_eq_size!(Option<Dim>, Dim);

impl From<Dim> for u8 {
    fn from(dim: Dim) -> Self {
        dim as u8
    }
}

impl From<Dim> for SymbolId {
    fn from(dim: Dim) -> Self {
        st::decimal1(dim as u8).as_sym()
    }
}

impl std::fmt::Display for Dim {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        (*self as u8).fmt(fmt)
    }
}

/// Machine representation of scalar data.
///
/// _NB: This was not enforced by the XSLT-based compiler._
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Dtype {
    /// {⊥,⊤} = {0,1} ⊂ ℤ
    Boolean,
    /// ℤ
    Integer,
    /// ℝ
    Float,
    /// ∅
    Empty,
}

impl Dtype {
    pub fn as_sym(&self) -> SymbolId {
        match self {
            Dtype::Boolean => st::L_BOOLEAN,
            Dtype::Integer => st::L_INTEGER,
            Dtype::Float => st::L_FLOAT,
            Dtype::Empty => st::L_EMPTY,
        }
        .as_sym()
    }
}

impl From<Dtype> for SymbolId {
    fn from(val: Dtype) -> Self {
        val.as_sym()
    }
}

impl std::fmt::Display for Dtype {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.as_sym())
    }
}
