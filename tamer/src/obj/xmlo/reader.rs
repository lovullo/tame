// XIR-based xmlo object file reader
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

#[cfg(feature = "wip-xmlo-xir-reader")]
use std::io::BufRead;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
mod quickxml;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
pub use quickxml::{XmloError, XmloEvent, XmloReader, XmloResult};

#[cfg(feature = "wip-xmlo-xir-reader")]
pub struct XmloReader<B: BufRead> {
    todo: std::marker::PhantomData<B>,
}
