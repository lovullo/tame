// TAME linker
//
//  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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

//! This is the TAME linker, so named after the traditional `ld` Unix
//! utility.  Its job is to take each of the compiled object files and
//! produce a final executable.
//!
//! For more information about the linker,
//!   see the [`tamer::ld`] module.

extern crate tamer;

use std::error::Error;
use tamer::ld::poc;

pub fn main() -> Result<(), Box<dyn Error>> {
    poc::main()
}
