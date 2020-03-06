// Object files
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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

//! Object file construction and processing.
//!
//! An _[object file][]_ contains relocatable compiled code, symbol tables,
//!   and other information produced by the compiler.
//! It is the responsibility of the [linker](super::ld) to construct a final
//!   executable from these files.
//!
//! [object file]: https://en.wikipedia.org/wiki/Object_file
//!
//! The only object file currently supported by TAMER is the [`xmlo`]
//!   format,
//!     produced by the XSLT compiler.
//! It will likely be replaced with [ELF] object files in the future.
//!
//! [ELF]: https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

pub mod xmle;
pub mod xmlo;
