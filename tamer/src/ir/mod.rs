// Intermediate representations (IRs)
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

//! Intermediate representations for TAME programs.
//!
//! [Intermediate representations][ir] (IRs) are data structures used to
//!   represent source code in a manner most suitable for a particular phase
//!   of compilation.
//! A single IR may be used by multiple compilation phases,
//!   or by multiple systems (e.g. various compilers or [linkers][]).
//!
//! [ir]: https://en.wikipedia.org/wiki/Intermediate_representation
//! [linkers]: crate::ld
//!
//! Each IR is responsible for raising lower-level IRs or source formats.
//!
//! Summary of IRs
//! --------------
//! Each input language begins as an [abstract syntax tree][ast] (AST),
//!   produced by the parser.
//! For TAME languages that are XML-based,
//!   the production of the AST is handled by [`quick_xml`],
//!     and is effectively the same as the source XML.
//!
//! [ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree

pub mod legacyir;
