// Intermediate representations (IRs)
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

//! Intermediate representations for TAME programs.
//!
//! [Intermediate representations][ir] (IRs) are data structures used to
//!   represent source data in a manner most suitable for a particular phase
//!   of compilation.
//! A single IR may be used by multiple compilation phases,
//!   or by multiple systems (e.g. various compilers or [linkers][]).
//!
//! [ir]: https://en.wikipedia.org/wiki/Intermediate_representation
//! [linkers]: crate::ld
//!
//!
//! Implicit AST
//! ============
//! Each input language begins as an [abstract syntax tree][ast] (AST),
//!   produced by the parser.
//! For TAME languages that are XML-based,
//!   the production of the AST is handled by [`quick_xml`],
//!     and is effectively the same as the source XML.
//! There is no explicit data structure to represent the AST of XML
//!   sources.
//!
//! [ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
//!
//!
//! Summary of IRs
//! ==============
//! There are currently two IRs:
//!
//!   1. **[Legacy IR](legacyir)** corresponds very closely to the structure
//!        of [`xmlo` object files](super::obj::xmlo).
//!      It contains a lot of cruft and will be replaced in the future with
//!        a more suitable IR.
//!      This stores very limited context for the information it provides,
//!        so it must quickly translate it to a higher-level IR for further
//!        processing before context is lost.
//!   2. The **[Abstract Semantic Graph (ASG)](asg)** is created from
//!        lower-level IRs.
//!      It stores relationships between identifiers and expressions within
//!        a graph data structure,
//!          and is capable of representing entire programs composed of many
//!          different packages.
//!
//! Lowering
//! ========
//! IRs are progressively _lowered_ to other IRs that are closer to the
//!   final representation emitted by the compiler ("lower"-level).
//!
//! - [`xmlo::XmloReader`](crate::obj::xmlo::XmloReader) produces
//!     [`XmloEvent`](crate::obj::xmlo::XmloEvent)s containing
//!     [`legacyir`].
//!   - [`xmlo::AsgBuilder`](crate::obj::xmlo::AsgBuilder) immediately lowers
//!       those into [`asg`].

pub mod asg;
pub mod legacyir;
pub mod xir;
