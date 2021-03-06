// xmlo object files
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

//! `xmlo` object file construction and processing.
//!
//! This object file format exists for compatibility with the old compiler
//!   written in XSLT;
//!     it will be removed in the future.
//!
//!
//! `xmlo` Object Files
//! ===================
//! An `xmlo` object file is produced by the for each source file.
//! It is a terribly inefficient object format and will be eliminated in the
//!   future.
//! The format is XML because the original compiler was written in XSLT.
//!
//! The general structure of an `xmlo` file consists of:
//!   - Package metadata as attributes on the root node;
//!   - A symbol table along with symbol metadata;
//!   - Symbol dependencies (as [adjacency lists][]);
//!   - Compiled JavaScript fragments for each applicable symbol; and
//!   - Expanded source XML.
//!
//! [adjacency lists]: https://en.wikipedia.org/wiki/Adjacency_list
//!
//! For example (with some extra information omitted):
//!
//! ```xml
//! <package xmlns="http://www.lovullo.com/rater"
//!          xmlns:preproc="http://www.lovullo.com/rater/preproc"
//!          title="Example Package"
//!          name="example/package"
//!          __rootpath="../"
//!          preproc:elig-class-yields="isEligexamplepackage">
//!   <!-- Symbol table -->
//!   <preproc:symtable>
//!     <preproc:sym name=":class:some-sym" type="class" ... />
//!     <!-- ... -->
//!   </preproc:symtable>
//!
//!   <!-- Dependency graph (adjacency lists) -->
//!   <preproc:sym-deps>
//!     <preproc:sym-dep name=":class:some-sym">
//!       <preproc:sym-ref name="someOtherSym" />
//!       <!-- ... -->
//!     </preproc:sym-dep>
//!   </preproc:sym-deps>
//!
//!   <!-- Compiled JS fragments -->
//!   <preproc:fragments>
//!     <preproc:fragment id=":class:some-sym">
//!       classes['some-sym'] = '...generated JS code...';
//!     </preproc:fragment>
//!   </preproc:fragments>
//!
//!   <!-- Expanded src -->
//! </package>
//! ```
//!
//! IR Lowering
//! ===========
//! `xmlo` files are represented by the [Legacy IR](crate::ir::legacyir),
//!   which is emitted during read by [`AsgBuilder`].
//! For more information about how they are lowered into the
//!   [ASG](crate::ir::asg),
//!     see [`asg_builder`].
//!
//! For a summary of IRs and how they interact,
//!   see [`crate::ir`].

mod asg_builder;
mod reader;

pub use asg_builder::{AsgBuilder, AsgBuilderState};
pub use reader::{XmloError, XmloEvent, XmloReader};
