// TAME linker library
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

//! Combine [object files](crate::obj) into a final executable.
//!
//! Its user-facing binary is [`tameld`](../../tameld).
//!
//!
//! Background Information
//! ======================
//! A [linker][] is responsible for combining individually compiled
//!   [object files](crate::obj) containing relocatable code into a final
//!   executable.
//! This involves putting the compiled code fragments into the right order
//!   and into the right place within the executable.
//!
//! [linker]: https://en.wikipedia.org/wiki/Linker_(computing)
//!
//! _See below for more information on why this linker currently produces
//!   another intermediate format (`xmle`) rather than a final executable._
//!
//! The type of relocatable code depends on the _target_.
//! Currently, the only target is JavaScript.
//!
//!
//! Backwards-Compatibility With XSLT System
//! -------------------------------------
//! This linker is part of the TAMER (TAME in Rust) project,
//!   which aims to incrementally rewrite TAME in Rust.
//! Consequently, it must be able to serve as a drop-in replacement for the
//!   existing (XSLT) linker,
//!     which takes as input `xmlo` files and produces as output an `xmle`
//!     file.
//! *This is not efficient*,
//!   and future versions will begin to migrate away from this strategy.
//!
//! The output `xmle` file can then be fed to a `standalone` command which
//!   extracts the JavaScript fragment and places it into its own file.
//! Even when that is replaced
//!   (when this just outputs a final JS file directly),
//!   the `xmle` file is still needed for other purposes,
//!     such as `summary` and `dote` generation.
//! Those too will eventually be linker targets.
//!
//!
//! Linking Process
//! ===============
//! The linker works in the following steps:
//!
//!   1. [Object files](crate::obj) are recursively read.
//!      They are used in a streaming manner for the next step of the
//!        process;
//!          they do not persist in memory.
//!      Only the required portions of the file are loaded.
//!      See [`XmloReader`](crate::obj::xmlo::XmloReader) for more
//!        information.
//!
//!   2. This information is used to populate the [ASG].
//!      Information is added to the graph as it is discovered during object
//!        file loading,
//!          so the graph initially contains edges to missing identifiers.
//!      Expressions are _not_ added to the graph,
//!        as they are not needed for linking.
//!      Once all data are loaded,
//!        the ASG contains relocatable code fragments for each identifier.
//!
//!   3. The ASG is [sorted topologically][topo-sort] so that dependencies
//!        will be written to the executable file before identifiers that
//!        depend on them.
//!      Roots for the sort are specified by the return map.
//!      _Identifiers that are not accessable from one of those roots will be
//!        omitted from the executable output._
//!      This operation is performed by [`xmle::lower::sort`],
//!        producing [`Sections`](xmle::Sections).
//!
//!   4. [`Sections`](xmle::Sections) is then lowered into a XIR token
//!        stream by [`xmle::xir`] for writing.
//!
//! [ASG]: crate::asg
//! [topo-sort]: https://en.wikipedia.org/wiki/Topological_sorting
//!
//! Steps 1 and 2 are performed at the same time:
//!   object files are used to immediately populate the [ASG][].
//! Since the ASG contains only partial information,
//!   it must perform other validations (such as extern resolution) during
//!   this process;
//!     see [crate::asg] for more information.
//!
//! Because the topological sort only considered explicitly defined roots,
//!   identifiers are only included in the final executable if they are
//!   either a root or are a dependency of a root.
//! This makes it possible to create large reusable packages without
//!   incurring a runtime cost for unused objects,
//!     which is especially important since templates may expand into many
//!     identifiers.

use crate::span::Span;
use crate::sym::st16;

pub mod poc;
pub mod xmle;

/// Span denoting a general linker operation.
///
/// This span may be used when the source of a given object is the linker
///   and it is not possible to derive a more useful span.
pub const LSPAN: Span = Span::st_ctx(st16::CTX_LINKER);
