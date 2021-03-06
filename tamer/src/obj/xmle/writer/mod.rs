// IdentObject file writer
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

//! xmle file writer.
//!
//! This defines a lower-level event-based `XmleWriter` similar to that of
//!   `quick_xml`, where the events are a slightly higher-level abstraction
//!   over the types of nodes present in the file.
//!
//! For more information on xmle files, see the [parent crate][`super`].
//!
//! The example below is incomplete, but shows the general usage.
//!
//! ```
//! use tamer::obj::xmle::writer::XmleWriter;
//! use tamer::ir::asg::{IdentObject, Sections};
//! use tamer::sym::{DefaultInterner, Interner, Symbol};
//! use std::io::Cursor;
//!
//! let interner = DefaultInterner::new();
//! let name = interner.intern(&String::from("foo"));
//!
//! let sections = Sections::<IdentObject>::new();
//! let writer = Cursor::new(Vec::new());
//! let mut xmle_writer = XmleWriter::new(writer);
//! xmle_writer.write(&sections, name, &String::from(""));
//! ```

mod writer;
mod xmle;

pub use writer::{Result, Writer, WriterError};

pub use xmle::XmleWriter;
