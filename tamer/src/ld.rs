// TAME linker library
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

//! The [linker][] is responsible for combining individually compiled
//!   [object files][] into a final executable.
//!
//! It's user-facing binary is [`tameld`][tameld].
//!
//! **TODO:** More information.
//!
//! [linker]: https://en.wikipedia.org/wiki/Linker_(computing)
//! [object files]: https://en.wikipedia.org/wiki/Object_file
//! [tameld]: ../../tameld
//!
//! Backwards-Compatibility (XSLT System)
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

pub mod poc;
