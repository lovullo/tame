// Wrappers around third-party modules
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

//! Wrappers around third-party modules to make them play nicely with TAMER.
//!
//! Some third-party libraries provide interfaces that present integration issues.
//! Those are often addressed in the context that they are used,
//!   but sometimes those adapters need to be shared across different parts
//!     of the system.
//! They live here.

pub mod quick_xml;
