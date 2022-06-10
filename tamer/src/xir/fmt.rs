// XIR formatting
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

//! XIR formatting types for use with [`crate::fmt`]

use crate::fmt::{AndQualConjList, Prefix, Raw, Tt};

/// Denote an XML attribute by prefixing the value with `@`.
pub type XmlAttr = Prefix<"@", Raw>;

/// A list of XML attributes [`Tt`]-quoted.
pub type XmlAttrList = AndQualConjList<"attribute", "attributes", Tt<XmlAttr>>;
