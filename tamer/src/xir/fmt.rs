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

use crate::fmt::{AndQualConjList, Delim, OrQualConjList, Prefix, Raw, Tt};

/// Denote an XML attribute by prefixing the value with `@`.
pub type XmlAttr = Prefix<"@", Raw>;

/// A list of XML attributes [`Tt`]-quoted.
pub type XmlAttrList = AndQualConjList<"attribute", "attributes", Tt<XmlAttr>>;

/// Opening tag for XML element.
pub type OpenXmlEle = Delim<"<", ">", Raw>;

/// Opening tag for XML element as teletypewriter
///   (for use in sentences).
pub type TtOpenXmlEle = Tt<OpenXmlEle>;

/// A choice of a list of opening XML tags.
pub type OpenEleSumList =
    OrQualConjList<"element", "one of elements", TtOpenXmlEle>;
