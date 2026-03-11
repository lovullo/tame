// XIR formatting
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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

use crate::fmt::{
    AndQualConjList, Delim, OrQualConjList, Prefix, Raw, Suffix, Tt, TtQuote,
};

/// Denote an XML attribute by prefixing the value with `@`.
pub type XmlAttr = Prefix<"@", Raw>;

/// [`XmlAttr`] formatted as teletypewriter
///   (for use in sentences).
pub type TtXmlAttr = Tt<XmlAttr>;

/// A list of XML attributes [`Tt`]-quoted.
pub type XmlAttrList = AndQualConjList<"attribute", "attributes", Tt<XmlAttr>>;

/// Opening tag for XML element.
pub type OpenXmlEle = Delim<"<", ">", Raw>;

/// Opening tag for XML element.
pub type CloseXmlEle = Delim<"</", ">", Raw>;

/// "`ns:*`" given a namespace prefix `ns`.
///
/// TODO: It'd be nice to be able to have Raw require a specific type to
///   ensure that we're given a prefix.
pub type XmlPrefixAnyLocal = Suffix<":*", Raw>;

/// Opening tag for XML element as teletypewriter
///   (for use in sentences).
pub type TtOpenXmlEle = Tt<OpenXmlEle>;

/// Closing tag for XML element as teletypewriter
///   (for use in sentences).
pub type TtCloseXmlEle = Tt<CloseXmlEle>;

/// A choice of a list of XML elements by name.
pub type EleSumList = OrQualConjList<"element", "one of elements", TtQuote>;

/// Quote an attribute value using double quotes.
pub type XmlAttrValueQuote = Delim<"\"", "\"", Raw>;
