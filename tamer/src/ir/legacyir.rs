// Legacy IR
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

//! Legacy IR faithful to the XSLT-based compiler.
//!
//! This represents the intermediate format (IR) used by the `xmlo` files
//!   (see [`crate::obj::xmlo`]) originally produced by the XSLT-based
//!   compiler.
//! It consists largely of metadata for object symbols.
//!
//! This IR should be converted into a higher-level IR quickly,
//!   especially considering that it will be going away in the future.

use crate::sym::{Symbol, SymbolIndexSize};
use std::convert::TryFrom;
use std::result::Result;

/// Toplevel package attributes.
#[derive(Debug, PartialEq, Eq)]
pub struct PackageAttrs<'i, Ix: SymbolIndexSize> {
    /// Unique package identifier.
    ///
    /// The package name is derived from the filename relative to the
    ///   project root during compilation (see `relroot`).
    pub name: Option<&'i Symbol<'i, Ix>>,

    /// Relative path from package to project root.
    pub relroot: Option<String>,

    /// Whether this package is a program.
    ///
    /// A _program_ is a package intended to be linked into a final
    ///   executable.
    /// Programs cannot be imported by other packages.
    /// Non-program packages cannot be linked.
    pub program: bool,

    /// Symbol representing package eligibility.
    ///
    /// A package is _eligible_ for computation if certain invariants are
    ///   met.
    /// This symbol is responsible for including each of those invariants as
    ///   dependencies so that they are included at link-time.
    pub elig: Option<&'i Symbol<'i, Ix>>,
}

// The derive macro seems to add an `Ix: Default` bound,
//   so we'll implement it manually to avoid that.
impl<'i, Ix: SymbolIndexSize> Default for PackageAttrs<'i, Ix> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            relroot: Default::default(),
            program: Default::default(),
            elig: Default::default(),
        }
    }
}

/// Symbol attributes.
///
/// This is a subset of all available attributes available on the
///   `preproc:sym` nodes;
///     more will be added as needed.
///
/// Not all symbols share the same set of attributes,
///   so this represents the union of all possible attribute sets.
///
/// Due to the number of possible attributes,
///   this is not an opaque type.
/// Consequently,
///   valid values should be enforced by the Rust's type system.
#[derive(Debug, PartialEq, Eq)]
pub struct SymAttrs<'i, Ix: SymbolIndexSize> {
    /// Relative path to the package that defined this symbol.
    ///
    /// Object files store relative paths so that they are somewhat
    ///   portable—the
    ///     entire project root should be able to be relocated.
    pub src: Option<&'i Symbol<'i, Ix>>,

    /// Symbol type.
    ///
    /// The type describes the purpose of the symbol and determines both how
    ///   it is compiled and its location in the final executable.
    pub ty: Option<SymType>,

    /// Number of dimensions.
    ///
    /// This determines the number of subscripts needed to access a scalar
    ///   value.
    /// A value of `0` indicates a scalar;
    ///   a value of `1` indicates a vector;
    ///   a value of `2` indicates a matrix;
    ///   and a value of `n` indicates a multi-dimensional array of
    ///     depth `n`.
    pub dim: Option<u8>,

    /// Type of underlying data.
    ///
    /// This is not a primitive,
    ///   and mostly represents whether or not floating point computations
    ///   will take place.
    pub dtype: Option<SymDtype>,

    /// Whether the symbol's location will be determined at link-time.
    ///
    /// Externs allow symbols to be referenced without having yet been given
    ///   a concrete definition,
    ///     provided that an eventual concrete definition matches the
    ///     provided declaration.
    /// The linker (see [`crate::ld`]) is responsible for ensuring that the
    ///   extern is satisfied and properly located in the final executable.
    pub extern_: bool,

    /// Unique package identifier.
    ///
    /// The name of a package is automatically derived from the package path
    ///   relative to the project root.
    /// _Note that this is problematic if one wants to compile the equivalent
    ///   of shared libraries._
    pub pkg_name: Option<&'i Symbol<'i, Ix>>,

    /// The identifier from which this one is derived.
    ///
    /// For example,
    ///   [`SymType::Cgen`] has a parent [`SymType::Class`] and
    ///   [`SymType::Gen`] has a parent [`SymType::Rate`].
    pub parent: Option<&'i Symbol<'i, Ix>>,

    /// Whether this identifier was generated by the compiler.
    ///
    /// A generated identifier is representative of an internal
    ///   implementation detail that should remain encapsulated from the
    ///   user and is subject to change over time.
    ///
    /// Identifiers created by templates are not considered to be generated.
    pub generated: bool,

    /// Child identifier associated with this identifier.
    ///
    /// For [`SymType::Class`],
    ///   this represents an associated [`SymType::Cgen`].
    pub yields: Option<&'i Symbol<'i, Ix>>,

    /// User-friendly identifier description.
    ///
    /// This is used primarily by [`SymType::Class`] and [`SymType::Gen`].
    pub desc: Option<String>,

    /// Related identifiers.
    ///
    /// These data represent a kluge created to add additional symbol
    /// information in two different contexts:
    ///
    ///  - [`SymType::Map`] includes the name of the source field; and
    ///  - [`SymType::Func`] lists params in order (so that the compiler
    ///    knows application order).
    pub from: Option<Vec<&'i Symbol<'i, Ix>>>,

    /// Whether symbol can be overridden.
    ///
    /// See also [`override`][SymAttrs::override_].
    pub virtual_: bool,

    /// Whether symbol is an override of a virtual symbol.
    ///
    /// See also [`virtual`][SymAttrs::virtual_].
    pub override_: bool,
}

// The derive macro seems to add an `Ix: Default` bound,
//   so we'll implement it manually to avoid that.
impl<'i, Ix: SymbolIndexSize> Default for SymAttrs<'i, Ix> {
    fn default() -> Self {
        Self {
            src: Default::default(),
            ty: Default::default(),
            dim: Default::default(),
            dtype: Default::default(),
            extern_: Default::default(),
            pkg_name: Default::default(),
            parent: Default::default(),
            generated: Default::default(),
            yields: Default::default(),
            desc: Default::default(),
            from: Default::default(),
            virtual_: Default::default(),
            override_: Default::default(),
        }
    }
}

/// Legacy symbol types.
///
/// This enum represents all symbol types represented in the `xmlo` files.
/// They are overly specialized and will be deprecated in favor of more
///   generalized dependent types in later IRs.
#[derive(Debug, PartialEq, Eq)]
pub enum SymType {
    /// Classification generator (from `lv:classify/@yields`).
    Cgen,
    /// Classification (from `lv:classify/@as`).
    Class,
    /// Constant (from `lv:const/@name`).
    Const,
    /// Function (from `lv:function/@name`).
    Func,
    /// Generator (from `lv:rate/@generates`).
    Gen,
    /// Local function parameter (from `lv:function/lv:param/@name`) or let
    ///   binding (from `lv:let/lv:values/lv:value/@name`).
    Lparam,
    /// Global parameter (from `lv:param/@name`).
    Param,
    /// Scalar calculation result (from `lv:rate/@yields`).
    Rate,
    /// Template (from `lv:template/@name`).
    Tpl,
    /// Typedef (from `lv:type/@name`).
    Type,
    /// Input map head (meta symbol generated by compiler for each input map).
    MapHead,
    /// Input field→param mapping (from `lvm:map`, `lvm:pass`).
    Map,
    /// Input map tail (meta symbol generated by compiler for each input map).
    MapTail,
    /// Return map head (meta symbol generated by compiler for each return map).
    RetMapHead,
    /// Return param→field mapping (from `lvm:map`, `lvm:pass`).
    RetMap,
    /// Return map tail (meta symbol generated by compiler for each return map).
    RetMapTail,
    /// Arbitrary metadata (from `lv:meta`).
    Meta,
    /// Rating worksheet (generated by compiler for worksheet packages).
    Worksheet,
}

impl TryFrom<&[u8]> for SymType {
    type Error = String;

    /// Determine symbol type from source `preproc:sym/@type`.
    ///
    /// This raises source `xmlo` data into this IR.
    /// See [`crate::obj::xmlo::XmloReader`].
    fn try_from(value: &[u8]) -> Result<SymType, Self::Error> {
        match value {
            b"cgen" => Ok(SymType::Cgen),
            b"class" => Ok(SymType::Class),
            b"const" => Ok(SymType::Const),
            b"func" => Ok(SymType::Func),
            b"gen" => Ok(SymType::Gen),
            b"lparam" => Ok(SymType::Lparam),
            b"param" => Ok(SymType::Param),
            b"rate" => Ok(SymType::Rate),
            b"tpl" => Ok(SymType::Tpl),
            b"type" => Ok(SymType::Type),
            b"retmap:head" => Ok(SymType::RetMapHead),
            b"retmap" => Ok(SymType::RetMap),
            b"retmap:tail" => Ok(SymType::RetMapTail),
            b"map:head" => Ok(SymType::MapHead),
            b"map" => Ok(SymType::Map),
            b"map:tail" => Ok(SymType::MapTail),
            b"meta" => Ok(SymType::Meta),
            b"worksheet" => Ok(SymType::Worksheet),
            _ => Err(format!(
                "unknown symbol type `{}`",
                String::from_utf8(value.to_vec())
                    .unwrap_or("(invalid UTF8)".into())
            )),
        }
    }
}

/// Underlying datatype.
///
/// This is the type of scalar data stored within the given symbol.
///
/// *NB:* This was _not enforced_ by the XSLT-based compiler.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SymDtype {
    /// {⊥,⊤} = {0,1} ⊂ ℤ
    Boolean,
    /// ℤ
    Integer,
    /// ℝ
    Float,
    /// ∅
    Empty,
}

impl AsRef<str> for SymDtype {
    /// Produce `xmlo`-compatible representation.
    fn as_ref(&self) -> &str {
        match self {
            SymDtype::Boolean => &"boolean",
            SymDtype::Integer => &"integer",
            SymDtype::Float => &"float",
            SymDtype::Empty => &"empty",
        }
    }
}

impl TryFrom<&[u8]> for SymDtype {
    type Error = String;

    /// Determine data type from source `preproc:sym/@dtype`.
    ///
    /// This raises source `xmlo` data into this IR.
    /// See [`crate::obj::xmlo::XmloReader`].
    fn try_from(value: &[u8]) -> Result<SymDtype, Self::Error> {
        match value {
            b"boolean" => Ok(SymDtype::Boolean),
            b"integer" => Ok(SymDtype::Integer),
            b"float" => Ok(SymDtype::Float),
            b"empty" => Ok(SymDtype::Empty),
            _ => Err(format!(
                "unknown symbol dtype `{}`",
                String::from_utf8(value.to_vec())
                    .unwrap_or("(invalid UTF8)".into())
            )),
        }
    }
}

impl std::fmt::Display for SymDtype {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(fmt, "boolean"),
            Self::Integer => write!(fmt, "integer"),
            Self::Float => write!(fmt, "float"),
            Self::Empty => write!(fmt, "(unknown)"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // We're not going to check every possible value here since we'd be
    // maintaining the mapping in two places; we can leave that to
    // integration tests.
    #[test]
    fn symtype_from_u8() {
        assert_eq!(Ok(SymType::Cgen), SymType::try_from(b"cgen" as &[u8]));
    }

    #[test]
    fn symtype_failure_from_unknown_u8() {
        match SymType::try_from(b"unknown" as &[u8]) {
            Err(s) => assert!(s.contains("unknown")),
            bad => panic!("expected error: {:?}", bad),
        }
    }

    #[test]
    fn symdtype_from_u8() {
        assert_eq!(
            Ok(SymDtype::Integer),
            SymDtype::try_from(b"integer" as &[u8])
        );
    }

    #[test]
    fn symdtype_failure_from_unknown_u8() {
        match SymDtype::try_from(b"unknownd" as &[u8]) {
            Err(s) => assert!(s.contains("unknownd")),
            bad => panic!("expected error: {:?}", bad),
        }
    }

    #[test]
    fn symdtype_as_str() {
        let boolean: &str = SymDtype::Boolean.as_ref();
        assert_eq!("boolean", boolean);
    }
}
