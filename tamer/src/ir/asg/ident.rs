// ASG identifiers
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

//! Identifiers (a type of [object][super::object::IdentObject]).

use crate::global;
use crate::ir::legacyir::{SymAttrs, SymDtype, SymType};
use crate::sym::{GlobalSymbolIntern, SymbolId, SymbolIndexSize};
use paste::paste;
use std::convert::TryFrom;
use std::error::Error;

/// Types of identifiers.
///
/// Here, the term _calculation_ refers to a composable expression that
///   produces a numeric result.
///
/// These are derived from [`legacyir::SymType`][crate::ir::legacyir::SymType]
///   and will be generalized in the future.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IdentKind {
    /// Classification generator.
    ///
    /// This has the same number of dimensions as its highest-dimension
    ///   predicate.
    /// Every [`Class`][IdentKind::Class] has an associated generator.
    Cgen(Dim),

    /// Boolean classification.
    ///
    /// This is an artifact of an ancient system.
    /// The dimensions here refers to the dimensions of the associated
    ///   [`Cgen`][IdentKind::Cgen].
    Class(Dim),

    /// Constant value.
    Const(Dim, DataType),

    /// Re-usable encapsulated expression.
    ///
    /// Functions are nothing more than expressions that can be re-used with
    ///   dynamic values at runtime.
    /// See also [`Lparam`][IdentKind::Lparam].
    Func(Dim, DataType),

    /// Generating calculation.
    ///
    /// Generators are associated with iterative expressions,
    ///   such as sums and products.
    /// They always have a parent [`Rate`][IdentKind::Rate].
    Gen(Dim, DataType),

    /// Local (non-global) parameter.
    ///
    /// Local parameters are lexically scoped to their parent expression:
    ///   - [`Func`][IdentKind::Func], where there exists one per defined
    ///     function parameter; and
    ///   - `let` expression bindings.
    ///
    /// This is not to be confused with the global
    ///   [`Param`][IdentKind::Param].
    Lparam(Dim, DataType),

    /// Global parameter.
    ///
    /// These parameters serve as inputs to the system.
    /// Input values are bound using [`Map`][IdentKind::Map].
    Param(Dim, DataType),

    /// Scalar result of a named calculation.
    ///
    /// The verb "rate" is historical,
    ///   since TAME was developed for insurance rating systems.
    /// This represents a named expression that yields a scalar value.
    ///
    /// This serves as a parent to [`Gen`][IdentKind::Gen].
    Rate(DataType),

    /// Template definition.
    ///
    /// A template is used only at expansion-time and,
    ///   unlike most other things in the system,
    ///   have no runtime value.
    Tpl,

    /// User-defined data type.
    ///
    /// The only types typically defined are enums and unions of enums.
    /// The type itself has no runtime value,
    ///   but each of the enum variants have an associated value of type
    ///   [`DataType`].
    Type(DataType),

    /// Input map head (meta identifier generated by compiler for each input
    ///   map).
    MapHead,

    /// Input field→param mapping.
    ///
    /// These may only map to [`Param`][IdentKind::Param].
    /// The source data is arbitrary and provided at runtime.
    Map,

    /// Input map tail (meta symbol generated by compiler for each input
    ///   map).
    MapTail,

    /// Return map head (meta symbol generated by compiler for each return
    ///   map).
    RetMapHead,

    /// Return param→field mapping.
    ///
    /// Return mappings export data to calling systems.
    /// They can map back any globally defined numeric expression.
    RetMap,

    /// Return map tail (meta symbol generated by compiler for each return
    ///   map).
    RetMapTail,

    /// Arbitrary metadata.
    ///
    /// This permits the definition of static key/value data that is
    ///   compiled into the final executable.
    Meta,

    /// Rating worksheet (generated by compiler for worksheet packages).
    ///
    /// The worksheet exposes intermediate calculation values in a much more
    ///   concise form than that of the Summary Page.
    Worksheet,
}

/// Produce [`AsRef`] impls for [`str`], [`global::ProgSymSize`] and
///   [`global::PkgSymSize`] for identifier kind strings.
macro_rules! kind_intern {
    ($($variant:ident $($v:pat)? => $str:expr),*) => {
        paste! {
            lazy_static! {
                $(
                    static ref [<PROG_KIND_ $variant:upper>]: SymbolId<global::ProgSymSize>
                        = $str.intern();
                    static ref [<PKG_KIND_ $variant:upper>]: SymbolId<global::PkgSymSize>
                        = $str.intern();
                )*
            }

            impl AsRef<str> for IdentKind {
                fn as_ref(&self) -> &'static str {
                    match self {
                        $(
                            Self::$variant$($v)* => $str,
                        )*
                    }
                }
            }

            impl AsRef<SymbolId<global::ProgSymSize>> for IdentKind {
                fn as_ref(&self) -> &SymbolId<global::ProgSymSize> {
                    match self {
                        $(
                            Self::$variant$($v)* => &[<PROG_KIND_ $variant:upper>],
                        )*
                    }
                }
            }

            impl AsRef<SymbolId<global::PkgSymSize>> for IdentKind {
                fn as_ref(&self) -> &SymbolId<global::PkgSymSize> {
                    match self {
                        $(
                            Self::$variant$($v)* => &[<PKG_KIND_ $variant:upper>],
                        )*
                    }
                }
            }
        }
    }
}

// In the future, we'll pre-populate the internment pool, like rustc.
kind_intern! {
    Cgen(_) => "cgen",
    Class(_) => "class",
    Const(_, _) => "const",
    Func(_, _) => "func",
    Gen(_, _) => "gen",
    Lparam(_, _) => "lparam",
    Param(_, _) => "param",
    Rate(_) => "rate",
    Tpl => "tpl",
    Type(_) => "type",
    MapHead => "map:head",
    Map => "map",
    MapTail => "map:tail",
    RetMapHead => "retmap:head",
    RetMap => "retmap",
    RetMapTail => "retmap:tail",
    Meta => "meta",
    Worksheet => "worksheet"
}

impl std::fmt::Display for IdentKind {
    /// Format identifier type for display to the user.
    ///
    /// TODO: We have not yet finalized how we will represent types in the
    ///   new type system,
    ///     so for now this just uses a syntax similar to Rust.
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name: &str = self.as_ref();

        match self {
            Self::Cgen(dim) => {
                write!(fmt, "{}[{}; {}]", name, DataType::Boolean, dim)
            }
            Self::Class(dim) => {
                write!(fmt, "{}[{}; {}]", name, DataType::Boolean, dim)
            }
            Self::Const(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Func(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Gen(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Lparam(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Param(dim, dtype) => {
                write!(fmt, "{}[{}; {}]", name, dtype, dim)
            }
            Self::Rate(dtype) => write!(fmt, "{}[{}; 0]", name, dtype),
            Self::Type(dtype) => write!(fmt, "{}[{}]", name, dtype),
            _ => write!(fmt, "{}", name),
        }
    }
}

impl<Ix> TryFrom<SymAttrs<Ix>> for IdentKind
where
    Ix: SymbolIndexSize,
{
    type Error = IdentKindError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: SymAttrs<Ix>) -> Result<Self, Self::Error> {
        Self::try_from(&attrs)
    }
}

impl<Ix> TryFrom<&SymAttrs<Ix>> for IdentKind
where
    Ix: SymbolIndexSize,
{
    type Error = IdentKindError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: &SymAttrs<Ix>) -> Result<Self, Self::Error> {
        let ty = attrs.ty.as_ref().ok_or(Self::Error::MissingType)?;

        macro_rules! ident {
            ($to:expr) => {
                Ok($to)
            };
            ($to:expr, dim) => {
                Ok($to(Dim(attrs.dim.ok_or(Self::Error::MissingDim)?)))
            };
            ($to:expr, dtype) => {
                Ok($to(attrs.dtype.ok_or(Self::Error::MissingDtype)?))
            };
            ($to:expr, dim, dtype) => {
                Ok($to(
                    Dim(attrs.dim.ok_or(Self::Error::MissingDim)?),
                    attrs.dtype.ok_or(Self::Error::MissingDtype)?,
                ))
            };
        }

        match ty {
            SymType::Cgen => ident!(Self::Cgen, dim),
            SymType::Class => ident!(Self::Class, dim),
            SymType::Const => ident!(Self::Const, dim, dtype),
            SymType::Func => ident!(Self::Func, dim, dtype),
            SymType::Gen => ident!(Self::Gen, dim, dtype),
            SymType::Lparam => ident!(IdentKind::Lparam, dim, dtype),
            SymType::Param => ident!(IdentKind::Param, dim, dtype),
            SymType::Rate => ident!(IdentKind::Rate, dtype),
            SymType::Tpl => ident!(IdentKind::Tpl),
            SymType::Type => ident!(IdentKind::Type, dtype),
            SymType::MapHead => ident!(IdentKind::MapHead),
            SymType::Map => ident!(IdentKind::Map),
            SymType::MapTail => ident!(IdentKind::MapTail),
            SymType::RetMapHead => ident!(IdentKind::RetMapHead),
            SymType::RetMap => ident!(IdentKind::RetMap),
            SymType::RetMapTail => ident!(IdentKind::RetMapTail),
            SymType::Meta => ident!(IdentKind::Meta),
            SymType::Worksheet => ident!(IdentKind::Worksheet),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IdentKindError {
    /// Symbol type was not provided.
    MissingType,

    /// Number of symbol dimensions were not provided.
    MissingDim,

    /// Symbol dtype was not provided.
    MissingDtype,
}

impl std::fmt::Display for IdentKindError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::MissingType => write!(fmt, "missing symbol type"),
            Self::MissingDim => write!(fmt, "missing dim"),
            Self::MissingDtype => write!(fmt, "missing dtype"),
        }
    }
}

impl Error for IdentKindError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Identifier dimensions.
///
/// This determines the number of subscripts needed to access a scalar
///   value.
/// A value of `0` indicates a scalar;
///   a value of `1` indicates a vector;
///   a value of `2` indicates a matrix;
///   and a value of `n` indicates a multi-dimensional array of
///     depth `n`.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Dim(u8);

impl Dim {
    pub fn from_u8(value: u8) -> Self {
        // TODO: 0≤n<10
        Self(value)
    }
}

/// Underlying datatype of identifier.
///
/// TODO: This will always be 0≤n≤9, so let's introduce a newtype for it.
impl AsRef<str> for Dim {
    fn as_ref(&self) -> &str {
        match self.0 {
            0 => &"0",
            1 => &"1",
            2 => &"2",
            3 => &"3",
            4 => &"4",
            5 => &"5",
            6 => &"6",
            7 => &"7",
            8 => &"8",
            9 => &"9",
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for Dim {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        (self.0).fmt(fmt)
    }
}

/// Underlying datatype of identifier.
pub type DataType = SymDtype;

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    type Ix = u16;

    #[test]
    fn dim_from_u8() {
        let n = 5u8;

        assert_eq!(Dim(n), Dim::from_u8(n));
    }

    #[test]
    fn dim_to_str() {
        // we'll just test high and low
        let low: &str = Dim(0).as_ref();
        let high: &str = Dim(9).as_ref();

        assert_eq!("0", low);
        assert_eq!("9", high);
    }

    macro_rules! test_kind {
        ($name:ident, $src:expr => $dest:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    Ok($dest),
                    SymAttrs::<Ix> {
                        ty: Some($src),
                        ..Default::default()
                    }
                    .try_into()
                );
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim) => {
            #[test]
            fn $name() {
                let dim = 1;

                assert_eq!(
                    Ok($dest(Dim(dim))),
                    SymAttrs::<Ix> {
                        ty: Some($src),
                        dim: Some(dim),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let result = IdentKind::try_from(SymAttrs::<Ix> {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(IdentKindError::MissingDim, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dtype) => {
            #[test]
            fn $name() {
                let dtype = SymDtype::Float;

                assert_eq!(
                    Ok($dest(dtype)),
                    SymAttrs::<Ix> {
                        ty: Some($src),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dtype
                let result = IdentKind::try_from(SymAttrs::<Ix> {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(IdentKindError::MissingDtype, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim, dtype) => {
            #[test]
            fn $name() {
                let dim = 1;
                let dtype = SymDtype::Float;

                assert_eq!(
                    Ok($dest(Dim(dim), dtype)),
                    SymAttrs::<Ix> {
                        ty: Some($src),
                        dim: Some(dim),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let dim_result = IdentKind::try_from(SymAttrs::<Ix> {
                    ty: Some($src),
                    dtype: Some(dtype),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(IdentKindError::MissingDim, dim_result);

                // no dtype
                let dtype_result = IdentKind::try_from(SymAttrs::<Ix> {
                    ty: Some($src),
                    dim: Some(dim),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(IdentKindError::MissingDtype, dtype_result);
            }
        };
    }

    test_kind!(cgen, SymType::Cgen => IdentKind::Cgen, dim);
    test_kind!(class, SymType::Class => IdentKind::Class, dim);
    test_kind!(r#const, SymType::Const => IdentKind::Const, dim, dtype);
    test_kind!(func, SymType::Func => IdentKind::Func, dim, dtype);
    test_kind!(gen, SymType::Gen => IdentKind::Gen, dim, dtype);
    test_kind!(lparam, SymType::Lparam => IdentKind::Lparam, dim, dtype);
    test_kind!(param, SymType::Param => IdentKind::Param, dim, dtype);
    test_kind!(rate, SymType::Rate => IdentKind::Rate, dtype);
    test_kind!(tpl, SymType::Tpl => IdentKind::Tpl);
    test_kind!(r#type, SymType::Type => IdentKind::Type, dtype);
    test_kind!(maphead, SymType::MapHead => IdentKind::MapHead);
    test_kind!(map, SymType::Map => IdentKind::Map);
    test_kind!(maptail, SymType::MapTail => IdentKind::MapTail);
    test_kind!(retmaphead, SymType::RetMapHead => IdentKind::RetMapHead);
    test_kind!(retmap, SymType::RetMap => IdentKind::RetMap);
    test_kind!(retmaptail, SymType::RetMapTail => IdentKind::RetMapTail);
    test_kind!(meta, SymType::Meta => IdentKind::Meta);
    test_kind!(worksheet, SymType::Worksheet => IdentKind::Worksheet);
}
