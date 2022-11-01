// IR that is "near" the source code.
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

//! An IR that is "near" the source code.
//!
//! This IR is "near" the source code written by the user,
//!   performing only basic normalization tasks like desugaring.
//! It takes a verbose input language and translates it into a much more
//!   concise internal representation.
//! The hope is that most desugaring will be done by templates in the future.
//!
//! NIR cannot completely normalize the source input because it does not
//!   have enough information to do so---the
//!     template system requires a compile-time interpreter that is beyond
//!     the capabilities of NIR,
//!       and so a final normalization pass must be done later on in the
//!       lowering pipeline.
//!
//! This is a streaming IR,
//!   meaning that the equivalent AST is not explicitly represented as a
//!   tree structure in memory.
//!
//! NIR is lossy and does not retain enough information for code
//!   formatting---that
//!     type of operation will require a mapping between
//!     XIRF and NIR,
//!       where the latter is used to gather enough context for formatting
//!       and the former is used as a concrete representation of what the user
//!       actually typed.
//!
//! For more information on the parser,
//!   see [`parse`].
//! The entry point for NIR in the lowering pipeline is exported as
//!   [`XirfToNir`].

mod desugar;
mod parse;

use crate::{
    diagnose::{Annotate, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::{Object, Token},
    span::{Span, UNKNOWN_SPAN},
    sym::{st::quick_contains_byte, GlobalSymbolResolve, SymbolId},
    xir::{
        attr::{Attr, AttrSpan},
        fmt::TtXmlAttr,
        QName,
    },
};
use memchr::memchr;
use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display},
};

pub use desugar::{DesugarNir, DesugarNirError};
pub use parse::{
    NirParseState as XirfToNir, NirParseStateError_ as XirfToNirError,
};

/// IR that is "near" the source code,
///   without its syntactic sugar.
///
/// This form contains only primitives that cannot be reasonably represented
///   by other primitives.
/// This is somewhat arbitrary and may change over time,
///   but represents a balance between the level of abstraction of the IR
///   and performance of lowering operations.
///
/// See [`SugaredNir`] for more information about the sugared form.
#[derive(Debug, PartialEq, Eq)]
pub enum PlainNir {
    Todo,
}

impl Token for PlainNir {
    fn ir_name() -> &'static str {
        "Plain NIR"
    }

    fn span(&self) -> Span {
        use PlainNir::*;

        match self {
            Todo => UNKNOWN_SPAN,
        }
    }
}

impl Object for PlainNir {}

impl Display for PlainNir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use PlainNir::*;

        match self {
            Todo => write!(f, "TODO"),
        }
    }
}

/// Syntactic sugar atop of [`PlainNir`].
///
/// NIR contains various syntax features that serve as mere quality-of-life
///   conveniences for users
///     ("sugar" to sweeten the experience).
/// These features do not add an expressiveness to the language,
///   and are able to be lowered into other primitives without changing
///   its meaning.
///
/// The process of lowering syntactic sugar into primitives is called
///   "desugaring" and is carried out by the [`DesugarNir`] lowering
///     operation,
///       producing [`PlainNir`].
#[derive(Debug, PartialEq, Eq)]
pub enum SugaredNir {
    /// A primitive token that may have sugared values.
    Primitive(PlainNir),
}

impl Token for SugaredNir {
    fn ir_name() -> &'static str {
        "Sugared NIR"
    }

    fn span(&self) -> Span {
        use SugaredNir::*;

        match self {
            Primitive(nir) => nir.span(),
        }
    }
}

impl Object for SugaredNir {}

impl Display for SugaredNir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SugaredNir::*;

        match self {
            Primitive(nir) => Display::fmt(nir, f),
        }
    }
}

impl From<PlainNir> for SugaredNir {
    fn from(nir: PlainNir) -> Self {
        Self::Primitive(nir)
    }
}

/// Tag representing the type of a NIR value.
///
/// NIR values originate from attributes,
///   which are refined into types as enough information becomes available.
/// Value parsing must be deferred if a value requires desugaring or
///   metavalue expansion.
#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum NirSymbolTy {
    AnyIdent,
    BooleanLiteral,
    CalcIdent,
    ClassIdent,
    ClassIdentList,
    ConstIdent,
    DescLiteral,
    Dim,
    DynNodeLiteral,
    FuncIdent,
    IdentDtype,
    IdentType,
    MapTransformLiteral,
    NumLiteral,
    ParamDefault,
    ParamIdent,
    ParamName,
    ParamType,
    PkgPath,
    PkgTitle,
    ShortDimNumLiteral,
    StringLiteral,
    SymbolTableKey,
    TexMathLiteral,
    Title,
    TplMetaIdent,
    TplName,
    TplParamIdent,
    TypeIdent,
    ValueIdent,
}

/// A ([`SymbolId`], [`Span`]) pair in an attribute value context that may
///   require desugaring and interpretation within the context of a template
///   application.
///
/// Interpolated values require desugaring;
///   see [`DesugarNir`] for more information.
///
/// _This object must be kept small_,
///   since it is used in objects that aggregate portions of the token
///   stream,
///     which must persist in memory for a short period of time,
///     and therefore cannot be optimized away as other portions of the IR.
/// As such,
///   this does not nest enums.
#[derive(Debug, PartialEq, Eq)]
pub enum SugaredNirSymbol<const TY: NirSymbolTy> {
    /// The symbol contains an expression representing the concatenation of
    ///   any number of literals and metavariables
    ///     (referred to as "string interpolation" in many languages).
    Interpolate(SymbolId, Span),

    /// It's not ripe yet.
    ///
    /// No parsing has been performed.
    Todo(SymbolId, Span),
}

// Force developer to be conscious of any changes in size;
//   see `SugaredNirSymbol` docs for more information.
assert_eq_size!(SugaredNirSymbol<{ NirSymbolTy::AnyIdent }>, u128);

/// Character whose presence in a string indicates that interpolation
///   parsing must occur.
pub const INTERPOLATE_CHAR: u8 = b'{';

#[derive(Debug, PartialEq, Eq)]
pub enum PkgType {
    /// Package is intended to produce an executable program.
    ///
    /// This is specified by the `rater` root node.
    Prog,
    /// Package is intended to be imported as a component of a larger
    ///   program.
    Mod,
}

/// Whether a value represented by the provided [`SymbolId`] requires
///   interpolation.
///
/// _NB: This dereferences the provided [`SymbolId`] if it is dynamically
///   allocated._
///
/// The provided value requires interpolation if it contains,
///   anywhere in the string,
///   the character [`INTERPOLATE_CHAR`].
/// This does not know if the string will parse correctly;
///   that job is left for desugaring,
///     and so this will flag syntactically invalid interpolated strings
///       (which is expected).
#[inline]
fn needs_interpolation(val: SymbolId) -> bool {
    // We can skip pre-interned symbols that we know cannot include the
    //   interpolation character.
    // TODO: Abstract into `sym::symbol` module.
    let ch = INTERPOLATE_CHAR;
    quick_contains_byte(val, ch)
        .or_else(|| memchr(ch, val.lookup_str().as_bytes()).map(|_| true))
        .unwrap_or(false)
}

impl<const TY: NirSymbolTy> TryFrom<(SymbolId, Span)> for SugaredNirSymbol<TY> {
    type Error = NirAttrParseError;

    fn try_from((val, span): (SymbolId, Span)) -> Result<Self, Self::Error> {
        match needs_interpolation(val) {
            true => Ok(SugaredNirSymbol::Interpolate(val, span)),
            false => Ok(SugaredNirSymbol::Todo(val, span)),
        }
    }
}

impl<const TY: NirSymbolTy> TryFrom<Attr> for SugaredNirSymbol<TY> {
    type Error = NirAttrParseError;

    fn try_from(attr: Attr) -> Result<Self, Self::Error> {
        match attr {
            Attr(_, val, AttrSpan(_, vspan)) => (val, vspan).try_into(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Literal<const S: SymbolId>;

impl<const S: SymbolId> TryFrom<Attr> for Literal<S> {
    type Error = NirAttrParseError;

    fn try_from(attr: Attr) -> Result<Self, Self::Error> {
        match attr {
            Attr(_, val, _) if val == S => Ok(Literal),
            Attr(name, _, aspan) => Err(NirAttrParseError::LiteralMismatch(
                name,
                aspan.value_span(),
                S,
            )),
        }
    }
}

impl From<Infallible> for NirAttrParseError {
    fn from(x: Infallible) -> Self {
        match x {}
    }
}

type ExpectedSymbolId = SymbolId;

#[derive(Debug, PartialEq, Eq)]
pub enum NirAttrParseError {
    LiteralMismatch(QName, Span, ExpectedSymbolId),
}

impl Error for NirAttrParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl Display for NirAttrParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LiteralMismatch(name, _, _) => {
                write!(f, "unexpected value for {}", TtXmlAttr::wrap(name),)
            }
        }
    }
}

impl Diagnostic for NirAttrParseError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        match self {
            Self::LiteralMismatch(_, span, expected) => span
                .error(format!("expecting {}", TtQuote::wrap(expected)))
                .into(),
        }
    }
}

#[cfg(test)]
mod test;
