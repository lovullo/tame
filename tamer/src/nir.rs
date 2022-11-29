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

mod interp;
mod parse;

use crate::{
    diagnose::{Annotate, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::{Object, Token},
    span::{Span, UNKNOWN_SPAN},
    sym::SymbolId,
    xir::{
        attr::{Attr, AttrSpan},
        fmt::TtXmlAttr,
        QName,
    },
};
use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display},
};

pub use parse::{
    NirParseState as XirfToNir, NirParseStateError_ as XirfToNirError,
};

use NirSymbolTy::*;

/// IR that is "near" the source code.
///
/// This represents the language of TAME after it has been extracted from
///   whatever document encloses it
///     (e.g. XML).
#[derive(Debug, PartialEq, Eq)]
pub enum Nir {
    Todo,

    TplParamOpen(Plain<{ TplParamIdent }>, Plain<{ DescLiteral }>),
    TplParamClose(Span),
    TplParamText(Plain<{ StringLiteral }>),
    TplParamValue(Plain<{ TplParamIdent }>),
}

type Plain<const TY: NirSymbolTy> = NirSymbol<TY>;

impl Token for Nir {
    fn ir_name() -> &'static str {
        "Plain NIR"
    }

    /// Identifying span of a token.
    ///
    /// An _identifying span_ is a selection of one of the (potentially
    ///   many) spans associated with a token that is most likely to be
    ///   associated with the identity of that token.
    fn span(&self) -> Span {
        use Nir::*;

        match self {
            Todo => UNKNOWN_SPAN,
            TplParamOpen(dfn, _) => dfn.span(),
            TplParamClose(span) => *span,
            TplParamText(text) => text.span(),
            TplParamValue(ident) => ident.span(),
        }
    }
}

impl Object for Nir {}

impl Display for Nir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Nir::*;

        match self {
            Todo => write!(f, "TODO"),
            TplParamOpen(dfn, desc) => {
                write!(f, "open template param {dfn} ({desc})")
            }
            TplParamClose(_span) => write!(f, "close template param"),
            TplParamText(text) => {
                write!(f, "open template param default text {text}")
            }
            TplParamValue(ident) => {
                write!(f, "value of template param {ident}")
            }
        }
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
    ShortDimNumLiteral,
    StringLiteral,
    SymbolTableKey,
    TexMathLiteral,
    Title,
    TplMetaIdent,
    TplIdent,
    TplParamIdent,
    TypeIdent,
    ValueIdent,
}

impl Display for NirSymbolTy {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirSymbolTy::*;

        match self {
            AnyIdent => write!(f, "any identifier"),
            BooleanLiteral => write!(
                f,
                "boolean literal {fmt_true} or {fmt_false}",
                fmt_true = TtQuote::wrap("true"),
                fmt_false = TtQuote::wrap("false"),
            ),
            ClassIdent => write!(f, "classification identifier"),
            ClassIdentList => {
                write!(f, "space-delimited list of classification identifiers")
            }
            ConstIdent => write!(f, "constant identifier"),
            DescLiteral => write!(f, "description literal"),
            Dim => write!(f, "dimension declaration"),
            DynNodeLiteral => write!(f, "dynamic node literal"),
            FuncIdent => write!(f, "function identifier"),
            IdentDtype => write!(f, "identifier primitive datatype"),
            IdentType => write!(f, "identifier type"),
            MapTransformLiteral => write!(f, "map transformation literal"),
            NumLiteral => write!(f, "numeric literal"),
            ParamDefault => write!(f, "param default"),
            ParamIdent => write!(f, "param identifier"),
            ParamName => write!(f, "param name"),
            ParamType => write!(f, "param type"),
            PkgPath => write!(f, "package path"),
            ShortDimNumLiteral => {
                write!(f, "short-hand dimensionalized numeric literal")
            }
            StringLiteral => write!(f, "string literal"),
            SymbolTableKey => write!(f, "symbol table key name"),
            TexMathLiteral => write!(f, "TeX math literal"),
            Title => write!(f, "title"),
            TplMetaIdent => write!(f, "template metadata identifier"),
            TplIdent => write!(f, "template name"),
            TplParamIdent => write!(f, "template param identifier"),
            TypeIdent => write!(f, "type identifier"),
            ValueIdent => write!(f, "value identifier"),
        }
    }
}

/// A ([`SymbolId`], [`Span`]) pair representing an
///   attribute value that may need to be interpreted within the context of
///   a template application.
///
/// _This object must be kept small_,
///   since it is used in objects that aggregate portions of the token
///   stream,
///     which must persist in memory for a short period of time,
///     and therefore cannot be optimized away as other portions of the IR.
#[derive(Debug, PartialEq, Eq)]
pub struct NirSymbol<const TY: NirSymbolTy>(SymbolId, Span);

impl<const TY: NirSymbolTy> Token for NirSymbol<TY> {
    fn ir_name() -> &'static str {
        // TODO: Include type?
        "NIR Symbol"
    }

    fn span(&self) -> Span {
        match self {
            Self(_, span) => *span,
        }
    }
}

impl<const TY: NirSymbolTy> Display for NirSymbol<TY> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self(sym, _span) => {
                write!(f, "{TY} {fmt_sym}", fmt_sym = TtQuote::wrap(sym),)
            }
        }
    }
}

impl<const TY: NirSymbolTy> From<(SymbolId, Span)> for NirSymbol<TY> {
    fn from((val, span): (SymbolId, Span)) -> Self {
        Self(val, span)
    }
}

impl<const TY: NirSymbolTy> From<Attr> for NirSymbol<TY> {
    fn from(attr: Attr) -> Self {
        match attr {
            Attr(_, val, AttrSpan(_, vspan)) => (val, vspan).into(),
        }
    }
}

// Force developer to be conscious of any changes in size;
//   see `NirSymbol` docs for more information.
assert_eq_size!(NirSymbol<{ NirSymbolTy::AnyIdent }>, (SymbolId, Span));

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
