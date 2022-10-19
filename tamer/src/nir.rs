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

use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display},
};

use crate::{
    diagnose::{Annotate, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::{Object, Token},
    span::{Span, UNKNOWN_SPAN},
    sym::SymbolId,
    xir::{attr::Attr, fmt::TtXmlAttr, QName},
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
/// Tokens that do not require desugaring are already represented as
///   [`PlainNir`] in the [`SugaredNir::Plain`] variant.
#[derive(Debug, PartialEq, Eq)]
pub enum SugaredNir {
    Plain(PlainNir),
}

impl Token for SugaredNir {
    fn ir_name() -> &'static str {
        "Sugared NIR"
    }

    fn span(&self) -> Span {
        use SugaredNir::*;

        match self {
            Plain(nir) => nir.span(),
        }
    }
}

impl Object for SugaredNir {}

impl Display for SugaredNir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SugaredNir::*;

        match self {
            Plain(nir) => Display::fmt(nir, f),
        }
    }
}

impl From<PlainNir> for SugaredNir {
    fn from(nir: PlainNir) -> Self {
        Self::Plain(nir)
    }
}

// TODO
type PkgPath = SymbolId;
type PkgTitle = SymbolId;
type Title = SymbolId;
type ParamName = SymbolId;
type ParamType = SymbolId;
type Dim = SymbolId;
type NumLiteral = SymbolId;
type DescLiteral = SymbolId;
type ParamDefault = SymbolId;
type ParamIdent = SymbolId;
type ClassIdent = SymbolId;
type ClassIdentList = SymbolId;
type BooleanLiteral = SymbolId;
type CalcIdent = SymbolId;
type ValueIdent = SymbolId;
type TplName = SymbolId;
type TplParamIdent = SymbolId;
type TplMetaIdent = SymbolId;
type TypeIdent = SymbolId;
type ConstIdent = SymbolId;
type TexMathLiteral = SymbolId;
type FuncIdent = SymbolId;
type ShortDimNumLiteral = SymbolId;
type StringLiteral = SymbolId;
type IdentType = SymbolId;
type AnyIdent = SymbolId;
type SymbolTableKey = SymbolId;
type IdentDtype = SymbolId;
type DynNodeLiteral = SymbolId;
type MapTransformLiteral = SymbolId;

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

impl From<Attr> for SymbolId {
    fn from(attr: Attr) -> Self {
        match attr {
            Attr(_, value, _) => value,
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
