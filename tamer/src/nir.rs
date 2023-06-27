// IR that is "near" the source code.
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

mod air;
mod interp;
mod parse;
mod tplshort;

use crate::{
    diagnose::{Annotate, Diagnostic},
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{util::SPair, Object, Token},
    span::Span,
    sym::SymbolId,
    xir::{
        attr::{Attr, AttrSpan},
        QName,
    },
};
use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display},
};

pub use air::{NirToAir, NirToAirError, NirToAirParseType};
pub use interp::{InterpError, InterpState as InterpolateNir};
pub use parse::{
    NirParseState as XirfToNir, NirParseStateError_ as XirfToNirError,
};
pub use tplshort::{TplShortDesugar, TplShortDesugarError};

/// IR that is "near" the source code.
///
/// This represents the language of TAME after it has been extracted from
///   whatever document encloses it
///     (e.g. XML).
#[derive(Debug, PartialEq, Eq)]
pub enum Nir {
    Todo(Span),
    TodoAttr(SPair),

    /// Begin the definition of some [`NirEntity`] and place it atop of the
    ///   stack.
    Open(NirEntity, Span),

    /// Finish definition of a [`NirEntity`] atop of the stack and pop it.
    Close(NirEntity, Span),

    /// Bind the given name as a concrete identifier for the entity atop of
    ///   the stack.
    ///
    /// [`Self::Ref`] references identifiers created using this token.
    ///
    /// See also [`Self::BindIdentAbstract`].
    BindIdent(SPair),

    /// Bind entity atop of the stack to an abstract identifier whose name
    ///   will eventually be derived from the metavariable identifier by the
    ///   given [`SPair`].
    ///
    /// The identifier is intended to become concrete when a lexical value
    ///   for the metavariable becomes available during expansion,
    ///     which is outside of the scope of NIR.
    ///
    /// See also [`Self::BindIdent`] for a concrete identifier.
    BindIdentAbstract(SPair),

    /// Reference the value of the given identifier as the subject of the
    ///   current expression.
    ///
    /// The source language of TAME is historically XML,
    ///   which permits attributes in any order.
    /// If an expression contains multiple references that the user expects
    ///   to mean a particular thing depending on attribute name,
    ///     then this can be used to disambiguate.
    /// Other references may use [`Self::Ref`].
    ///
    /// In XML notation,
    ///   examples include `match/@on` and `c:value-of/@name`.
    ///
    /// NIR may at its discretion use this token to impose ordering,
    ///   which would have the effect of imposing XML attribute order.
    RefSubject(SPair),

    /// Reference the value of the given identifier.
    ///
    /// Permissible identifiers and values depend on the context in which
    ///   this appears.
    /// Identifiers are defined using [`Self::BindIdent`].
    ///
    /// This reference has no particular significance other than not being
    ///   the [`Self::RefSubject`] of the expression;
    ///     all [`Self::Ref`]s within a given expression are semantically
    ///     equivalent,
    ///       and so should not be distinguished to the user by different
    ///       attribute names unless those names are too semantically
    ///       equivalent.
    Ref(SPair),

    /// Describe the [`NirEntity`] atop of the stack.
    Desc(SPair),

    /// A string literal.
    ///
    /// The meaning of this string depends on context.
    /// For example,
    ///   it may represent literate documentation or a literal in a
    ///   metavariable definition.
    Text(SPair),

    /// Import a package identified by the provided namespec.
    Import(SPair),

    /// "No-op" (no operation) that does nothing.
    ///
    /// Since this is taking user input and effectively discarding it,
    ///   this contains a [`Span`],
    ///   so that we can clearly see the source code associated with what we
    ///     chose to discard.
    ///
    /// Ideally this can be eliminated in the future by causing an
    ///   incomplete parse,
    ///     which is all this does in the end.
    /// See its uses for more information.
    Noop(Span),
}

impl Nir {
    /// Retrieve a _concrete_ inner [`SymbolId`] that this token represents,
    ///   if any.
    ///
    /// Not all NIR tokens contain associated symbols;
    ///   a token's [`SymbolId`] is retained only if it provides additional
    ///   information over the token itself.
    ///
    /// An abstract identifier will yield [`None`],
    ///   since its concrete symbol has yet to be defined;
    ///     the available symbol instead represents the name of the
    ///     metavariable from which the concrete symbol will eventually
    ///     have its value derived.
    ///
    /// See also [`Nir::map`] if you wish to change the symbol,
    ///   noting however that it does not distinguish between notions of
    ///   concrete and abstract as this method does.
    pub fn concrete_symbol(&self) -> Option<SymbolId> {
        use Nir::*;

        match self {
            Todo(_) => None,
            TodoAttr(spair) => Some(spair.symbol()),

            Open(_, _) | Close(_, _) => None,

            BindIdent(spair) | RefSubject(spair) | Ref(spair) | Desc(spair)
            | Text(spair) | Import(spair) => Some(spair.symbol()),

            // An abstract identifier does not yet have a concrete symbol
            //   assigned;
            //     the available symbol represents the metavariable from
            //     which a symbol will eventually be derived during
            //     expansion.
            BindIdentAbstract(_) => None,

            Noop(_) => None,
        }
    }
}

impl Functor<SymbolId> for Nir {
    /// Map over a token's [`SymbolId`].
    ///
    /// This allows modifying a token's [`SymbolId`] while retaining the
    ///   associated [`Span`].
    /// This is the desired behavior when modifying the source code the user
    ///   entered,
    ///     since diagnostic messages will reference the original source
    ///     location that the modification was derived from.
    ///
    /// If a token does not contain a symbol,
    ///   this returns the token unchanged.
    ///
    /// See also [`Nir::concrete_symbol`].
    fn map(self, f: impl FnOnce(SymbolId) -> SymbolId) -> Self {
        use Nir::*;

        match self {
            Todo(_) => self,
            TodoAttr(spair) => TodoAttr(spair.map(f)),

            Open(_, _) | Close(_, _) => self,

            BindIdent(spair) => BindIdent(spair.map(f)),
            BindIdentAbstract(spair) => BindIdentAbstract(spair.map(f)),
            RefSubject(spair) => RefSubject(spair.map(f)),
            Ref(spair) => Ref(spair.map(f)),
            Desc(spair) => Desc(spair.map(f)),
            Text(spair) => Text(spair.map(f)),
            Import(spair) => Import(spair.map(f)),

            Noop(_) => self,
        }
    }
}

/// An object upon which other [`Nir`] tokens act.
#[derive(Debug, PartialEq, Eq)]
pub enum NirEntity {
    /// Package of identifiers.
    Package,

    /// Rate (verb) block,
    ///   representing a calculation with a scalar result.
    Rate,
    /// Summation (Σ) expression.
    Sum,
    /// Product (Π) expression.
    Product,
    /// Ceiling (⌈) expression.
    Ceil,
    /// Floor (⌊) expression.
    Floor,

    // Classification.
    Classify,
    /// Conjunctive (∧) expression.
    All,
    /// Disjunctive (∨) expression.
    Any,
    /// Logical predicate.
    Match,

    /// Template.
    Tpl,

    /// Template application (long form).
    TplApply,
    /// Template parameter (long form).
    TplParam,

    /// Template application (shorthand).
    TplApplyShort(QName),
    /// Template parameter (shorthand).
    ///
    /// The non-`@`-padded name is the first of the pair and the value as
    ///   the second.
    ///
    /// A shorthand param is implicitly closed and should not have a
    ///   matching [`Nir::Close`] token.
    TplParamShort(SPair, SPair),
}

impl NirEntity {
    pub fn open<S: Into<Span>>(self, span: S) -> Nir {
        Nir::Open(self, span.into())
    }

    pub fn close<S: Into<Span>>(self, span: S) -> Nir {
        Nir::Close(self, span.into())
    }
}

impl Display for NirEntity {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirEntity::*;

        match self {
            Package => write!(f, "package"),

            Rate => write!(f, "rate block"),
            Sum => write!(f, "sum (∑) expression"),
            Product => write!(f, "product (Π) expression"),
            Ceil => write!(f, "ceiling (⌈) expression"),
            Floor => write!(f, "floor (⌊) expression"),

            Classify => write!(f, "classification"),
            All => write!(f, "conjunctive (∧) expression"),
            Any => write!(f, "disjunctive (∨) expression"),
            Match => write!(f, "logical predicate (match)"),

            Tpl => write!(f, "template"),
            TplParam => write!(f, "template param (metavariable)"),
            TplParamShort(name, val) => write!(
                f,
                "shorthand template param key {} with value {}",
                TtQuote::wrap(name),
                TtQuote::wrap(val),
            ),
            TplApply => {
                write!(f, "full template application and expansion")
            }
            TplApplyShort(qname) => write!(
                f,
                "full shorthand template application and expansion of {}",
                TtQuote::wrap(qname.local_name())
            ),
        }
    }
}

impl Token for Nir {
    fn ir_name() -> &'static str {
        "NIR"
    }

    /// Identifying span of a token.
    ///
    /// An _identifying span_ is a selection of one of the (potentially
    ///   many) spans associated with a token that is most likely to be
    ///   associated with the identity of that token.
    fn span(&self) -> Span {
        use Nir::*;

        match self {
            Todo(span) => *span,
            TodoAttr(spair) => spair.span(),

            Open(_, span) => *span,
            Close(_, span) => *span,

            BindIdent(spair)
            | BindIdentAbstract(spair)
            | RefSubject(spair)
            | Ref(spair)
            | Desc(spair)
            | Text(spair)
            | Import(spair) => spair.span(),

            // A no-op is discarding user input,
            //   so we still want to know where that is so that we can
            //   explicitly inquire about and report on it.
            Noop(span) => *span,
        }
    }
}

impl Object for Nir {}

impl Display for Nir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Nir::*;

        match self {
            Todo(_) => write!(f, "TODO"),
            TodoAttr(spair) => write!(f, "TODO Attr {spair}"),

            Open(entity, _) => write!(f, "open {entity} entity"),
            Close(entity, _) => write!(f, "close {entity} entity"),
            BindIdent(spair) => {
                write!(
                    f,
                    "bind to concrete identifier {}",
                    TtQuote::wrap(spair)
                )
            }
            BindIdentAbstract(spair) => {
                write!(
                    f,
                    "bind to abstract identifier with future value of \
                        metavariable {}",
                    TtQuote::wrap(spair)
                )
            }
            RefSubject(spair) => {
                write!(f, "subject ref {}", TtQuote::wrap(spair))
            }
            Ref(spair) => write!(f, "ref {}", TtQuote::wrap(spair)),

            // TODO: TtQuote doesn't yet escape quotes at the time of writing!
            Desc(spair) => write!(f, "description {}", TtQuote::wrap(spair)),

            // TODO: Not yet safe to output arbitrary text;
            //   need to determine how to handle newlines and other types of
            //   output.
            Text(_) => write!(f, "text"),

            Import(namespec) => write!(
                f,
                "import package with namespec {}",
                TtQuote::wrap(namespec)
            ),

            Noop(_) => write!(f, "no-op"),
        }
    }
}

impl<E> From<Nir> for Result<Nir, E> {
    fn from(val: Nir) -> Self {
        Ok(val)
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

/// Assert that a literal value `S` was provided,
///   yielding a [`Nir::Noop`] if successful.
pub fn literal<const S: SymbolId>(
    value: SPair,
) -> Result<Nir, NirAttrParseError> {
    match value {
        SPair(val, span) if val == S => Ok(Nir::Noop(span)),
        _ => Err(NirAttrParseError::LiteralMismatch(value.span(), S)),
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
    LiteralMismatch(Span, ExpectedSymbolId),
}

impl Error for NirAttrParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl Display for NirAttrParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LiteralMismatch(_, expected) => {
                write!(f, "expected literal {}", TtQuote::wrap(expected),)
            }
        }
    }
}

impl Diagnostic for NirAttrParseError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        match self {
            Self::LiteralMismatch(span, expected) => span
                .error(format!("expecting {}", TtQuote::wrap(expected)))
                .into(),
        }
    }
}
