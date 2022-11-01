// Normalized source IR
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

//! NIR parser.
//!
//! For general information about NIR,
//!   see the [parent module](super).
//!
//! The entry point for this parser in the lowering pipeline is
//!   [`NirParseState`].
//! The grammar is defined declaratively using the [`ele_parse!`]
//!   parser-generator,
//!     which yields a parser compatible with TAME's [`crate::parse`]
//!     framework.
//!
//! Grammar Definition
//! ==================
//! The grammar can be seen in the TAMER sources;
//!   if you are viewing the generated documentation,
//!     it can be viewed by clicking on "source" in the upper-right-hand
//!     corner of this page,
//!       or on each individual identifier.
//!
//! The grammar defines nonterminals (NTs) of two forms:
//!
//!   1. [XIR](crate::xir) elements with their attributes and child NTs; and
//!   2. Sum NTs of the form `(NT₀ | NT₁ | … | NTₙ)` which match on any of
//!      inner NTs.
//!
//! Terminals are specified in element name and attribute contexts as
//!   [static QName](crate::xir::st::qname) constants of the form `QN_*`.
//! These constants are defined in [`crate::xir::st::qname`] and allow the
//!   to efficiently match on element and attribute names by comparing a
//!   single 64-bit integer value,
//!     which in turn may be optimized to compare many possible QName
//!     values simultaneously.
//!
//! The style of the grammar is meant to be a combination of a BNF and Rust
//!   syntax.
//!
//! Repetition and Templates
//! ------------------------
//! _All NTs are implicitly defined as zero-or-more_
//!   (as with the Kleene star),
//!   and this behavior cannot be overridden.
//! The rationale for this is somewhat complex,
//!   but the tradeoff greatly simplifies the [`ele_parse!`]
//!   parser-generator in recognition of a simple fact about NIR:
//!     it cannot determine statically whether a source file will conform to
//!     TAME's grammar when all templates are expanded.
//!
//! Templates require an interpreter and are expanded later in the lowering
//!   pipeline.
//! NIR is unable to perform that expansion,
//!   and so we do the best we can do in this situation:
//!     verify that templates,
//!       when expanded,
//!       will expand into primitives known to NIR,
//!         and validate those primitives when possible.
//! This can only go so far,
//!   given that templates can appear virtually anywhere in the source tree.
//!
//! Because templates are able to expand into anything that is known to
//!   NIR's grammar,
//!     NIR cannot know whether a required element has been provided or not.
//! Consequently,
//!   we cannot require that an element be present as part of NIR's grammar,
//!     since it may have been hidden behind a template application.
//! For the same reason,
//!   we cannot place _any_ restrictions on the number of repetitions of a
//!   particular element.
//!
//! The best we can do is therefore to merely validate that,
//!   whatever _is_ present,
//!   is conceivably valid at that position within the grammar.
//! It is then the burden of a future lowering operation to validate the
//!   grammar post-expansion.
//!
//! What NIR therefore provides is an IR that is _closed_ under template
//!   application---this
//!     means that,
//!       when a template _is_ expanded into an application site,
//!       it _will_ expand into a sequence of parsed NIR tokens and cannot
//!       possibly expand into anything else.
//! What the template system does with those tokens is beyond our concern.
//!
//! See [`TplKw`] for template tokens that are accepted anywhere.

use super::{NirSymbolTy::*, *};
use crate::{
    ele_parse,
    sym::st::raw::*,
    xir::{
        attr::Attr,
        st::{prefix::*, qname::*},
    },
};

type N<const TY: NirSymbolTy> = SugaredNirSymbol<TY>;

ele_parse! {
    /// Parser lowering [XIR](crate::xir) into [`SugaredNir`].
    ///
    /// TAME's grammar is embedded within XML.
    /// The outer XML document has its own grammar,
    ///   which is parsed by [XIR](crate::xir);
    ///     this parser is responsible for taking the TAME grammar within
    ///     a valid XML document and parsing it into [NIR](crate::nir).
    ///
    /// Limitations of NIR
    /// ------------------
    /// It is important to understand the purposeful
    ///   (and practical)
    ///   limitations of NIR.
    /// The grammar of NIR declares what _could acceptably appear_ in
    ///   various contexts;
    ///     it is _not_ intended to comprehensively validate what _ought_ to
    ///     appear in every conceivable context.
    /// Because TAME is a metalanguage
    ///   (through use of its template system),
    ///     we are not able to know the full grammar of the language without
    ///     compile-time template evaluation,
    ///      and so NIR's grammar will always accept a _superset_ of all
    ///      valid programs.
    ///
    /// With that said,
    ///   NIR will always lower primitives,
    ///     including within template definitions.
    /// Because of this,
    ///   all programs _are_ closed under NIR,
    ///     and we can be confident that all expanded templates will be able
    ///     to expand into a program that can be represented by NIR.
    /// Whether or not a particular expansion is semantically valid is
    ///   beyond the scope of NIR and should be handled as part of another
    ///   lowering operation.
    ///
    /// See the [parent module](super) for more information.
    ///
    /// Superstate
    /// ----------
    pub enum NirParseState;

    type AttrValueError = NirAttrParseError;
    type Object = SugaredNir;

    // Text and template expressions may appear at any point within the
    //   program;
    //     see [`NirParseState`] for more information.
    [super] {
        [text](_sym, _span) => SugaredNir::Todo,
        TplKw
    };

    /// All valid root elements declaring valid package types.
    ///
    /// Historically (in XSLT),
    ///   these packages did not all share the same compiler.
    /// This is not the case with TAMER.
    ///
    /// When the term "package" is used without an explicit qualifier,
    ///   it generally refers to a package containing only calculations and
    ///   classifications.
    PkgTypeStmt := (
        RaterStmt
        | PackageStmt
        | ProgramMapStmt
        | ReturnMapStmt
        | WorksheetStmt
    );


    /////////////////////////
    ////////////////////////
    ////
    //// Package Stmts
    ////

    /// Like a [`PackageStmt`],
    ///   but producing an executable program.
    ///
    /// The term "rater" is historical,
    ///   since TAME was designed for producing insurance rating systems.
    RaterStmt := QN_RATER {
        @ {
            _xmlns: (QN_XMLNS) => Literal<URI_LV_RATER>,
            _xmlns_c: (QN_XMLNS_C) => Literal<URI_LV_CALC>,
            _xmlns_t: (QN_XMLNS_T) => Literal<URI_LV_TPL>,

            // TODO: Is this still needed?
            // TODO: PkgName type
            _name: (QN_NAME) => N<{PkgPath}>,
        } => SugaredNir::Todo,

        ImportStmt,
        PkgBodyStmt,
    };

    /// Non-program package for calculations and logic.
    ///
    /// A package is a reusable module that can be imported by other
    ///   packages.
    /// See [`PkgTypeStmt`] for more information on the distinction between
    ///   different package types.
    PackageStmt := QN_PACKAGE {
        @ {
            _xmlns: (QN_XMLNS) => Literal<URI_LV_RATER>,
            _xmlns_c: (QN_XMLNS_C) => Literal<URI_LV_CALC>,
            _xmlns_t: (QN_XMLNS_T) => Literal<URI_LV_TPL>,

            // TODO: Having trouble getting rid of `@xmlns:lv` using Saxon
            //   for `progui-pkg`,
            //     so just allow for now.
            // It can't actually be used on nodes.
            _xmlns_lv: (QN_XMLNS_LV?) => Option<Literal<URI_LV_RATER>>,

            _id: (QN_ID?) => Option<N<{PkgPath}>>,
            _title: (QN_TITLE?) => Option<N<{PkgTitle}>>,
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
            // TODO: When can we get rid of this?
            _core: (QN_CORE?) => Option<N<{BooleanLiteral}>>,
            _program: (QN_PROGRAM?) => Option<N<{BooleanLiteral}>>,

            // TODO: Can this go away now?
            _name: (QN_NAME?) => Option<N<{PkgPath}>>,
        } => SugaredNir::Todo,

        ImportStmt,
        PkgBodyStmt,
    };

    /// Import another package's symbol table into this one.
    ///
    /// Imports allow referencing identifiers from another package and allow
    ///   for composing larger systems out of smaller components.
    ImportStmt := QN_IMPORT {
        @ {
            _pkg: (QN_PACKAGE) => N<{PkgPath}>,
            _export: (QN_EXPORT?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// A statement that is accepted within the body of a package.
    ///
    /// The parent context for these statements is most often
    ///   [`PackageStmt`].
    PkgBodyStmt := (
        ExternStmt
        | ParamStmt
        | ConstStmt
        | ClassifyStmt
        | RateStmt
        | RateEachStmt
        | TypedefStmt
        | YieldStmt
        | SectionStmt
        | TemplateStmt
        | FunctionStmt
    );

    /// Statements that are valid within the context of a [`PkgBodyStmt`]
    ///   and may be directly referenced within the body of a template.
    ///
    /// See [`AnyStmtOrExpr`] for more information on why this is needed.
    PkgStmtInner := (
        ConstStmtBody
        | InnerTypedefStmt
    );

    /// Declare a symbol that must be defined in some other package.
    ///
    /// Externs are effectively the same concept as in C---they
    ///   declare symbols that we /expect/ to exist at some point,
    ///     but we do not know where they will be defined.
    /// The linker will verify,
    ///   while linking the program,
    ///   that /at most one/ other package provides a definition for this
    ///   symbol and that the definition is compatible with this
    ///   declaration.
    ExternStmt := QN_EXTERN {
        @ {
            _name: (QN_NAME) => N<{AnyIdent}>,
            _ty: (QN_TYPE) => N<{IdentType}>,
            _dtype: (QN_DTYPE?) => Option<N<{IdentDtype}>>,
            _dim: (QN_DIM) => N<{NumLiteral}>,
            _parent: (QN_PARENT?) => Option<N<{AnyIdent}>>,
            _yields: (QN_YIELDS?) => Option<N<{ValueIdent}>>,
        } => SugaredNir::Todo,
    };

    /// Define an input parameter accepting data from an external system.
    ///
    /// Parameters are generally populated via a map,
    ///   such as [`ProgramMapStmt`].
    ParamStmt := QN_PARAM {
        @ {
            _name: (QN_NAME) => N<{ParamName}>,
            _ty: (QN_TYPE) => N<{ParamType}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
            // This is a misnomer.
            _set: (QN_SET?) => Option<N<{Dim}>>,
            _default: (QN_DEFAULT?) => Option<N<{ParamDefault}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Associate static data with an identifier.
    ///
    /// Constants may be associated with scalar, vector, or matrix values.
    /// Since all values in TAME are immutable,
    ///   constants are a way to denote values that are entirely hard-coded
    ///   rather than being derived from some external input.
    ///
    /// In the future,
    ///   constants ought to be defined as expressions that can be evaluated
    ///   at compile-time,
    ///     and re-use that familiar syntax.
    ConstStmt := QN_CONST {
        @ {
            _name: (QN_NAME) => N<{ConstIdent}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
            _value: (QN_VALUE?) => Option<N<{NumLiteral}>>,
            _values: (QN_VALUES?) => Option<N<{ShortDimNumLiteral}>>,
            // TODO: deprecate?
            _ty: (QN_TYPE?) => Option<N<{TypeIdent}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
            // TODO: Misnomer
            _set: (QN_SET?) => Option<N<{Dim}>>,
        } => SugaredNir::Todo,

        ConstStmtBody,
    };

    /// Body of a [`ConstStmt`] defining a vector value or a matrix row.
    ///
    /// Scalar constants utilize [`QN_VALUE`] instead of this body.
    ///
    /// See also [`QN_VALUES`],
    ///   which can be used as a short-hand form of this body.
    ConstStmtBody := (ConstMatrixRow | ConstVectorItem);

    /// Constant matrix row definition.
    ///
    /// TODO: The use of [`QN_SET`] is a terrible misnomer representing
    ///   dimensionality and will be changed in future versions.
    ConstMatrixRow := QN_SET {
        @ {
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,

        ConstVectorItem,
    };

    /// Constant vector scalar item definition.
    ConstVectorItem := QN_ITEM {
        @ {
            _value: (QN_VALUE) => N<{NumLiteral}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Define a classification and associate it with an identifier.
    ///
    /// A classification is a logic expression yielding a boolean result
    ///   with the dimensionality matching the largest dimensionality of its
    ///   inputs.
    ClassifyStmt := QN_CLASSIFY {
        @ {
            _name: (QN_AS) => N<{ClassIdent}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
            _any: (QN_ANY?) => Option<N<{BooleanLiteral}>>,
            _yields: (QN_YIELDS?) => Option<N<{ValueIdent}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
            _terminate: (QN_TERMINATE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,

        LogExpr,
    };

    /// Define a calculation and associate it with an identifier.
    ///
    /// The term "rate" is intended as a verb,
    ///   and represents an arbitrary calculation;
    ///     the term originates from TAME's history as an insurance rating
    ///     system.
    /// This will eventually be renamed to a more general term.
    RateStmt := QN_RATE {
        @ {
            _class: (QN_CLASS?) => Option<N<{ClassIdent}>>,
            _no: (QN_NO?) => Option<N<{ClassIdentList}>>,
            _yields: (QN_YIELDS) => N<{CalcIdent}>,
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,

            // TODO: This is still recognized by the XSLT-based compiler,
            //   so we need to support it until it's removed.
            _gentle_no: (QN_GENTLE_NO?) => Option<N<{BooleanLiteral}>>,

            // TODO: We'll have private-by-default later.
            //   This is a kludge.
            _local: (QN_LOCAL?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Define a calculation that maps a calculation to each item of a
    ///   vector,
    ///     and associate it with an identifier.
    ///
    /// This expands into an equivalent [`RateStmt`] with a nested
    ///   [`SumExpr`] serving as the item-wise map.
    RateEachStmt := QN_RATE_EACH {
        @ {
            _class: (QN_CLASS) => N<{ClassIdentList}>,
            _no: (QN_NO?) => Option<N<{ClassIdentList}>>,
            _generates: (QN_GENERATES?) => Option<N<{ValueIdent}>>,
            _index: (QN_INDEX) => N<{ValueIdent}>,
            _yields: (QN_YIELDS?) => Option<N<{ValueIdent}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
            _gensym: (QN_GENSYM?) => Option<N<{TexMathLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Define a new type that restricts the domain of data.
    TypedefStmt := QN_TYPEDEF {
        @ {
            _name: (QN_NAME) => N<{TypeIdent}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
        } => SugaredNir::Todo,

        InnerTypedefStmt,
    };

    /// Body of a [`TypedefStmt`].
    InnerTypedefStmt := (BaseTypeStmt | EnumStmt | UnionStmt);

    /// Indicate that the type is defined by the TAME compiler.
    ///
    /// This is used for primitives and allows for core types to be exposed
    ///   to the user.
    BaseTypeStmt := QN_BASE_TYPE {
        @ {} => SugaredNir::Todo,
    };

    /// Define an enumerated type.
    ///
    /// Enums are types that have an explicit set of values,
    ///   each with associated constant identifiers.
    EnumStmt := QN_ENUM {
        @ {
            _ty: (QN_TYPE) => N<{TypeIdent}>,
        } => SugaredNir::Todo,

        ItemEnumStmt,
    };

    /// Define an item of the domain of an enumerated type and associate it
    ///   with a constant identifier.
    ItemEnumStmt := QN_ITEM {
        @ {
            _name: (QN_NAME) => N<{ConstIdent}>,
            _value: (QN_VALUE) => N<{NumLiteral}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Define a type whose domain is the union of the domains of multiple
    ///   other types.
    UnionStmt := QN_UNION {
        @ {} => SugaredNir::Todo,

        TypedefStmt,
    };

    /// A final numerical value to be yielded by a program.
    ///
    /// This value has historical significance,
    ///   but is slowly being deprecated.
    /// Any number of values can be returned to the caller via a return map
    ///   (see [`ReturnMapStmt`]).
    ///
    /// This is being replaced with the `__yield__` template in `core`
    ///   (this statement predates the template system in TAME).
    YieldStmt := QN_YIELD {
        @ {} => SugaredNir::Todo,

        CalcExpr,
    };

    /// Declare that the body of this statement ought to be delimited from
    ///   the surrounding definitions with a heading when visualized.
    ///
    /// This is intended primarily for documentation,
    ///   and serves as an alternative to using packages for sectioning.
    /// Since definitions in TAME are independent from the order of
    ///   execution of the resulting executable,
    ///     definitions tend to be linear and can sometimes benefit from
    ///     grouping for organization and to help guide the reader.
    ///
    /// Otherwise,
    ///   the body of a section is the same as that of [`PackageStmt`],
    ///     with the exception of imports,
    ///       which must appear outside of sections.
    SectionStmt := QN_SECTION {
        @ {
            _title: (QN_TITLE) => N<{Title}>,
        } => SugaredNir::Todo,

        PkgBodyStmt,
    };

    /// Define a function and associate it with an identifier.
    FunctionStmt := QN_FUNCTION {
        @ {
            _name: (QN_NAME) => N<{FuncIdent}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
        } => SugaredNir::Todo,

        FunctionParamStmt,
        CalcExpr,
    };

    /// Define a function parameter and associate it with an identifier that
    ///   is scoped to the function body.
    FunctionParamStmt := QN_PARAM {
        @ {
            _name: (QN_NAME) => N<{ParamIdent}>,
            _ty: (QN_TYPE) => N<{TypeIdent}>,
            // _TODO: This is a misnomer.
            _set: (QN_SET?) => Option<N<{Dim}>>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,
    };


    /////////////////////////
    ////////////////////////
    ////
    //// Logic Expressions
    ////

    /// A logic expression.
    ///
    /// See _The TAME Programming Language_ document for a formal definition
    ///   of this subsystem and its syntax.
    LogExpr := (MatchExpr | AnyExpr | AllExpr);

    /// Scalar value predicate as part of a logic expression.
    ///
    /// The dimensionality of the expression will be automatically
    ///   determined by the dimensionality of the matches' [`@on`](QN_ON).
    MatchExpr := QN_MATCH {
        @ {
            _on: (QN_ON) => N<{ValueIdent}>,
            _value: (QN_VALUE?) => Option<N<{ValueIdent}>>,
            _index: (QN_INDEX?) => Option<N<{ValueIdent}>>,
            _anyof: (QN_ANY_OF?) => Option<N<{TypeIdent}>>,
        } => SugaredNir::Todo,

        CalcPredExpr,
    };

    /// Logical disjunction (∨).
    ///
    /// This represents an expression that matches when _any_ of its inner
    ///   [`LogExpr`] expressions match.
    AnyExpr := QN_ANY {
        @ {} => SugaredNir::Todo,

        LogExpr,
    };

    /// Logical conjunction (∧).
    ///
    /// This represents an expression that matches when _all_ of its inner
    ///   [`LogExpr`] expressions match.
    AllExpr := QN_ALL {
        @ {} => SugaredNir::Todo,

        LogExpr,
    };


    /////////////////////////
    ////////////////////////
    ////
    //// Calculations
    ////

    /// An expression producing a scalar result.
    ///
    /// Some expressions may support binding to additional identifiers.
    CalcExpr := (
        SumExpr
        | ProductExpr
        | QuotientExpr
        | ExptExpr
        | ValueOfExpr
        | ConstExpr
        | VectorExpr
        | CasesExpr
        | CeilExpr
        | FloorExpr
        | LengthOfExpr
        | LetExpr
        | ApplyExpr
        | RecurseExpr
        | ConsExpr
        | CarExpr
        | CdrExpr
    );

    /// Expressions that are valid within the context of one or more
    ///   [`CalcExpr`] and may be directly referenced within the body of a
    ///   template.
    ///
    /// See [`AnyStmtOrExpr`] for more information on why this is needed.
    CalcExprInner := (
        CalcPredExpr
        | CaseExpr
        | OtherwiseExpr
        | LetValues
        | LetValue
        | WhenExpr
        | ApplyArg
    );

    /// Summation (Σ) expression.
    ///
    /// When using [`@of`](QN_OF),
    ///   summation can also be used to produce a generator where each
    ///   iteration over `@of` yields a corresponding element in the vector
    ///   identified by [`@generates`](QN_GENERATES).
    ///
    /// Summation is generated automatically by [`RateEachStmt`].
    SumExpr := QN_C_SUM {
        @ {
            _of: (QN_OF?) => Option<N<{ValueIdent}>>,
            _generates: (QN_GENERATES?) => Option<N<{CalcIdent}>>,
            _index: (QN_INDEX?) => Option<N<{CalcIdent}>>,
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
            _dim: (QN_DIM?) => Option<N<{Dim}>>,
        } => SugaredNir::Todo,

        WhenExpr,
        CalcExpr,
    };

    /// Product (Π) expression.
    ///
    /// When using [`@of`](QN_OF),
    ///   product can also be used to produce a generator where each
    ///   iteration over `@of` yields a corresponding element in the vector
    ///   identified by [`@generates`](QN_GENERATES).
    ProductExpr := QN_C_PRODUCT {
        @ {
            _of: (QN_OF?) => Option<N<{ValueIdent}>>,
            _generates: (QN_GENERATES?) => Option<N<{CalcIdent}>>,
            _index: (QN_INDEX?) => Option<N<{CalcIdent}>>,
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
            _dot: (QN_DOT?) => Option<N<{BooleanLiteral}>>,
            _sym: (QN_SYM?) => Option<N<{TexMathLiteral}>>,
            _dim: (QN_DIM?) => Option<N<{Dim}>>,
        } => SugaredNir::Todo,

        WhenExpr,
        CalcExpr,
    };

    /// Quotient (÷) expression.
    ///
    /// Traditionally,
    ///   TAME expected quotients to contain a numerator and a denominator
    ///   as only two [`CalcExpr`] expressions
    ///     (though either could be a [`QuotientExpr`] as well).
    /// TAMER will be relaxing that restriction.
    QuotientExpr := QN_C_QUOTIENT {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Exponentiation (_xʸ_) expression.
    ///
    /// The first [`CalcExpr`] will be raised to the power of the second
    ///   [`CalcExpr`],
    ///     which will be raised to the power of any third,
    ///     and so on.
    /// Traditionally,
    ///   TAME expected only a base and an exponent
    ///     (respectively),
    ///   but TAMER will be relaxing that restriction.
    ExptExpr := QN_C_EXPT {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Expression yielding a scalar value of the provided identifier.
    ///
    /// The identifier is named by [`@name`](QN_NAME),
    ///   with vectors requiring an [`@index`](QN_INDEX).
    /// Matrices require use of a nested [`IndexExpr`] qualifier to resolve
    ///   a scalar.
    ValueOfExpr := QN_C_VALUE_OF {
        @ {
            _name: (QN_NAME) => N<{ValueIdent}>,
            _index: (QN_INDEX?) => Option<N<{ValueIdent}>>,
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        IndexExpr,
        WhenExpr,
    };

    /// Expression qualifying an index of a parent expresion.
    ///
    /// The result of the inner [`CalcExpr`] is used as a subscript of the
    ///   parent expression.
    /// Sibling [`IndexExpr`]s evaluate to nested subscripts where the
    ///   subling applies to the result of the previous index operation
    ///     such that **M**_ⱼ,ₖ_ ≡ (**M**_ⱼ_)_ₖ_.
    IndexExpr := QN_C_INDEX {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,
        CalcExpr,
    };

    /// Expression yielding a constant scalar value.
    ConstExpr := QN_C_CONST {
        @ {
            _value: (QN_VALUE) => N<{NumLiteral}>,
            // TODO: Description was historically required to avoid magic
            //   values,
            //     but we now have short-hand constants which do not require
            //     descriptions.
            // We should probably require both or neither,
            //   but requiring `c:value-of` short-hand wouldn't be
            //   the responsibility of NIR,
            //     so perhaps then neither should be.
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
            // _TODO: deprecate?
            _ty: (QN_TYPE?) => Option<N<{TypeIdent}>>,
        } => SugaredNir::Todo,

        WhenExpr,
    };

    /// Ceiling (⌈_x_⌉) expression.
    CeilExpr := QN_C_CEIL {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,
        CalcExpr,
    };

    /// Floor (⌊_x_⌋) expression.
    FloorExpr := QN_C_FLOOR {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,
        CalcExpr,
    };

    /// An expression that conditionally evaluates to sub-expressions
    ///   depending on a list of predicates.
    ///
    /// Individual cases are evaluated in order,
    ///   and the first case whose predicates
    ///     (also called "guards")
    ///     are satisfied will have its expression evaluated and yielded as
    ///     the result of the entire [`CasesExpr`].
    ///
    /// If no predicates match,
    ///   [`OtherwiseExpr`] is evaluated,
    ///     if pressent,
    ///     otherwise the value `0` is yielded.
    CasesExpr := QN_C_CASES {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        CaseExpr,
        OtherwiseExpr,
    };

    /// A predicated case of a [`CasesExpr`] with an associated
    ///   [`CalcExpr`].
    ///
    /// Cases are evaluated in the order in which they appear.
    /// If all of the [`WhenExpr`]s evaluate truthfully,
    ///   then the inner [`CalcExpr`] will be evaluated and its result
    ///   yielded as the value of this expression
    ///     (and therefore the result of the parent [`CasesExpr`]).
    /// Otherwise,
    ///   evaluation continues with the next sibling case,
    ///     if any.
    CaseExpr := QN_C_CASE {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        WhenExpr,
        CalcExpr,
    };

    /// A case of a [`CasesExpr`] that always matches.
    ///
    /// This should be used as a catch-all when no sibling [`CaseExpr`]
    ///   matches.
    /// The inner [`CalcExpr`] will be evaluated and its result yielded as
    ///   the result of this expression
    ///     (and therefore the result of the parent [`CasesExpr`]).
    ///
    /// In absence of this expression,
    ///   [`CasesExpr`] may fall through with no matching expressions and
    ///   yield `0`.
    /// If this behavior is unclear within a given context,
    ///   then [`OtherwiseExpr`] ought to be used to make the behavior
    ///   explicit.
    OtherwiseExpr := QN_C_OTHERWISE {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Length of a vector (|**v**|).
    ///
    /// This also yields the number of rows of a matrix,
    ///   which are vectors of vectors.
    /// It is not defined for scalars.
    LengthOfExpr := QN_C_LENGTH_OF {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Let expression.
    ///
    /// This is equivalent to a let expression in the Lisp family of
    ///   languages,
    ///     where the inner [`LetValues`] defines a set of mutually
    ///     independent expressions whose associated identifiers are
    ///     lexically scoped to the inner [`CalcExpr`].
    /// The result of the let expression is the result of the inner
    ///   [`CalcExpr`].
    LetExpr := QN_C_LET {
        @ {} => SugaredNir::Todo,
        LetValues,
        CalcExpr,
    };

    /// A set of mutually independent expressions and associated identifiers
    ///   to be lexically scoped to the sibling [`CalcExpr`].
    ///
    /// See [`LetExpr`] for more information.
    LetValues := QN_C_VALUES {
        @ {} => SugaredNir::Todo,
        LetValue,
    };

    /// An expression bound to an associated identifier that is lexically
    ///   scoped to a parent [`LetValues`]' sibling [`CalcExpr`].
    ///
    /// A value cannot observe sibling values,
    ///   but it can observe values of an ancestor [`LetExpr`] that is not
    ///   its parent.
    LetValue := QN_C_VALUE {
        @ {
            _name: (QN_NAME) => N<{ParamIdent}>,
            _ty: (QN_TYPE) => N<{TypeIdent}>,
            // Misnomer
            _set: (QN_SET?) => Option<N<{Dim}>>,
            _desc: (QN_DESC?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// An expression yielding a vector consisting of each of its child
    ///   expressions' values as respective items.
    VectorExpr := QN_C_VECTOR {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Function application.
    ///
    /// The value of the expression is the return value of the function
    ///   applied to its argument list [`ApplyArg`].
    ///
    /// The attribute [`@name`](QN_NAME) contains the name of the function
    ///   to apply.
    /// All other arguments are desugared into child [`ApplyArg`]s with a
    ///   body [`ValueOfExpr`] such that `α="x"` expands into
    ///   `<`[`c:arg`](QN_C_ARG)` name="α"><`[`c:value-of`](QN_C_VALUE_OF)
    ///     `name="x" /></c:arg>`.
    ApplyExpr := QN_C_APPLY {
        @ {} => SugaredNir::Todo,

        [attr](_attr) => SugaredNir::Todo,

        ApplyArg,
    };

    /// Argument for function application.
    ///
    /// Alternatively,
    ///   the parent element [`ApplyExpr`] may contain short-hand arguments
    ///   as attributes.
    ApplyArg := QN_C_ARG {
        @ {
            _name: (QN_NAME) => N<{ParamIdent}>,
        } => SugaredNir::Todo,

        CalcExpr,
    };

    /// Function application recursing on the parent [`ApplyExpr`].
    ///
    /// This expression desugars into an [`ApplyExpr`] with the same name as
    ///   the parent [`ApplyExpr`] and copies all parent [`ApplyArg`]
    ///     expressions.
    /// Any child [`ApplyArg`] of this expression will override the
    ///   arguments of the parent,
    ///     allowing for concise recursion in terms of only what has changed
    ///     in that recursive step.
    RecurseExpr := QN_C_RECURSE {
        @ {} => SugaredNir::Todo,

        [attr](_attr) => SugaredNir::Todo,

        ApplyArg,
    };

    /// Construct a list (vector) by providing a new head ("car") and a
    ///   (possibly empty) tail ("cdr").
    ///
    /// This terminology originates from Lisp.
    /// It is equivalent to an `unshift` operation.
    ConsExpr := QN_C_CONS {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Retrieve the first element in a list (vector).
    ///
    /// This terminology originates from Lisp.
    CarExpr := QN_C_CAR {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,
        CalcExpr,
    };

    /// Retrieve all but the first element of a list (vector).
    ///
    /// This terminology originates from Lisp,
    ///   and is pronounced "could-er".
    /// It is also called "tail".
    CdrExpr := QN_C_CDR {
        @ {
            _label: (QN_LABEL?) => Option<N<{DescLiteral}>>,
        } => SugaredNir::Todo,
        CalcExpr,
    };

    /// Predicate the parent expression,
    ///   producing a value of `0` if the predicate does not match.
    ///
    /// In expressions that do not require the use of [`WhenExpr`] as a
    ///   guard,
    ///     this is styled and interpreted as Iverson's brackets,
    ///     but there is no distinction between using [`WhenExpr`] and
    ///     multiplying by the value of the predicate;
    ///       the two forms are a matter of style.
    ///
    /// The exception is [`CaseExpr`],
    ///   which requires [`WhenExpr`] as part of its grammar to define
    ///   conditions for which case to evaluate.
    WhenExpr := QN_C_WHEN {
        @ {
            _name: (QN_NAME) => N<{ValueIdent}>,
            _index: (QN_INDEX?) => Option<N<{ValueIdent}>>,
            _value: (QN_VALUE?) => Option<N<{ValueIdent}>>,
        } => SugaredNir::Todo,

        CalcPredExpr,
    };

    /// Calculation predicates.
    ///
    /// These predicates are used to compare two values.
    /// They are used by [`WhenExpr`] and [`MatchExpr`].
    CalcPredExpr := (
        EqCalcPredExpr
        | NeCalcPredExpr
        | LtCalcPredExpr
        | GtCalcPredExpr
        | LteCalcPredExpr
        | GteCalcPredExpr
    );

    /// Equality predicate (=).
    EqCalcPredExpr := QN_C_EQ {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Non-equality predicate (≠).
    NeCalcPredExpr := QN_C_NE {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Less-than predicate (<).
    LtCalcPredExpr := QN_C_LT {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Greater-than predicate (>).
    GtCalcPredExpr := QN_C_GT {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Less-than or equality predicate (≤).
    LteCalcPredExpr := QN_C_LTE {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };

    /// Greater-than or equality predicate (≥).
    GteCalcPredExpr := QN_C_GTE {
        @ {} => SugaredNir::Todo,
        CalcExpr,
    };



    /////////////////////////
    ////////////////////////
    ////
    //// Map Packages
    ////


    /// Define a mapping from a Liza program definition into TAME
    ///   parameters.
    ///
    /// The coupling of this mapping is historical,
    ///   since TAME was developed to work with the Liza data collection
    ///   framework.
    /// The mapping occurs between the bucket and TAME params.
    ///
    /// This will be generalized in the future.
    ProgramMapStmt := QN_PROGRAM_MAP {
        @ {
            _xmlns: (QN_XMLNS) => Literal<URI_LV_PROGRAM_MAP>,
            _xmlnslv: (QN_XMLNS_LV) => Literal<URI_LV_RATER>,
            _src: (QN_SRC) => N<{PkgPath}>,
        } => SugaredNir::Todo,

        MapPkgImportStmt,
        MapImportStmt,
        MapBody,
    };

    /// Declare a mapping from TAME values into a key/value object to be
    ///   returned to the caller.
    ///
    /// This decouples TAME's calculations from the interface expected by
    ///   the caller.
    /// This is also the only place where TAME is able to produce dynamic
    ///   string values.
    ReturnMapStmt := QN_RETURN_MAP {
        @ {
            _xmlns: (QN_XMLNS) => Literal<URI_LV_PROGRAM_MAP>,
            _xmlnslv: (QN_XMLNS_LV) => Literal<URI_LV_RATER>,
        } => SugaredNir::Todo,

        MapPkgImportStmt,
        MapImportStmt,
        MapBody,
    };

    /// Alias for [`ImportStmt`].
    ///
    /// This is only necessary because of [`MapImportStmt`];
    ///   both that and [`MapPkgImportStmt`] will be removed in the future
    ///   in favor of [`ImportStmt`].
    MapPkgImportStmt := QN_LV_IMPORT {
        @ {
            _package: (QN_PACKAGE) => N<{PkgPath}>,
            _export: (QN_EXPORT?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Import a map package.
    ///
    /// The distinction between this an [`ImportStmt`] is historical and is
    ///   no longer meaningful;
    ///     it will be removed in the future.
    MapImportStmt := QN_IMPORT {
        @ {
            _path: (QN_PATH) => N<{PkgPath}>,
        } => SugaredNir::Todo,
    };

    /// Define the value of a key in the destination.
    MapBody := (MapPassStmt | MapStmt);

    /// Map a value into a key of the destination without modification.
    ///
    /// See also [`MapStmt`] if the value needs to be modified in some way.
    MapPassStmt := QN_PASS {
        @ {
            _name: (QN_NAME) => N<{AnyIdent}>,
            _default: (QN_DEFAULT?) => Option<N<{NumLiteral}>>,
            _scalar: (QN_SCALAR?) => Option<N<{BooleanLiteral}>>,
            _override: (QN_OVERRIDE?) => Option<N<{BooleanLiteral}>>,
            _novalidate: (QN_NOVALIDATE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Map a value into a key of the destination.
    ///
    /// See also [`MapPassStmt`] if the value does not need modification.
    MapStmt := QN_MAP {
        @ {
            _to: (QN_TO) => N<{AnyIdent}>,
            _from: (QN_FROM?) => Option<N<{AnyIdent}>>,
            // We need to be permissive in what we accept since this may
            //   match in different contexts;
            //     downstream IR will validate the against the map
            //     destination.
            _value: (QN_VALUE?) => Option<N<{StringLiteral}>>,
            _default: (QN_DEFAULT?) => Option<N<{NumLiteral}>>,
            _scalar: (QN_SCALAR?) => Option<N<{BooleanLiteral}>>,
            _override: (QN_OVERRIDE?) => Option<N<{BooleanLiteral}>>,
            _novalidate: (QN_NOVALIDATE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,

        MapStmtBody,
    };

    /// Methods for mapping a value.
    MapStmtBody := (MapFromStmt | MapSetStmt | MapTransformStmt);

    /// Source of data for a map operation.
    MapFromStmt := QN_FROM {
        @ {
            _name: (QN_NAME) => N<{AnyIdent}>,
            _default: (QN_DEFAULT?) => Option<N<{NumLiteral}>>,
            _scalar: (QN_SCALAR?) => Option<N<{BooleanLiteral}>>,
            _novalidate: (QN_NOVALIDATE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,

        MapTranslateStmt,
    };

    /// List of 1:1 value translations for a map.
    MapTranslateStmt := QN_TRANSLATE {
        @ {
            _key: (QN_KEY) => N<{StringLiteral}>,
            _value: (QN_VALUE) => N<{NumLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Yield a vector of values where each item corresponds to the
    ///   respective child expression.
    ///
    /// TODO: This is a misnomer,
    ///   since the result is a vector,
    ///   not a set.
    MapSetStmt := QN_SET {
        @ {} => SugaredNir::Todo,

        MapSetBody,
    };

    /// Permitted mappings in a [`MapSetStmt`].
    MapSetBody := (MapFromStmt | MapConstStmt);

    /// Map from a constant value.
    MapConstStmt := QN_CONST {
        @ {
            _value: (QN_VALUE) => N<{StringLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Transform a value using some function.
    ///
    /// This is currently only meaningful for string inputs,
    ///   for example to convert input string case and hash values.
    ///
    /// Transformations may be composed via nesting.
    MapTransformStmt := QN_TRANSFORM {
        @ {
            _method: (QN_METHOD) => N<{MapTransformLiteral}>,
        } => SugaredNir::Todo,

        MapStmtBody,
    };


    /////////////////////////
    ////////////////////////
    ////
    //// Worksheets
    ////


    /// Define a calculation worksheet.
    ///
    /// This is also referred to as a "rating worksheet" because of TAME's
    ///   history as an insurance rating system.
    ///
    /// A worksheet displays simplified human-readable calculations and
    ///   their results.
    /// This is an alternative to the Summary Page,
    ///   which provides a complete view of the system and is likely far too
    ///   much information for most users.
    ///
    /// Calculations are rendered in the order in which they appear in this
    ///   definition.
    WorksheetStmt := QN_WORKSHEET {
        @ {
            _xmlns: (QN_XMLNS) => Literal<URI_LV_WORKSHEET>,

            _name: (QN_NAME) => N<{PkgPath}>,
            _pkg: (QN_PACKAGE) => N<{PkgPath}>,
        } => SugaredNir::Todo,

        ExpandFunctionStmt,
        DisplayStmt,
    };

    /// Render function arguments when encountered within a calculation
    ///   referenced by [`DisplayStmt`].
    ///
    /// If a function is not expanded,
    ///   then its application is replaced with the name of the function.
    /// The default behavior is intended to encapsulate details of functions
    ///   that happen to be used by the system but that the user is unlikely
    ///   to care about.
    ExpandFunctionStmt := QN_EXPAND_FUNCTION {
        @ {
            _name: (QN_NAME) => N<{FuncIdent}>,
        } => SugaredNir::Todo,
    };

    /// Render a simplified, human-readable display of the calculation,
    ///   along with its result.
    DisplayStmt := QN_DISPLAY {
        @ {
            _name: (QN_NAME) => N<{ValueIdent}>,
        } => SugaredNir::Todo,
    };



    /////////////////////////
    ////////////////////////
    ////
    //// Template System
    ////


    /// Any statement or expression that may conceivably be permitted within
    ///   the expansion context of a template.
    ///
    /// Since templates may be used almost anywhere,
    ///   NIR must accept any statement or expression that is valid in an
    ///   expansion context.
    /// This must include not only the toplevel statements and expressions,
    ///   such as [`PkgBodyStmt`],
    ///   but also _inner_ statements.
    /// For example,
    ///   consider this common pattern:
    ///
    /// ```xml
    ///   <c:cases>
    ///     <c:case>
    ///       <t:when-gt name="foo" value="#5" />
    ///       <c:value-of name="bar" />
    ///     </c:case>
    ///
    ///     <!-- ... -->
    ///   </c:cases>
    /// ```
    ///
    /// In the above [`CasesExpr`],
    ///   a template appears where a [`WhenExpr`] is expected,
    ///   within a [`CaseExpr`].
    /// The template `__when-gt__` will be defined something like this:
    ///
    /// ```xml
    ///   <template name="__when-gt__" desc="...">
    ///     <!-- params ... -->
    ///
    ///     <c:when name="@name@">
    ///       <c:gt>
    ///         <c:value-of name="@value@" />
    ///       </c:gt>
    ///     </c:when>
    ///   </template>
    /// ```
    ///
    /// Therefore,
    ///   [`WhenExpr`] must be permitted as a direct child of
    ///   [`TemplateStmt`].
    /// Whether or not such a thing is semantically valid depends on the
    ///   context in which the application of `__when-gt__` occurs,
    ///     which cannot be known by NIR since templates are not evaluated
    ///     at this stage;
    ///       that is the responsibility of later lowering stages.
    AnyStmtOrExpr := (
        PkgBodyStmt
        // Until we fix QN_SET ambiguity, this should take precedence.
        | InlineTemplateArgSet
        | PkgStmtInner
        | LogExpr
        | CalcExpr
        | CalcExprInner
    );


    /// Define a template.
    ///
    /// Templates are TAME's metaprogramming facility and allow for
    ///   extending the grammar of TAME.
    /// The body of a template is expanded into its application site.
    ///
    /// A template may expand into multiple statements or expressions,
    ///   or even a mix of both,
    ///   with statements being hoisted automatically out of an expression
    ///   context.
    ///
    /// For more information on what may be contained within a template body
    ///   and the context of its expansion,
    ///     see [`AnyStmtOrExpr`].
    ///
    /// See also [`InlineTemplate`] for template definitions.
    ///
    /// Templates are applied using [`ApplyTemplate`] or [`TplApplyShort`].
    TemplateStmt := QN_TEMPLATE {
        @ {
            _name: (QN_NAME) => N<{TplName}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,

        TplHeading,
        AnyStmtOrExpr,
    };

    /// Heading of a template definition.
    ///
    /// The should consist entirely of [`TplParamStmt`],
    ///   but there is also a convention of placing [`TplIf`] and
    ///   [`TplUnless`] alongside those params when they perform input
    ///   validation.
    TplHeading := (TplParamStmt | TplIf | TplUnless);

    /// Define a template parameter.
    ///
    /// Template parameters have the form `@name@` and represent
    ///   placeholders for expansion data.
    /// Parameters are treated as string data during application,
    ///   but their final type depends on the context into which they are
    ///   expanded.
    TplParamStmt := QN_PARAM {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
            _desc: (QN_DESC) => N<{DescLiteral}>,
        } => SugaredNir::Todo,

        TplParamDefault,
    };

    /// Define the default value for a parameter when a value is not
    ///   provided by a template application.
    ///
    /// When a template is applied using [`ApplyTemplate`] or
    ///   [`TplApplyShort`],
    ///     a parameter will evaluate this default expression if there is no
    ///     argument present with the same name as the parameter.
    TplParamDefault := (
        TplText
        | TplParamValue
        | TplParamInherit
        | TplParamAdd
        | TplParamClassToYields
        | TplParamTypedefLookup
        | TplParamSymValue
    );

    /// Default a parameter to a string value.
    ///
    /// All template params are strings until they are expanded into a
    ///   context,
    ///     so this can be used for everything from identifier generation to
    ///     providing constant values.
    /// The result will be as if the user typed the text themselves in the
    ///   associated template application argument.
    TplText := QN_TEXT {
        @ {
            _unique: (QN_UNIQUE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Default the param to the value of another template param,
    ///   optionally transformed.
    ///
    /// This is used primarily for generating identifiers.
    /// This list of attributes represent methods to be applied.
    ///
    /// This list will be refined further in TAMER,
    ///   since manipulation of values in the XSLT-based TAME was
    ///   cumbersome and slow
    TplParamValue := QN_PARAM_VALUE {
        @ {
            _name: (QN_NAME) => N<{ParamIdent}>,
            _dash: (QN_DASH?) => Option<N<{BooleanLiteral}>>,
            _upper: (QN_UPPER?) => Option<N<{BooleanLiteral}>>,
            _lower: (QN_LOWER?) => Option<N<{BooleanLiteral}>>,
            _ucfirst: (QN_UCFIRST?) => Option<N<{BooleanLiteral}>>,
            _rmdash: (QN_RMDASH?) => Option<N<{BooleanLiteral}>>,
            _rmunderscore: (QN_RMUNDERSCORE?) => Option<N<{BooleanLiteral}>>,
            _identifier: (QN_IDENTIFIER?) => Option<N<{BooleanLiteral}>>,
            _snake: (QN_SNAKE?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Inherit a default value from a metavalue.
    ///
    /// Metavalues allow templates to communicate with one-another in an
    ///   expansion environment.
    /// They are defined using [`TplParamMeta`],
    ///   and this expression will retrieve the "closest" preceding value
    ///     from siblings and ancestors,
    ///       which is defined lexically relative to the expansion position
    ///       of the template.
    TplParamInherit := QN_PARAM_INHERIT {
        @ {
            _meta: (QN_META) => N<{TplMetaIdent}>,
        } => SugaredNir::Todo,
    };

    /// Sum a numeric value with a numeric template parameter.
    ///
    /// Combined with [`TplParamInherit`],
    ///   this can be used to perform bounded recursive template expansion.
    TplParamAdd := QN_PARAM_ADD {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
            _value: (QN_VALUE) => N<{NumLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Look up the [`@yields`](QN_YIELDS) of a [`ClassifyStmt`].
    ///
    /// This allows templates to accept classification names and use them in
    ///   an expression context.
    /// This is necessary since,
    ///   for historical reasons (accumulators),
    ///   classification names do not represent values.
    /// Instead,
    ///   to treat a classification as a value,
    ///   its corresponding [`@yields`](QN_YIELDS) must be used.
    ///
    /// Every [`ClassifyStmt`] has a yields generated for it if one is not
    ///   defined,
    ///     so this will always produce some valid identifier for a
    ///     classification.
    TplParamClassToYields := QN_PARAM_CLASS_TO_YIELDS {
        @ {
            _name: (QN_NAME) => N<{ClassIdent}>,
        } => SugaredNir::Todo,
    };

    /// Given a numeric literal,
    ///   look up the associated constant item identifier within the
    ///   provided type [`@name`](QN_NAME).
    ///
    /// The type must have been defined using [`TypedefStmt`] and must
    ///   utilize [`EnumStmt`].
    ///
    /// Since all values in TAME are referentially transparent,
    ///   this has no effect at runtime.
    /// Instead,
    ///   the purpose of this template is to allow generated code to
    ///   do two things:
    ///
    ///   1. Ensure that a numeric value is within the domain of a given
    ///        type at compile time; and
    ///   2. Produce an edge to that item
    ///        (and consequently type)
    ///        in TAME's dependency graph.
    ///
    /// By providing an edge in the dependency graph to that item,
    ///   the graph can be used to query for what parts of the system
    ///   utilize particular values within the context of a type.
    ///
    /// In this sense,
    ///   this introduces a form of nominal typing,
    ///   where the type can be used as a database of values and the
    ///     dependency graph can be used as a database of references.
    ///
    /// For example,
    ///   in insurance,
    ///   a _class code_ is a numeric identifier representing some type of
    ///   potentially insurable risk.
    /// By defining those class codes in types,
    ///   the system can be used to accurately report on what calculations
    ///   and classifications are affected by that class code.
    /// Without the use of types,
    ///   querying for a constant numeric value would be ambiguous and
    ///   potentially yield false matches.
    TplParamTypedefLookup := QN_PARAM_TYPEDEF_LOOKUP {
        @ {
            _name: (QN_NAME) => N<{TypeIdent}>,
            _value: (QN_VALUE) => N<{NumLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Look up an attribute from the symbol table for a given identifier.
    TplParamSymValue := QN_PARAM_SYM_VALUE {
        @ {
            _name: (QN_NAME) => N<{AnyIdent}>,
            _value: (QN_VALUE) => N<{SymbolTableKey}>,
            _prefix: (QN_PREFIX?) => Option<N<{AnyIdent}>>,
            _ignore_missing: (QN_IGNORE_MISSING?) => Option<N<{BooleanLiteral}>>,
        } => SugaredNir::Todo,
    };

    /// Keywords that trigger template expansion.
    ///
    /// These expressions may appear virtually anywhere in NIR,
    ///   since templates may be used to augment virtually every portion of
    ///   TAME's grammar.
    /// The context into which a template is expanded may not be valid,
    ///   but this will not be known until templates are evaluated,
    ///     which is not the responsibility of NIR.
    ///
    /// Note that these are expressions in a compile-time _expansion_
    ///   context,
    ///     not a runtime calculation context as other expressions in NIR.
    /// The result of a template expression is conceptually an XML tree,
    ///   as if the user pasted the body of the template into place and
    ///   manually replaced all parameters with their intended values.
    /// Not all expressions yield a tree,
    ///   and some may yield multiple trees;
    ///     NIR does not know or care.
    TplKw := (
        ApplyTemplate
        | TplApplyShort
        | InlineTemplate
        | ExpandSequence
        | ExpandGroup
        | ExpandBarrier
        | TplIf
        | TplUnless
        | TplParamCopy
        | TplParamMeta
        | ErrorKw
        | WarningKw
        | DynNode
    );

    // TODO: This has to go away so that we can always statically lower all
    //   primitives without having to perform template expansion in order to
    //   determine what they may be.
    DynNode := QN_DYN_NODE {
        @ {
            _name: (QN_NAME) => N<{DynNodeLiteral}>,
        } => SugaredNir::Todo,

        // But we can at least restrict it for now by ensuring that it's
        //   used only to contain expressions.
        CalcExpr,
    };

    /// Produce a compiler error whose message is the expansion of the body
    ///   of this expression.
    ///
    /// This template yields an empty result.
    ///
    /// Errors will result in a compilation failure.
    /// See also [`WarningKw`] to provide a message to the user as
    ///   compiler output without failing compilation.
    ErrorKw := QN_ERROR {
        @ {} => SugaredNir::Todo,

        // In addition to text that is globally permitted.
        TplParamValue,
    };

    /// Produce a compiler warning whose message is the expansion of the
    ///   body of this expression.
    ///
    /// This template yields an empty result.
    ///
    /// Warnings do not result in a compilation failure and may therefore be
    ///   missed in a sea of build output;
    ///     you should consider using [`ErrorKw`] whenever possible to
    ///     ensure that problems are immediately resolved.
    WarningKw := QN_WARNING {
        @ {} => SugaredNir::Todo,

        // In addition to text that is globally permitted.
        TplParamValue,
    };

    /// Long-form template application.
    ///
    /// This is neither a statement nor an expression as a part of this
    ///   grammar,
    ///     because this application is replaced entirely with its body
    ///     during expansion.
    /// Further,
    ///   the template could expand into multiple statements or expressions,
    ///     or even a mix of the two
    ///       (with statements hoisted out of expressions).
    ///
    /// TODO: This is apparently unused by the current system,
    ///   in favor of a transition to [`TplApplyShort`],
    ///   but this is still needed to support dynamic template application
    ///     (templates whose names are derived from other template inputs).
    ApplyTemplate := QN_APPLY_TEMPLATE {
        @ {} => SugaredNir::Todo,

        // TODO
    };

    /// Short-hand template application.
    ///
    /// This expands into an equivalent [`ApplyTemplate`] form where each
    ///   attribute is a template argument,
    ///     and where the body of this application is the `@values@`
    ///     template argument.
    /// See [`ApplyTemplate`] for more information.
    TplApplyShort := NS_T {
        @ {} => SugaredNir::Todo,

        // Streaming attribute parsing;
        //   this takes precedence over any attribute parsing above
        //     (which is used only for emitting the opening object).
        [attr](_attr) => SugaredNir::Todo,

        // Template bodies depend on context,
        //   so we have to just accept everything and defer to a future
        //   lowering operation to validate semantics.
        AnyStmtOrExpr,
    };

    /// Define an anonymous template and immediately apply it zero or more
    ///   times.
    ///
    /// Inline templates allow for the definition of a template at its
    ///   expansion site,
    ///     where a re-usable named template is not necessary.
    ///
    /// Inline templates are also used for iterating over a list defined by
    ///   [`InlineTemplateForEach`],
    ///     and have the unique ability to perform symbol table
    ///     introspection using [`InlineTemplateSymSet`].
    InlineTemplate := QN_INLINE_TEMPLATE {
        @ {} => SugaredNir::Todo,

        InlineTemplateForEach,
        AnyStmtOrExpr,
    };

    /// Define a list of [`InlineTemplateArgs`] over which an inline
    ///   template will be applied.
    ///
    /// If there are N [`InlineTemplateArgs`],
    ///   then the body of the parent [`InlineTemplate`] will be applied
    ///   N times,
    ///     each with the respective [`InlineTemplateArgs`] set as its
    ///     arguments.
    InlineTemplateForEach := QN_FOR_EACH {
        @ {} => SugaredNir::Todo,

        InlineTemplateArgs,
    };

    /// Inline template argument sets.
    InlineTemplateArgs := (InlineTemplateArgSet | InlineTemplateSymSet);

    /// Define an argument set for an ancestor [`InlineTemplate`]
    ///   application.
    ///
    /// Each key represents the name of a template parameter,
    ///   and the value represents the string value to bind to that
    ///   parameter as an argument.
    ///
    /// See also parent [`InlineTemplateForEach`].
    InlineTemplateArgSet := QN_SET {
        @ {} => SugaredNir::Todo,

        // Streaming attribute parsing.
        [attr](_attr) => SugaredNir::Todo,

        // TODO: REMOVE ME
        //   (bug in `ele_parse!` requiring at least one NT in this
        //      context.)
        CalcExpr,
    };

    /// Derive template arguments from symbol table introspection.
    ///
    /// This defines template arguments for the ancestor [`InlineTemplate`]
    ///   by querying the symbol table and exposing attributes associated
    ///   with that symbol.
    ///
    /// See also [`ExpandSequence`] to control when symbol table querying
    ///   takes place to ensure that all identifiers in the same package are
    ///   defined before querying.
    ///
    /// TODO: This is a really powerful feature that needs plenty of
    ///   documentation and examples.
    InlineTemplateSymSet := QN_SYM_SET {
        @ {
            _name_prefix: (QN_NAME_PREFIX?) => Option<N<{StringLiteral}>>,
            _type: (QN_TYPE?) => Option<N<{IdentType}>>,
            // TODO: Look at XSL sources for others
        } => SugaredNir::Todo,
    };

    /// Perform template expansion on each successive child node in order,
    ///   as if it were a separate template pass each time.
    ///
    /// Each child is recursively expanded before moving on to expansion of
    ///   the next child.
    ///
    /// The purpose of this sequence is to ensure that identifiers are
    ///   defined before templates that query the symbol table via
    ///   [`InlineTemplateSymSet`];
    ///     otherwise.
    /// It is otherwise not possible to guarantee that identifiers produced
    ///   by template expansions in the same package are complete before
    ///   the query takes place.
    ///
    /// The XSLT-based version of TAME forced a separate template pass for
    ///   each and every child in this sequence,
    ///     which is expensive;
    ///       [`ExpandGroup`] was added to help mitigate the cost of this
    ///       operation.
    ///
    /// TAMER hopes to remove the need for expansion sequences entirely,
    ///   since it makes complex use of the template system difficult to
    ///   understand,
    ///     and error-prone.
    /// The concept originates from TeX's `\expandafter`, `\edef`, and
    ///   related macros.
    ExpandSequence := QN_EXPAND_SEQUENCE {
        @ {} => SugaredNir::Todo,
        AnyStmtOrExpr,
    };

    /// Groups nodes to be expanded together during [`ExpandSequence`].
    ///
    /// This exists to work around performance pitfalls of the XSLT-based
    ///   implementation of [`ExpandSequence`];
    ///     see that NT for more information.
    ExpandGroup := QN_EXPAND_GROUP {
        @ {} => SugaredNir::Todo,
        AnyStmtOrExpr,
    };

    /// Prohibit template expansion beyond this point.
    ///
    /// An expansion barrier is a seldom-needed feature that stops the
    ///   template system from expanding its body beyond a certain point,
    ///     which is sometimes needed for template-producing templates.
    ExpandBarrier := QN_EXPAND_BARRIER {
        @ {} => SugaredNir::Todo,
        AnyStmtOrExpr,
    };

    /// Inline the value of a parameter as a tree.
    ///
    /// This is only useful for the special `@values@` parameter,
    ///   whose value is (conceptually) an XML tree.
    ///
    /// This allows creating templates that accept children.
    TplParamCopy := QN_PARAM_COPY {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
        } => SugaredNir::Todo,
    };

    /// Define a metavalue at this point in the expansion environment.
    ///
    /// For more information on how these values are used,
    ///   see [`TplParamInherit`].
    TplParamMeta := QN_PARAM_META {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
            _value: (QN_VALUE) => N<{StringLiteral}>,
        } => SugaredNir::Todo,
    };

    /// Conditionally expand the body if the provided predicate matches.
    TplIf := QN_IF {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
            _eq: (QN_EQ?) => Option<N<{StringLiteral}>>,
            _gt: (QN_GT?) => Option<N<{NumLiteral}>>,
            _gte: (QN_GTE?) => Option<N<{NumLiteral}>>,
            _lt: (QN_LT?) => Option<N<{NumLiteral}>>,
            _lte: (QN_LTE?) => Option<N<{NumLiteral}>>,
            _prefix: (QN_PREFIX?) => Option<N<{StringLiteral}>>,
            _suffix: (QN_SUFFIX?) => Option<N<{StringLiteral}>>,
        } => SugaredNir::Todo,

        AnyStmtOrExpr,
    };

    /// Conditionally expand the body if the provided predicate does not
    ///   match.
    ///
    /// This can be used as a sibling of [`TplIf`] to create the equivalent
    ///   of an `else` clause.
    TplUnless := QN_UNLESS {
        @ {
            _name: (QN_NAME) => N<{TplParamIdent}>,
            _eq: (QN_EQ?) => Option<N<{StringLiteral}>>,
            _gt: (QN_GT?) => Option<N<{NumLiteral}>>,
            _gte: (QN_GTE?) => Option<N<{NumLiteral}>>,
            _lt: (QN_LT?) => Option<N<{NumLiteral}>>,
            _lte: (QN_LTE?) => Option<N<{NumLiteral}>>,
            _prefix: (QN_PREFIX?) => Option<N<{StringLiteral}>>,
            _suffix: (QN_SUFFIX?) => Option<N<{StringLiteral}>>,
        } => SugaredNir::Todo,

        AnyStmtOrExpr,
    };
}
