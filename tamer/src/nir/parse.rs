// Normalized source IR
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

//! Decompose a [XIRF](crate::xir::flat) stream into NIR.
//!
//! TAME's grammar is embedded within the grammar of a document,
//!   in this case XML.
//! The purpose of this parser is to extract the grammar of TAME from the
//!   XML document and represent it as NIR.
//! This parser merely describes _the permissable structure of the
//!   document_,
//!     but nothing more.
//! For example,
//!   whether an attribute is required depends on the what parsers later in
//!   the lowering pipeline require of NIR within a given context;
//!     this parser merely describes how to translate an attribute into NIR
//!     if it happens to be present,
//!       and rejects attributes that it does not know about.
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

use super::{Nir::*, *};
use crate::{
    ele_parse,
    sym::st::raw::*,
    xir::st::{prefix::*, qname::*},
};

ele_parse! {
    /// Parser lowering [XIR](crate::xir) into [`Nir`].
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
    type Object = Nir;

    // Text and template expressions may appear at any point within the
    //   program;
    //     see [`NirParseState`] for more information.
    [super] {
        [text](sym, span) => Nir::Text(SPair(sym, span)),
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
    RaterStmt := QN_RATER(_, ospan) {
        @ {
            QN_XMLNS => TodoAttr,
            QN_XMLNS_C => TodoAttr,
            QN_XMLNS_T => TodoAttr,

            // TODO: Is this still needed?
            // TODO: PkgName type
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),

        ImportStmt,
        PkgBodyStmt,
    };

    /// Non-program package for calculations and logic.
    ///
    /// A package is a reusable module that can be imported by other
    ///   packages.
    /// See [`PkgTypeStmt`] for more information on the distinction between
    ///   different package types.
    PackageStmt := QN_PACKAGE(_, ospan) {
        @ {
            QN_XMLNS => literal::<{URI_LV_RATER}>,
            QN_XMLNS_C => literal::<{URI_LV_CALC}>,
            QN_XMLNS_T => literal::<{URI_LV_TPL}>,

            // TODO: Having trouble getting rid of `@xmlns:lv` using Saxon
            //   for `progui-pkg`,
            //     so just allow for now.
            // It can't actually be used on nodes.
            QN_XMLNS_LV => literal::<{URI_LV_RATER}>,

            QN_ID => TodoAttr,
            QN_TITLE => TodoAttr,
            QN_DESC => TodoAttr,

            // TODO: When can we get rid of this?
            QN_CORE => TodoAttr,
            QN_PROGRAM => TodoAttr,

            // TODO: Can this go away now?
            QN_NAME => TodoAttr,
        } => NirEntity::Package.open(ospan),
        /(cspan) => NirEntity::Package.close(cspan),

        ImportStmt,
        PkgBodyStmt,
    };

    /// Import another package's symbol table into this one.
    ///
    /// Imports allow referencing identifiers from another package and allow
    ///   for composing larger systems out of smaller components.
    ImportStmt := QN_IMPORT(_, ospan) {
        @ {
            QN_PACKAGE => Import,
            QN_EXPORT => TodoAttr,
        } => Noop(ospan.into()),
        //   ^ we only care about the `Ref`
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
    ExternStmt := QN_EXTERN(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_TYPE => TodoAttr,
            QN_DTYPE => TodoAttr,
            QN_DIM => TodoAttr,
            QN_PARENT => TodoAttr,
            QN_YIELDS => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Define an input parameter accepting data from an external system.
    ///
    /// Parameters are generally populated via a map,
    ///   such as [`ProgramMapStmt`].
    ParamStmt := QN_PARAM(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_TYPE => TodoAttr,
            QN_DESC => TodoAttr,
            // This is a misnomer.
            QN_SET => TodoAttr,
            QN_DEFAULT => TodoAttr,
            QN_SYM => TodoAttr,
        } => Todo(ospan.into()),
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
    ConstStmt := QN_CONST(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DESC => TodoAttr,
            QN_VALUE => TodoAttr,
            QN_VALUES => TodoAttr,
            // TODO: deprecate?
            QN_TYPE => TodoAttr,
            QN_SYM => TodoAttr,
            // TODO: Misnomer
            QN_SET => TodoAttr,
        } => Todo(ospan.into()),

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
    ConstMatrixRow := QN_SET(_, ospan) {
        @ {
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),

        ConstVectorItem,
    };

    /// Constant vector scalar item definition.
    ConstVectorItem := QN_ITEM(_, ospan) {
        @ {
            QN_VALUE => TodoAttr,
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Define a classification and associate it with an identifier.
    ///
    /// A classification is a logic expression yielding a boolean result
    ///   with the dimensionality matching the largest dimensionality of its
    ///   inputs.
    ClassifyStmt := QN_CLASSIFY(_, ospan) {
        @ {
            QN_AS => BindIdent,
            QN_DESC => Desc,
            QN_ANY => TodoAttr,
            QN_YIELDS => TodoAttr,
            QN_SYM => TodoAttr,
            QN_TERMINATE => TodoAttr,
        } => NirEntity::Classify.open(ospan),
        /(cspan) => NirEntity::Classify.close(cspan),

        LogExpr,
    };

    /// Define a calculation and associate it with an identifier.
    ///
    /// The term "rate" is intended as a verb,
    ///   and represents an arbitrary calculation;
    ///     the term originates from TAME's history as an insurance rating
    ///     system.
    /// This will eventually be renamed to a more general term.
    RateStmt := QN_RATE(_, ospan) {
        @ {
            QN_CLASS => TodoAttr,
            QN_NO => TodoAttr,
            QN_YIELDS => BindIdent,
            QN_DESC => TodoAttr,
            QN_SYM => TodoAttr,

            // TODO: This is still recognized by the XSLT-based compiler,
            //   so we need to support it until it's removed.
            QN_GENTLE_NO => TodoAttr,

            // TODO: We'll have private-by-default later.
            //   This is a kludge.
            QN_LOCAL => TodoAttr,
        } => NirEntity::Rate.open(ospan),
        /(cspan) => NirEntity::Rate.close(cspan),

        CalcExpr,
    };

    /// Define a calculation that maps a calculation to each item of a
    ///   vector,
    ///     and associate it with an identifier.
    ///
    /// This expands into an equivalent [`RateStmt`] with a nested
    ///   [`SumExpr`] serving as the item-wise map.
    RateEachStmt := QN_RATE_EACH(_, ospan) {
        @ {
            QN_CLASS => TodoAttr,
            QN_NO => TodoAttr,
            QN_GENERATES => TodoAttr,
            QN_INDEX => TodoAttr,
            QN_YIELDS => TodoAttr,
            QN_SYM => TodoAttr,
            QN_GENSYM => TodoAttr,
        } => Todo(ospan.into()),

        CalcExpr,
    };

    /// Define a new type that restricts the domain of data.
    TypedefStmt := QN_TYPEDEF(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DESC => TodoAttr,
            QN_SYM => TodoAttr,
        } => Todo(ospan.into()),

        InnerTypedefStmt,
    };

    /// Body of a [`TypedefStmt`].
    InnerTypedefStmt := (BaseTypeStmt | EnumStmt | UnionStmt);

    /// Indicate that the type is defined by the TAME compiler.
    ///
    /// This is used for primitives and allows for core types to be exposed
    ///   to the user.
    BaseTypeStmt := QN_BASE_TYPE(_, ospan) {
        @ {} => Todo(ospan.into()),
    };

    /// Define an enumerated type.
    ///
    /// Enums are types that have an explicit set of values,
    ///   each with associated constant identifiers.
    EnumStmt := QN_ENUM(_, ospan) {
        @ {
            QN_TYPE => TodoAttr,
        } => Todo(ospan.into()),

        ItemEnumStmt,
    };

    /// Define an item of the domain of an enumerated type and associate it
    ///   with a constant identifier.
    ItemEnumStmt := QN_ITEM(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_VALUE => TodoAttr,
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Define a type whose domain is the union of the domains of multiple
    ///   other types.
    UnionStmt := QN_UNION(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    YieldStmt := QN_YIELD(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    SectionStmt := QN_SECTION(_, ospan) {
        @ {
            QN_TITLE => TodoAttr,
        } => Todo(ospan.into()),

        PkgBodyStmt,
    };

    /// Define a function and associate it with an identifier.
    FunctionStmt := QN_FUNCTION(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DESC => TodoAttr,
            QN_SYM => TodoAttr,
        } => Todo(ospan.into()),

        FunctionParamStmt,
        CalcExpr,
    };

    /// Define a function parameter and associate it with an identifier that
    ///   is scoped to the function body.
    FunctionParamStmt := QN_PARAM(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_TYPE => TodoAttr,
            // _TODO: This is a misnomer.
            QN_SET => TodoAttr,
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),
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
    MatchExpr := QN_MATCH(_, ospan) {
        @ {
            QN_ON => RefSubject,
            QN_VALUE => Ref,
            QN_INDEX => TodoAttr,
            QN_ANY_OF => TodoAttr,
        } => NirEntity::Match.open(ospan),
        /(cspan) => NirEntity::Match.close(cspan),

        CalcPredExpr,
    };

    /// Logical disjunction (∨).
    ///
    /// This represents an expression that matches when _any_ of its inner
    ///   [`LogExpr`] expressions match.
    AnyExpr := QN_ANY(_, ospan) {
        @ {} => NirEntity::Any.open(ospan),
        /(cspan) => NirEntity::Any.close(cspan),

        LogExpr,
    };

    /// Logical conjunction (∧).
    ///
    /// This represents an expression that matches when _all_ of its inner
    ///   [`LogExpr`] expressions match.
    AllExpr := QN_ALL(_, ospan) {
        @ {} => NirEntity::All.open(ospan),
        /(cspan) => NirEntity::All.close(cspan),

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
    SumExpr := QN_C_SUM(_, ospan) {
        @ {
            QN_OF => TodoAttr,
            QN_GENERATES => TodoAttr,
            QN_INDEX => TodoAttr,
            QN_DESC => TodoAttr,
            QN_LABEL => TodoAttr,
            QN_SYM => TodoAttr,
            QN_DIM => TodoAttr,
        } => NirEntity::Sum.open(ospan),
        /(cspan) => NirEntity::Sum.close(cspan),

        WhenExpr,
        CalcExpr,
    };

    /// Product (Π) expression.
    ///
    /// When using [`@of`](QN_OF),
    ///   product can also be used to produce a generator where each
    ///   iteration over `@of` yields a corresponding element in the vector
    ///   identified by [`@generates`](QN_GENERATES).
    ProductExpr := QN_C_PRODUCT(_, ospan) {
        @ {
            QN_OF => TodoAttr,
            QN_GENERATES => TodoAttr,
            QN_INDEX => TodoAttr,
            QN_DESC => TodoAttr,
            QN_LABEL => TodoAttr,
            QN_DOT => TodoAttr,
            QN_SYM => TodoAttr,
            QN_DIM => TodoAttr,
        } => NirEntity::Product.open(ospan),
        /(cspan) => NirEntity::Product.close(cspan),

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
    QuotientExpr := QN_C_QUOTIENT(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

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
    ExptExpr := QN_C_EXPT(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Expression yielding a scalar value of the provided identifier.
    ///
    /// The identifier is named by [`@name`](QN_NAME),
    ///   with vectors requiring an [`@index`](QN_INDEX).
    /// Matrices require use of a nested [`IndexExpr`] qualifier to resolve
    ///   a scalar.
    ValueOfExpr := QN_C_VALUE_OF(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_INDEX => TodoAttr,
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

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
    IndexExpr := QN_C_INDEX(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),
        CalcExpr,
    };

    /// Expression yielding a constant scalar value.
    ConstExpr := QN_C_CONST(_, ospan) {
        @ {
            QN_VALUE => TodoAttr,
            // TODO: Description was historically required to avoid magic
            //   values,
            //     but we now have short-hand constants which do not require
            //     descriptions.
            // We should probably require both or neither,
            //   but requiring `c:value-of` short-hand wouldn't be
            //   the responsibility of NIR,
            //     so perhaps then neither should be.
            QN_DESC => TodoAttr,
            // _TODO: deprecate?
            QN_TYPE => TodoAttr,
        } => Todo(ospan.into()),

        WhenExpr,
    };

    /// Ceiling (⌈_x_⌉) expression.
    CeilExpr := QN_C_CEIL(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => NirEntity::Ceil.open(ospan),
        /(cspan) => NirEntity::Ceil.close(cspan),

        CalcExpr,
    };

    /// Floor (⌊_x_⌋) expression.
    FloorExpr := QN_C_FLOOR(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => NirEntity::Floor.open(ospan),
        /(cspan) => NirEntity::Floor.close(cspan),

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
    CasesExpr := QN_C_CASES(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

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
    CaseExpr := QN_C_CASE(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

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
    OtherwiseExpr := QN_C_OTHERWISE(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

        CalcExpr,
    };

    /// Length of a vector (|**v**|).
    ///
    /// This also yields the number of rows of a matrix,
    ///   which are vectors of vectors.
    /// It is not defined for scalars.
    LengthOfExpr := QN_C_LENGTH_OF(_, ospan) {
        @ {} => Todo(ospan.into()),
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
    LetExpr := QN_C_LET(_, ospan) {
        @ {} => Todo(ospan.into()),
        LetValues,
        CalcExpr,
    };

    /// A set of mutually independent expressions and associated identifiers
    ///   to be lexically scoped to the sibling [`CalcExpr`].
    ///
    /// See [`LetExpr`] for more information.
    LetValues := QN_C_VALUES(_, ospan) {
        @ {} => Todo(ospan.into()),
        LetValue,
    };

    /// An expression bound to an associated identifier that is lexically
    ///   scoped to a parent [`LetValues`]' sibling [`CalcExpr`].
    ///
    /// A value cannot observe sibling values,
    ///   but it can observe values of an ancestor [`LetExpr`] that is not
    ///   its parent.
    LetValue := QN_C_VALUE(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_TYPE => TodoAttr,
            // Misnomer
            QN_SET => TodoAttr,
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),

        CalcExpr,
    };

    /// An expression yielding a vector consisting of each of its child
    ///   expressions' values as respective items.
    VectorExpr := QN_C_VECTOR(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),

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
    ApplyExpr := QN_C_APPLY(_, ospan) {
        @ {} => Todo(ospan.into()),

        [attr](attr) => TodoAttr(SPair(attr.value(), attr.span())),

        ApplyArg,
    };

    /// Argument for function application.
    ///
    /// Alternatively,
    ///   the parent element [`ApplyExpr`] may contain short-hand arguments
    ///   as attributes.
    ApplyArg := QN_C_ARG(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),

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
    RecurseExpr := QN_C_RECURSE(_, ospan) {
        @ {} => Todo(ospan.into()),

        [attr](attr) => TodoAttr(SPair(attr.value(), attr.span())),

        ApplyArg,
    };

    /// Construct a list (vector) by providing a new head ("car") and a
    ///   (possibly empty) tail ("cdr").
    ///
    /// This terminology originates from Lisp.
    /// It is equivalent to an `unshift` operation.
    ConsExpr := QN_C_CONS(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Retrieve the first element in a list (vector).
    ///
    /// This terminology originates from Lisp.
    CarExpr := QN_C_CAR(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),
        CalcExpr,
    };

    /// Retrieve all but the first element of a list (vector).
    ///
    /// This terminology originates from Lisp,
    ///   and is pronounced "could-er".
    /// It is also called "tail".
    CdrExpr := QN_C_CDR(_, ospan) {
        @ {
            QN_LABEL => TodoAttr,
        } => Todo(ospan.into()),
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
    WhenExpr := QN_C_WHEN(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_INDEX => TodoAttr,
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),

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
    EqCalcPredExpr := QN_C_EQ(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Non-equality predicate (≠).
    NeCalcPredExpr := QN_C_NE(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Less-than predicate (<).
    LtCalcPredExpr := QN_C_LT(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Greater-than predicate (>).
    GtCalcPredExpr := QN_C_GT(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Less-than or equality predicate (≤).
    LteCalcPredExpr := QN_C_LTE(_, ospan) {
        @ {} => Todo(ospan.into()),
        CalcExpr,
    };

    /// Greater-than or equality predicate (≥).
    GteCalcPredExpr := QN_C_GTE(_, ospan) {
        @ {} => Todo(ospan.into()),
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
    ProgramMapStmt := QN_PROGRAM_MAP(_, ospan) {
        @ {
            QN_XMLNS => TodoAttr,
            QN_XMLNS_LV => TodoAttr,
            QN_SRC => TodoAttr,
        } => Todo(ospan.into()),

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
    ReturnMapStmt := QN_RETURN_MAP(_, ospan) {
        @ {
            QN_XMLNS => TodoAttr,
            QN_XMLNS_LV => TodoAttr,
        } => Todo(ospan.into()),

        MapPkgImportStmt,
        MapImportStmt,
        MapBody,
    };

    /// Alias for [`ImportStmt`].
    ///
    /// This is only necessary because of [`MapImportStmt`];
    ///   both that and [`MapPkgImportStmt`] will be removed in the future
    ///   in favor of [`ImportStmt`].
    MapPkgImportStmt := QN_LV_IMPORT(_, ospan) {
        @ {
            QN_PACKAGE => TodoAttr,
            QN_EXPORT => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Import a map package.
    ///
    /// The distinction between this an [`ImportStmt`] is historical and is
    ///   no longer meaningful;
    ///     it will be removed in the future.
    MapImportStmt := QN_IMPORT(_, ospan) {
        @ {
            QN_PATH => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Define the value of a key in the destination.
    MapBody := (MapPassStmt | MapStmt);

    /// Map a value into a key of the destination without modification.
    ///
    /// See also [`MapStmt`] if the value needs to be modified in some way.
    MapPassStmt := QN_PASS(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DEFAULT => TodoAttr,
            QN_SCALAR => TodoAttr,
            QN_OVERRIDE => TodoAttr,
            QN_NOVALIDATE => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Map a value into a key of the destination.
    ///
    /// See also [`MapPassStmt`] if the value does not need modification.
    MapStmt := QN_MAP(_, ospan) {
        @ {
            QN_TO => TodoAttr,
            QN_FROM => TodoAttr,
            // We need to be permissive in what we accept since this may
            //   match in different contexts;
            //     downstream IR will validate the against the map
            //     destination.
            QN_VALUE => TodoAttr,
            QN_DEFAULT => TodoAttr,
            QN_SCALAR => TodoAttr,
            QN_OVERRIDE => TodoAttr,
            QN_NOVALIDATE => TodoAttr,
        } => Todo(ospan.into()),

        MapStmtBody,
    };

    /// Methods for mapping a value.
    MapStmtBody := (MapFromStmt | MapSetStmt | MapTransformStmt);

    /// Source of data for a map operation.
    MapFromStmt := QN_FROM(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DEFAULT => TodoAttr,
            QN_SCALAR => TodoAttr,
            QN_NOVALIDATE => TodoAttr,
        } => Todo(ospan.into()),

        MapTranslateStmt,
    };

    /// List of 1:1 value translations for a map.
    MapTranslateStmt := QN_TRANSLATE(_, ospan) {
        @ {
            QN_KEY => TodoAttr,
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Yield a vector of values where each item corresponds to the
    ///   respective child expression.
    ///
    /// TODO: This is a misnomer,
    ///   since the result is a vector,
    ///   not a set.
    MapSetStmt := QN_SET(_, ospan) {
        @ {} => Todo(ospan.into()),

        MapSetBody,
    };

    /// Permitted mappings in a [`MapSetStmt`].
    MapSetBody := (MapFromStmt | MapConstStmt);

    /// Map from a constant value.
    MapConstStmt := QN_CONST(_, ospan) {
        @ {
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Transform a value using some function.
    ///
    /// This is currently only meaningful for string inputs,
    ///   for example to convert input string case and hash values.
    ///
    /// Transformations may be composed via nesting.
    MapTransformStmt := QN_TRANSFORM(_, ospan) {
        @ {
            QN_METHOD => TodoAttr,
        } => Todo(ospan.into()),

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
    WorksheetStmt := QN_WORKSHEET(_, ospan) {
        @ {
            QN_XMLNS => TodoAttr,

            QN_NAME => TodoAttr,
            QN_PACKAGE => TodoAttr,
        } => Todo(ospan.into()),

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
    ExpandFunctionStmt := QN_EXPAND_FUNCTION(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Render a simplified, human-readable display of the calculation,
    ///   along with its result.
    DisplayStmt := QN_DISPLAY(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),
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
    TemplateStmt := QN_TEMPLATE(_, ospan) {
        @ {
            QN_NAME => BindIdent,
            QN_DESC => Desc,
        } => NirEntity::Tpl.open(ospan),
        /(cspan) => NirEntity::Tpl.close(cspan),

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
    TplParamStmt := QN_PARAM(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DESC => TodoAttr,
        } => Todo(ospan.into()),

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
    TplText := QN_TEXT(_, ospan) {
        @ {
            QN_UNIQUE => TodoAttr,
        } => Todo(ospan.into()),
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
    TplParamValue := QN_PARAM_VALUE(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_DASH => TodoAttr,
            QN_UPPER => TodoAttr,
            QN_LOWER => TodoAttr,
            QN_UCFIRST => TodoAttr,
            QN_RMDASH => TodoAttr,
            QN_RMUNDERSCORE => TodoAttr,
            QN_IDENTIFIER => TodoAttr,
            QN_SNAKE => TodoAttr,
        } => Todo(ospan.into()),
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
    TplParamInherit := QN_PARAM_INHERIT(_, ospan) {
        @ {
            QN_META => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Sum a numeric value with a numeric template parameter.
    ///
    /// Combined with [`TplParamInherit`],
    ///   this can be used to perform bounded recursive template expansion.
    TplParamAdd := QN_PARAM_ADD(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),
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
    TplParamClassToYields := QN_PARAM_CLASS_TO_YIELDS(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),
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
    TplParamTypedefLookup := QN_PARAM_TYPEDEF_LOOKUP(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Look up an attribute from the symbol table for a given identifier.
    TplParamSymValue := QN_PARAM_SYM_VALUE(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_VALUE => TodoAttr,
            QN_PREFIX => TodoAttr,
            QN_IGNORE_MISSING => TodoAttr,
        } => Todo(ospan.into()),
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
    DynNode := QN_DYN_NODE(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),

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
    ErrorKw := QN_ERROR(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    WarningKw := QN_WARNING(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    /// See also [`TplApplyShort`],
    ///   which gets desugared into this via [`super::tplshort`].
    ApplyTemplate := QN_APPLY_TEMPLATE(_, ospan) {
        @ {
            QN_NAME => RefSubject,
        } => Nir::Open(NirEntity::TplApply, ospan.into()),
        /(cspan) => Nir::Close(NirEntity::TplApply, cspan.into()),

        ApplyTemplateParam,
    };

    /// Long-form template argument.
    ///
    /// Template arguments are lexical.
    ///
    /// See also [`TplApplyShort`],
    ///   which gets desugared into this via [`super::tplshort`].
    ApplyTemplateParam := QN_WITH_PARAM(_, ospan) {
        @ {
            QN_NAME => BindIdent,
            QN_VALUE => Text,
        } => Nir::Open(NirEntity::TplParam, ospan.into()),
        /(cspan) => Nir::Close(NirEntity::TplParam, cspan.into()),
    };

    /// Shorthand template application.
    ///
    /// This expands into an equivalent [`ApplyTemplate`] form where each
    ///   attribute is a template argument,
    ///     and where the body of this application is the `@values@`
    ///     template argument.
    /// See [`ApplyTemplate`] for more information.
    ///
    /// The name of the template omits the surrounding `_`s;
    ///   `t:foo` will desugar into the template name `_foo_`.
    /// Params similarly omit `@` and are derived from the _local name
    ///   only_;
    ///     so `bar="baz"` will be desugared into a param `@bar@` with a
    ///     text value `baz`.
    TplApplyShort := NS_T(qname, ospan) {
        @ {} => Nir::Open(NirEntity::TplApplyShort(qname), ospan.into()),
        /(cspan) => Nir::Close(NirEntity::TplApplyShort(qname), cspan.into()),

        // Streaming attribute parsing;
        //   this takes precedence over any attribute parsing above
        //     (which is used only for emitting the opening object).
        [attr](Attr(name, value, AttrSpan(name_span, value_span))) => {
            Nir::Open(
                // TODO: This simply _ignores_ the namespace prefix.
                //   If it's not a useful construct,
                //     it ought to be rejected.
                NirEntity::TplParamShort(
                    SPair(*name.local_name(), name_span),
                    SPair(value, value_span),
                ),
                name_span,
            )
        },

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
    InlineTemplate := QN_INLINE_TEMPLATE(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    InlineTemplateForEach := QN_FOR_EACH(_, ospan) {
        @ {} => Todo(ospan.into()),

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
    InlineTemplateArgSet := QN_SET(_, ospan) {
        @ {} => Todo(ospan.into()),

        // Streaming attribute parsing.
        [attr](attr) => TodoAttr(SPair(attr.value(), attr.span())),

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
    InlineTemplateSymSet := QN_SYM_SET(_, ospan) {
        @ {
            QN_NAME_PREFIX => TodoAttr,
            QN_TYPE => TodoAttr,
            // TODO: Look at XSL sources for others
        } => Todo(ospan.into()),
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
    ExpandSequence := QN_EXPAND_SEQUENCE(_, ospan) {
        @ {} => Todo(ospan.into()),
        AnyStmtOrExpr,
    };

    /// Groups nodes to be expanded together during [`ExpandSequence`].
    ///
    /// This exists to work around performance pitfalls of the XSLT-based
    ///   implementation of [`ExpandSequence`];
    ///     see that NT for more information.
    ExpandGroup := QN_EXPAND_GROUP(_, ospan) {
        @ {} => Todo(ospan.into()),
        AnyStmtOrExpr,
    };

    /// Prohibit template expansion beyond this point.
    ///
    /// An expansion barrier is a seldom-needed feature that stops the
    ///   template system from expanding its body beyond a certain point,
    ///     which is sometimes needed for template-producing templates.
    ExpandBarrier := QN_EXPAND_BARRIER(_, ospan) {
        @ {} => Todo(ospan.into()),
        AnyStmtOrExpr,
    };

    /// Inline the value of a parameter as a tree.
    ///
    /// This is only useful for the special `@values@` parameter,
    ///   whose value is (conceptually) an XML tree.
    ///
    /// This allows creating templates that accept children.
    TplParamCopy := QN_PARAM_COPY(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Define a metavalue at this point in the expansion environment.
    ///
    /// For more information on how these values are used,
    ///   see [`TplParamInherit`].
    TplParamMeta := QN_PARAM_META(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_VALUE => TodoAttr,
        } => Todo(ospan.into()),
    };

    /// Conditionally expand the body if the provided predicate matches.
    TplIf := QN_IF(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_EQ => TodoAttr,
            QN_GT => TodoAttr,
            QN_GTE => TodoAttr,
            QN_LT => TodoAttr,
            QN_LTE => TodoAttr,
            QN_PREFIX => TodoAttr,
            QN_SUFFIX => TodoAttr,
        } => Todo(ospan.into()),

        AnyStmtOrExpr,
    };

    /// Conditionally expand the body if the provided predicate does not
    ///   match.
    ///
    /// This can be used as a sibling of [`TplIf`] to create the equivalent
    ///   of an `else` clause.
    TplUnless := QN_UNLESS(_, ospan) {
        @ {
            QN_NAME => TodoAttr,
            QN_EQ => TodoAttr,
            QN_GT => TodoAttr,
            QN_GTE => TodoAttr,
            QN_LT => TodoAttr,
            QN_LTE => TodoAttr,
            QN_PREFIX => TodoAttr,
            QN_SUFFIX => TodoAttr,
        } => Todo(ospan.into()),

        AnyStmtOrExpr,
    };
}
