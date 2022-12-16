// XIR-based xmlo object file reader
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

use std::fmt::Display;

use super::{SymAttrs, XmloError};
use crate::{
    fmt::{DisplayWrapper, TtQuote},
    num::{Dim, Dtype},
    obj::xmlo::SymType,
    parse::{
        self, util::SPair, ClosedParseState, EmptyContext, NoContext,
        ParseState, Token, Transition, TransitionResult, Transitionable,
    },
    span::Span,
    sym::{st::raw, SymbolId},
    xir::{
        attr::{Attr, AttrSpan},
        flat::{Text, XirfToken as Xirf},
        st::qname::*,
        EleSpan, QName,
    },
};

/// `xmlo` reader events.
///
/// All data are parsed rather than being returned as [`u8`] slices,
///   which avoids having to deal with awkward borrows or data copying since
///   these data will likely be persisted in memory anyway.
///
/// To avoid extra data copying,
///   we should instead prefer not to put data into object files that won't
///   be useful and can't be easily skipped without parsing.
#[derive(Debug, PartialEq, Eq)]
pub enum XmloToken {
    /// Canonical package name.
    PkgName(SPair),
    /// Relative path from package to project root.
    PkgRootPath(SPair),
    /// Indicates that the package is a program.
    PkgProgramFlag(Span),
    /// Name of package eligibility classification.
    PkgEligClassYields(SPair),

    /// Symbol declaration.
    ///
    /// This represents an entry in the symbol table,
    ///   which includes a symbol along with its variable metadata as
    ///   [`SymAttrs`].
    SymDecl(SPair, SymAttrs),

    /// Begin adjacency list for a given symbol and interpret subsequent
    ///   symbols as edges (dependencies).
    SymDepStart(SPair),

    /// A symbol reference whose interpretation is dependent on the current
    ///   state.
    ///
    /// The span currently references the object file itself,
    ///   but in the future this will resolve to a span stored within the
    ///   object file representing the source location of this symbol.
    Symbol(SPair),

    /// Text (compiled code) fragment for a given symbol.
    ///
    /// This contains the compiler output for a given symbol,
    ///   and is returned here as an owned value.
    /// Given that fragments can be quite large,
    ///   a caller not interested in these data should choose to skip
    ///   fragments entirely rather than simply ignoring fragment events.
    Fragment(SPair, SymbolId),

    /// End-of-header.
    ///
    /// The header of an `xmlo` file is defined as the symbol table;
    ///   dependency list; and fragments.
    /// This event is emitted at the closing `preproc:fragment` node.
    Eoh(Span),
}

impl parse::Object for XmloToken {}

impl Token for XmloToken {
    fn ir_name() -> &'static str {
        "xmlo"
    }

    fn span(&self) -> Span {
        use XmloToken::*;

        match self {
            // Note that even the spans for the package metadata are
            //   important since these initial tokens seed
            //   `Parser::last_span`,
            //     which is used for early error messages.
            PkgName(SPair(_, span))
            | PkgRootPath(SPair(_, span))
            | PkgProgramFlag(span)
            | PkgEligClassYields(SPair(_, span))
            | SymDecl(SPair(_, span), _)
            | SymDepStart(SPair(_, span))
            | Symbol(SPair(_, span))
            | Fragment(SPair(_, span), _)
            | Eoh(span) => *span,
        }
    }
}

impl Display for XmloToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XmloToken::*;

        match self {
            PkgName(sym) => write!(f, "package of name {}", TtQuote::wrap(sym)),
            PkgRootPath(sym) => {
                write!(f, "package root path {}", TtQuote::wrap(sym))
            }
            PkgProgramFlag(_) => write!(f, "package program flag"),
            PkgEligClassYields(sym) => {
                write!(
                    f,
                    "package eligibility classification {}",
                    TtQuote::wrap(sym)
                )
            }
            SymDecl(sym, ..) => {
                write!(f, "symbol {} declaration", TtQuote::wrap(sym))
            }
            SymDepStart(sym) => {
                write!(
                    f,
                    "beginning of symbol {} dependency list",
                    TtQuote::wrap(sym)
                )
            }
            Symbol(sym) => write!(f, "symbol {}", TtQuote::wrap(sym)),
            Fragment(sym, _) => {
                write!(f, "symbol {} code fragment", TtQuote::wrap(sym))
            }
            Eoh(..) => write!(f, "end of header"),
        }
    }
}

/// A parser capable of being composed with [`XmloReader`].
pub trait XmloState =
    ClosedParseState<Token = Xirf<Text>, Context = EmptyContext>
    where
        Self: Default,
        <Self as ParseState>::Error: Into<XmloError>,
        <Self as ParseState>::Object: Into<XmloToken>;

#[derive(Debug, Default, PartialEq, Eq)]
pub enum XmloReader<
    SS: XmloState = SymtableState,
    SD: XmloState = SymDepsState,
    SF: XmloState = FragmentsState,
> {
    /// Parser has not yet processed any input.
    #[default]
    Ready,
    /// Processing `package` attributes.
    Package(Span),
    /// Expecting a symbol declaration or closing `preproc:symtable`.
    Symtable(Span, SS),
    /// Symbol dependencies are expected next.
    SymDepsExpected,
    /// Expecting symbol dependency list or closing `preproc:sym-deps`.
    SymDeps(Span, SD),
    /// Compiled text fragments are expected next.
    FragmentsExpected,
    /// Expecting text fragment or closing `preproc:fragments`.
    Fragments(Span, SF),
    /// End of header parsing.
    Eoh,
    /// `xmlo` file has been fully read.
    Done,
}

impl<SS: XmloState, SD: XmloState, SF: XmloState> ParseState
    for XmloReader<SS, SD, SF>
{
    type Token = Xirf<Text>;
    type Object = XmloToken;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: NoContext,
    ) -> TransitionResult<Self> {
        use XmloReader::*;

        match (self, tok) {
            (Ready, Xirf::Open(QN_LV_PACKAGE | QN_PACKAGE, span, ..)) => {
                Transition(Package(span.tag_span())).incomplete()
            }

            (Ready, tok) => {
                Transition(Ready).err(XmloError::UnexpectedRoot(tok))
            }

            (Package(span), Xirf::Attr(Attr(name, value, aspan))) => {
                // TODO: These spans do not encompass the entire token for errors,
                //   which can result in confusing output depending on the context;
                //     we ought to retain _both_ token- and value-spans.
                Transition(Package(span)).ok(match name {
                    QN_NAME => {
                        XmloToken::PkgName(SPair(value, aspan.value_span()))
                    }
                    QN_UUROOTPATH => {
                        XmloToken::PkgRootPath(SPair(value, aspan.value_span()))
                    }
                    QN_PROGRAM => XmloToken::PkgProgramFlag(aspan.key_span()),
                    QN_P_ELIG_CLASS_YIELDS => XmloToken::PkgEligClassYields(
                        SPair(value, aspan.value_span()),
                    ),
                    // Ignore unknown attributes for now to maintain BC,
                    //   since no strict xmlo schema has been defined.
                    _ => return Transition(Package(span)).incomplete(),
                })
            }

            // Empty package (should we allow this?);
            //   XIRF guarantees a matching closing tag.
            (Package(_), Xirf::Close(..)) => Transition(Done).incomplete(),

            (Package(_), Xirf::Open(QN_P_SYMTABLE, span, ..)) => {
                Transition(Symtable(span.tag_span(), SS::default()))
                    .incomplete()
            }

            (Symtable(_, ss), Xirf::Close(Some(QN_P_SYMTABLE), ..))
                if ss.is_accepting(ctx) =>
            {
                Transition(SymDepsExpected).incomplete()
            }

            // TOOD: It'd be nice to augment errors with the symbol table
            //   span as well (e.g. "while processing symbol table at <loc>").
            (Symtable(span, ss), tok) => ss.delegate(
                tok,
                ctx,
                |ss| Transition(Symtable(span, ss)),
                || unreachable!(), // TODO: currently caught by preceding match
            ),

            (SymDepsExpected, Xirf::Open(QN_P_SYM_DEPS, span, _)) => {
                Transition(SymDeps(span.tag_span(), SD::default())).incomplete()
            }

            (SymDeps(_, sd), Xirf::Close(None | Some(QN_P_SYM_DEPS), ..))
                if sd.is_accepting(ctx) =>
            {
                Transition(FragmentsExpected).incomplete()
            }

            (SymDeps(span, sd), tok) => sd.delegate(
                tok,
                ctx,
                |sd| Transition(SymDeps(span, sd)),
                || unreachable!(), // TODO: currently caught by preceding match
            ),

            (FragmentsExpected, Xirf::Open(QN_P_FRAGMENTS, span, _)) => {
                Transition(Fragments(span.tag_span(), SF::default()))
                    .incomplete()
            }

            (
                Fragments(_, sf),
                Xirf::Close(None | Some(QN_P_FRAGMENTS), span, _),
            ) if sf.is_accepting(ctx) => {
                Transition(Eoh).ok(XmloToken::Eoh(span.tag_span()))
            }

            (Fragments(span, sf), tok) => sf.delegate(
                tok,
                ctx,
                |sf| Transition(Fragments(span, sf)),
                || unreachable!(), // TODO: currently caught by preceding match
            ),

            (Eoh, Xirf::Close(Some(QN_PACKAGE), ..)) => {
                Transition(Done).incomplete()
            }

            // TODO: For whitespace, which can be stripped by XIRF.
            (st, Xirf::Text(..)) => Transition(st).incomplete(),

            (st, unknown) => {
                Transition(st).err(XmloError::UnexpectedToken(unknown))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        *self == Self::Eoh || *self == Self::Done
    }
}

impl<SS: XmloState, SD: XmloState, SF: XmloState> Display
    for XmloReader<SS, SD, SF>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XmloReader::*;

        match self {
            Ready => write!(f, "awaiting xmlo input"),
            Package(_) => write!(f, "processing package attributes"),
            Symtable(_, ss) => Display::fmt(ss, f),
            SymDepsExpected => write!(f, "expecting symbol dependency list"),
            SymDeps(_, sd) => Display::fmt(sd, f),
            FragmentsExpected => write!(f, "expecting start of fragments"),
            Fragments(_, sf) => Display::fmt(sf, f),
            Eoh => write!(f, "finished parsing header"),
            Done => write!(f, "finished parsing xmlo file"),
        }
    }
}

/// Symbol table parser operating within a delimited context.
///
/// This parser expects a parent [`ParseState`] to indicate when symtable
///   parsing ought to start and end—
///     this parser does not recognize any opening or closing tags.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum SymtableState {
    /// Symbol table declaration found;
    ///   symbols declarations expected.
    #[default]
    Ready,
    /// Processing a symbol.
    Sym(Span, Option<SymbolId>, SymAttrs),
    /// Awaiting a symbol map name.
    SymMapFrom(Span, SymbolId, SymAttrs, Span),
    /// Used by functions to declare their parameters.
    SymRef(Span, SymbolId, SymAttrs, Span),
}

impl ParseState for SymtableState {
    type Token = Xirf<Text>;
    type Object = XmloToken;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use SymtableState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_P_SYM, span, _)) => {
                Transition(Sym(span.tag_span(), None, SymAttrs::default()))
                    .incomplete()
            }

            (Sym(span, None, attrs), Xirf::Close(..)) => {
                Transition(Sym(span, None, attrs))
                    .err(XmloError::UnassociatedSym(span))
            }

            // Completed symbol.
            (Sym(span, Some(name), attrs), Xirf::Close(..)) => {
                Transition(Ready)
                    .ok(XmloToken::SymDecl(SPair(name, span), attrs))
            }

            // Symbol @name found.
            (Sym(_, None, attrs), Xirf::Attr(Attr(QN_NAME, name, aspan))) => {
                Transition(Sym(aspan.value_span(), Some(name), attrs))
                    .incomplete()
            }

            (
                Sym(span_sym, name, mut attrs),
                Xirf::Attr(Attr(key, value, AttrSpan(_, span_attrval))),
            ) => Self::parse_sym_attr(&mut attrs, key, value, span_attrval)
                .transition(Sym(span_sym, name, attrs)),

            // `preproc:from` supported only for `type="map"`.
            // TODO: The compiler really ought to just make this an
            //   attribute now so we can simplify parsing here.
            (
                Sym(span_sym, Some(name), attrs),
                Xirf::Open(QN_P_FROM, span_from, _),
            ) if attrs.ty == Some(SymType::Map)
                || attrs.ty == Some(SymType::RetMap) =>
            {
                Transition(SymMapFrom(
                    span_sym,
                    name,
                    attrs,
                    span_from.tag_span(),
                ))
                .incomplete()
            }

            (
                SymMapFrom(span_sym, name, mut attrs, span_from),
                Xirf::Attr(Attr(QN_NAME, from_name, _)),
            ) => match attrs.from.replace(from_name) {
                Some(_) => Err(XmloError::MapFromMultiple(name, span_from)),
                None => Ok(()),
            }
            .transition(SymMapFrom(span_sym, name, attrs, span_from)),

            (SymMapFrom(span_sym, name, attrs, span_from), Xirf::Close(..)) => {
                if attrs.from.is_none() {
                    return Transition(SymMapFrom(
                        span_sym, name, attrs, span_from,
                    ))
                    .err(XmloError::MapFromNameMissing(name, span_from));
                }

                Transition(Sym(span_sym, Some(name), attrs)).incomplete()
            }

            // TODO: These don't yield any events;
            //   can they be removed from the compiler?
            // The old XmloReader ignored these.
            (
                Sym(span_sym, Some(name), attrs),
                Xirf::Open(QN_P_SYM_REF, span_ref, _),
            ) => Transition(SymRef(span_sym, name, attrs, span_ref.tag_span()))
                .incomplete(),

            (SymRef(span_sym, name, attrs, _), Xirf::Close(..)) => {
                Transition(Sym(span_sym, Some(name), attrs)).incomplete()
            }

            (st @ SymRef(..), _) => Transition(st).incomplete(),

            // TODO: For whitespace, which can be stripped by XIRF.
            (st, Xirf::Text(..)) => Transition(st).incomplete(),

            (st, unknown) => {
                Transition(st).err(XmloError::UnexpectedToken(unknown))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        *self == Self::Ready
    }
}

impl SymtableState {
    /// Parse attributes of a `preproc:symtable/preproc:sym` element,
    ///   representing attributes of a symbol in the symbol table.
    ///
    /// Note that `@name` is expected to have already been processed by the
    ///   caller and is not expected to occur a second time.
    fn parse_sym_attr(
        attrs: &mut SymAttrs,
        key: QName,
        value: SymbolId,
        span: Span,
    ) -> Result<(), XmloError> {
        use raw::L_TRUE;

        match key {
            QN_DIM => {
                attrs.dim.replace(
                    Self::parse_dim(value)
                        .ok_or(XmloError::InvalidDim(value, span))?,
                );
            }
            QN_DTYPE => {
                attrs.dtype.replace(
                    Self::parse_dtype(value)
                        .ok_or(XmloError::InvalidDtype(value, span))?,
                );
            }
            QN_TYPE => {
                attrs.ty.replace(
                    Self::parse_symtype(value)
                        .ok_or(XmloError::InvalidType(value, span))?,
                );
            }

            QN_SRC => {
                attrs.src.replace(value);
            }
            QN_EXTERN => {
                attrs.extern_ = value == L_TRUE;
            }
            QN_YIELDS => {
                attrs.yields.replace(value);
            }
            QN_PARENT => {
                attrs.parent.replace(value);
            }
            QN_DESC => {
                attrs.desc.replace(value);
            }
            QN_VIRTUAL => {
                attrs.virtual_ = value == L_TRUE;
            }
            QN_ISOVERRIDE => {
                attrs.override_ = value == L_TRUE;
            }
            QN_P_GENERATED => {
                attrs.generated = value == L_TRUE;
            }

            // If we actually hit this,
            //   we may want to add a proper error to provide more context.
            // It is not expected to be hit,
            //   since it would mean that there is a duplicate attribute and
            //   this xmlo file is hopefully produced by the compiler.
            QN_NAME => panic!(
                "preproc:sym/@name already processed \
                                 (duplicate attribute); \
                                 the xmlo file is corrupt"
            ),

            // To maintain BC,
            //   ignore unknown attrs for now until we are confident that we
            //   have a comprehensive schema.
            // TODO: Error here.
            _ => (),
        }

        Ok(())
    }

    /// Parse a numeric `preproc:sym/@dim` attribute.
    fn parse_dim(value: SymbolId) -> Option<Dim> {
        use raw::*;

        match value {
            N0 => Some(Dim::Scalar),
            N1 => Some(Dim::Vector),
            N2 => Some(Dim::Matrix),
            _ => None,
        }
    }

    /// Parse a `preproc:sym/@dtype` attribute.
    fn parse_dtype(value: SymbolId) -> Option<Dtype> {
        use raw::*;

        match value {
            L_BOOLEAN => Some(Dtype::Boolean),
            L_INTEGER => Some(Dtype::Integer),
            L_FLOAT => Some(Dtype::Float),
            L_EMPTY => Some(Dtype::Empty),
            _ => None,
        }
    }

    /// Parse a `preproc:sym/@type` attribute.
    fn parse_symtype(value: SymbolId) -> Option<SymType> {
        use raw::*;

        match value {
            L_CGEN => Some(SymType::Cgen),
            L_CLASS => Some(SymType::Class),
            L_CONST => Some(SymType::Const),
            L_FUNC => Some(SymType::Func),
            L_GEN => Some(SymType::Gen),
            L_LPARAM => Some(SymType::Lparam),
            L_PARAM => Some(SymType::Param),
            L_RATE => Some(SymType::Rate),
            L_TPL => Some(SymType::Tpl),
            L_TYPE => Some(SymType::Type),
            L_RETMAP_HEAD => Some(SymType::RetMapHead),
            L_RETMAP => Some(SymType::RetMap),
            L_RETMAP_TAIL => Some(SymType::RetMapTail),
            L_MAP_HEAD => Some(SymType::MapHead),
            L_MAP => Some(SymType::Map),
            L_MAP_TAIL => Some(SymType::MapTail),
            L_META => Some(SymType::Meta),
            L_WORKSHEET => Some(SymType::Worksheet),
            _ => None,
        }
    }
}

impl Display for SymtableState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SymtableState::*;

        match self {
            Ready => write!(f, "expecting symbol declaration"),
            Sym(_, Some(sym), _) => write!(f, "parsing symbol `{sym}`"),
            Sym(_, None, _) => write!(f, "parsing a symbol"),
            SymMapFrom(_, sym, _, _) => {
                write!(f, "expecting map name for symbol `{sym}`")
            }
            SymRef(_, sym, _, _) => {
                write!(f, "parsing refs for symbol `{sym}`")
            }
        }
    }
}

/// Symbol dependency list (graph adjacency list) parser for
///   `preproc:sym-deps` children.
///
/// This parser expects a parent [`ParseState`] to indicate when dependency
///   parsing ought to start and end—
///     this parser does not recognize any opening or closing
///     `preproc:sym-deps` tags.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum SymDepsState {
    /// Symbol table declaration found;
    ///   symbols declarations expected.
    #[default]
    Ready,
    SymUnnamed(Span),
    Sym(Span, SymbolId),
    SymRefUnnamed(Span, SymbolId, Span),
    SymRefDone(Span, SymbolId, Span),
}

impl ParseState for SymDepsState {
    type Token = Xirf<Text>;
    type Object = XmloToken;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use SymDepsState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_P_SYM_DEP, span, _)) => {
                Transition(SymUnnamed(span.tag_span())).incomplete()
            }

            (SymUnnamed(span), Xirf::Attr(Attr(QN_NAME, name, _))) => {
                Transition(Sym(span, name))
                    .ok(XmloToken::SymDepStart(SPair(name, span)))
            }

            (SymUnnamed(span), _) => Transition(SymUnnamed(span))
                .err(XmloError::UnassociatedSymDep(span)),

            (Sym(span, name), Xirf::Open(QN_P_SYM_REF, span_ref, _)) => {
                Transition(SymRefUnnamed(span, name, span_ref.tag_span()))
                    .incomplete()
            }

            (
                SymRefUnnamed(span, name, span_ref),
                Xirf::Attr(Attr(QN_NAME, ref_name, AttrSpan(_, span_ref_name))),
            ) => Transition(SymRefDone(span, name, span_ref))
                .ok(XmloToken::Symbol(SPair(ref_name, span_ref_name))),

            // TODO: For xmlns attributes, which will go away in XIRF.
            (SymRefUnnamed(span, name, span_ref), Xirf::Attr(..)) => {
                Transition(SymRefUnnamed(span, name, span_ref)).incomplete()
            }

            (SymRefUnnamed(span, name, span_ref), _) => {
                Transition(SymRefUnnamed(span, name, span_ref))
                    .err(XmloError::MalformedSymRef(name, span_ref))
            }

            // TODO: For xmlns attributes, which will go away in XIRF.
            (SymRefDone(span, name, ref_span), Xirf::Attr(..)) => {
                Transition(SymRefDone(span, name, ref_span)).incomplete()
            }

            (SymRefDone(span, name, _), Xirf::Close(..)) => {
                Transition(Sym(span, name)).incomplete()
            }

            (Sym(..), Xirf::Close(..)) => Transition(Ready).incomplete(),

            // TODO: For whitespace, which can be stripped by XIRF.
            (st, Xirf::Text(..)) => Transition(st).incomplete(),

            (st, unknown) => {
                Transition(st).err(XmloError::UnexpectedToken(unknown))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        *self == Self::Ready
    }
}

impl Display for SymDepsState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SymDepsState::*;

        match self {
            Ready => write!(f, "expecting symbol table entry"),
            SymUnnamed(_) => write!(f, "expecting name for symbol table entry"),
            Sym(_, sym) => write!(
                f,
                "expecting dependencies for symbol table entry `{sym}`"
            ),
            SymRefUnnamed(_, sym, _) => write!(
                f,
                "expecting dependency name for symbol table entry `{sym}`"
            ),
            SymRefDone(_, sym, _) => write!(
                f,
                "expending end of dependency for symbol table entry `{sym}`"
            ),
        }
    }
}

/// Text fragment (compiled code) parser for `preproc:fragments` children.
///
/// This parser expects a parent [`ParseState`] to indicate when dependency
///   parsing ought to start and end—
///     this parser does not recognize any opening or closing
///     `preproc:fragments` tags.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum FragmentsState {
    #[default]
    Ready,
    FragmentUnnamed(Span),
    Fragment(SPair),
    FragmentDone(SPair),
}

impl ParseState for FragmentsState {
    type Token = Xirf<Text>;
    type Object = XmloToken;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use FragmentsState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_P_FRAGMENT, span, _)) => {
                Transition(FragmentUnnamed(span.tag_span())).incomplete()
            }

            // TODO: For whitespace, which can be stripped by XIRF.
            (Ready, Xirf::Text(..)) => Transition(Ready).incomplete(),

            (FragmentUnnamed(span), Xirf::Attr(Attr(QN_ID, id, _))) => {
                match id {
                    // See "compiler bug" comment below.
                    raw::WS_EMPTY => {
                        Transition(FragmentUnnamed(span)).incomplete()
                    }
                    id => Transition(Fragment(SPair(id, span))).incomplete(),
                }
            }

            (FragmentUnnamed(span), Xirf::Attr(Attr(key, ..)))
                if key != QN_ID =>
            {
                Transition(FragmentUnnamed(span)).incomplete()
            }

            // Compiler bug: `<preproc:fragment id="" />` in `rater/core/base`.
            (FragmentUnnamed(_span), Xirf::Close(None, ..)) => {
                Transition(Ready).incomplete()
            }

            (FragmentUnnamed(span), _) => Transition(FragmentUnnamed(span))
                .err(XmloError::UnassociatedFragment(span)),

            (Fragment(name), Xirf::Text(Text(text, _), _)) => {
                Transition(FragmentDone(name))
                    .ok(XmloToken::Fragment(name, text))
            }

            // TODO: Also a compiler bug, for some generated classes.
            // This needs fixing in the compiler.
            (Fragment(_), Xirf::Close(..)) => {
                //eprintln!("warning: empty fragment text for {id} at {span}");
                Transition(Ready).incomplete()
            }

            (FragmentDone(..), Xirf::Close(..)) => {
                Transition(Ready).incomplete()
            }

            (st, unknown) => {
                Transition(st).err(XmloError::UnexpectedToken(unknown))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        *self == Self::Ready
    }
}

impl Display for FragmentsState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use FragmentsState::*;

        match self {
            Ready => write!(f, "expecting fragment"),
            FragmentUnnamed(_) => {
                write!(f, "expecting fragment association id")
            }
            Fragment(name) => {
                write!(
                    f,
                    "expecting fragment text for symbol {}",
                    TtQuote::wrap(name)
                )
            }
            FragmentDone(name) => {
                write!(
                    f,
                    "expecting end of fragment for symbol {}",
                    TtQuote::wrap(name)
                )
            }
        }
    }
}

#[cfg(test)]
mod test;
