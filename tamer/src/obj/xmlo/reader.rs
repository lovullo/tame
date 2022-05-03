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

use super::{SymAttrs, XmloError};
use crate::{
    obj::xmlo::{Dim, SymDtype, SymType},
    parse::{
        self, EmptyContext, NoContext, ParseState, Transition,
        TransitionResult, Transitionable,
    },
    span::Span,
    sym::{st::*, SymbolId},
    xir::{attr::Attr, flat::Object as Xirf, QName},
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
pub enum XmloEvent {
    /// Canonical package name.
    PkgName(SymbolId),
    /// Relative path from package to project root.
    PkgRootPath(SymbolId),
    /// Indicates that the package is a program.
    PkgProgramFlag,
    /// Name of package eligibility classification.
    PkgEligClassYields(SymbolId),

    /// Symbol declaration.
    ///
    /// This represents an entry in the symbol table,
    ///   which includes a symbol along with its variable metadata as
    ///   [`SymAttrs`].
    SymDecl(SymbolId, SymAttrs, Span),

    /// Begin adjacency list for a given symbol and interpret subsequent
    ///   symbols as edges (dependencies).
    SymDepStart(SymbolId, Span),

    /// A symbol reference whose interpretation is dependent on the current
    ///   state.
    Symbol(SymbolId, Span),

    /// Text (compiled code) fragment for a given symbol.
    ///
    /// This contains the compiler output for a given symbol,
    ///   and is returned here as an owned value.
    /// Given that fragments can be quite large,
    ///   a caller not interested in these data should choose to skip
    ///   fragments entirely rather than simply ignoring fragment events.
    Fragment(SymbolId, SymbolId, Span),

    /// End-of-header.
    ///
    /// The header of an `xmlo` file is defined as the symbol table;
    ///   dependency list; and fragments.
    /// This event is emitted at the closing `preproc:fragment` node.
    Eoh(Span),
}

impl parse::Object for XmloEvent {}

/// A [`Result`] with a hard-coded [`XmloError`] error type.
///
/// This is the result of every [`XmloReader`] operation that could
///   potentially fail in error.
pub type XmloResult<T> = Result<T, XmloError>;

qname_const! {
    QN_DESC: :L_DESC,
    QN_DIM: :L_DIM,
    QN_DTYPE: :L_DTYPE,
    QN_ELIG_CLASS_YIELDS: L_PREPROC:L_ELIG_CLASS_YIELDS,
    QN_EXTERN: :L_EXTERN,
    QN_FRAGMENT: L_PREPROC:L_FRAGMENT,
    QN_FRAGMENTS: L_PREPROC:L_FRAGMENTS,
    QN_FROM: L_PREPROC:L_FROM,
    QN_GENERATED: L_PREPROC:L_GENERATED,
    QN_ID: :L_ID,
    QN_ISOVERRIDE: :L_ISOVERRIDE,
    QN_LV_PACKAGE: L_LV:L_PACKAGE,
    QN_NAME: :L_NAME,
    QN_PACKAGE: :L_PACKAGE,
    QN_PARENT: :L_PARENT,
    QN_PROGRAM: :L_PROGRAM,
    QN_SRC: :L_SRC,
    QN_SYM: L_PREPROC:L_SYM,
    QN_SYMTABLE: L_PREPROC:L_SYMTABLE,
    QN_SYM_DEP: L_PREPROC:L_SYM_DEP,
    QN_SYM_DEPS: L_PREPROC:L_SYM_DEPS,
    QN_SYM_REF: L_PREPROC:L_SYM_REF,
    QN_TYPE: :L_TYPE,
    QN_UUROOTPATH: :L_UUROOTPATH,
    QN_VIRTUAL: :L_VIRTUAL,
    QN_YIELDS: :L_YIELDS,
}

/// A parser capable of being composed with [`XmloReader`].
pub trait XmloState = ParseState<Token = Xirf, Context = EmptyContext>
where
    <Self as ParseState>::Error: Into<XmloError>,
    <Self as ParseState>::Object: Into<XmloEvent>;

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
    Package,
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
    type Token = Xirf;
    type Object = XmloEvent;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: NoContext,
    ) -> TransitionResult<Self> {
        use XmloReader::*;

        match (self, tok) {
            (Ready, Xirf::Open(QN_LV_PACKAGE | QN_PACKAGE, ..)) => {
                Transition(Package).incomplete()
            }

            (Ready, tok) => {
                Transition(Ready).err(XmloError::UnexpectedRoot(tok))
            }

            (Package, Xirf::Attr(Attr(name, value, _))) => {
                Transition(Package).ok(match name {
                    QN_NAME => XmloEvent::PkgName(value),
                    QN_UUROOTPATH => XmloEvent::PkgRootPath(value),
                    QN_PROGRAM => XmloEvent::PkgProgramFlag,
                    QN_ELIG_CLASS_YIELDS => {
                        XmloEvent::PkgEligClassYields(value)
                    }
                    // Ignore unknown attributes for now to maintain BC,
                    //   since no strict xmlo schema has been defined.
                    _ => return Transition(Package).incomplete(),
                })
            }

            // Empty package (should we allow this?);
            //   XIRF guarantees a matching closing tag.
            (Package, Xirf::Close(..)) => Transition(Done).incomplete(),

            (Package, Xirf::Open(QN_SYMTABLE, span, ..)) => {
                Transition(Symtable(span, SS::default())).incomplete()
            }

            (Symtable(_, ss), Xirf::Close(Some(QN_SYMTABLE), ..))
                if ss.is_accepting() =>
            {
                Transition(SymDepsExpected).incomplete()
            }

            // TOOD: It'd be nice to augment errors with the symbol table
            //   span as well (e.g. "while processing symbol table at <loc>").
            (Symtable(span, ss), tok) => {
                ss.delegate(ctx, tok, |ss| Symtable(span, ss))
            }

            (SymDepsExpected, Xirf::Open(QN_SYM_DEPS, span, _)) => {
                Transition(SymDeps(span, SD::default())).incomplete()
            }

            (SymDeps(_, sd), Xirf::Close(None | Some(QN_SYM_DEPS), ..))
                if sd.is_accepting() =>
            {
                Transition(FragmentsExpected).incomplete()
            }

            (SymDeps(span, sd), tok) => {
                sd.delegate(ctx, tok, |sd| SymDeps(span, sd))
            }

            (FragmentsExpected, Xirf::Open(QN_FRAGMENTS, span, _)) => {
                Transition(Fragments(span, SF::default())).incomplete()
            }

            (
                Fragments(_, sf),
                Xirf::Close(None | Some(QN_FRAGMENTS), span, _),
            ) if sf.is_accepting() => Transition(Eoh).ok(XmloEvent::Eoh(span)),

            (Fragments(span, sf), tok) => {
                sf.delegate(ctx, tok, |sf| Fragments(span, sf))
            }

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

    fn is_accepting(&self) -> bool {
        *self == Self::Eoh || *self == Self::Done
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

impl parse::Object for (SymbolId, SymAttrs, Span) {}

impl ParseState for SymtableState {
    type Token = Xirf;
    type Object = (SymbolId, SymAttrs, Span);
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use SymtableState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_SYM, span, _)) => {
                Transition(Sym(span, None, SymAttrs::default())).incomplete()
            }

            (Sym(span, None, attrs), Xirf::Close(..)) => {
                Transition(Sym(span, None, attrs))
                    .err(XmloError::UnassociatedSym(span))
            }

            // Completed symbol.
            (Sym(span, Some(name), attrs), Xirf::Close(..)) => {
                Transition(Ready).ok((name, attrs, span))
            }

            // Symbol @name found.
            (Sym(span, None, attrs), Xirf::Attr(Attr(QN_NAME, name, _))) => {
                Transition(Sym(span, Some(name), attrs)).incomplete()
            }

            (
                Sym(span_sym, name, mut attrs),
                Xirf::Attr(Attr(key, value, (_, span_attrval))),
            ) => Self::parse_sym_attr(&mut attrs, key, value, span_attrval)
                .transition(Sym(span_sym, name, attrs)),

            // `preproc:from` supported only for `type="map"`.
            // TODO: The compiler really ought to just make this an
            //   attribute now so we can simplify parsing here.
            (
                Sym(span_sym, Some(name), attrs),
                Xirf::Open(QN_FROM, span_from, _),
            ) if attrs.ty == Some(SymType::Map)
                || attrs.ty == Some(SymType::RetMap) =>
            {
                Transition(SymMapFrom(span_sym, name, attrs, span_from))
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
                Xirf::Open(QN_SYM_REF, span_ref, _),
            ) => {
                Transition(SymRef(span_sym, name, attrs, span_ref)).incomplete()
            }

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

    fn is_accepting(&self) -> bool {
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
            QN_GENERATED => {
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
    fn parse_dtype(value: SymbolId) -> Option<SymDtype> {
        use raw::*;

        match value {
            L_BOOLEAN => Some(SymDtype::Boolean),
            L_INTEGER => Some(SymDtype::Integer),
            L_FLOAT => Some(SymDtype::Float),
            L_EMPTY => Some(SymDtype::Empty),
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

impl From<(SymbolId, SymAttrs, Span)> for XmloEvent {
    fn from(tup: (SymbolId, SymAttrs, Span)) -> Self {
        match tup {
            (sym, attrs, span) => Self::SymDecl(sym, attrs, span),
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
    type Token = Xirf;
    type Object = XmloEvent;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use SymDepsState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_SYM_DEP, span, _)) => {
                Transition(SymUnnamed(span)).incomplete()
            }

            (SymUnnamed(span), Xirf::Attr(Attr(QN_NAME, name, _))) => {
                Transition(Sym(span, name))
                    .ok(XmloEvent::SymDepStart(name, span))
            }

            (SymUnnamed(span), _) => Transition(SymUnnamed(span))
                .err(XmloError::UnassociatedSymDep(span)),

            (Sym(span, name), Xirf::Open(QN_SYM_REF, span_ref, _)) => {
                Transition(SymRefUnnamed(span, name, span_ref)).incomplete()
            }

            (
                SymRefUnnamed(span, name, span_ref),
                Xirf::Attr(Attr(QN_NAME, ref_name, (_, span_ref_name))),
            ) => Transition(SymRefDone(span, name, span_ref))
                .ok(XmloEvent::Symbol(ref_name, span_ref_name)),

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

    fn is_accepting(&self) -> bool {
        *self == Self::Ready
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
    Fragment(Span, SymbolId),
    FragmentDone(Span, SymbolId),
}

impl ParseState for FragmentsState {
    type Token = Xirf;
    type Object = XmloEvent;
    type Error = XmloError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use FragmentsState::*;

        match (self, tok) {
            (Ready, Xirf::Attr(..)) => Transition(Ready).incomplete(),

            (Ready, Xirf::Open(QN_FRAGMENT, span, _)) => {
                Transition(FragmentUnnamed(span)).incomplete()
            }

            // TODO: For whitespace, which can be stripped by XIRF.
            (Ready, Xirf::Text(..)) => Transition(Ready).incomplete(),

            (FragmentUnnamed(span), Xirf::Attr(Attr(QN_ID, id, _))) => {
                match id {
                    // See "compiler bug" comment below.
                    raw::WS_EMPTY => {
                        Transition(FragmentUnnamed(span)).incomplete()
                    }
                    id => Transition(Fragment(span, id)).incomplete(),
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

            (Fragment(span, id), Xirf::Text(text, _)) => {
                Transition(FragmentDone(span, id))
                    .ok(XmloEvent::Fragment(id, text, span))
            }

            // TODO: Also a compiler bug, for some generated classes.
            // This needs fixing in the compiler.
            (Fragment(_span, _id), Xirf::Close(..)) => {
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

    fn is_accepting(&self) -> bool {
        *self == Self::Ready
    }
}

#[cfg(test)]
mod test;
