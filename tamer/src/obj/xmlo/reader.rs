// XIR-based xmlo object file reader
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

use super::{SymAttrs, XmloError};
use crate::{
    obj::xmlo::Dim,
    parse::{self, ParseState, ParseStatus, Transition, TransitionResult},
    span::Span,
    sym::{st::*, SymbolId},
    xir::{attr::Attr, flat::Object as Xirf},
};

// While the _use_ is gated, this isn't, to ensure that we still try to
// compile it while the flag is off (and so it's parsed by the language
// server).
mod quickxml;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
pub use quickxml::XmloReader;

#[cfg(feature = "wip-xmlo-xir-reader")]
pub use XmloReaderState as XmloReader;

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
    SymDepStart(SymbolId),

    /// A symbol reference whose interpretation is dependent on the current
    ///   state.
    Symbol(SymbolId),

    /// Text (compiled code) fragment for a given symbol.
    ///
    /// This contains the compiler output for a given symbol,
    ///   and is returned here as an owned value.
    /// Given that fragments can be quite large,
    ///   a caller not interested in these data should choose to skip
    ///   fragments entirely rather than simply ignoring fragment events.
    Fragment(SymbolId, SymbolId),

    /// End-of-header.
    ///
    /// The header of an `xmlo` file is defined as the symbol table;
    ///   dependency list; and fragments.
    /// This event is emitted at the closing `preproc:fragment` node.
    Eoh,
}

impl parse::Object for XmloEvent {}

/// A [`Result`] with a hard-coded [`XmloError`] error type.
///
/// This is the result of every [`XmloReader`] operation that could
///   potentially fail in error.
pub type XmloResult<T> = Result<T, XmloError>;

qname_const! {
    QN_LV_PACKAGE: L_LV:L_PACKAGE,
    QN_PACKAGE: :L_PACKAGE,
    QN_NAME: :L_NAME,
    QN_UUROOTPATH: :L_UUROOTPATH,
    QN_PROGRAM: :L_PROGRAM,
    QN_ELIG_CLASS_YIELDS: L_PREPROC:L_ELIG_CLASS_YIELDS,
    QN_SYMTABLE: L_PREPROC:L_SYMTABLE,
    QN_SYM: L_PREPROC:L_SYM,
    QN_DIM: :L_DIM,
}

pub trait XmloSymtableState =
    ParseState<Token = Xirf, Object = (SymbolId, SymAttrs, Span)>
    where <Self as ParseState>::Error: Into<XmloError>;

#[derive(Debug, Default, PartialEq, Eq)]
pub enum XmloReaderState<SS: XmloSymtableState = SymtableState> {
    /// Parser has not yet processed any input.
    #[default]
    Ready,
    /// Processing `package` attributes.
    Package,
    /// Expecting a symbol declaration or end of symbol table.
    Symtable(Span, SS),
    /// `xmlo` file has been fully read.
    Done,
}

impl<SS: XmloSymtableState> ParseState for XmloReaderState<SS> {
    type Token = Xirf;
    type Object = XmloEvent;
    type Error = XmloError;

    fn parse_token(self, tok: Self::Token) -> TransitionResult<Self> {
        use XmloReaderState::*;

        match (self, tok) {
            (Ready, Xirf::Open(QN_LV_PACKAGE | QN_PACKAGE, ..)) => {
                Transition(Package).incomplete()
            }

            (Ready, _) => Transition(Ready).err(XmloError::UnexpectedRoot),

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
                Transition(Done).incomplete()
            }

            // TOOD: It'd be nice to augment errors with the symbol table
            //   span as well (e.g. "while processing symbol table at <loc>").
            (Symtable(span, ss), tok) => ss.delegate(span, tok, Symtable),

            todo => todo!("{todo:?}"),
        }
    }

    fn is_accepting(&self) -> bool {
        *self == Self::Done
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
}

impl parse::Object for (SymbolId, SymAttrs, Span) {}

impl ParseState for SymtableState {
    type Token = Xirf;
    type Object = (SymbolId, SymAttrs, Span);
    type Error = XmloError;

    fn parse_token(self, tok: Self::Token) -> TransitionResult<Self> {
        use SymtableState::*;

        match (self, tok) {
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

            (Sym(span, name, mut attrs), Xirf::Attr(Attr(key, value, _))) => {
                match key {
                    QN_DIM => {
                        use crate::sym::st::raw::{N0, N1, N2};

                        let result = match value {
                            N0 => Ok(Dim::Scalar),
                            N1 => Ok(Dim::Vector),
                            N2 => Ok(Dim::Matrix),
                            _ => Err(XmloError::InvalidDim(value, span)),
                        }
                        .and_then(|dim| {
                            attrs.dim.replace(dim);
                            Ok(ParseStatus::Incomplete)
                        });

                        Transition(Sym(span, name, attrs)).result(result)
                    }
                    QN_NAME => unreachable!("@name already processed"),
                    todo => todo!("{todo}"),
                }
            }

            todo => todo!("{todo:?}"),
        }
    }

    fn is_accepting(&self) -> bool {
        *self == Self::Ready
    }
}

impl From<(SymbolId, SymAttrs, Span)> for XmloEvent {
    fn from(tup: (SymbolId, SymAttrs, Span)) -> Self {
        match tup {
            (sym, attrs, span) => Self::SymDecl(sym, attrs, span),
        }
    }
}

#[cfg(test)]
mod test;
