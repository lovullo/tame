// XIR-based xmlo object errors
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

//! Errors while processing `xmlo` object files.

use crate::diagnose::{Annotate, AnnotatedSpan, Diagnostic};
use crate::parse::Token;
use crate::span::Span;
use crate::sym::SymbolId;
use crate::xir::flat::XirfToken;
use std::fmt::Display;

/// Error during `xmlo` processing.
///
/// Errors contain only owned values rather than references to original
///   data since they represent conditions requiring termination from
///   malformed compiler output,
///     and so should rarely occur.
/// This drastically simplifies the reader and [`Result`] chaining.
///
/// TODO: These errors provide no context (byte offset).
#[derive(Debug, PartialEq, Eq)]
pub enum XmloError {
    /// The root node was not an `lv:package`.
    UnexpectedRoot(XirfToken),
    /// A `preproc:sym` node was found, but is missing `@name`.
    UnassociatedSym(Span),
    /// The provided `preproc:sym/@type` is unknown or invalid.
    InvalidType(SymbolId, Span),
    /// The provided `preproc:sym/@dtype` is unknown or invalid.
    InvalidDtype(SymbolId, Span),
    /// The provided `preproc:sym/@dim` is invalid.
    InvalidDim(SymbolId, Span),
    /// A `preproc:sym-dep` element was found, but is missing `@name`.
    UnassociatedSymDep(Span),
    /// The `preproc:sym[@type="map"]` is missing a @name.
    MapFromNameMissing(SymbolId, Span),
    /// Multiple `preproc:from` nodes found.
    MapFromMultiple(SymbolId, Span),
    /// Invalid dependency in adjacency list
    ///   (`preproc:sym-dep/preproc:sym-ref`).
    MalformedSymRef(SymbolId, Span),
    /// A `preproc:fragment` element was found, but is missing `@id`.
    UnassociatedFragment(Span),
    /// A `preproc:fragment` element was found, but is missing `text()`.
    MissingFragmentText(SymbolId, Span),
    /// A token of input was unexpected.
    ///
    /// Ideally we would provide a better error depending on the context,
    ///   but this serves as a fallback if the input is completely
    ///   unexpected.
    UnexpectedToken(XirfToken),
}

impl Display for XmloError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XmloError::*;

        match self {
            UnexpectedRoot(_tok) => {
                write!(fmt, "expected `package` root element")
            }

            UnassociatedSym(_) => {
                write!(fmt, "unassociated symbol table entry")
            }

            InvalidType(ty, _) => {
                write!(fmt, "invalid symbol type `{ty}`")
            }

            InvalidDtype(dtype, _) => {
                write!(fmt, "invalid symbol dtype `{dtype}`")
            }

            InvalidDim(dim, _) => {
                write!(fmt, "invalid dimensionality `{dim}`")
            }

            MapFromNameMissing(sym, _) => {
                write!(fmt, "map `from` name missing for symbol `{sym}`")
            }

            MapFromMultiple(sym, _) => {
                write!(fmt, "multiple map `from` for `{sym}`")
            }

            UnassociatedSymDep(_) => {
                write!(fmt, "unassociated dependency list")
            }

            MalformedSymRef(name, _) => {
                write!(fmt, "malformed dependency ref for symbol {name}")
            }

            UnassociatedFragment(_) => write!(fmt, "unassociated fragment"),

            MissingFragmentText(sym, _) => {
                write!(fmt, "missing fragment text for symbol `{sym}`")
            }

            UnexpectedToken(tok) => write!(fmt, "unexpected {tok}"),
        }
    }
}

impl std::error::Error for XmloError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Diagnostic for XmloError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use XmloError::*;

        let malformed = "this `xmlo` file is malformed or corrupt; \
                           try removing it to force it to be rebuilt, \
                             and please report this error";

        // Note that these errors _could_ potentially be more descriptive
        //   and contain more spans,
        //     but they are internal compiler errors and so it's not yet
        //     deemed to be worth the effort.
        match self {
            UnexpectedRoot(tok) => {
                // TODO: If we recommend `<package`,
                //   we ought to have a span that guarantees that `<` will
                //   be included.
                tok.span()
                    .error("`<package` expected here")
                    .with_help(
                        "an `xmlo` file was expected, \
                           but this appears to be something else",
                    )
                    .into()
            }

            UnassociatedSym(span) => span
                .internal_error("`@name` is missing")
                .with_help(malformed)
                .into(),

            InvalidType(_ty, span) => span
                .internal_error("the type `{ty}` is unknown")
                .with_help(malformed)
                .into(),

            InvalidDtype(_dtype, span) => span
                .internal_error("the dtype `{dtype}` is unknown")
                .with_help(malformed)
                .into(),

            InvalidDim(_dim, span) => span
                .internal_error(
                    "the number of dimensions must be `0`, `1`, or `2`",
                )
                .with_help(malformed)
                .into(),

            MapFromNameMissing(_sym, span) => span
                .internal_error("`@name` is missing")
                .with_help(malformed)
                .into(),

            MapFromMultiple(_sym, span) => span
                .internal_error("extra `preproc:from` here")
                .with_help(malformed)
                .into(),

            UnassociatedSymDep(span) => span
                .internal_error("`@name` is missing")
                .with_help(malformed)
                .into(),

            MalformedSymRef(_name, span) => span
                .internal_error("`@name` is missing")
                .with_help(malformed)
                .into(),

            UnassociatedFragment(span) => span
                .internal_error("@id is missing")
                .with_help(malformed)
                .into(),

            MissingFragmentText(_sym, span) => span
                .internal_error("missing fragment text")
                .with_help(malformed)
                .into(),

            UnexpectedToken(tok) => tok
                .span()
                .internal_error("unknown or unexpected token")
                .with_help(malformed)
                .into(),
        }
    }
}
