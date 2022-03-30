// XIR-based xmlo object errors
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

//! Errors while processing `xmlo` object files.

use crate::parse::ParseError;
use crate::span::Span;
use crate::sym::SymbolId;
use crate::tpwrap::quick_xml::{Error as XmlError, InnerXmlError};
use crate::xir::{tree::StackError, Token};
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
    /// XML parsing error (legacy, quick-xml).
    XmlError(XmlError),
    /// XIR parsing error.
    XirtError(ParseError<Token, StackError>),
    /// The root node was not an `lv:package`.
    UnexpectedRoot,
    /// A `preproc:sym` node was found, but is missing `@name`.
    UnassociatedSym(Span),
    /// The provided `preproc:sym/@type` is unknown or invalid.
    InvalidType(SymbolId, Span),
    /// The provided `preproc:sym/@dtype` is unknown or invalid.
    InvalidDtype(SymbolId, Span),
    /// The provided `preproc:sym/@dim` is invalid.
    InvalidDim(SymbolId, Span),
    /// A `preproc:sym-dep` element was found, but is missing `@name`.
    UnassociatedSymDep,
    /// The `preproc:sym[@type="map"]` contains unexpected or invalid data.
    InvalidMapFrom(String),
    /// Invalid dependency in adjacency list
    ///   (`preproc:sym-dep/preproc:sym-ref`).
    MalformedSymRef(String),
    /// A `preproc:fragment` element was found, but is missing `@id`.
    UnassociatedFragment,
    /// A `preproc:fragment` element was found, but is missing `text()`.
    MissingFragmentText(SymbolId),
    /// Token stream ended unexpectedly.
    UnexpectedEof,
}

impl From<InnerXmlError> for XmloError {
    fn from(e: InnerXmlError) -> Self {
        XmloError::XmlError(e.into())
    }
}

impl From<ParseError<Token, StackError>> for XmloError {
    fn from(e: ParseError<Token, StackError>) -> Self {
        Self::XirtError(e)
    }
}

impl Display for XmloError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::XmlError(e) => e.fmt(fmt),
            Self::XirtError(e) => e.fmt(fmt),
            Self::UnexpectedRoot => {
                write!(fmt, "unexpected package root (is this a package?)")
            }
            Self::UnassociatedSym(span) => write!(
                fmt,
                "unassociated symbol table entry: \
                     preproc:sym/@name missing at {span}"
            ),
            Self::InvalidType(ty, span) => {
                write!(fmt, "invalid preproc:sym/@type `{ty}` at {span}")
            }
            Self::InvalidDtype(dtype, span) => {
                write!(fmt, "invalid preproc:sym/@dtype `{dtype}` at {span}")
            }
            Self::InvalidDim(dim, span) => {
                write!(fmt, "invalid preproc:sym/@dim `{dim}` at {span}")
            }
            Self::InvalidMapFrom(msg) => {
                write!(fmt, "invalid preproc:sym[@type=\"map\"]: {}", msg)
            }
            Self::UnassociatedSymDep => write!(
                fmt,
                "unassociated dependency list: preproc:sym-dep/@name missing"
            ),
            Self::MalformedSymRef(msg) => {
                write!(fmt, "malformed dependency ref: {}", msg)
            }
            Self::UnassociatedFragment => write!(
                fmt,
                "unassociated fragment: preproc:fragment/@id missing"
            ),
            Self::MissingFragmentText(symname) => write!(
                fmt,
                "fragment found, but missing text for symbol `{}`",
                symname,
            ),
            Self::UnexpectedEof => write!(fmt, "unexpected EOF"),
        }
    }
}

impl std::error::Error for XmloError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::XmlError(e) => Some(e),
            Self::XirtError(e) => Some(e),
            _ => None,
        }
    }
}
