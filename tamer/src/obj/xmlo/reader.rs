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

use crate::tpwrap::quick_xml::{Error as XmlError, InnerXmlError};
use crate::{
    ir::legacyir::{PackageAttrs, SymAttrs},
    sym::SymbolId,
};
use std::{fmt::Display, io::BufRead};

#[cfg(feature = "wip-xmlo-xir-reader")]
use crate::ir::xir::reader::XmlXirReader;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
mod quickxml;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
pub use quickxml::XmloReader;

/// A [`Result`] with a hard-coded [`XmloError`] error type.
///
/// This is the result of every [`XmloReader`] operation that could
///   potentially fail in error.
pub type XmloResult<T> = Result<T, XmloError>;

#[cfg(feature = "wip-xmlo-xir-reader")]
pub struct XmloReader<B: BufRead> {
    _reader: XmlXirReader<B>,
}

#[cfg(feature = "wip-xmlo-xir-reader")]
impl<B: BufRead> XmloReader<B> {
    pub fn new(reader: B) -> Self {
        let reader = XmlXirReader::new(reader);

        Self { _reader: reader }
    }

    pub fn read_event<'a>(&mut self) -> XmloResult<XmloEvent> {
        todo!("XmloReader::read_event")
    }
}

impl<B> Iterator for XmloReader<B>
where
    B: BufRead,
{
    type Item = XmloResult<XmloEvent>;

    /// Invoke [`XmloReader::read_event`] and yield the result via an
    ///   [`Iterator`] API.
    ///
    /// *Warning*: This will always return [`Some`] for now.
    /// Future changes may alter this behavior.
    /// To terminate the iterator,
    ///   it's recommended that you use [`Iterator::take_while`] to filter
    ///   on the desired predicate,
    ///     such as [`XmloEvent::Eoh`].
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read_event())
    }
}

impl<B> From<B> for XmloReader<B>
where
    B: BufRead,
{
    fn from(buf: B) -> Self {
        Self::new(buf)
    }
}

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
    /// Package declaration.
    ///
    /// This contains data gathered from the root `lv:package` node.
    Package(PackageAttrs),

    /// Symbol declaration.
    ///
    /// This represents an entry in the symbol table,
    ///   which includes a symbol along with its variable metadata as
    ///   [`SymAttrs`].
    SymDecl(SymbolId, SymAttrs),

    /// Dependencies of a given symbol.
    ///
    /// Note that, for simplicity, an owned vector is returned rather than a
    ///   slice into an internal buffer.
    SymDeps(SymbolId, Vec<SymbolId>),

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

/// Error during `xmlo` processing.
///
/// Errors contain only owned values rather than references to original
///   data since they represent conditions requiring termination from
///   malformed compiler output,
///     and so should rarely occur.
/// This drastically simplifies the reader and [`Result`] chaining.
///
/// TODO: These errors provide no context (byte offset).
#[derive(Debug, PartialEq)]
pub enum XmloError {
    /// XML parsing error.
    XmlError(XmlError),
    /// The root node was not an `lv:package`.
    UnexpectedRoot,
    /// A `preproc:sym` node was found, but is missing `@name`.
    UnassociatedSym,
    /// The provided `preproc:sym/@type` is unknown or invalid.
    InvalidType(String),
    /// The provided `preproc:sym/@dtype` is unknown or invalid.
    InvalidDtype(String),
    /// The provided `preproc:sym/@dim` is invalid.
    InvalidDim(String),
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
}

impl From<InnerXmlError> for XmloError {
    fn from(e: InnerXmlError) -> Self {
        XmloError::XmlError(e.into())
    }
}

impl Display for XmloError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            XmloError::XmlError(e) => e.fmt(fmt),
            XmloError::UnexpectedRoot => {
                write!(fmt, "unexpected package root (is this a package?)")
            }
            XmloError::UnassociatedSym => write!(
                fmt,
                "unassociated symbol table entry: preproc:sym/@name missing"
            ),
            XmloError::InvalidType(ty) => {
                write!(fmt, "invalid preproc:sym/@type `{}`", ty)
            }
            XmloError::InvalidDtype(dtype) => {
                write!(fmt, "invalid preproc:sym/@dtype `{}`", dtype)
            }
            XmloError::InvalidDim(dim) => {
                write!(fmt, "invalid preproc:sym/@dim `{}`", dim)
            }
            XmloError::InvalidMapFrom(msg) => {
                write!(fmt, "invalid preproc:sym[@type=\"map\"]: {}", msg)
            }
            XmloError::UnassociatedSymDep => write!(
                fmt,
                "unassociated dependency list: preproc:sym-dep/@name missing"
            ),
            XmloError::MalformedSymRef(msg) => {
                write!(fmt, "malformed dependency ref: {}", msg)
            }
            XmloError::UnassociatedFragment => write!(
                fmt,
                "unassociated fragment: preproc:fragment/@id missing"
            ),
            XmloError::MissingFragmentText(symname) => write!(
                fmt,
                "fragment found, but missing text for symbol `{}`",
                symname,
            ),
        }
    }
}

impl std::error::Error for XmloError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::XmlError(e) => Some(e),
            _ => None,
        }
    }
}
