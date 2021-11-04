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

use super::{PackageAttrs, SymAttrs};
use crate::sym::SymbolId;
use crate::tpwrap::quick_xml::{Error as XmlError, InnerXmlError};
use std::fmt::Display;

// While the _use_ is gated, this isn't, to ensure that we still try to
// compile it while the flag is off (and so it's parsed by the language
// server).
mod quickxml;

#[cfg(not(feature = "wip-xmlo-xir-reader"))]
pub use quickxml::XmloReader;

#[cfg(feature = "wip-xmlo-xir-reader")]
pub use new::XmloReader;

/// A [`Result`] with a hard-coded [`XmloError`] error type.
///
/// This is the result of every [`XmloReader`] operation that could
///   potentially fail in error.
pub type XmloResult<T> = Result<T, XmloError>;

#[cfg(feature = "wip-xmlo-xir-reader")]
mod new {
    //! Re-implementation of `XmloReader` using a [`TokenStream`].
    //!
    //! This module will be merged into [`super`] once complete;
    //!   it exists to make feature-flagging less confusing and error-prone.

    use super::{XmloError, XmloEvent, XmloResult};
    use crate::sym::st::*;
    use crate::xir::{Token, TokenStream};

    qname_const! {
        QN_LV_PACKAGE: L_LV:L_PACKAGE,
        QN_PACKAGE: :L_PACKAGE,
    }

    pub struct XmloReader<I: TokenStream> {
        reader: I,
        seen_root: bool,
    }

    impl<I> XmloReader<I>
    where
        I: TokenStream,
    {
        pub fn from_reader(reader: I) -> Self {
            Self {
                reader,
                seen_root: false,
            }
        }

        pub fn read_event(&mut self) -> XmloResult<XmloEvent> {
            let token = self.reader.next().ok_or(XmloError::UnexpectedEof)?;

            if !self.seen_root {
                match token {
                    Token::Open(QN_LV_PACKAGE | QN_PACKAGE, _) => {
                        //self.seen_root = true;
                    }

                    _ => return Err(XmloError::UnexpectedRoot),
                }
            }

            match token {
                todo => todo!("read_event: {:?}", todo),
            }
        }
    }

    impl<I> Iterator for XmloReader<I>
    where
        I: TokenStream,
    {
        type Item = XmloResult<XmloEvent>;

        fn next(&mut self) -> Option<Self::Item> {
            Some(self.read_event())
        }
    }

    impl<I> From<I> for XmloReader<I>
    where
        I: TokenStream,
    {
        fn from(toks: I) -> Self {
            Self::from_reader(toks)
        }
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
    /// XML parsing error (legacy, quick-xml).
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
    /// Token stream ended unexpectedly.
    UnexpectedEof,
}

impl From<InnerXmlError> for XmloError {
    fn from(e: InnerXmlError) -> Self {
        XmloError::XmlError(e.into())
    }
}

impl Display for XmloError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::XmlError(e) => e.fmt(fmt),
            Self::UnexpectedRoot => {
                write!(fmt, "unexpected package root (is this a package?)")
            }
            Self::UnassociatedSym => write!(
                fmt,
                "unassociated symbol table entry: preproc:sym/@name missing"
            ),
            Self::InvalidType(ty) => {
                write!(fmt, "invalid preproc:sym/@type `{}`", ty)
            }
            Self::InvalidDtype(dtype) => {
                write!(fmt, "invalid preproc:sym/@dtype `{}`", dtype)
            }
            Self::InvalidDim(dim) => {
                write!(fmt, "invalid preproc:sym/@dim `{}`", dim)
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
            _ => None,
        }
    }
}

#[cfg(feature = "wip-xmlo-xir-reader")]
#[cfg(test)]
mod test;
