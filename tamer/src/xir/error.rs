// XIR errors
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

//! XIR error information.

use crate::{span::Span, sym::SymbolId, tpwrap::quick_xml};
use std::{fmt::Display, str::Utf8Error};

/// Error attempting to produce a XIR object.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Provided name contains a `':'`.
    NCColon(SymbolId, Span),
    /// Provided string contains non-ASCII-whitespace characters.
    NotWhitespace(String),
    /// Provided QName is not valid.
    InvalidQName(SymbolId, Span),
    /// A UTF-8 error together with the byte slice that caused it.
    ///
    /// By storing the raw bytes instead of a string,
    ///   we allow the displayer to determine how to handle invalid UTF-8
    ///   encodings.
    /// Further,
    ///   we cannot intern strings that are not valid UTF-8.
    InvalidUtf8(Utf8Error, Vec<u8>, Span),
    /// XML 1.0 only.
    ///
    /// Other versions are not widely in use
    ///   (only 1.1 exists at the time of writing)
    ///   and providing that is either in error,
    ///     copy/paste,
    ///     or the user is expecting something they're not going to get.
    UnsupportedXmlVersion(SymbolId, Span),
    /// TAMER expects UTF-8 encoding for everything,
    ///   which should not be an unreasonable expectation.
    UnsupportedEncoding(SymbolId, Span),

    // TODO: Better error translation.
    QuickXmlError(quick_xml::Error, Span),
}

impl Error {
    pub fn from_with_span<E: Into<SpanlessError>>(
        span: Span,
    ) -> impl FnOnce(E) -> Self {
        move |e: E| e.into().with_span(span)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NCColon(sym, span) => {
                write!(f, "NCName `{sym}` cannot contain ':' at {span}",)
            }
            Self::NotWhitespace(s) => {
                write!(f, "string contains non-ASCII-whitespace: `{}`", s)
            }
            Self::InvalidQName(qname, span) => {
                write!(f, "invalid QName `{qname}` at {span}")
            }
            Self::InvalidUtf8(inner, bytes, span) => {
                write!(
                    f,
                    "{inner} for string `{}` with bytes `{bytes:?}` at {span}",
                    String::from_utf8_lossy(bytes)
                )
            }
            Self::UnsupportedXmlVersion(ver, span) => {
                write!(
                    f,
                    "expected XML version `1.0` at {span}, \
                       but found unsupported version `{ver}`"
                )
            }
            Self::UnsupportedEncoding(enc, span) => {
                // TODO: when we have hints,
                //   indicate that they can also entirely remove this
                //   attribute to resolve the error
                write!(
                    f,
                    "expected `utf-8` or `UTF-8` encoding at {span}, \
                       but found unsupported encoding `{enc}`"
                )
            }
            // TODO: Translate error messages
            Self::QuickXmlError(inner, span) => {
                write!(f, "internal parser error: {inner} at {span}")
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidUtf8(e, ..) => Some(e),
            Self::QuickXmlError(e, ..) => Some(e),
            _ => None,
        }
    }
}

/// An [`Error`] that requires its [`Span`] to be filled in by the caller.
///
/// These errors should not be converted automatically,
///   since only the caller can know the correct information to provide for
///   a useful [`Span`].
/// Failure to provide a useful span will betray the user when they need us
///   the most:
///     debugging an error.
///
/// As such,
///   please do not implement `From<SpanlessError> for Error`;
///   use [`SpanlessError::with_span`] instead.
#[derive(Debug, PartialEq)]
pub enum SpanlessError {
    NCColon(SymbolId),
    InvalidQName(SymbolId),
    InvalidUtf8(Utf8Error, Vec<u8>),
    QuickXmlError(quick_xml::Error),
}

impl SpanlessError {
    pub fn with_span(self, span: Span) -> Error {
        match self {
            Self::NCColon(sym) => Error::NCColon(sym, span),
            Self::InvalidQName(qname) => Error::InvalidQName(qname, span),
            Self::InvalidUtf8(inner, bytes) => {
                Error::InvalidUtf8(inner, bytes, span)
            }
            Self::QuickXmlError(inner) => Error::QuickXmlError(inner, span),
        }
    }

    pub fn into_with_span<E>(span: Span) -> impl FnOnce(E) -> Error
    where
        E: Into<SpanlessError>,
    {
        move |e: E| e.into().with_span(span)
    }
}

impl std::error::Error for SpanlessError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidUtf8(inner, ..) => Some(inner),
            Self::QuickXmlError(inner) => Some(inner),
            _ => None,
        }
    }
}

impl Display for SpanlessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // This isn't friendly, since it shouldn't occur.
        write!(f, "internal error: missing span for error: {self:?}")
    }
}

impl From<(Utf8Error, &[u8])> for SpanlessError {
    fn from((err, bytes): (Utf8Error, &[u8])) -> Self {
        Self::InvalidUtf8(err, bytes.to_owned())
    }
}

impl<E: Into<quick_xml::Error>> From<E> for SpanlessError {
    fn from(err: E) -> Self {
        Self::QuickXmlError(err.into())
    }
}
