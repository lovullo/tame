// XIR errors
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

//! XIR error information.

use super::QName;
use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    span::Span,
    sym::SymbolId,
};
use std::{fmt::Display, str::Utf8Error};

/// Error attempting to produce a XIR object.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Provided name contains a `':'`.
    NCColon(SymbolId, Span),
    /// Provided string contains non-ASCII-whitespace characters.
    NotWhitespace(SymbolId, Span),
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
    /// The named attribute is missing a value.
    ///
    /// The span is expected to placed at the offset where the value is
    ///   expected.
    /// The character `=` may or may not be present.
    AttrValueExpected(Option<QName>, Span),
    /// An attribute value was found but was not quoted.
    ///
    /// The symbol here should be the name of the attribute.
    AttrValueUnquoted(Option<QName>, Span),

    // TODO: Better error translation.
    QuickXmlError(QuickXmlError, Span),
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
        use Error::*;

        match self {
            NCColon(sym, _) => {
                write!(f, "NCName `{sym}` cannot contain `:`",)
            }

            NotWhitespace(_s, _) => {
                write!(f, "whitespace expected")
            }

            InvalidQName(qname, _) => {
                write!(f, "invalid QName `{qname}`")
            }

            InvalidUtf8(inner, _bytes, _) => Display::fmt(inner, f),

            UnsupportedXmlVersion(ver, _) => {
                write!(f, "unsupported XML version `{ver}`")
            }

            UnsupportedEncoding(enc, _) => {
                // TODO: when we have hints,
                //   indicate that they can also entirely remove this
                //   attribute to resolve the error
                write!(f, "unsupported encoding `{enc}`")
            }

            AttrValueExpected(Some(name), _) => {
                write!(f, "value expected for attribute `@{name}`")
            }

            // TODO: Parsers should provide the name.
            AttrValueExpected(None, _) => {
                write!(f, "value expected for attribute")
            }

            AttrValueUnquoted(Some(name), _) => {
                write!(f, "attribute `@{name}` missing quotes")
            }

            // TODO: Parsers should provide the name.
            AttrValueUnquoted(None, _) => {
                write!(f, "value for attribute is missing quotes")
            }

            // TODO: Translate error messages
            QuickXmlError(inner, _) => {
                write!(f, "internal parser error: {inner}")
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

impl Diagnostic for Error {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use Error::*;

        match self {
            // NB: This is often constructed from a QName and so we may not
            //   have as much context as we would like;
            //     don't be too specific.
            NCColon(_, span) => span.error("unexpected `:` here").into(),

            NotWhitespace(_, span) => {
                span.error("whitespace expected here").into()
            }

            InvalidQName(_, span) => span.mark_error().into(),

            InvalidUtf8(_, bytes, span) => {
                span.error(format!("has byte sequence `{bytes:?}`",)).into()
            }

            UnsupportedXmlVersion(_, span) => {
                // TODO: suggested fix: replacement of span with `1.0`
                span.error("expected version `1.0`").into()
            }

            UnsupportedEncoding(_, span) => {
                // TODO: suggested fix: remove attribute and whitespace
                span.error("expected `utf-8` or `UTF-8`").into()
            }

            AttrValueExpected(_, span) => {
                span.error("attribute value expected").into()
            }

            AttrValueUnquoted(_, span) => {
                // TODO: suggested fix: wrap in quotes
                span.error("quotes expected around this value").into()
            }

            QuickXmlError(_, span) => {
                // TODO: note saying that this should probably be reported
                //   to provide a better error
                span.mark_error().into()
            }
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
    NotWhitespace(SymbolId),
    InvalidQName(SymbolId),
    InvalidUtf8(Utf8Error, Vec<u8>),
    QuickXmlError(QuickXmlError),
}

impl SpanlessError {
    pub fn with_span(self, span: Span) -> Error {
        match self {
            Self::NCColon(sym) => Error::NCColon(sym, span),
            Self::NotWhitespace(sym) => Error::NotWhitespace(sym, span),
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

impl<E: Into<QuickXmlError>> From<E> for SpanlessError {
    fn from(err: E) -> Self {
        Self::QuickXmlError(err.into())
    }
}

/// Thin wrapper around [`quick_xml::Error`] to implement [`PartialEq`].
///
/// This will always yield `false`,
///   but allows us to derive the trait on types using [`Error`];
///     otherwise, this madness propagates indefinitely.
#[derive(Debug)]
pub struct QuickXmlError(pub quick_xml::Error);

impl PartialEq for QuickXmlError {
    /// [`quick_xml::Error`] does not implement [`PartialEq`] and so this
    ///   will always yield `false`.
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Eq for QuickXmlError {}

impl From<quick_xml::Error> for QuickXmlError {
    fn from(e: quick_xml::Error) -> Self {
        Self(e)
    }
}

impl From<QuickXmlError> for quick_xml::Error {
    fn from(e: QuickXmlError) -> Self {
        match e {
            QuickXmlError(e) => e,
        }
    }
}

impl Display for QuickXmlError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        // NB: If we eventually use `source` to display a hierarchy of
        //   errors, then we likely do not want the duplication here.
        self.0.fmt(fmt)
    }
}

impl std::error::Error for QuickXmlError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.0)
    }
}
