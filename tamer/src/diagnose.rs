// Diagnostic system
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

//! Diagnostic system for error reporting.
//!
//! This system is heavily motivated by Rust's.
//! While the data structures and organization may differ,
//!   the diagnostic output is visually similar.

mod report;
mod resolver;

pub use report::{Reporter, VisualReporter};
pub use resolver::FsSpanResolver;

use core::fmt;
use std::{borrow::Cow, error::Error, fmt::Display};

use crate::span::Span;

/// Diagnostic report.
///
/// This describes an error condition or other special event using a series
///   of [`Span`]s to describe the source, cause, and circumstances around
///   an event.
pub trait Diagnostic: Error + Sized {
    /// Produce a series of [`AnnotatedSpan`]s describing the source and
    ///   circumstances of the diagnostic event.
    fn describe(&self) -> Vec<AnnotatedSpan>;
}

/// Diagnostic severity level.
///
/// Levels are used both for entire reports and for styling of individual
///   [`AnnotatedSpan`]s.
///
/// Lower levels are more severe
///   (e.g. level 1 is the worst).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[repr(u8)]
pub enum Level {
    /// An error internal to TAMER that the user cannot resolve,
    ///   but may be able to work around.
    InternalError = 1,

    /// A user-resolvable error.
    ///
    /// These represent errors resulting from the user's input.
    #[default]
    Error,

    /// Useful information that supplements other messages.
    ///
    /// This is most often used when multiple spans are in play for a given
    ///   diagnostic report.
    Note,

    /// Additional advice to the user that may help in debugging or fixing a
    ///   problem.
    ///
    /// These messages may suggest concrete fixes and are intended to
    ///   hopefully replace having to request advice from a human.
    /// Unlike other severity levels which provide concrete factual
    ///   information,
    ///     help messages may be more speculative.
    Help,
}

impl Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Level::InternalError => write!(f, "internal error"),
            Level::Error => write!(f, "error"),
            Level::Note => write!(f, "note"),
            Level::Help => write!(f, "help"),
        }
    }
}

/// A label associated with a report or [`Span`].
///
/// See [`AnnotatedSpan`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label<'a>(Cow<'a, str>);

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'a> From<String> for Label<'a> {
    fn from(s: String) -> Self {
        Self(Cow::Owned(s))
    }
}

impl<'a> From<&'a str> for Label<'a> {
    fn from(s: &'a str) -> Self {
        Self(Cow::Borrowed(s))
    }
}

/// A span with an associated severity level and optional label.
///
/// Annotated spans are intended to guide users through debugging a
///   diagnostic message by describing important source locations that
///   contribute to a given diagnostic event.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AnnotatedSpan<'l>(Span, Level, Option<Label<'l>>);

impl<'l> AnnotatedSpan<'l> {
    pub fn with_help<L: Into<Label<'l>>>(
        self,
        label: L,
    ) -> [AnnotatedSpan<'l>; 2] {
        let span = self.0;
        [self, span.help(label)]
    }
}

impl<'l> From<AnnotatedSpan<'l>> for Vec<AnnotatedSpan<'l>> {
    fn from(x: AnnotatedSpan<'l>) -> Self {
        vec![x]
    }
}

pub trait Annotate: Sized {
    /// Annotate a [`Span`] with a severity [`Level`] and an optional
    ///   [`Label`] to display alongside of it.
    ///
    /// You may wish to use one of the more specific methods that provide a
    ///   more pleasent interface.
    fn annotate(self, level: Level, label: Option<Label>) -> AnnotatedSpan;

    /// Annotate a span as an internal error that the user is not expected
    ///   to be able to resolve,
    ///     but may be able to work around.
    ///
    /// Since internal errors are one of the work things that can happen to
    ///   a user of a programming language,
    ///     given that they can only work around it,
    ///     this method mandates a help label that provides additional
    ///     context and a possible workaround.
    fn internal_error<'l, L: Into<Label<'l>>>(
        self,
        label: L,
    ) -> AnnotatedSpan<'l> {
        self.annotate(Level::InternalError, Some(label.into()))
    }

    /// Annotate a span with a clarifying label styled as an error.
    ///
    /// This label is intended to augment the error message to help guide
    ///   the user to a resolution.
    /// If the label does not include additional _useful_ information over
    ///   the generic message,
    ///     then it may be omitted in favor of `Annotate::mark_error` to
    ///     simply mark the location of the error.
    ///
    /// (This is not named `err` since it does not return an [`Err`].)
    fn error<'l, L: Into<Label<'l>>>(self, label: L) -> AnnotatedSpan<'l> {
        self.annotate(Level::Error, Some(label.into()))
    }

    /// Like [`Annotate::error`],
    ///   but only styles the span as a [`Level::Error`] without attaching a
    ///   label.
    ///
    /// This may be appropriate when a label would provide no more useful
    ///   information and would simply repeat the generic error text.
    /// With that said,
    ///   if the repeat message seems psychologically beneficial in context,
    ///   you may wish to use [`Annotate::error`] anyway.
    fn mark_error(self) -> AnnotatedSpan<'static> {
        self.annotate(Level::Error, None)
    }

    /// Supplemental annotated span providing additional context for another
    ///   span.
    ///
    /// For example,
    ///   if an error is related to a conflict with how an identifier is
    ///     defined,
    ///       then a note span may indicate the location of the identifier
    ///       definition.
    fn note<'l, L: Into<Label<'l>>>(self, label: L) -> AnnotatedSpan<'l> {
        self.annotate(Level::Note, Some(label.into()))
    }

    /// Provide additional information that may be used to help the user in
    ///   debugging or fixing a diagnostic.
    ///
    /// While the other severity levels denote factual information,
    ///   this provides more loose guidance.
    /// It may also include concrete suggested fixes.
    fn help<'l, L: Into<Label<'l>>>(self, label: L) -> AnnotatedSpan<'l> {
        self.annotate(Level::Help, Some(label.into()))
    }
}

impl<S: Into<Span>> Annotate for S {
    fn annotate(self, level: Level, label: Option<Label>) -> AnnotatedSpan {
        AnnotatedSpan(self.into(), level, label)
    }
}
