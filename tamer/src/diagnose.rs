// Diagnostic system
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

//! Diagnostic system for error reporting and logging.
//!
//! This system is heavily motivated by Rust's.
//! While the data structures and organization may differ,
//!   the diagnostic output is visually similar.
//!
//! Visual Report
//! -------------
//! The primary output of the system is a [`Report`](report::Report).
//! A report consists not only of the diagnostic information provided by the
//!   system
//!     (such as error messages),
//!     but also annotated source code.
//! Reports attempt to create a narrative that walks the user through why
//!   and how problems arose,
//!     and hopefully provides information on how to resolve the problem.
//!
//! Here is an example report:
//!
//! ```text
//! error: expected closing tag for `classify`
//!   --> /home/user/program/foo.xml:16:5
//!    |
//! 16 |     <classify as="foo" desc="Example classification">
//!    |     --------- note: element `classify` is opened here
//!
//!   --> /home/user/program/foo.xml:24:5
//!    |
//! 24 |     </wrong>
//!    |     ^^^^^^^^ error: expected `</classify>`
//! ```
//!
//! A single report is produced for each error
//!   (or other suitable diagnostic event).
//! The system may produce multiple reports at a time if it discovers
//!   multiple issues and is able to recover enough to continue to discover
//!   others.
//!
//! Each report is separated into a number of sections,
//!   with each section delimited by a header.
//! Each section describes one or more spans related to a range of related
//!   source lines.
//! Those source lines are annotated using the [`Span`]s associated with the
//!   emitted diagnostic data,
//!     such as an error.
//!
//! Each section has a _gutter_ containing the line number of source lines.
//! The area below the section header and to the right of the gutter is
//!   called the _body_ of the section.
//! The gutter will expand as needed to fit the line number.
//!
//! _Warning: Reports do not yet strip terminal escape sequences,
//!   which may interfere with the diagnostic output.
//! This may represent a security risk depending on your threat model,
//!   but does require access to the source code being compiled._
//!
//! See the [`report`] module for more information.

#[macro_use]
pub mod panic;

mod report;
mod resolve;

use std::{
    borrow::Cow,
    convert::Infallible,
    fmt::{self, Debug, Display},
};

pub use report::{Reporter, VisualReporter};
pub use resolve::FsSpanResolver;

use crate::span::Span;

/// No annotated description is applicable for the diagnostic message.
///
/// The system should strive to give the user as much relevant information
///   as is useful to resolve the problem.
/// Whether or not the absence of this description represents a deficiency
///   depends on the error context.
pub const NO_DESC: Vec<AnnotatedSpan> = vec![];

/// An event able to describe itself for diagnostic reporting.
///
/// This describes an event using a series of [`Span`]s to describe the
///   source, cause, and circumstances around an event.
///
/// A diagnostic event is not necessarily an error condition;
///   for example,
///     a user may request logging of compilation events to inspect the
///     state of the system or help them to debug why the system is
///     interpreting their program in a certain way.
pub trait Diagnostic: Display + Debug + Sized {
    /// Produce a series of [`AnnotatedSpan`]s describing the source and
    ///   circumstances of the diagnostic event.
    fn describe(&self) -> Vec<AnnotatedSpan>;
}

impl Diagnostic for Infallible {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        // This should never actually happen unless someone is explicitly
        //   invoking this method on `Infallible`.
        unreachable!("Infallible is not supposed to fail")
    }
}

/// Diagnostic severity level.
///
/// Levels are used both for entire reports and for styling of individual
///   [`AnnotatedSpan`]s.
///
/// Higher severity levels are represented by lower integer values
///   (e.g. level 1 is the worst),
///   like DEFCON levels.
/// The rationale here is that,
///   provided that you remember that these are 1-indexed,
///   you do not need to know how many levels exist to know how severe it
///   is.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[repr(u8)]
pub enum Level {
    /// An error internal to TAMER that the user cannot resolve,
    ///   but may be able to work around.
    InternalError = 1,

    /// A user-resolvable error.
    ///
    /// These represent errors resulting from the user's input.
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
    #[default]
    Help,
}

impl Level {
    /// Whether this error level represents an error.
    pub fn is_error(self) -> bool {
        self <= Self::Error
    }
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

impl<'a> From<&'a String> for Label<'a> {
    fn from(s: &'a String) -> Self {
        Self(Cow::Borrowed(s))
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

    /// The [`Span`] with which the annotation is associated.
    pub fn span(&self) -> Span {
        match self {
            AnnotatedSpan(span, ..) => *span,
        }
    }

    /// A reference to the label of the annotation,
    ///   if available.
    pub fn label(&self) -> Option<&Label<'l>> {
        match self {
            AnnotatedSpan(.., label) => label.as_ref(),
        }
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

    /// Like [`Annotate::error`]r
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

/// Generate a variant-less error enum akin to [`Infallible`].
///
/// This is used to create [`Infallible`]-like newtypes where unique error
///   types are beneficial.
/// For example,
///   this can be used so that [`From`] implementations can be exclusively
///   used to widen errors
///     (or lack thereof)
///     into error sum variants,
///       and is especially useful when code generation is involved to avoid
///       generation of overlapping [`From`] `impl`s.
///
/// The generated enum is convertable [`Into`] and [`From`] [`Infallible`].
macro_rules! diagnostic_infallible {
    ($vis:vis enum $name:ident {}) => {
        /// A unique [`Infallible`](std::convert::Infallible) type.
        #[derive(Debug, PartialEq)]
        $vis enum $name {}

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!($name))
            }
        }

        impl $crate::diagnose::Diagnostic for $name {
            fn describe(&self) -> Vec<$crate::diagnose::AnnotatedSpan> {
                // This is a unit struct and should not be able to be
                //   instantiated!
                unreachable!(
                    concat!(
                        stringify!($name),
                        " should be unreachable!"
                    )
                )
            }
        }

        impl From<std::convert::Infallible> for $name {
            fn from(_: std::convert::Infallible) -> Self {
                unreachable!()
            }
        }

        impl From<$name> for std::convert::Infallible {
            fn from(_: $name) -> Self {
                unreachable!()
            }
        }
    }
}
