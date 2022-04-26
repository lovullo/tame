// Diagnostic system rendering
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

//! Rendering of diagnostic information.

use super::{
    AnnotatedSpan, Diagnostic, Label, Level, ResolvedSpan, SpanResolver,
    SpanResolverError,
};
use crate::span::{Context, Span, UNKNOWN_SPAN};
use std::fmt::{self, Display, Write};

pub trait Reporter {
    /// Render diagnostic report.
    ///
    /// Please be mindful of where this report is being rendered `to`.
    /// For example,
    ///   if rendering to standard out,
    ///   it is a good idea to buffer the entire report before flushing to
    ///     stdout,
    ///       otherwise the report may become interleaved with other
    ///       concurrent processes
    ///         (e.g. if TAMER is being invoked using `make -jN`).
    fn render(
        &mut self,
        diagnostic: &impl Diagnostic,
        to: &mut impl Write,
    ) -> Result<(), fmt::Error>;

    /// Render a diagnostic report into an owned [`String`].
    ///
    /// This invokes [`Reporter::render`] on a newly allocated [`String`].
    fn render_to_string(
        &mut self,
        diagnostic: &impl Diagnostic,
    ) -> Result<String, fmt::Error> {
        let mut str = String::new();
        self.render(diagnostic, &mut str)?;
        Ok(str)
    }
}

/// Render diagnostic report in a highly visual way.
///
/// This report is modeled after Rust's default error reporting,
///   most notable for including sections of source code associated with
///   spans,
///     underlining spans,
///     and including helpful information that walks the user through
///       understanding why the error occurred and how to approach resolving
///       it.
pub struct VisualReporter<R: SpanResolver> {
    resolver: R,
}

impl<R: SpanResolver> VisualReporter<R> {
    pub fn new(resolver: R) -> Self {
        Self { resolver }
    }
}

impl<R: SpanResolver> Reporter for VisualReporter<R> {
    // _TODO: This is a proof-of-concept._
    fn render(
        &mut self,
        diagnostic: &impl Diagnostic,
        to: &mut impl Write,
    ) -> fmt::Result {
        // TODO: not only errors; get the max level from the annotated spans
        writeln!(to, "error: {}", diagnostic)?;

        let mut prev_span = UNKNOWN_SPAN;

        for AnnotatedSpan(span, level, olabel) in diagnostic.describe() {
            if span != prev_span {
                let mspan = MaybeResolvedSpan::from(
                    self.resolver.resolve(span).map_err(|e| (e, span)),
                );

                write!(to, "  {}", mspan.header())?;

                for label in mspan.system_labels() {
                    write!(to, "{label}\n")?;
                }
            }

            if let Some(label) = olabel {
                writeln!(to, "{}", SpanLabel(level, label))?;
            }

            prev_span = span;
        }

        Ok(())
    }
}

/// A [`Span`] that may have been resolved into a [`ResolvedSpan`].
///
/// The span will remain unresolved if an error occurred,
///   in which case the error will be provided.
/// The idea is to provide as much fallback information as is useful to the
///   user so that they can still debug the problem without the benefit of
///   the resolved context.
///
/// Furthermore,
///   it is important that the underlying diagnostic message
///     (e.g. error)
///     never be masked by an error of our own.
#[derive(Debug)]
enum MaybeResolvedSpan {
    Resolved(ResolvedSpan),
    Unresolved(Span, SpanResolverError),
}

impl MaybeResolvedSpan {
    /// Span header containing the (hopefully resolved) context.
    fn header(&self) -> SpanHeader {
        match self {
            Self::Resolved(rspan) => {
                SpanHeader(rspan.ctx(), HeaderLineNum::Resolved(&rspan))
            }

            Self::Unresolved(span, _) => {
                SpanHeader(span.ctx(), HeaderLineNum::Unresolved(*span))
            }
        }
    }

    /// We should never mask an error with our own;
    ///   the diagnostic system is supposed to _help_ the user in diagnosing
    ///   problems,
    ///     not hinder them by masking it.
    fn system_labels(&self) -> Vec<SpanLabel<'static>> {
        match self {
            Self::Resolved(rspan) if rspan.col_num().is_none() => vec![
                SpanLabel(
                    Level::Help,
                    "unable to calculate columns because the line is \
                        not a valid UTF-8 string"
                        .into(),
                ),
                SpanLabel(
                    Level::Help,
                    "you have been provided with 0-indexed \
                        line-relative inclusive byte offsets"
                        .into(),
                ),
            ],

            Self::Unresolved(_, e) => {
                vec![SpanLabel(
                    Level::Help,
                    format!(
                        "there was an error trying to look up \
                         information about this span: {e}"
                    )
                    .into(),
                )]
            }

            _ => vec![],
        }
    }
}

impl From<Result<ResolvedSpan, (SpanResolverError, Span)>>
    for MaybeResolvedSpan
{
    fn from(result: Result<ResolvedSpan, (SpanResolverError, Span)>) -> Self {
        match result {
            Ok(rspan) => Self::Resolved(rspan),
            Err((e, span)) => Self::Unresolved(span, e),
        }
    }
}

/// Header describing the context of a (hopefully resolved) span.
///
/// The ideal header contains the context along with the line, and column
///   numbers,
///     visually distinguishable from surrounding lines to allow the user to
///     quickly skip between reports.
#[derive(Debug)]
struct SpanHeader<'s>(Context, HeaderLineNum<'s>);

impl<'s> Display for SpanHeader<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ctx, line) = self;
        write!(f, "--> {ctx}{line}\n")
    }
}

/// Span line number or fallback representation.
///
/// This is also responsible for attempting to produce a column number,
///   provided that a line number is available.
///
/// If a span could not be resolved,
///   offsets should be rendered in place of lines and columns.
#[derive(Debug)]
enum HeaderLineNum<'s> {
    /// Failed to resolve the [`Span`] into a [`ResolvedSpan`].
    Unresolved(Span),

    /// The [`Span`] was resolved into one or more [`SourceLine`]s.
    Resolved(&'s ResolvedSpan),
}

impl<'s> Display for HeaderLineNum<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // This is not ideal,
            //   but provides reasonable fallback information in a
            //   situation where the diagnostic system fails.
            // The user still has enough information to diagnose the
            //   problem,
            //     albeit presented in a significantly less friendly way.
            Self::Unresolved(span) => {
                write!(
                    f,
                    " offset {}--{}",
                    span.offset(),
                    span.endpoints_saturated().1.offset(),
                )
            }

            Self::Resolved(rspan) => {
                let col = HeaderColNum(rspan);
                write!(f, ":{}{col}", rspan.line_num())
            }
        }
    }
}

/// Column number or fallback representation.
///
/// If a column could not be resolved,
///   it should fall back to displaying byte offsets relative to the start
///   of the line.
#[derive(Debug)]
struct HeaderColNum<'s>(&'s ResolvedSpan);

impl<'s> Display for HeaderColNum<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(rspan) = self;
        let span = rspan.unresolved_span();

        match rspan.col_num() {
            Some(col) => write!(f, ":{}", col),

            // The column is unavailable,
            //   which means that the line must have contained invalid UTF-8.
            // Output what we can in an attempt to help the user debug.
            None => {
                let rel = span
                    .relative_to(rspan.first_line_span())
                    .unwrap_or(UNKNOWN_SPAN);

                write!(
                    f,
                    " bytes {}--{}",
                    rel.offset(),
                    rel.endpoints_saturated().1.offset()
                )
            }
        }
    }
}

/// A label describing a span.
#[derive(Debug)]
struct SpanLabel<'l>(Level, Label<'l>);

impl<'l> Display for SpanLabel<'l> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(level, label) = self;
        write!(f, "      {level}: {label}")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod integration;
}
