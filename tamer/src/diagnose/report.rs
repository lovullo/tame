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
};
use crate::span::{Span, SpanOffsetSize, UNKNOWN_SPAN};
use std::fmt::{self, Write};

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

    /// Render a raw [`Span`] that could not be resolved into a [`ResolvedSpan`].
    ///
    /// This is not ideal,
    ///   but provides reasonable fallback information in a situation where
    ///   the diagnostic system fails.
    /// The user still has enough information to diagnose the problem,
    ///   albeit presented in a significantly less friendly way.
    fn render_fallback_span_offset(
        to: &mut impl Write,
        span: Span,
    ) -> fmt::Result {
        writeln!(
            to,
            " offset {}--{}",
            span.offset(),
            span.offset() + span.len() as SpanOffsetSize
        )
    }

    fn render_label(
        to: &mut impl Write,
        level: Level,
        label: Label,
    ) -> fmt::Result {
        writeln!(to, "      {level}: {label}")
    }

    /// Attempt to render column offset.
    ///
    /// The happy path simply outputs `":N\n"`,
    ///   where `N` is the column number.
    ///
    /// If the column is not available,
    ///   then the line did not contain valid UTF-8.
    /// In this case,
    ///   raw relative byte offsets are output along with help information
    ///   notifying the user of the issue;
    ///     this is hopefully enough information to quickly diagnose the
    ///     problem.
    fn render_col(to: &mut impl Write, rspan: ResolvedSpan) -> fmt::Result {
        let span = rspan.span;

        match rspan.col_num() {
            Some(col) => writeln!(to, ":{}", col)?,

            // The column is unavailable,
            //   which means that the line must have contained invalid UTF-8.
            // Output what we can in an attempt to help the user debug.
            None => {
                let rel = span
                    .relative_to(rspan.first_line_span())
                    .unwrap_or(UNKNOWN_SPAN);

                writeln!(
                    to,
                    " bytes {}--{}",
                    rel.offset(),
                    rel.endpoints_saturated().1.offset()
                )?;

                Self::render_label(
                    to,
                    Level::Help,
                    "unable to calculate columns because the line is \
                       not a valid UTF-8 string"
                        .into(),
                )?;

                Self::render_label(
                    to,
                    Level::Help,
                    "you have been provided with 0-indexed \
                       line-relative inclusive byte offsets"
                        .into(),
                )?;
            }
        }

        Ok(())
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
                write!(to, "  --> {}", span.ctx(),)?;

                match self.resolver.resolve(span) {
                    // We should never mask an error with our own;
                    //   the diagnostic system is supposed to _help_ the
                    //   user in diagnosing problems,
                    //     not hinder them by masking it.
                    Err(e) => {
                        Self::render_fallback_span_offset(to, span)?;

                        // Let the user know that something bad happened,
                        //   even though this probably won't make any sense.
                        Self::render_label(
                            to,
                            Level::Help,
                            format!(
                                "there was an error trying to look up \
                                       information about this span: {e}"
                            )
                            .into(),
                        )?;
                    }
                    Ok(rspan) => {
                        write!(to, ":{}", rspan.line_num())?;
                        Self::render_col(to, rspan)?;
                    }
                }
            }

            if let Some(label) = olabel {
                Self::render_label(to, level, label)?;
            }

            prev_span = span;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod integration;
}
