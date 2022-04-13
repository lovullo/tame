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

use super::{AnnotatedSpan, Diagnostic};
use crate::span::UNKNOWN_SPAN;
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
pub struct VisualReporter;

impl VisualReporter {
    pub fn new() -> Self {
        Self
    }
}

impl Reporter for VisualReporter {
    // _TODO: This is a proof-of-concept._
    fn render(
        &mut self,
        diagnostic: &impl Diagnostic,
        to: &mut impl Write,
    ) -> Result<(), fmt::Error> {
        // TODO: not only errors; get the max level from the annotated spans
        writeln!(to, "error: {}", diagnostic)?;

        let mut prev_span = UNKNOWN_SPAN;

        for AnnotatedSpan(span, level, olabel) in diagnostic.describe() {
            if span != prev_span {
                writeln!(
                    to,
                    "  --> {} offset {}--{}",
                    span.ctx(),
                    span.offset(),
                    span.offset() + span.len() as u32
                )?;
            }

            if let Some(label) = olabel {
                writeln!(to, "      {level}: {label}")?;
            }

            prev_span = span;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    //! Integration tests for the diagnostic system.
    //!
    //! These tests have the potential to be rather rigid,
    //!   but the proper rendering of the diagnostic system is very
    //!   important,
    //!     given that the slightest misrendering could cause significant
    //!     confusion.
    //!
    //! While it may be tempting to use format strings to reduce the
    //!   maintenance burden,
    //!     this should be avoided most cases,
    //!     since writing out the entire expected value will allow the
    //!       developer to visualize what output will be produced and
    //!       whether it is appropriate for the user.
    //!
    //! In essence:
    //!   this diagnostic report is effectively another compiler target,
    //!     and it must be byte-for-byte identical to what is expected.
    //! With that said,
    //!   _do not interpret these tests as providing a stable,
    //!     unchanging diagnostic report output_.
    //! This can and will change over time,
    //!   and separate reporters will be provided for machine-readable
    //!   formats if that is what is needed.

    use crate::{diagnose::Annotate, span::Context};

    use super::*;
    use std::{error::Error, fmt::Display};

    #[derive(Debug)]
    struct StubError(String, Vec<AnnotatedSpan>);

    impl Display for StubError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Error for StubError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            None
        }
    }

    impl Diagnostic for StubError {
        fn describe(&self) -> Vec<AnnotatedSpan> {
            self.1.clone()
        }
    }

    macro_rules! assert_report {
        ($msg:expr, $aspans:expr, $expected:expr) => {
            let mut sut = VisualReporter::new();

            assert_eq!(
                sut.render_to_string(&StubError($msg.into(), $aspans)),
                Ok($expected.into()),
            );
        };
    }

    #[test]
    fn no_spans() {
        assert_report!(
            "test with no spans",
            vec![],
            // No spans will result in the `Display` of the error only.
            "error: test with no spans\n"
        );
    }

    #[test]
    fn span_error_no_label() {
        let span = Context::from("foo/bar").span(50, 5);

        assert_report!(
            "single span no label",
            vec![span.mark_error()],
            // Context and span are rendered without a label.
            "\
error: single span no label
  --> foo/bar offset 50--55
"
        );
    }

    #[test]
    fn span_error_with_label() {
        let span = Context::from("bar/baz").span(60, 2);

        assert_report!(
            "single span with label",
            vec![span.error("span label here")],
            // Context and span are rendered without a label.
            "\
error: single span with label
  --> bar/baz offset 60--62
      error: span label here
"
        );
    }

    #[test]
    fn adjacent_eq_span_no_labels_collapsed() {
        let ctx = Context::from("foo/bar");
        let span = ctx.span(50, 5);

        assert_report!(
            "multiple adjacent same span no label",
            vec![span.mark_error(), span.mark_error()],
            // Collapsed into one `-->` line since the spans are the same.
            // This is unlikely to happen,
            //   given that there is not much use in having multiple
            //   duplicate spans without some additional context.
            "\
error: multiple adjacent same span no label
  --> foo/bar offset 50--55
"
        );
    }

    #[test]
    fn adjacent_eq_span_labels_collapsed() {
        let ctx = Context::from("baz/quux");
        let span = ctx.span(50, 5);

        assert_report!(
            "multiple adjacent same span with labels",
            vec![
                span.error("A label"),
                span.mark_error(), // no label
                span.error("C label"),
            ],
            // Labels are collapsed under the same `-->` line since the
            //   spans are the same.
            "\
error: multiple adjacent same span with labels
  --> baz/quux offset 50--55
      error: A label
      error: C label
"
        );
    }

    #[test]
    fn adjacent_eq_context_neq_offset_len_spans_not_collapsed() {
        let ctx = Context::from("quux/quuux");

        assert_report!(
            "multiple adjacent different context",
            vec![
                // -->
                ctx.span(10, 5).mark_error(),
                ctx.span(10, 5).error("A, first label"), // collapse
                // -->
                ctx.span(10, 6).error("B, different length"),
                ctx.span(10, 6).mark_error(), // collapse
                ctx.span(10, 6).error("B, collapse"),
                // -->
                ctx.span(15, 6).error("C, different offset"),
                // -->
                // Back to (10, 6), but not adjacent to previous
                ctx.span(10, 6).error("B', not adjacent"),
            ],
            "\
error: multiple adjacent different context
  --> quux/quuux offset 10--15
      error: A, first label
  --> quux/quuux offset 10--16
      error: B, different length
      error: B, collapse
  --> quux/quuux offset 15--21
      error: C, different offset
  --> quux/quuux offset 10--16
      error: B', not adjacent
"
        );
    }

    #[test]
    fn adjacent_neq_context_spans_not_collapsed() {
        // Note that the offsets and lengths are purposefully the same to
        //   ensure that the differentiator is exclusively the context.
        let span_a = Context::from("foo/bar").span(10, 3);
        let span_b = Context::from("bar/baz").span(10, 3);

        assert_report!(
            "multiple adjacent different context",
            vec![
                // -->
                span_a.mark_error(),
                span_a.error("A, first"),
                span_a.error("A, collapsed"),
                span_a.mark_error(), // collapsed, same
                // -->
                span_b.error("B, first"),
                span_b.error("B, collapsed"),
                span_b.mark_error(), // collapsed, same
                // -->
                // Back to 'a' again, but we can't collapse now since we're
                //   adjacent to 'b'
                span_a.error("A, not collapsed"),
                // Back to 'b' again, but we can't collapse now since we're
                //   adjacent to 'a'
                //     (same as prev but without label this time)
                span_b.mark_error(),
                // And just so we have two adjacent label-less spans
                span_a.mark_error(),
            ],
            "\
error: multiple adjacent different context
  --> foo/bar offset 10--13
      error: A, first
      error: A, collapsed
  --> bar/baz offset 10--13
      error: B, first
      error: B, collapsed
  --> foo/bar offset 10--13
      error: A, not collapsed
  --> bar/baz offset 10--13
  --> foo/bar offset 10--13
"
        );
    }

    #[test]
    fn severity_levels_reflected() {
        let ctx = Context::from("foo/bar");
        let span = ctx.span(50, 5);

        assert_report!(
            "multiple spans with labels of different severity level",
            vec![
                span.internal_error("an internal error"),
                span.error("an error"),
                span.note("a note"),
                span.help("a help message"),
            ],
            "\
error: multiple spans with labels of different severity level
  --> foo/bar offset 50--55
      internal error: an internal error
      error: an error
      note: a note
      help: a help message
"
        );
    }
}
