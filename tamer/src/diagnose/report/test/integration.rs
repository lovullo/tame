// Diagnostic system report integration tests
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

//! Integration tests for the diagnostic reporting system.
//!
//! These tests assert,
//!   byte-for-byte,
//!   against the report output that will be rendered to the user.
//! This makes the tests more fragile,
//!   but the cost is intentional:
//!     This is a complex system where presentation matters,
//!       and the slightest misrendering could cause significant confusion
//!       or even completely change the meaning of the output.
//!     The format of the system is meticulously designed and should not
//!       deviate unless done intentionally.
//!
//! The format of these tests also means that we are able to use these tests
//!   while prototyping changes to the design of the report
//!     and be confident that those changes will be presented to the user in
//!     a manner that we intended.
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

use crate::span::Context;

use super::super::super::{resolver::BufSpanResolver, Annotate};
use super::*;
use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    io::{self, Cursor},
};

#[derive(Debug)]
struct StubError(String, Vec<AnnotatedSpan<'static>>);

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

const FILE_FOO_BAR: &[u8] =
    b"foo/bar line 1\nfoo/bar line 2\nfoo/bar line 3\nfoo/bar line 4";
//    |-------+--+-|  |-------+--+-|  |-------+--+-|  |-------+--+-|
//    0       |  |13  15      |  |28  30      |  |43  45      |  |58
//            8 11           23  26          38  41          53  56
//       len: 14

const FILE_BAR_BAZ: &[u8] =
    b"bar/baz line 1\nbar/baz line 2\nbar/baz line 3\nbar/baz line 4";
// Offsets for this are the same as `FILE_FOO_BAR`.

const FILE_INVALID_UTF8: &[u8] = b"bad \xC0!";
//                                 |----   |
//                                 0       5

macro_rules! assert_report {
    ($msg:expr, $aspans:expr, $expected:expr) => {
        let mut resolver = HashMap::<Context, BufSpanResolver<_>>::new();

        let ctx_foo_bar = Context::from("foo/bar");
        let ctx_bar_baz = Context::from("bar/baz");
        let ctx_inv_utf = Context::from("invalid/utf8");

        resolver.insert(
            ctx_foo_bar,
            BufSpanResolver::new(Cursor::new(FILE_FOO_BAR), ctx_foo_bar),
        );
        resolver.insert(
            ctx_bar_baz,
            BufSpanResolver::new(Cursor::new(FILE_BAR_BAZ), ctx_bar_baz),
        );
        resolver.insert(
            ctx_inv_utf,
            BufSpanResolver::new(Cursor::new(FILE_INVALID_UTF8), ctx_inv_utf),
        );

        let mut sut = VisualReporter::new(resolver);

        assert_eq!(
            sut.render(&StubError($msg.into(), $aspans)).to_string(),
            $expected,
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
    let span = Context::from("foo/bar").span(53, 4);

    assert_report!(
        "single span no label",
        vec![span.mark_error()],
        // Context and span are rendered without a label.
        "\
error: single span no label
  --> foo/bar:4:9
   |
   | foo/bar line 4
   |         ^^^^
"
    );
}

#[test]
fn span_error_with_label() {
    let span = Context::from("bar/baz").span(30, 3);

    assert_report!(
        "single span with label",
        vec![span.error("span label here")],
        // Context and span are rendered without a label.
        "\
error: single span with label
  --> bar/baz:3:1
   |
   | bar/baz line 3
   | ^^^ error: span label here
"
    );
}

#[test]
fn adjacent_eq_span_no_labels_collapsed() {
    let ctx = Context::from("foo/bar");
    let span = ctx.span(53, 1);

    assert_report!(
        "multiple adjacent same span no label",
        vec![span.mark_error(), span.mark_error()],
        // Collapsed into one `-->` line since the spans are the same.
        // This is unlikely to happen,
        //   given that there is not much use in having multiple
        //   duplicate spans without some additional context.
        "\
error: multiple adjacent same span no label
  --> foo/bar:4:9
   |
   | foo/bar line 4
   |         ^
"
    );
}

#[test]
fn adjacent_eq_span_labels_collapsed() {
    let ctx = Context::from("bar/baz");
    let span = ctx.span(8, 6);

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
  --> bar/baz:1:9
   |
   | bar/baz line 1
   |         ^^^^^^ error: A label
   = error: C label
"
    );
}

#[test]
fn adjacent_eq_context_neq_offset_len_spans_not_collapsed() {
    let ctx = Context::from("bar/baz");

    assert_report!(
        "eq context neq offset/len",
        vec![
            // -->
            ctx.span(0, 3).mark_error(),
            // Note that this appears _after_ a marked span,
            //   and so the error will not be displayed on the same line as
            //   the mark.
            ctx.span(0, 3).error("A, first label, after mark"), // collapse
            // -->
            ctx.span(0, 7).error("B, different length"),
            ctx.span(0, 7).mark_error(), // collapse
            ctx.span(0, 7).error("B, collapse"),
            // -->
            ctx.span(15, 4).error("C, different offset"),
            // -->
            // Back to (0, 7), but not adjacent to previous
            ctx.span(0, 7).error("B', not adjacent"),
        ],
        "\
error: eq context neq offset/len
  --> bar/baz:1:1
   |
   | bar/baz line 1
   | ^^^
   = error: A, first label, after mark

  --> bar/baz:1:1
   |
   | bar/baz line 1
   | ^^^^^^^ error: B, different length
   = error: B, collapse

  --> bar/baz:2:1
   |
   | bar/baz line 2
   | ^^^^ error: C, different offset

  --> bar/baz:1:1
   |
   | bar/baz line 1
   | ^^^^^^^ error: B', not adjacent
"
    );
}

#[test]
fn adjacent_neq_context_spans_not_collapsed() {
    // Note that the offsets and lengths are purposefully the same to
    //   ensure that the differentiator is exclusively the context.
    let span_a = Context::from("foo/bar").span(0, 7);
    let span_b = Context::from("bar/baz").span(0, 7);

    assert_report!(
        "multiple adjacent different context",
        vec![
            // -->
            span_a.mark_error(),
            // Note that this appears _after_ a marked span,
            //   and so the error will not be displayed on the same line as
            //   the mark.
            span_a.error("A, first, after marked"),
            span_a.error("A, collapsed"),
            span_a.mark_error(), // collapsed, same
            // -->
            span_b.error("B, first, no marked"),
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
  --> foo/bar:1:1
   |
   | foo/bar line 1
   | ^^^^^^^
   = error: A, first, after marked
   = error: A, collapsed

  --> bar/baz:1:1
   |
   | bar/baz line 1
   | ^^^^^^^ error: B, first, no marked
   = error: B, collapsed

  --> foo/bar:1:1
   |
   | foo/bar line 1
   | ^^^^^^^ error: A, not collapsed

  --> bar/baz:1:1
   |
   | bar/baz line 1
   | ^^^^^^^

  --> foo/bar:1:1
   |
   | foo/bar line 1
   | ^^^^^^^
"
    );
}

#[test]
fn severity_levels_reflected() {
    let ctx = Context::from("foo/bar");
    let span = ctx.span(53, 6);

    assert_report!(
        "multiple spans with labels of different severity level",
        vec![
            span.internal_error("an internal error"),
            span.error("an error"),
            span.note("a note"),
            span.help("a help message"),
        ],
        "\
internal error: multiple spans with labels of different severity level
  --> foo/bar:4:9
   |
   | foo/bar line 4
   |         !!!!!! internal error: an internal error
   = error: an error
   = note: a note
   = help: a help message
"
    );
}

#[test]
fn multi_line_span() {
    let ctx = Context::from("foo/bar");

    // First two lines.
    let span = ctx.span(8, 19);

    // This is obviously terrible-looking;
    //   it'll be condensed as this evolves further.
    assert_report!(
        "multi-line span",
        vec![span.error("label to be on last line")],
        "\
error: multi-line span
  --> foo/bar:1:9
   |
   | foo/bar line 1
   |         ^^^^^^
   |
   | foo/bar line 2
   | ^^^^^^^^^^^^ error: label to be on last line
"
    );
}

// If a span fails to resolve
//   (maybe the file cannot be read for some reason,
//     or maybe there's some bug in TAMER such that the context is
//     incorrect and cannot be resolved),
//   we should still provide what information we _can_ rather than
//     masking the original error with an error of our own.
// The diagnostic system is supposed to aid the user in resolving
//   issues,
//     not make them _more_ difficult to resolve.
#[test]
fn fallback_when_span_fails_to_resolve() {
    let ctx = Context::from("unknown/context");

    // Doesn't matter what this is.
    let span = ctx.span(50, 5);

    // This could change between Rust versions or OSes.
    let ioerr = io::ErrorKind::NotFound;

    // It's not ideal that the help appears first,
    //   but this should only happen under very exceptional
    //   circumstances so it's not worth trying to resolve.
    // If you're reading this and it's trivial to swap these with the
    //   current state of the system,
    //     go for it.
    assert_report!(
            "unresolvable context fallback",
            vec![span.error("an error we do not want to suppress"),],
            format!("\
error: unresolvable context fallback
  --> unknown/context offset 50--55
   = error: an error we do not want to suppress
   = help: an error occurred while trying to look up information about this span: {ioerr}
")
        );
}

/// If the span columns cannot be determined,
///   we can still display everything else.
/// Such a thing should only happen if the line contains invalid UTF-8,
///   so we want to be able to help the user track down the invalid byte.
#[test]
fn fallback_when_column_fails_to_resolve() {
    let ctx = Context::from("invalid/utf8");

    let span = ctx.span(4, 2);

    let lossy = String::from_utf8_lossy(FILE_INVALID_UTF8);

    // It's not ideal that the help appears first,
    //   but this should only happen under very exceptional
    //   circumstances so it's not worth trying to resolve.
    // If you're reading this and it's trivial to swap these with the
    //   current state of the system,
    //     go for it.
    assert_report!(
            "column resolution failure",
            vec![span.error("an error we do not want to suppress"),],
            format!("\
error: column resolution failure
  --> invalid/utf8:1 bytes 4--6
   |
   | {lossy}
   |
   = error: an error we do not want to suppress
   = help: unable to calculate columns because the line is not a valid UTF-8 string
   = help: you have been provided with 0-indexed line-relative inclusive byte offsets
")
        );
}

#[test]
fn offset_at_newline_marked() {
    let ctx = Context::from("foo/bar");

    // This represents the newline of line 1.
    let span = ctx.span(14, 1);

    assert_report!(
        "offset at newline",
        vec![span.mark_error()],
        // Note that the trailing newline _should not be rendered_,
        //   which would otherwise result in an extra (un-guttered) line
        //   between the source line and the mark line.
        "\
error: offset at newline
  --> foo/bar:1:15
   |
   | foo/bar line 1
   |               ^
"
    );
}

#[test]
fn zero_length_span_column() {
    let ctx = Context::from("foo/bar");

    // Before the space, after "bar".
    let span = ctx.span(7, 0);

    assert_report!(
        "offset at newline",
        vec![span.mark_error()],
        // Renders _at_ the character it comes before.
        "\
error: offset at newline
  --> foo/bar:1:8
   |
   | foo/bar line 1
   |        ^
"
    );
}

#[test]
fn error_level_mark_styling() {
    let ctx = Context::from("foo/bar");

    assert_report!(
        "multiple level mark styles",
        vec![
            ctx.span(0, 7).internal_error("A"),
            ctx.span(15, 7).error("B"),
            ctx.span(30, 7).note("C"),
            ctx.span(45, 7).help("D"),
        ],
        "\
internal error: multiple level mark styles
  --> foo/bar:1:1
   |
   | foo/bar line 1
   | !!!!!!! internal error: A

  --> foo/bar:2:1
   |
   | foo/bar line 2
   | ^^^^^^^ error: B

  --> foo/bar:3:1
   |
   | foo/bar line 3
   | ------- note: C

  --> foo/bar:4:1
   |
   | foo/bar line 4
   | ------- help: D
"
    );
}
