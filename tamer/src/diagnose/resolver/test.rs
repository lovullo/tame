// Diagnostic span resolver tests
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

use std::io::{self, Cursor};

use crate::convert::ExpectInto;

use super::*;

#[test]
fn rejects_context_mismatch() {
    let ctx_a = Context::from("ctx_a");
    let ctx_b = Context::from("ctx_b");

    let mut sut = BufSpanResolver::new(io::empty(), ctx_a);

    assert_eq!(
        Err(SpanResolverError::ContextMismatch {
            given: ctx_b,
            expected: ctx_a
        }),
        sut.resolve(ctx_b.span(0, 0)),
    );
}

#[test]
fn rejects_span_with_endpoint_past_eof() {
    let ctx = Context::from("pasteof");
    let buf = "01234";

    // Intentionally the byte _directly_ after EOF.
    let span_high_offset = ctx.span(5, 0);
    let span_high_len = ctx.span(3, 5);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Err(SpanResolverError::OutOfRange(4)),
        sut.resolve(span_high_offset),
    );

    assert_eq!(
        Err(SpanResolverError::OutOfRange(4)),
        sut.resolve(span_high_len),
    );

    // Sanity check just to verify that we don't have an off-by-1 error
    assert!(sut.resolve(ctx.span(0, 5)).is_ok());
}

// Span starts on the first byte of the line.
//
// In particular,
//   we want to ensure that the reader doesn't have off-by-one errors
//   related to the terminating newline that may cause it to pick up a
//   line too early.
#[test]
fn first_byte_of_line() {
    let ctx = Context::from("foo");
    let buf = "line 1\nline 2\nline 3\nline 4";
    //                 |--| |
    //                 7 10 |
    //                 |----|
    //                     12

    let span = ctx.span(7, 4);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    4.unwrap_into()
                )),
                span: ctx.span(7, 6),
                text: "line 2".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

#[test]
fn last_byte_of_line() {
    let ctx = Context::from("foo");
    let buf = "line 1\nline 2\nline 3\nline 4";
    //                         |    |
    //                         |   19
    //                         |----|
    //                         14

    let span = ctx.span(19, 1);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 3.unwrap_into(),
                column: Some(Column::At(6.unwrap_into(),)),
                span: ctx.span(14, 6),
                text: "line 3".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// Should be same concept as above,
//   but the difference this time is that we have no trailing newline
//   and hit EOF first.
#[test]
fn last_byte_of_file_no_trailing_nl() {
    let ctx = Context::from("foo");
    let buf = "line 1\nline 2\nline 3";
    //                         | |--|
    //                         | 16 19
    //                         |----|
    //                         14

    let span = ctx.span(16, 4);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 3.unwrap_into(),
                column: Some(Column::Endpoints(
                    3.unwrap_into(),
                    6.unwrap_into()
                )),
                span: ctx.span(14, 6),
                text: "line 3".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// A first and last line.
#[test]
fn multiple_lines_first_last() {
    let ctx = Context::from("foobar");
    let buf = "line 1\nline start 2\nend line 3";
    //                 |    |-----+- +-|      |
    //                 |    12    |  |22      |
    //                 |----------|  |--------|
    //                 7        18   20      29

    let span = ctx.span(12, 11);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![
                SourceLine {
                    num: 2.unwrap_into(),
                    // From the point, to the end of the line.
                    column: Some(Column::Endpoints(
                        6.unwrap_into(),
                        12.unwrap_into()
                    )),
                    span: ctx.span(7, 12),
                    text: "line start 2".into(),
                },
                SourceLine {
                    num: 3.unwrap_into(),
                    // From the beginning of the line, to the point.
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        3.unwrap_into()
                    )),
                    span: ctx.span(20, 10),
                    text: "end line 3".into(),
                },
            ])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// If there are more than two lines,
//   middle lines' column ranges span the entire line.
#[test]
fn multiple_lines_middle_line_endpoints() {
    let ctx = Context::from("foobar");
    let buf = "line start 1\nline 2\nend line 3";
    //         |    |-----+- +----+- +-|      |
    //         |    5     |  |    |  |22      |
    //         |----------|  |----|  |--------|
    //         0         11  13  18  20      29

    let span = ctx.span(5, 18);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![
                SourceLine {
                    num: 1.unwrap_into(),
                    // From the point, to the end of the line.
                    column: Some(Column::Endpoints(
                        6.unwrap_into(),
                        12.unwrap_into()
                    )),
                    span: ctx.span(0, 12),
                    text: "line start 1".into(),
                },
                SourceLine {
                    num: 2.unwrap_into(),
                    // Entire line.
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: ctx.span(13, 6),
                    text: "line 2".into(),
                },
                SourceLine {
                    num: 3.unwrap_into(),
                    // From the beginning of the line, to the point.
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        3.unwrap_into()
                    )),
                    span: ctx.span(20, 10),
                    text: "end line 3".into(),
                },
            ])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// This should work fine based on the above,
//   but just in case,
//   since it does begin before any newline is encountered and we want
//     to make sure the implementation isn't doing something silly that
//     requires that it saw at least one newline.
#[test]
fn first_line() {
    let ctx = Context::from("foobar");
    let buf = "line 1\n";
    //         |----|
    //         0    5

    let span = ctx.span(0, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    6.unwrap_into()
                )),
                span: ctx.span(0, 6),
                text: "line 1".into(),
            },])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// If a span appears between two lines
//   (that is, the byte is the newline),
//   then we just have to do something reasonable.
// Taking the preceding line so that we can visually underline the
//   newline at the end of the line would match developers' intuition of
//   what a line is.
#[test]
fn newline_between_lines() {
    let ctx = Context::from("foo");
    let buf = "line 1\nline 2\nline 3";
    //                 |    ||
    //                 |    |13
    //                 |----|
    //                 7   12

    let span = ctx.span(13, 1);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::At(7.unwrap_into())),
                // Trailing newline _is not_ stripped since it was
                //   explicitly referenced;
                //     we don't want our line span to not contain the
                //     requested span.
                span: ctx.span(7, 7),
                text: "line 2\n".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// Zero-length spans have offsets but no length.
// They act like a cursor between two characters in a text editor
//   (well, I use a block cursor, but nobody was asking).
#[test]
fn zero_length_span() {
    let ctx = Context::from("foo");
    let buf = "line 1\nline 2\nline 3";
    //                 |  | |
    //                 | 10 |
    //                 |----|
    //                 7   12

    let span = ctx.span(10, 0);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Before(4.unwrap_into())),
                span: ctx.span(7, 6),
                text: "line 2".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// A zero-length span at the end of a line,
//   _before_ the terminating newline,
//   belongs to the line before it,
//     as if your cursor were at the end of the line,
//       ready to continue typing on that line.
#[test]
fn zero_length_span_at_eol() {
    let ctx = Context::from("zeol");
    let buf = "line 1\nline 2\nline 3";
    //                 |    ||
    //                 |    |13
    //                 |----|
    //                 7   12

    let span = ctx.span(13, 0);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Before(7.unwrap_into())),
                // Trailing newline _is not_ stripped since it was
                //   explicitly referenced;
                //     we don't want our line span to not contain the
                //     requested span.
                span: ctx.span(7, 7),
                text: "line 2\n".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// A zero-length span at the _beginning_ of a line,
//   _after_ the terminating newline,
//   belongs to the line _after_ it,
//     as if your cursor were at the beginning of the line,
//       ready to type ahead of what's already there.
#[test]
fn zero_length_span_at_bol() {
    let ctx = Context::from("zeol");
    let buf = "line 1\nline 2\nline 3";
    //                 |    |
    //                 7    |
    //                 |----|
    //                     12

    let span = ctx.span(7, 0);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Before(1.unwrap_into())),
                span: ctx.span(7, 6),
                text: "line 2".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// Re-using the reader to resolve multiple spans requires that it
//   persist its state between calls.
#[test]
fn resolve_multiple_spans() {
    let ctx = Context::from("multi");
    let buf = "line 1\nline 2\nline 3";
    //                 |----|  |----|
    //                 7   12  14  19
    //                   A       B

    let span_a = ctx.span(7, 6);
    let span_b = ctx.span(14, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_a,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    6.unwrap_into()
                )),
                span: span_a,
                text: "line 2".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_a),
    );

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_b,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 3.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    6.unwrap_into()
                )),
                span: span_b,
                text: "line 3".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_b),
    );
}

#[test]
fn resolve_same_span_multiple_times() {
    let ctx = Context::from("multi");
    let buf = "line 1\nline 2\nline 3";
    //                 |----|
    //                 7   12
    //                   A

    let span = ctx.span(7, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    (1..=2).for_each(|_| {
        assert_eq!(
            Ok(ResolvedSpan {
                span: span,
                lines: NonEmptyVec::new(vec![SourceLine {
                    num: 2.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: span,
                    text: "line 2".into(),
                }])
                .unwrap(),
            }),
            sut.resolve(span),
        );
    });
}

#[test]
fn resolve_earlier_span_after_later() {
    let ctx = Context::from("multi");
    let buf = "line 1\nline 2\nline 3";
    //         |----|  |----|
    //         0    5  7   12
    //        earlier   later

    let span_later = ctx.span(7, 6);
    let span_earlier = ctx.span(0, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    // First, the later span.
    assert_eq!(
        Ok(ResolvedSpan {
            span: span_later,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    6.unwrap_into()
                )),
                span: span_later,
                text: "line 2".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_later),
    );

    // Then a span that comes before it,
    //   which requires rewinding.
    assert_eq!(
        Ok(ResolvedSpan {
            span: span_earlier,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    6.unwrap_into()
                )),
                span: span_earlier,
                text: "line 1".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_earlier),
    );
}

// We cannot properly determine the column if a line contains invalid
//   unicode,
//     because we cannot confidently determine how the line ought to be
//     displayed to the user
//       (that's up to their terminal).
//
// But we should display what we can,
//   which means still producing the line itself,
//   so that we can help the user track down the bad byte sequence that
//     was almost certainly unintentional and may have even come from
//     pasting text from another document.
#[test]
fn invalid_unicode_no_column() {
    let ctx = Context::from("invalid-unicode");

    let mut buf = b"bad \xC0!\n".to_vec();
    //              |----   |
    //              0       5

    let span = ctx.span(0, 4);

    let mut sut = BufSpanResolver::new(Cursor::new(buf.clone()), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                column: None,
                span: ctx.span(0, 6),
                text: {
                    // Make sure we're still trimming despite the
                    //   error.
                    buf.pop();
                    buf.into()
                },
            }])
            .unwrap(),
        }),
        sut.resolve(span),
    );
}

// Account for the width of unicode characters with a fixed-width font,
//   in a manner similar to POSIX `wcwidth(3)`.
// TAMER uses the `unicode-width` crate,
//   which is the same crate used by Rustc.
#[test]
fn unicode_width() {
    let ctx = Context::from("unicode-width");

    let buf = "0:\0\n1:“\n2:😊";
    //         |-|   |-|  |--|
    // bytes:  0 2   4 8  10 15
    //   col:  1 2   1 3  1  4

    // Remember: spans are _byte_-oriented.
    let span_0 = ctx.span(0, 3);
    let span_1 = ctx.span(4, 5);
    let span_2 = ctx.span(10, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_0,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    2.unwrap_into()
                )),
                span: span_0,
                text: "0:\0".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_0),
    );

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_1,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 2.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    3.unwrap_into()
                )),
                span: span_1,
                text: "1:“".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_1),
    );

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_2,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 3.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    4.unwrap_into()
                )),
                span: span_2,
                text: "2:😊".into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_2),
    );
}

// If a span somehow points to a byte that does not represent a valid
//   UTF-8 character boundary,
//     then we still want to produce sensible output.
//
// The behavior here is a consequence of implementation details.
// This test merely acknowledges the behavior to show that it has been
//   considered,
//     and to bring attention to the issue if the implementation details
//     cause a change in behavior.
// At this time,
//   there's no compelling reason to complicate the implementation to
//   add additional checks that would produce more intuitive column
//   values for these cases that are very unlikely to occur.
#[test]
fn at_invalid_char_boundary() {
    let ctx = Context::from("unicode-width");

    // Charcater is 4 bytes.
    let buf = "(😊)";
    //         |--|
    // bytes:  0  5
    //   col:  1  4

    // Ends at the first byte of the multibyte char.
    let span_end_bad = ctx.span(0, 2);
    // Starts at byte 2 of 4 for the multibyte char.
    let span_start_bad = ctx.span(3, 2);
    // _Both_ starts _and_ ends in the middle of the char.
    let span_all_bad = ctx.span(2, 1);

    let line_span = ctx.span(0, 6);

    let mut sut = BufSpanResolver::new(Cursor::new(buf.clone()), ctx);

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_end_bad,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                column: Some(Column::Endpoints(
                    1.unwrap_into(),
                    3.unwrap_into()
                )),
                span: line_span,
                text: buf.clone().into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_end_bad),
    );

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_start_bad,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                // Intuitively this really should be [2,4],
                //   but the implementation shouldn't change to
                //   accommodate this very unlikely case.
                column: Some(Column::At(4.unwrap_into(),)),
                span: line_span,
                text: buf.clone().into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_start_bad),
    );

    assert_eq!(
        Ok(ResolvedSpan {
            span: span_all_bad,
            lines: NonEmptyVec::new(vec![SourceLine {
                num: 1.unwrap_into(),
                // Also unideal,
                //   but see comment for previous assertion.
                column: Some(Column::At(4.unwrap_into(),)),
                span: line_span,
                text: buf.clone().into(),
            }])
            .unwrap(),
        }),
        sut.resolve(span_all_bad),
    );
}
