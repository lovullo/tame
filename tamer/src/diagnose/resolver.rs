// Diagnostic span resolver.
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

//! Resolve [`Span`]s into line:column source locations.

use crate::span::{Context, Span};
use std::{
    io::{self, BufRead},
    num::NonZeroU32,
};

/// Resolves [`Span`]s into line:column source locations.
///
/// A [`Span`] contains a [`Context`] and raw byte offsets gathered during
///   parsing.
/// These byte offsets can be used to go back to read the source file
///   referred to by a [`Context`] to resolve the byte offsets to line and
///   column numbers.
/// This is not done during parsing because this information is only useful
///   in a diagnostic context,
///     which is not the typical happy path of the compiler.
///
/// Another upside to this approach is that we can provide snippets of the
///   source file to annotate with diagnostic information.
/// This is also something that would be very wasteful to collect until we
///   know it is needed.
///
/// A downside to this approach is that,
///   for this system to be able to function,
///   a [`Context`] must reference a location that may be read in a pass
///   separate from parsing.
/// This doesn't necessarily mean that the path has to be seekable,
///   but it does mean that it must be able to be loaded a second time,
///     otherwise we will be unable to provide information beyond the name of
///     the [`Context`] itself
///       (and the offset/length,
///         if that is even useful).
pub trait SpanResolver {
    /// Resolve the provided [`Span`] into line:column source locations.
    ///
    /// See [`SpanResolver`] for more information.
    fn resolve(
        &mut self,
        span: Span,
    ) -> Result<ResolvedSpan, SpanResolverError>;
}

/// A [`Span`] resolved to its source location.
///
/// [`Span`] itself is optimized for size---it
///   is a small value (64 bits) that has no cost to copy or pass around,
///     which is important because a large source program may produce
///     hundreds of millions of them.
/// But the raw data stored by those spans is not useful for human
///   consumption,
///     containing raw byte offsets and lengths.
///
/// This represents the useful information a human would want to know about
///   a [`Span`],
///     having read its [`Context`] to determine the range of lines relevant
///     to the original span.
/// Since this process seldom occurs
///   (perhaps a handful, not millions of times),
///     and is only used in a limited context for a brief period of time
///       (diagnostic report processing),
///     we are not constrained by size.
#[derive(Debug, PartialEq, Eq)]
pub struct ResolvedSpan {
    /// The original [`Span`] whose resolution was requested.
    span: Span,

    /// The lines of source code that correspond to this [`Span`],
    ///   if known.
    ///
    /// It should be the case that the [`Context`] of each [`SourceLine`] of
    ///   this field is equal to the [`Context`] of the `span` field.
    lines: Vec<SourceLine>,

    /// Column offset pair within the first and last [`SourceLine`]s.
    ///
    /// Column begins at `1`,
    ///   so if the [`Span`] begins at the first byte within
    ///   `lines.first()`,
    ///     the first column will have a value of `1`.
    /// The ending column represens the 1-indexed offset relative to
    ///   `lines.last()`.
    ///
    /// If there are no `lines` available,
    ///   then the columns are not known and will be [`None`].
    columns: Option<(NonZeroU32, NonZeroU32)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourceLine {
    /// 1-indexed line number relative to the entire source [`Context`].
    line: NonZeroU32,

    /// The [`Span`] representing the entire source line.
    span: Span,

    /// Source code text of the line _excluding_ the newline.
    ///
    /// This is stored as a byte vector,
    ///   rather than a string,
    ///   so that we can still output source code verbatim even if it is
    ///     invalid UTF-8.
    /// This could also allow for potential future enhancements,
    ///   like outputting binary data as stylized hexadecimal
    ///     (e.g. a future `xmlo` replacement).
    text: Vec<u8>,
}

/// Resolve a [`Span`] using any generic [`BufRead`].
pub struct BufSpanResolver<R: BufRead> {
    reader: R,
    ctx: Context,
}

impl<R: BufRead> BufSpanResolver<R> {
    pub fn new(reader: R, ctx: Context) -> Self {
        Self { reader, ctx }
    }
}

impl<R: BufRead> SpanResolver for BufSpanResolver<R> {
    fn resolve(
        &mut self,
        span: Span,
    ) -> Result<ResolvedSpan, SpanResolverError> {
        if self.ctx != span.ctx() {
            return Err(SpanResolverError::ContextMismatch {
                given: span.ctx(),
                expected: self.ctx,
            });
        }

        // Line length will not often exceed this capacity
        //   (but it's okay if it does).
        fn new_line_buf() -> Vec<u8> {
            Vec::with_capacity(128)
        }

        let mut lines = Vec::new();

        let mut buf = new_line_buf();
        let mut pos = 0;
        let mut line = NonZeroU32::MIN;

        loop {
            let bytes = self.reader.read_until(b'\n', &mut buf)?;
            if bytes == 0 {
                // TODO: Do we care that we may not have found anything for
                //   this span?
                break;
            }

            let new_pos = pos + bytes;

            if new_pos > span.offset() as usize {
                let tail_offset = match buf.last() {
                    Some(b)
                        if (new_pos - 1) != span.offset() as usize
                            && *b == b'\n' =>
                    {
                        buf.pop();
                        1
                    }
                    _ => 0,
                };

                lines.push(SourceLine {
                    line,
                    span: self.ctx.span_or_zz(pos, new_pos - pos - tail_offset),
                    text: buf,
                });

                buf = new_line_buf();

                let end = (span.offset() + span.len() as u32) as usize;

                if new_pos - tail_offset >= end {
                    break;
                }
            }

            buf.clear();
            pos = new_pos;

            // Saturating because I don't think we have to worry about
            //   (legitimate) inputs with billions of lines.
            line = line.saturating_add(1);
        }

        Ok(ResolvedSpan {
            span,
            lines,
            columns: None,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SpanResolverError {
    Io(io::ErrorKind),

    /// The [`Context`] of the provided [`Span`] does not match the
    ///   [`Context`] of the [`SpanResolver`].
    ContextMismatch {
        given: Context,
        expected: Context,
    },
}

impl From<io::Error> for SpanResolverError {
    fn from(e: io::Error) -> Self {
        Self::Io(e.kind())
    }
}

#[cfg(test)]
mod test {
    use std::io;

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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 3.unwrap_into(),
                    span: ctx.span(14, 6),
                    text: "line 3".into(),
                }],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 3.unwrap_into(),
                    span: ctx.span(14, 6),
                    text: "line 3".into(),
                }],
                columns: None,
            }),
            sut.resolve(span),
        );
    }

    #[test]
    fn multiple_lines() {
        let ctx = Context::from("foobar");
        let buf = "line 1\nline start 2\nend line 3";
        //                 |    |-----+- +-|      |
        //                 |    12    |  |22      |
        //                 |----------|  |--------|
        //                 7        18   20      29

        let span = ctx.span(12, 11);

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![
                    SourceLine {
                        line: 2.unwrap_into(),
                        span: ctx.span(7, 12),
                        text: "line start 2".into(),
                    },
                    SourceLine {
                        line: 3.unwrap_into(),
                        span: ctx.span(20, 10),
                        text: "end line 3".into(),
                    },
                ],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 1.unwrap_into(),
                    span: ctx.span(0, 6),
                    text: "line 1".into(),
                },],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    // Trailing newline _is not_ stripped since it was
                    //   explicitly referenced;
                    //     we don't want our line span to not contain the
                    //     requested span.
                    span: ctx.span(7, 7),
                    text: "line 2\n".into(),
                }],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    // Trailing newline _is not_ stripped since it was
                    //   explicitly referenced;
                    //     we don't want our line span to not contain the
                    //     requested span.
                    span: ctx.span(7, 7),
                    text: "line 2\n".into(),
                }],
                columns: None,
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

        let mut sut = BufSpanResolver::new(buf.as_bytes(), ctx);

        assert_eq!(
            Ok(ResolvedSpan {
                span,
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
                columns: None,
            }),
            sut.resolve(span),
        );
    }

    // TODO: Read later span then previous (requires rewinding)
}
