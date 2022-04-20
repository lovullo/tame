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
    collections::HashMap,
    fs,
    hash::BuildHasher,
    io::{self, BufRead, BufReader, Seek},
    mem::take,
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
    pub span: Span,

    /// The lines of source code that correspond to this [`Span`],
    ///   if known.
    ///
    /// It should be the case that the [`Context`] of each [`SourceLine`] of
    ///   this field is equal to the [`Context`] of the `span` field.
    pub lines: Vec<SourceLine>,

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
    pub columns: Option<(NonZeroU32, NonZeroU32)>,
}

impl ResolvedSpan {
    pub fn line(&self) -> Option<NonZeroU32> {
        self.lines.get(0).map(|line| line.line)
    }
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
pub struct BufSpanResolver<R: BufRead + Seek> {
    reader: R,
    ctx: Context,
    line_num: NonZeroU32,
}

impl<R: BufRead + Seek> BufSpanResolver<R> {
    pub fn new(reader: R, ctx: Context) -> Self {
        Self {
            reader,
            ctx,
            line_num: NonZeroU32::MIN,
        }
    }

    /// Rewind the buffer for the provided [`Span`],
    ///   if necessary.
    ///
    /// If `span` precedes the current buffer position,
    ///   the buffer will be rewound and the line count reset.
    /// This is heavy-handed,
    ///   but avoids maintaining a line cache that may never be needed.
    /// This may be worth optimizing in the future depending on how TAMER's
    ///   diagnostic and query facilities evolve,
    ///     but it's not worth it at the time of writing.
    fn rewind_for_span(&mut self, span: Span) -> Result<(), SpanResolverError> {
        if span.offset() as usize <= self.reader.stream_position()? as usize {
            self.reader.rewind()?;
            self.line_num = NonZeroU32::MIN;
        }

        Ok(())
    }

    /// Attempt to read another [`Line`],
    ///   stopping after the provided [`Span`] has been fully read.
    ///
    /// A line is delimited by `b'\n'`.
    /// If the seek position in the underlying reader is beyond the end of
    ///   the span,
    ///     [`None`] will be returned early instead of reading until EOF.
    ///
    /// This method _does not rewind_;
    ///   see [`BufSpanResolver::rewind_for_span`].
    fn read_next_line(
        &mut self,
        mut buf: Vec<u8>,
        span: Span,
    ) -> Result<Option<Line>, SpanResolverError> {
        let offset_end = span.offset() as usize + span.len().max(1) as usize;
        let prev_bytes_read = self.reader.stream_position()? as usize;

        if prev_bytes_read >= offset_end {
            return Ok(None);
        }

        // Clear any previous line.
        buf.clear();

        match self.reader.read_until(b'\n', &mut buf)? {
            0 => Ok(None),
            _ => Ok(Some(Line {
                bytes: Some(buf.into()),
                offset_start: prev_bytes_read,
            })),
        }
    }
}

impl<R: BufRead + Seek> SpanResolver for BufSpanResolver<R> {
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

        /// New buffer expected to be able to hold the majority of lines
        ///   that are read without having to expand capacity.
        fn new_line_buf() -> Vec<u8> {
            Vec::with_capacity(128)
        }

        let mut lines = Vec::<SourceLine>::new();
        let mut buf = new_line_buf();

        self.rewind_for_span(span)?;

        while let Some(mut line) = self.read_next_line(buf, span)? {
            if line.at_or_beyond(span) {
                let (text, line_span) = line.format_for(span);

                lines.push(SourceLine {
                    line: self.line_num,
                    text,
                    span: line_span,
                });
            }

            buf = line.take_buf().unwrap_or_else(new_line_buf);

            // Saturating add will handle billions of lines,
            //   which is not expected to happen,
            //   but avoids a panic at the cost of inaccurate information in
            //     the unlikely event that it does
            //       (bad input).
            self.line_num = self.line_num.saturating_add(1);
        }

        Ok(ResolvedSpan {
            span,
            lines,
            columns: None,
        })
    }
}

/// Raw bytes associated with a source line.
///
/// This simply forces the acknowledgement of how the last byte of the line
///   is handled.
enum LineBytes {
    /// Line ends with `b'\n'`.
    WithNewline(Vec<u8>),

    /// Line has been trimmed and no longer ends with `b'n'`.
    WithoutNewline(Vec<u8>),

    /// Line ended at EOF,
    ///   and so does not end with `b'\n'`.
    WithEof(Vec<u8>),

    /// EOF reached (no line).
    Eof,
}

impl LineBytes {
    /// The total number of bytes read for this line,
    ///   regardless of the number of bytes that will be returned to the
    ///   caller.
    ///
    /// For example,
    ///   a line that has been stripped of its trailing newline will return
    ///   the length of the line as if the newline were still present.
    fn read_len(&self) -> usize {
        match self {
            Self::WithNewline(bytes) | Self::WithEof(bytes) => bytes.len(),
            Self::WithoutNewline(bytes) => bytes.len() + 1,
            Self::Eof => 0,
        }
    }

    /// Trim a trailing newline,
    ///   if the last byte on the line is `b'\n'` and it has not been trimmed
    ///   already.
    fn trim_nl(self) -> Self {
        match self {
            Self::WithNewline(mut bytes) if bytes.last() == Some(&b'\n') => {
                bytes.pop();
                Self::WithoutNewline(bytes)
            }
            _ => self,
        }
    }

    /// Yield the inner line buffer,
    ///   if available.
    fn into_buf(self) -> Option<Vec<u8>> {
        match self {
            Self::Eof => None,
            Self::WithNewline(buf)
            | Self::WithoutNewline(buf)
            | Self::WithEof(buf) => Some(buf),
        }
    }
}

impl From<Vec<u8>> for LineBytes {
    fn from(bytes: Vec<u8>) -> Self {
        match bytes.last() {
            // TODO: Do we care that we may not have found anything for
            //   this span?
            None => LineBytes::Eof,

            Some(&b'\n') => LineBytes::WithNewline(bytes),
            Some(_) => LineBytes::WithEof(bytes),
        }
    }
}

/// A source line's bytes and its starting offset relative to the beginning
///   of the underlying buffer
///     (its seek position).
///
/// The various interpretations of positions and offsets can be confusing.
/// It can be visualized as such:
///
/// ```text
///              "foo\nbar\nbaz"
///     offset:   0123 4567 8
/// bytes_read:   1234 5678 9
///                  /  \
///      `bytes` ends    next line starts
/// and too bytes_read     (offset is previous `bytes_read`)
/// ```
struct Line {
    bytes: Option<LineBytes>,
    offset_start: usize,
}

impl Line {
    /// Number of bytes read relative to the start of the source buffer
    ///   (e.g. file).
    fn total_bytes_read(&self) -> usize {
        self.offset_start
            + self.bytes.as_ref().map(LineBytes::read_len).unwrap_or(0)
    }

    /// Maximum offset read.
    ///
    /// The offset is the number of bytes read minus one
    ///   (see visualization on [`Line`] doc).
    /// Technically,
    ///   if the read position is 0,
    ///   then there is no offset since we have not yet reached a byte,
    ///     but this method will still return `0` in that case.
    fn total_offset_read(&self) -> usize {
        self.total_bytes_read().saturating_sub(1)
    }

    /// Whether the line contains bytes referenced by the provided [`Span`],
    ///   or exceeds the span's offset.
    ///
    /// This does _not_ answer whether the line is within range of the
    ///   span---once
    ///     it becomes true,
    ///       it will remain true from that position onward.
    fn at_or_beyond(&self, span: Span) -> bool {
        self.total_offset_read() >= span.offset() as usize
    }

    /// Format the line buffer and provide an associated line [`Span`]
    ///   that provides a source context suitable for the provided span.
    ///
    /// Roughly,
    ///   this means that any trailing newline will be stripped unless it is
    ///   directly referenced by the `span` offset
    ///     (starts _at_ the newline).
    fn format_for(&mut self, span: Span) -> (Vec<u8>, Span) {
        // Trim the newline (if any) unless the span starts at the last
        //   byte,
        //     which would reference the newline character itself.
        if span.offset() as usize != self.total_offset_read() {
            self.bytes = take(&mut self.bytes).map(LineBytes::trim_nl);
        }

        let offset_start = self.offset_start;
        let buf = self.take_buf().unwrap_or(vec![]);
        let span = span.ctx().span_or_zz(offset_start, buf.len());

        (buf, span)
    }

    /// Take ownership of the line buffer.
    ///
    /// This exists to reuse the buffer for another [`Line`].
    fn take_buf(&mut self) -> Option<Vec<u8>> {
        take(&mut self.bytes).and_then(LineBytes::into_buf)
    }
}

/// Resolve spans by reading [`Context`]s from a filesystem.
///
/// This uses [`BufSpanResolver`].
pub struct FsSpanResolver;

impl SpanResolver for FsSpanResolver {
    fn resolve(
        &mut self,
        span: Span,
    ) -> Result<ResolvedSpan, SpanResolverError> {
        let file = fs::File::open(span.ctx())?;

        let mut resolver =
            BufSpanResolver::new(BufReader::new(file), span.ctx());

        // After this,
        //   the file is dropped and the descriptor closed,
        //   which is good in the sense that we don't want a lot of files open,
        //     but may be inefficient if there are a lot of reports for a
        //     single file.
        // If you're reading this message because of performance issues,
        //   then now's the time to do something better,
        //     like maintain a short-lived cache that limits open file
        //     descriptors
        //       (ring buffer, maybe?).
        resolver.resolve(span)
    }
}

impl<R, S> SpanResolver for HashMap<Context, R, S>
where
    R: SpanResolver,
    S: BuildHasher,
{
    fn resolve(
        &mut self,
        span: Span,
    ) -> Result<ResolvedSpan, SpanResolverError> {
        self.get_mut(&span.ctx())
            .ok_or(SpanResolverError::Io(io::ErrorKind::NotFound))
            .and_then(|resolver| resolver.resolve(span))
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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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

        let mut sut = BufSpanResolver::new(Cursor::new(buf), ctx);

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
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    span: span_a,
                    text: "line 2".into(),
                }],
                columns: None,
            }),
            sut.resolve(span_a),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_b,
                lines: vec![SourceLine {
                    line: 3.unwrap_into(),
                    span: span_b,
                    text: "line 3".into(),
                }],
                columns: None,
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
                    lines: vec![SourceLine {
                        line: 2.unwrap_into(),
                        span: span,
                        text: "line 2".into(),
                    }],
                    columns: None,
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
                lines: vec![SourceLine {
                    line: 2.unwrap_into(),
                    span: span_later,
                    text: "line 2".into(),
                }],
                columns: None,
            }),
            sut.resolve(span_later),
        );

        // Then a span that comes before it,
        //   which requires rewinding.
        assert_eq!(
            Ok(ResolvedSpan {
                span: span_earlier,
                lines: vec![SourceLine {
                    line: 1.unwrap_into(),
                    span: span_earlier,
                    text: "line 1".into(),
                }],
                columns: None,
            }),
            sut.resolve(span_earlier),
        );
    }
}
