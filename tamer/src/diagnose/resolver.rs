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
    error::Error,
    fmt::Display,
    fs,
    hash::BuildHasher,
    io::{self, BufRead, BufReader, Seek},
    mem::take,
    num::NonZeroU32,
    str::Utf8Error,
};
use unicode_width::UnicodeWidthChar;

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
}

impl ResolvedSpan {
    pub fn line(&self) -> Option<NonZeroU32> {
        self.lines.get(0).map(|line| line.num)
    }

    pub fn col(&self) -> Option<Column> {
        self.lines.get(0).and_then(|line| line.column)
    }

    pub fn first_line_span(&self) -> Option<Span> {
        self.lines.get(0).map(|line| line.span)
    }
}

/// Source column offsets.
///
/// A "column" is somewhat loosely defined as a terminal cell.
/// Certain unicode characters occupy more than one cell,
///   while others occupy none.
/// Consequently,
///   a column can be thought of a "visual [`Span`]",
///     representing what the user would perceive as a column in a fixed
///     with font rather than a byte offset.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Column {
    /// A 1-indexed column number.
    At(NonZeroU32),

    /// A range of 1-indexed columns, inclusive.
    Endpoints(NonZeroU32, NonZeroU32),

    /// Immediately before a column.
    ///
    /// This is conceptually like a bar cursor
    ///   (non-block)
    ///   that places itself between two columns.
    /// It is caused by a zero-length [`Span`].
    Before(NonZeroU32),
}

impl Display for Column {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Coerces to a single column number.
            Self::At(at) | Self::Endpoints(at, _) | Self::Before(at) => {
                Display::fmt(at, f)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourceLine {
    /// 1-indexed line number relative to the entire source [`Context`].
    num: NonZeroU32,

    /// 1-indexed column number(s) relative to the beginning of the line.
    ///
    /// If the line contains invalid UTF-8,
    ///   this may be [`None`].
    column: Option<Column>,

    /// The [`Span`] representing the entire source line.
    span: Span,

    /// Source code text of the line _excluding_ the newline.
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
                let (text, line_span, column) = line.format_for(span);

                lines.push(SourceLine {
                    num: self.line_num,
                    column,
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

        Ok(ResolvedSpan { span, lines })
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

    fn as_str(&self) -> Result<&str, Utf8Error> {
        match self {
            Self::Eof => Ok(&""),
            Self::WithNewline(buf)
            | Self::WithoutNewline(buf)
            | Self::WithEof(buf) => std::str::from_utf8(buf),
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

    /// Line buffer as a UTF-8 slice.
    fn line_as_str(&self) -> Result<&str, Utf8Error> {
        self.bytes
            .as_ref()
            .map(LineBytes::as_str)
            .unwrap_or(Ok(&""))
    }

    /// Produce formatted output for a line containing invalid UTF-8 data.
    ///
    /// This still produces the actual line so that we can help the user
    ///   track down the invalid byte sequences,
    ///     but it is unable to resolve columns.
    ///
    /// This is delegated to by [`Line::format_for`].
    fn format_invalid_utf8_for(
        &mut self,
        span: Span,
    ) -> (Vec<u8>, Span, Option<Column>) {
        let bytes = self.take_buf().unwrap_or(vec![]);
        let span = span.ctx().span_or_zz(0, bytes.len());

        (bytes, span, None)
    }

    /// Format the line buffer and provide an associated line [`Span`]
    ///   that provides a source context suitable for the provided span.
    ///
    /// Roughly,
    ///   this means that any trailing newline will be stripped unless it is
    ///   directly referenced by the `span` offset
    ///     (starts _at_ the newline).
    fn format_for(&mut self, span: Span) -> (Vec<u8>, Span, Option<Column>) {
        // Trim any newline unless the span starts at the last byte,
        //   which would reference the newline character itself.
        if span.offset() as usize != self.total_offset_read() {
            self.bytes = take(&mut self.bytes).map(LineBytes::trim_nl);
        }

        let line = match self.line_as_str() {
            Ok(s) => s,
            Err(_) => return self.format_invalid_utf8_for(span),
        };

        let column = self.resolve_columns(line, span);

        let offset_start = self.offset_start;
        let buf = self.take_buf().unwrap_or(vec![]);
        let line_span = span.ctx().span_or_zz(offset_start, buf.len());

        (buf, line_span, Some(column))
    }

    /// Determine the [`Span`] endpoint offsets relative to the line start.
    fn relative_byte_offsets(&self, span: Span) -> (usize, usize) {
        let span_offset_end =
            span.offset() as usize + span.len().max(1) as usize - 1;

        (
            (span.offset() as usize).saturating_sub(self.offset_start),
            span_offset_end.saturating_sub(self.offset_start),
        )
    }

    /// Determine the 1-indexed column number for each [`Span`] endpoint,
    ///   relative to the start of the line.
    ///
    /// For multi-line spans,
    ///   the column endpoints for the first line will continue to the end
    ///   of the line,
    ///     columns for the middle lines will encompass the entire line,
    ///     and the last line will begin at column 1.
    /// That is:
    ///
    /// ```text
    ///    span start
    ///    v
    /// line 1
    /// line 2
    /// line 4
    ///    ^ span end
    ///
    /// # Will have its columns reported as:
    /// line 1
    ///    |-|  [4,6]
    /// line 2
    /// |----|  [1,6]
    /// line 4
    /// |^^|    [1,4]
    /// ```
    fn resolve_columns(&self, line: &str, span: Span) -> Column {
        // The max(1) here is intended to accommodate zero-length spans.
        let span_offset_end =
            span.offset() as usize + span.len().max(1) as usize - 1;

        // We should stop calculating widths after this offset,
        //   which is EOL or the span ending offset,
        //   whichever comes first.
        let max_offset = self.total_offset_read().min(span_offset_end);

        // This will produce `(index, width)` pairs for the line until we
        //   reach `max_offset` above.
        let widths = line.char_indices().map_while(|(i, c)| {
            (i <= max_offset).then(|| (i, c.width().unwrap_or(0)))
        });

        let (rel_start, rel_end) = self.relative_byte_offsets(span);

        // Count columns according to character widths in a single pass over
        //   the line.
        //
        // Note that this is summing the two column values _independently_;
        //   this is not the most efficient way to proceed,
        //     but it is good enough for our uses without starting to get
        //     creative,
        //       for which there are a number of possible approaches.
        // Once we start processing spans in bulk
        //   (e.g. using the diagnostic system to produce information for
        //     every identifier in a file),
        //   additional optimizations will be needed anyway,
        //     so there's no use in doing something more complicated until
        //     we know specifically what use cases we'll be optimizing for.
        let (start, end) = widths.fold((1, 0), |(start, end), (i, width)| {
            (
                (i < rel_start).then(|| start + width).unwrap_or(start),
                (i <= rel_end).then(|| end + width).unwrap_or(end),
            )
        });

        // If the system is operating correctly,
        //   both column endpoints should be non-zero.
        // With that said,
        //   we never want the diagnostic system to panic,
        //   so play it safe anyway just in case.
        //
        // When we start processing spans in bulk we may wish to tighten our
        //   guarantees to eliminate these checks.
        let (col_start, col_end) = (
            NonZeroU32::new(start.try_into().unwrap_or(0))
                .unwrap_or(NonZeroU32::MIN),
            NonZeroU32::new(end.try_into().unwrap_or(0))
                .unwrap_or(NonZeroU32::MIN),
        );

        // Start will only be > end (by 1) if the span begins on a newline.
        if span.len() == 0 {
            Column::Before(col_start)
        } else if col_start >= col_end {
            Column::At(col_start)
        } else {
            Column::Endpoints(col_start, col_end)
        }
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

impl Display for SpanResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(kind) => Display::fmt(kind, f),
            Self::ContextMismatch { given, expected } => write!(
                f,
                "attempted to read context {given} \
                   using a resolver for context {expected}"
            ),
        }
    }
}

impl Error for SpanResolverError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        4.unwrap_into()
                    )),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
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
                    num: 3.unwrap_into(),
                    column: Some(Column::At(6.unwrap_into(),)),
                    span: ctx.span(14, 6),
                    text: "line 3".into(),
                }],
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
                    num: 3.unwrap_into(),
                    column: Some(Column::Endpoints(
                        3.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: ctx.span(14, 6),
                    text: "line 3".into(),
                }],
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
                lines: vec![
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
                ],
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
                lines: vec![
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
                ],
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
                    num: 1.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: ctx.span(0, 6),
                    text: "line 1".into(),
                },],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::At(7.unwrap_into())),
                    // Trailing newline _is not_ stripped since it was
                    //   explicitly referenced;
                    //     we don't want our line span to not contain the
                    //     requested span.
                    span: ctx.span(7, 7),
                    text: "line 2\n".into(),
                }],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Before(4.unwrap_into())),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Before(7.unwrap_into())),
                    // Trailing newline _is not_ stripped since it was
                    //   explicitly referenced;
                    //     we don't want our line span to not contain the
                    //     requested span.
                    span: ctx.span(7, 7),
                    text: "line 2\n".into(),
                }],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Before(1.unwrap_into())),
                    span: ctx.span(7, 6),
                    text: "line 2".into(),
                }],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: span_a,
                    text: "line 2".into(),
                }],
            }),
            sut.resolve(span_a),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_b,
                lines: vec![SourceLine {
                    num: 3.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: span_b,
                    text: "line 3".into(),
                }],
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
                        num: 2.unwrap_into(),
                        column: Some(Column::Endpoints(
                            1.unwrap_into(),
                            6.unwrap_into()
                        )),
                        span: span,
                        text: "line 2".into(),
                    }],
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
                    num: 2.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: span_later,
                    text: "line 2".into(),
                }],
            }),
            sut.resolve(span_later),
        );

        // Then a span that comes before it,
        //   which requires rewinding.
        assert_eq!(
            Ok(ResolvedSpan {
                span: span_earlier,
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        6.unwrap_into()
                    )),
                    span: span_earlier,
                    text: "line 1".into(),
                }],
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
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    column: None,
                    span: ctx.span(0, 6),
                    text: {
                        // Make sure we're still trimming despite the
                        //   error.
                        buf.pop();
                        buf.into()
                    },
                }],
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
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        2.unwrap_into()
                    )),
                    span: span_0,
                    text: "0:\0".into(),
                }],
            }),
            sut.resolve(span_0),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_1,
                lines: vec![SourceLine {
                    num: 2.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        3.unwrap_into()
                    )),
                    span: span_1,
                    text: "1:“".into(),
                }],
            }),
            sut.resolve(span_1),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_2,
                lines: vec![SourceLine {
                    num: 3.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        4.unwrap_into()
                    )),
                    span: span_2,
                    text: "2:😊".into(),
                }],
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
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    column: Some(Column::Endpoints(
                        1.unwrap_into(),
                        3.unwrap_into()
                    )),
                    span: line_span,
                    text: buf.clone().into(),
                }],
            }),
            sut.resolve(span_end_bad),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_start_bad,
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    // Intuitively this really should be [2,4],
                    //   but the implementation shouldn't change to
                    //   accommodate this very unlikely case.
                    column: Some(Column::At(4.unwrap_into(),)),
                    span: line_span,
                    text: buf.clone().into(),
                }],
            }),
            sut.resolve(span_start_bad),
        );

        assert_eq!(
            Ok(ResolvedSpan {
                span: span_all_bad,
                lines: vec![SourceLine {
                    num: 1.unwrap_into(),
                    // Also unideal,
                    //   but see comment for previous assertion.
                    column: Some(Column::At(4.unwrap_into(),)),
                    span: line_span,
                    text: buf.clone().into(),
                }],
            }),
            sut.resolve(span_all_bad),
        );
    }
}
