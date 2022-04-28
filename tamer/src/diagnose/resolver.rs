// Diagnostic span resolver
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

/// Wrapper around a non-empty [`Vec`].
///
/// This is for correctness,
///   not performance.
/// This type is half-assed and is only what we need for this module.
#[derive(Debug, PartialEq, Eq)]
struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    fn new(from: Vec<T>) -> Option<Self> {
        if from.is_empty() {
            return None;
        }

        Some(NonEmptyVec(from))
    }

    /// Returns the first element of the [`Vec`].
    fn first(&self) -> &T {
        self.0.first().unwrap()
    }
}

impl<T> AsRef<Vec<T>> for NonEmptyVec<T> {
    fn as_ref(&self) -> &Vec<T> {
        &self.0
    }
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
    ///
    /// _This vector will always have at least one line._
    lines: NonEmptyVec<SourceLine>,
}

/// Data interpreted from a [`ResolvedSpan`] or equivalent.
pub trait ResolvedSpanData {
    /// Line number representing the offset of the [`Span`].
    ///
    /// This is intended to answer the question of "what line?",
    ///   to which one would typically reply with the line that a particular
    ///   issue starts on.
    ///
    /// More concretely,
    ///   this is the line number expected to appear in a report heading
    ///   alongside the context,
    ///     or in an error summary
    ///     (e.g. "path/to/file:1:2").
    fn line_num(&self) -> NonZeroU32;

    /// Column number(s) relative to the beginning of the first line
    ///   representing the offset of the [`Span`].
    ///
    /// The column may not be able to be resolved if the line contains
    ///   invalid UTF-8 data.
    fn col_num(&self) -> Option<Column>;

    /// A [`Span`] representing the first line.
    ///
    /// Note that many spans have only one line associated with them.
    fn first_line_span(&self) -> Span;

    /// [`Context`] of the [`Span`] used for resolution.
    fn context(&self) -> Context;

    /// The original [`Span`] before resolution.
    fn unresolved_span(&self) -> Span;

    /// Consume self and yield owned inner [`SourceLine`]s.
    fn into_lines(self) -> Vec<SourceLine>;
}

impl ResolvedSpanData for ResolvedSpan {
    fn line_num(&self) -> NonZeroU32 {
        self.lines.first().num
    }

    fn col_num(&self) -> Option<Column> {
        self.lines.first().column
    }

    fn first_line_span(&self) -> Span {
        self.lines.first().span
    }

    fn context(&self) -> Context {
        self.span.context()
    }

    fn unresolved_span(&self) -> Span {
        self.span
    }

    fn into_lines(self) -> Vec<SourceLine> {
        self.lines.0
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
            Self::Endpoints(at, _) | Self::Before(at) => Display::fmt(at, f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl SourceLine {
    pub fn column(&self) -> Option<Column> {
        self.column
    }

    #[cfg(test)]
    pub fn new_stub(
        num: NonZeroU32,
        column: Option<Column>,
        span: Span,
        text: Vec<u8>,
    ) -> Self {
        Self {
            num,
            column,
            span,
            text,
        }
    }
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
        if self.ctx != span.context() {
            return Err(SpanResolverError::ContextMismatch {
                given: span.context(),
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

        // The empty check should be redundant,
        //   but is meant to guarantee that we actually do have lines.
        let pos = self.reader.stream_position()? as usize;
        let nelines = (pos >= span.endpoints_saturated().1.offset() as usize)
            .then(|| NonEmptyVec::new(lines))
            .flatten()
            .ok_or(SpanResolverError::OutOfRange(pos - 1))?;

        Ok(ResolvedSpan {
            span,
            lines: nelines,
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
        let span = span.context().span_or_zz(0, bytes.len());

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
        let line_span = span.context().span_or_zz(offset_start, buf.len());

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
        } else {
            Column::Endpoints(col_start, col_end.max(col_start))
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
        let file = fs::File::open(span.context())?;

        let mut resolver =
            BufSpanResolver::new(BufReader::new(file), span.context());

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
        self.get_mut(&span.context())
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

    /// The [`Span`] was not in range of the provided buffer.
    ///
    /// This means that the span's offset+len ≥ EOF.
    /// The provided size is the maximum offset of the buffer
    ///   (seek position - 1).
    OutOfRange(usize),
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
            Self::OutOfRange(eof_pos) => {
                write!(f, "span exceeds context size of {eof_pos} bytes")
            }
        }
    }
}

impl Error for SpanResolverError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod test;
