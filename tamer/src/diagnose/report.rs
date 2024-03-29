// Diagnostic system rendering
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

//! Rendering of diagnostic report.
//!
//! This module is responsible for [resolving](super::resolve) and
//!   rendering spans into a formatted [`Report`],
//!     which in turn can be rendered into a string with [`Display::fmt`].
//!
//! See the [parent module](super) for more summary information.

// NB: `write!` together with `\n` is preferred to `writeln!` so that there
//   is only a single sequence of characters to search for while tracking
//   down newlines,
//     rather than using both.

use super::{
    resolve::{
        Column, ResolvedSpan, ResolvedSpanData, SourceLine, SpanResolver,
        SpanResolverError,
    },
    AnnotatedSpan, Diagnostic, Label, Level,
};
use crate::span::{Context, Span, UNKNOWN_SPAN};
use std::{
    fmt::{self, Display, Write},
    num::NonZeroU32,
    ops::Add,
};

/// Render a [`Report`] with detailed diagnostic information.
///
/// Rendering of reports is layered---this
///   report can be further rendered into a string using [`Display::fmt`].
pub trait Reporter {
    /// Render diagnostic report.
    ///
    /// The provided [`Report`] implements [`Display`].
    ///
    /// Please be mindful of where this report is being rendered to
    ///   (via [`Display`]).
    /// For example,
    ///   if rendering to standard out,
    ///   it is a good idea to buffer the entire report before flushing to
    ///     stdout,
    ///       otherwise the report may become interleaved with other
    ///       concurrent processes
    ///         (e.g. if TAMER is being invoked using `make -jN`).
    ///
    /// It is also important to note that this method
    ///   _does not return [`Result`]_ and should never fail,
    ///     unless due to a panic in the standard library
    ///       (e.g. due to allocation failure).
    /// The report absorbs errors during processing and renders those errors
    ///   to the report itself,
    ///     ensuring both that the user is made aware of the problem
    ///     and that we're not inadvertently suppressing the actual
    ///       diagnostic messages that were requested.
    fn render<'d, D: Diagnostic>(&mut self, diagnostic: &'d D)
        -> Report<'d, D>;

    /// Whether any reports have been rendered with an error level or higher.
    fn has_errors(&self) -> bool;

    /// Number of reports with an error level or higher that have been
    ///   rendered.
    fn error_count(&self) -> usize;
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
    /// Span resolver.
    ///
    /// This is responsible for resolving a span to a filename with line and
    ///   column numbers.
    resolver: R,

    /// Number of reports with a severity level of error or higher.
    err_n: usize,
}

impl<R: SpanResolver> VisualReporter<R> {
    pub fn new(resolver: R) -> Self {
        Self { resolver, err_n: 0 }
    }
}

impl<R: SpanResolver> Reporter for VisualReporter<R> {
    fn render<'d, D: Diagnostic>(
        &mut self,
        diagnostic: &'d D,
    ) -> Report<'d, D> {
        let mspans =
            describe_resolved(|span| self.resolver.resolve(span), diagnostic);

        let mut report = Report::empty(Message(diagnostic));
        report.extend(mspans.map(Into::into));
        report.normalize_gutters();
        report.finalize_sections();

        if report.level.is_error() {
            // Not worried about overflow panic
            //   (you have bigger problems if there are that many errors).
            self.err_n += 1;
        }

        report
    }

    fn has_errors(&self) -> bool {
        self.error_count() > 0
    }

    fn error_count(&self) -> usize {
        self.err_n
    }
}

/// Request a diagnostic description and immediately resolve the provided
///   [`AnnotatedSpan`]s into [`MaybeResolvedSpan`]s.
///
/// Adjacent identical [`Span`]s are elided such that the first one is
///   resolved but the second one produces [`MaybeResolvedSpan::Elided`];
///     this avoids the expensive column resolution and source line
///     allocation for a span that was just processed and whose section will
///     be squashed anyway
///       (see [`Section::maybe_squash_into`]).
fn describe_resolved<D, F>(
    mut resolve: F,
    diagnostic: &D,
) -> impl Iterator<Item = MaybeResolvedSpan<ResolvedSpan>>
where
    D: Diagnostic,
    F: FnMut(Span) -> Result<ResolvedSpan, SpanResolverError>,
{
    diagnostic.describe().into_iter().scan(
        UNKNOWN_SPAN,
        move |prev_span, AnnotatedSpan(span, level, olabel)| {
            // Avoid re-resolving
            //   (and allocating memory for the source lines of)
            //   a span that was just resolved,
            //     which will just be squashed with the previous anyway.
            if *prev_span == span {
                return Some(MaybeResolvedSpan::Elided(span, level, olabel));
            }

            *prev_span = span;

            Some(match resolve(span) {
                Ok(rspan) => MaybeResolvedSpan::Resolved(rspan, level, olabel),
                Err(e) => MaybeResolvedSpan::Unresolved(span, level, olabel, e),
            })
        },
    )
}

/// A [`Span`] that may have been resolved.
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
#[derive(Debug, PartialEq, Eq)]
enum MaybeResolvedSpan<'d, S: ResolvedSpanData> {
    Resolved(S, Level, Option<Label<'d>>),
    Elided(Span, Level, Option<Label<'d>>),
    Unresolved(Span, Level, Option<Label<'d>>, SpanResolverError),
}

impl<'d, S: ResolvedSpanData> MaybeResolvedSpan<'d, S> {
    /// We should never mask an error with our own;
    ///   the diagnostic system is supposed to _help_ the user in diagnosing
    ///   problems,
    ///     not hinder them by masking it.
    fn system_lines(&self) -> Vec<SectionLine<'static>> {
        match self {
            Self::Resolved(rspan, ..) if rspan.col_num().is_none() => vec![
                SectionLine::Footnote(
                    Level::Help,
                    "unable to calculate columns because the line is \
                        not a valid UTF-8 string"
                        .into(),
                ),
                SectionLine::Footnote(
                    Level::Help,
                    "you have been provided with 0-indexed \
                        line-relative inclusive byte offsets"
                        .into(),
                ),
            ],

            Self::Unresolved(.., e) => {
                vec![SectionLine::Footnote(
                    Level::Help,
                    format!(
                        "an error occurred while trying to look up \
                         information about this span: {e}"
                    )
                    .into(),
                )]
            }

            _ => vec![],
        }
    }
}

/// Diagnostic report.
///
/// This contains the raw data that,
///   when rendered with [`Display::fmt`],
///   will produce a robust report to help guide the user through diagnosing
///     and resolving problems.
#[derive(Debug)]
pub struct Report<'d, D: Diagnostic> {
    /// Summary message of the contents of the report.
    ///
    /// This message should be suitable on its own,
    ///   e.g. a typical one-line error message.
    msg: Message<'d, D>,

    /// The largest severity level found within all of the [`Section`]s of
    ///   the report.
    ///
    /// This level should be used alongside the summary message to describe
    ///   how severe of a problem this report represents.
    level: Level,

    /// Sections of the report.
    ///
    /// A section contains a header describing a location in the source
    ///   code (line and column numbers),
    ///     followed by annotated source code and descriptive labels.
    secs: Vec<Section<'d>>,

    /// The maximum line number encountered in each of the [`Section`]s of
    ///   the report.
    ///
    /// This number is used to determine the gutter width,
    ///   which contains the line numbers of the annotated source lines.
    /// It can be propagated to all [`Section`]s using
    ///   [`normalize_gutters`](Report::normalize_gutters).
    line_max: NonZeroU32,
}

impl<'d, D: Diagnostic> Report<'d, D> {
    /// Create an empty report.
    ///
    /// To add to the body of the report,
    ///   use [`Extend::extend`].
    fn empty(msg: Message<'d, D>) -> Self {
        Self {
            msg,
            secs: Vec::new(),
            level: Level::default(),
            line_max: NonZeroU32::MIN,
        }
    }

    /// Make all sections' gutters the same width.
    ///
    /// This is only necessary because [`Section`] is expected to be wholly
    ///   self-contained when rendering.
    fn normalize_gutters(&mut self) {
        for sec in self.secs.iter_mut() {
            sec.line_max = self.line_max;
        }
    }

    /// Finalize section formatting before display to the user.
    fn finalize_sections(&mut self) {
        self.secs.iter_mut().for_each(Section::finalize)
    }
}

impl<'d, D: Diagnostic> Extend<Section<'d>> for Report<'d, D> {
    /// Extend the body of the report.
    ///
    /// This tracks the most severe [`Level`] and highest line number seen.
    /// Further,
    ///   adjacent sections may be squashed if they meet certain criteria
    ///     (see [`Section::maybe_squash_into`]).
    fn extend<T: IntoIterator<Item = Section<'d>>>(&mut self, secs: T) {
        for sec in secs {
            self.level = self.level.min(sec.level());
            self.line_max = self.line_max.max(sec.line_max);

            // Add the section if it cannot be squashed into the previous.
            let remain = sec.maybe_squash_into(self.secs.last_mut());
            self.secs.extend(remain);
        }
    }
}

impl<'d, D: Diagnostic> Display for Report<'d, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{level}: {msg}\n", level = self.level, msg = self.msg)?;

        self.secs.iter().try_fold(true, |first, sec| {
            if !first {
                f.write_char('\n')?;
            }

            sec.fmt(f)?;
            Ok(false)
        })?;

        Ok(())
    }
}

/// Summary diagnostic message.
#[derive(Debug)]
struct Message<'d, D: Diagnostic>(&'d D);

impl<'d, D: Diagnostic> Display for Message<'d, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self.0, f)
    }
}

/// A section of a [`Report`] describing a [`Span`].
///
/// Adjacent sections describing the same [`Span`] ought to be squashed
///   (see [`Section::maybe_squash_into`]),
///   but not non-adjacent ones,
///     since reports ought to be able to produce narratives that may
///       revisit previous spans in an attempt to describe what occurred and
///       how to correct it.
///
/// Each section is delimited by a heading that provides a summary context
///   and enough of a visual distinction to be able to skim diagnostic
///   messages quickly.
#[derive(Debug, PartialEq, Eq)]
struct Section<'d> {
    /// Heading that delimits the beginning of each section.
    ///
    /// The heading describes the location of its principal [`Span`].
    heading: SpanHeading,

    /// The most severe [`Level`] encountered in this section body.
    level: Level,

    /// The principal [`Span`] that this section describes.
    ///
    /// If a section contains information about multiple spans,
    ///   this represents the one that the user should focus on.
    span: Span,

    /// Annotated source lines and labels.
    body: Vec<SectionLine<'d>>,

    /// The largest line number encountered in this section.
    ///
    /// This is used to determine how wide to render the gutter,
    ///   which contain the line numbers for source lines.
    line_max: NonZeroU32,
}

impl<'d> Section<'d> {
    /// Create a new section from a resolved span.
    fn new_resolved<S: ResolvedSpanData>(
        rspan: S,
        level: Level,
        mut olabel: Option<Label<'d>>,
        syslines: Vec<SectionLine<'static>>,
    ) -> Self {
        let heading = SpanHeading::from(&rspan);

        let span = rspan.unresolved_span();
        let src = rspan.into_lines();
        let nlines = src.len();

        let mut body = Vec::with_capacity(4);
        let mut line_max = NonZeroU32::MIN;

        src.into_iter().enumerate().for_each(|(i, srcline)| {
            let line_num = srcline.num();

            // Note that lines are intentionally _not_ ordered,
            //   since reports may jump around a file to produce a
            //   narrative.
            line_max = line_max.max(line_num);

            let label = if i == nlines - 1 { olabel.take() } else { None };

            Self::render_src(&mut body, srcline, level, label);
        });

        // System messages should appear _after_ all requested diagnostic
        //   messages.
        body.extend(syslines);

        Section {
            heading,
            span,
            level,
            body,
            line_max,
        }
    }

    /// Create a new section from a span that failed to resolve.
    fn new_unresolved(
        span: Span,
        level: Level,
        olabel: Option<Label<'d>>,
        syslines: Vec<SectionLine<'static>>,
    ) -> Self {
        let mut body = Vec::new();

        body.extend(olabel.map(|label| SectionLine::Footnote(level, label)));
        body.extend(syslines);

        Section {
            heading: SpanHeading(
                span.context(),
                HeadingLineNum::Unresolved(span),
            ),
            level,
            span,
            body,
            line_max: NonZeroU32::MIN,
        }
    }

    /// Render a `SourceLine` to the body with the appropriate annotations.
    ///
    /// If the columns are known,
    ///   then the span will be marked along with an optional label.
    /// Otherwise,
    ///   the line will be rendered without annotations,
    ///     with any optional label appearing as a footnote.
    fn render_src(
        dest: &mut Vec<SectionLine<'d>>,
        src: SourceLine,
        level: Level,
        label: Option<Label<'d>>,
    ) {
        if let Some(col) = src.column() {
            dest.extend(vec![
                SectionLine::SourceLinePadding,
                SectionLine::SourceLine(src.into()),
                SectionLine::SourceLineMark(LineMark { col, level, label }),
                SectionLine::SourceLinePadding,
            ]);
        } else {
            dest.extend(vec![
                SectionLine::SourceLinePadding,
                SectionLine::SourceLine(src.into()),
                SectionLine::SourceLinePadding,
            ]);

            dest.extend(label.map(|l| SectionLine::Footnote(level, l)));
        }
    }

    /// Most severe [`Level`] associated with this section.
    ///
    /// This value is updated as lines are added to the body.
    fn level(&self) -> Level {
        self.level
    }

    /// Squash self into the provided [`Section`] if they represent the same
    ///   [`Span`],
    ///     otherwise do nothing.
    ///
    /// If squashed,
    ///   [`None`] is returned.
    /// Otherwise [`Some`] is returned with `self`.
    /// This return value can be used with [`Extend`] to extend a vector of
    ///   sections with the value after this operation.
    ///
    /// The term "squash" is borrowed from `git rebase`.
    fn maybe_squash_into(
        self,
        extend: Option<&mut Section<'d>>,
    ) -> Option<Self> {
        match extend {
            Some(extend_sec) if self.span == extend_sec.span => {
                // Note that system labels shouldn't exist for elided spans
                //   and so they should not be duplicated when squashing.
                extend_sec.body.extend(
                    self.body
                        .into_iter()
                        .filter_map(SectionLine::into_footnote),
                );

                None
            }

            _ => Some(self),
        }
    }

    /// Maximum width of the text in the gutter.
    ///
    /// Note that the gutter contains a single character of padding before
    ///   its delimiter,
    ///     which this width _does not_ account for.
    ///
    /// The minimum width is 2.
    ///
    /// ```text
    ///   --> heading
    ///    |
    ///  1 | source line
    /// ^^
    ///  gutter width is 2
    /// ```
    fn gutter_text_width(&self) -> usize {
        self.line_max.ilog10().add(1).max(2) as usize
    }

    /// Finalize formatting of this section before display to the user.
    ///
    /// This is the last chance to clean things up.
    fn finalize(&mut self) {
        use SectionLine::SourceLinePadding;

        // Padding is added conservatively during generation,
        //   which may lead to adjacent padding for multi-line spans.
        // That padding can be merged into a single line.
        self.body.dedup_by(|a, b| {
            matches!((a, b), (SourceLinePadding, SourceLinePadding),)
        });

        // Padding at the very end of the section is not desirable since the
        //   report already adds padding around sections.
        if self.body.last() == Some(&SourceLinePadding) {
            self.body.pop();
        }
    }
}

impl<'d, S> From<MaybeResolvedSpan<'d, S>> for Section<'d>
where
    S: ResolvedSpanData,
{
    fn from(mspan: MaybeResolvedSpan<'d, S>) -> Self {
        let syslines = mspan.system_lines();

        match mspan {
            MaybeResolvedSpan::Resolved(rspan, level, olabel) => {
                Self::new_resolved(rspan, level, olabel, syslines)
            }
            // Note that elided will be squashed,
            //   so it's okay that it's marked as unresolved here.
            MaybeResolvedSpan::Elided(span, level, olabel)
            | MaybeResolvedSpan::Unresolved(span, level, olabel, _) => {
                Self::new_unresolved(span, level, olabel, syslines)
            }
        }
    }
}

impl<'d> Display for Section<'d> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let gutterw = self.gutter_text_width();

        // The heading has a hanging indentation,
        //   which is accomplished by simply omitting spaces above the
        //   gutter's `" |"`,
        //     which amounts to two characters wide.
        write!(f, "{:gutterw$}{heading}\n", "", heading = self.heading)?;

        self.body.iter().try_for_each(|line| {
            line.fmt_gutter(gutterw, f)?;
            write!(f, "{line}\n")
        })
    }
}

/// Heading describing the context of a (hopefully resolved) span.
///
/// The ideal header contains the context along with the line, and column
///   numbers,
///     visually distinguishable from surrounding lines to allow the user to
///     quickly skip between reports.
#[derive(Debug, PartialEq, Eq)]
struct SpanHeading(Context, HeadingLineNum);

impl Display for SpanHeading {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ctx, line) = self;
        write!(f, "--> {ctx}{line}")
    }
}

impl<S: ResolvedSpanData> From<&S> for SpanHeading {
    /// Span header containing the (hopefully resolved) context.
    fn from(rspan: &S) -> Self {
        SpanHeading(
            rspan.context(),
            HeadingLineNum::Resolved(
                rspan.line_num(),
                rspan.col_num().map(HeadingColNum::Resolved).unwrap_or_else(
                    || HeadingColNum::Unresolved {
                        unresolved_span: rspan.unresolved_span(),
                        first_line_span: rspan.first_line_span(),
                    },
                ),
            ),
        )
    }
}

/// Span line number or fallback representation.
///
/// This is also responsible for attempting to produce a column number,
///   provided that a line number is available.
///
/// If a span could not be resolved,
///   offsets should be rendered in place of lines and columns.
#[derive(Debug, PartialEq, Eq)]
enum HeadingLineNum {
    Resolved(NonZeroU32, HeadingColNum),
    Unresolved(Span),
}

impl Display for HeadingLineNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Resolved(line_num, col) => {
                write!(f, ":{line_num}{col}")
            }

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
        }
    }
}

/// Column number or fallback representation.
///
/// If a column could not be resolved,
///   it should fall back to displaying byte offsets relative to the start
///   of the line.
#[derive(Debug, PartialEq, Eq)]
enum HeadingColNum {
    Resolved(Column),
    Unresolved {
        unresolved_span: Span,
        first_line_span: Span,
    },
}

impl Display for HeadingColNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Resolved(col) => write!(f, ":{col}"),

            // The column is unavailable,
            //   which means that the line must have contained invalid UTF-8.
            // Output what we can in an attempt to help the user debug.
            Self::Unresolved {
                unresolved_span,
                first_line_span,
            } => {
                let rel = unresolved_span
                    .relative_to(*first_line_span)
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

/// Line of output in a [`Section`] body.
#[derive(Debug, PartialEq, Eq)]
enum SectionLine<'d> {
    /// Padding for a possibly annotated source line.
    ///
    /// A padding line is intended to add extra space above or below a
    ///   source line to make it easier to read.
    /// Padding lines contain a gutter,
    ///   but no line number.
    SourceLinePadding,

    /// A line of source code.
    SourceLine(SectionSourceLine),

    /// Source line annotations
    ///   (marks and labels).
    SourceLineMark(LineMark<'d>),

    /// A label that is not rendered as a line annotation.
    ///
    /// Footnotes are intended too appear at the end of a [`Section`] and
    ///   contain supplemental information.
    Footnote(Level, Label<'d>),
}

impl<'d> SectionLine<'d> {
    /// Format the gutter to the left of the section body for this line.
    ///
    /// Note that the provided `text_width` _does not include_ a single
    ///   character of padding and a single-character delimiter that are
    ///   expected to follow.
    /// The width is guaranteed to be at least as wide as the number of
    ///   characters needed to represent the line number in base-10.
    ///
    /// For example:
    ///
    /// ```text
    ///    |
    ///  1 | source line
    ///    | ------
    ///    = note: notice the delim change for this footnote
    /// ^^
    ///  gutter `text_width` of 2
    /// ```
    fn fmt_gutter(
        &self,
        text_width: usize,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        match self {
            Self::SourceLinePadding | Self::SourceLineMark(..) => {
                write!(f, "{:text_width$} |", "")
            }
            Self::SourceLine(src) => write!(f, "{:>text_width$} |", src.num()),
            Self::Footnote(..) => write!(f, "{:text_width$} =", ""),
        }
    }

    /// Attempt to convert a line into a footnote.
    ///
    /// If there is no [`Level`] and [`Label`] available,
    ///   [`None`] is returned.
    fn into_footnote(self) -> Option<Self> {
        match self {
            Self::SourceLinePadding => None,
            Self::SourceLine(..) => None,
            Self::SourceLineMark(LineMark { level, label, .. }) => {
                label.map(|l| Self::Footnote(level, l))
            }

            Self::Footnote(..) => Some(self),
        }
    }
}

impl<'d> Display for SectionLine<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SourceLinePadding => Ok(()),
            Self::SourceLine(line) => line.fmt(f),
            Self::SourceLineMark(mark) => mark.fmt(f),
            Self::Footnote(level, label) => write!(f, " {level}: {label}"),
        }
    }
}

/// A [`SourceLine`] displayed within a [`Section`].
#[derive(Debug, PartialEq, Eq)]
struct SectionSourceLine(SourceLine);

impl SectionSourceLine {
    fn num(&self) -> NonZeroU32 {
        self.0.num()
    }
}

impl From<SourceLine> for SectionSourceLine {
    fn from(line: SourceLine) -> Self {
        Self(line)
    }
}

impl Display for SectionSourceLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " {line}", line = self.0)
    }
}

/// A type of line annotation that marks columns and provides labels,
///   if available.
///
/// Marks are displayed below a [`SectionSourceLine`] and are intended to
///   visually display a [`Span`].
/// Column resolution
///   (see [`super::resolve`])
///   exists primarily for mark rendering.
#[derive(Debug, PartialEq, Eq)]
struct LineMark<'d> {
    level: Level,
    col: Column,
    label: Option<Label<'d>>,
}

impl<'d> Display for LineMark<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { col, level, .. } = self;

        let underline = level
            .mark_char()
            .to_string()
            .repeat((col.end().get() - col.start().get()) as usize + 1);

        let lpad = col.start().get() as usize - 1;

        write!(f, " {:lpad$}{underline}", "")?;

        if let Some(label) = self.label.as_ref() {
            // TODO: If the span is at the end of a long line,
            //   this is more likely to wrap on the user's terminal and be
            //   unpleasant to read.
            write!(f, " {level}: {label}", level = self.level)?;
        }

        Ok(())
    }
}

/// Mark styling.
trait MarkChar {
    /// Character used to underline the columns applicable to a given span
    ///   underneath a source line.
    fn mark_char(&self) -> char;
}

impl MarkChar for Level {
    fn mark_char(&self) -> char {
        match self {
            Level::InternalError => '!',
            Level::Error => '^',
            Level::Note | Level::Help => '-',
        }
    }
}

#[cfg(test)]
mod test;
