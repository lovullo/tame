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

// NB: `write!` together with `\n` is preferred to `writeln!` so that there
//   is only a single sequence of characters to search for while tracking
//   down newlines,
//     rather than using both.

use super::{
    resolver::{
        Column, ResolvedSpanData, SourceLine, SpanResolver, SpanResolverError,
    },
    AnnotatedSpan, Diagnostic, Label, Level,
};
use crate::span::{Context, Span, UNKNOWN_SPAN};
use std::{
    fmt::{self, Display, Write},
    num::NonZeroU32,
    ops::Add,
};

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
    fn render<'d, D: Diagnostic>(
        &mut self,
        diagnostic: &'d D,
    ) -> Report<'d, D> {
        // TODO: Avoid duplicate lookups of the same span,
        //   or at least adjacent ones.
        let mspans = diagnostic.describe().into_iter().map(
            |AnnotatedSpan(span, level, olabel)| {
                let slabel = olabel.map(|label| SpanLabel(level, label));

                match self.resolver.resolve(span) {
                    Ok(rspan) => MaybeResolvedSpan::Resolved(rspan, slabel),
                    Err(e) => MaybeResolvedSpan::Unresolved(span, slabel, e),
                }
            },
        );

        let mut report = Report::empty(Message(diagnostic));
        report.extend(mspans.map(Into::into));

        // Make each section's gutters the same size,
        //   which is more aesthetically pleasing.
        report.normalize_gutters();

        report
    }
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
    Resolved(S, Option<SpanLabel<'d>>),
    Unresolved(Span, Option<SpanLabel<'d>>, SpanResolverError),
}

impl<'d, S: ResolvedSpanData> MaybeResolvedSpan<'d, S> {
    /// We should never mask an error with our own;
    ///   the diagnostic system is supposed to _help_ the user in diagnosing
    ///   problems,
    ///     not hinder them by masking it.
    fn system_lines(&self) -> Vec<SectionLine<'static>> {
        match self {
            Self::Resolved(rspan, _) if rspan.col_num().is_none() => vec![
                SectionLine::Footnote(SpanLabel(
                    Level::Help,
                    "unable to calculate columns because the line is \
                        not a valid UTF-8 string"
                        .into(),
                )),
                SectionLine::Footnote(SpanLabel(
                    Level::Help,
                    "you have been provided with 0-indexed \
                        line-relative inclusive byte offsets"
                        .into(),
                )),
            ],

            Self::Unresolved(_, _, e) => {
                vec![SectionLine::Footnote(SpanLabel(
                    Level::Help,
                    format!(
                        "an error occurred while trying to look up \
                         information about this span: {e}"
                    )
                    .into(),
                ))]
            }

            _ => vec![],
        }
    }
}

#[derive(Debug)]
pub struct Report<'d, D: Diagnostic> {
    msg: Message<'d, D>,
    secs: Vec<Section<'d>>,
    level: Level,
    line_max: NonZeroU32,
}

impl<'d, D: Diagnostic> Report<'d, D> {
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
}

impl<'d, D: Diagnostic> Extend<Section<'d>> for Report<'d, D> {
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
    heading: SpanHeading,
    level: Level,
    span: Span,
    body: Vec<SectionLine<'d>>,
    line_max: NonZeroU32,
}

impl<'s, 'd> Section<'d> {
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
            // TODO: Take highest level.
            Some(extend_sec) if self.span == extend_sec.span => {
                // TODO: At the time of writing this will cause duplication of
                //   system labels,
                //     which is not desirable.
                extend_sec.body.extend(
                    // TODO: The system wastefully allocates duplicate source
                    //   lines when resolving spans only to discard them here.
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
        self.line_max.log10().add(1).max(2) as usize
    }
}

impl<'d, 'a, S> From<MaybeResolvedSpan<'d, S>> for Section<'d>
where
    S: ResolvedSpanData,
{
    fn from(mspan: MaybeResolvedSpan<'d, S>) -> Self {
        let heading = SpanHeading::from(&mspan);
        let syslines = mspan.system_lines();

        let mut body = Vec::new();
        let mut line_max = NonZeroU32::MIN;

        let (span, level) = match mspan {
            MaybeResolvedSpan::Resolved(rspan, oslabel) => {
                let span = rspan.unresolved_span();
                let src = rspan.into_lines();

                let (level, mut olabel) = match oslabel {
                    Some(SpanLabel(level, label)) => (level, Some(label)),
                    None => (Default::default(), None),
                };

                let nlines = src.len();

                src.into_iter().enumerate().for_each(|(i, srcline)| {
                    let line_num = srcline.num();

                    // Note that lines are intentionally _not_ ordered,
                    //   since reports may jump around a file to produce a
                    //   narrative.
                    line_max = line_max.max(line_num);

                    let label =
                        if i == nlines - 1 { olabel.take() } else { None };

                    if let Some(col) = srcline.column() {
                        body.extend(vec![
                            SectionLine::SourceLinePadding,
                            SectionLine::SourceLine(srcline.into()),
                            SectionLine::SourceLineMark(LineMark {
                                col,
                                level,
                                label,
                            }),
                        ]);
                    } else {
                        body.extend(label.map(|l| {
                            SectionLine::Footnote(SpanLabel(level, l))
                        }));
                    }
                });

                (span, level)
            }
            MaybeResolvedSpan::Unresolved(span, olabel, _) => {
                let level =
                    olabel.as_ref().map(SpanLabel::level).unwrap_or_default();

                body.extend(olabel.map(SectionLine::Footnote));
                (span, level)
            }
        };

        body.extend(syslines);

        Section {
            heading,
            span,
            level,
            body,
            line_max,
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

impl<'s, 'd, S> From<&'s MaybeResolvedSpan<'d, S>> for SpanHeading
where
    S: ResolvedSpanData,
{
    /// Span header containing the (hopefully resolved) context.
    fn from(mspan: &'s MaybeResolvedSpan<'d, S>) -> Self {
        match mspan {
            MaybeResolvedSpan::Resolved(rspan, _) => SpanHeading(
                rspan.context(),
                HeadingLineNum::Resolved(
                    rspan.line_num(),
                    rspan
                        .col_num()
                        .map(HeadingColNum::Resolved)
                        .unwrap_or_else(|| HeadingColNum::Unresolved {
                            unresolved_span: rspan.unresolved_span(),
                            first_line_span: rspan.first_line_span(),
                        }),
                ),
            ),

            MaybeResolvedSpan::Unresolved(span, _, _) => {
                SpanHeading(span.context(), HeadingLineNum::Unresolved(*span))
            }
        }
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
            Self::Resolved(col) => write!(f, ":{}", col),

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

/// A label describing a span.
#[derive(Debug, PartialEq, Eq)]
struct SpanLabel<'d>(Level, Label<'d>);

impl<'d> SpanLabel<'d> {
    fn level(&self) -> Level {
        self.0
    }
}

impl<'d> Display for SpanLabel<'d> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(level, label) = self;
        write!(f, " {level}: {label}")
    }
}

/// Line of output in a [`Section`] body.
#[derive(Debug, PartialEq, Eq)]
enum SectionLine<'d> {
    SourceLinePadding,
    SourceLine(SectionSourceLine),
    SourceLineMark(LineMark<'d>),
    Footnote(SpanLabel<'d>),
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

    fn into_footnote(self) -> Option<Self> {
        match self {
            Self::SourceLinePadding => None,
            Self::SourceLine(..) => None,
            Self::SourceLineMark(LineMark { level, label, .. }) => {
                label.map(|l| Self::Footnote(SpanLabel(level, l)))
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
            Self::Footnote(label) => label.fmt(f),
        }
    }
}

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

trait MarkChar {
    fn mark_char(&self) -> char;
}

impl MarkChar for Level {
    /// Character used to underline the columns applicable to a given span
    ///   underneath a source line.
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
