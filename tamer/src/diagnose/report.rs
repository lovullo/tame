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
        let mspans = diagnostic
            .describe()
            .into_iter()
            .map(|AnnotatedSpan(span, level, olabel)| {
                let slabel = olabel.map(|label| SpanLabel(level, label));

                match self.resolver.resolve(span) {
                    Ok(rspan) => MaybeResolvedSpan::Resolved(rspan, slabel),
                    Err(e) => MaybeResolvedSpan::Unresolved(span, slabel, e),
                }
            })
            .collect::<Vec<_>>();

        let mut report = Report::empty(Message(diagnostic));
        report.extend(mspans.into_iter().map(Into::into));
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
}

impl<'d, D: Diagnostic> Report<'d, D> {
    fn empty(msg: Message<'d, D>) -> Self {
        Self {
            msg,
            secs: Vec::new(),
            level: Level::default(),
        }
    }
}

impl<'d, D: Diagnostic> Extend<Section<'d>> for Report<'d, D> {
    fn extend<T: IntoIterator<Item = Section<'d>>>(&mut self, secs: T) {
        for sec in secs {
            self.level = self.level.min(sec.level());

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
}

impl<'d, 'a, S> From<MaybeResolvedSpan<'d, S>> for Section<'d>
where
    S: ResolvedSpanData,
{
    fn from(mspan: MaybeResolvedSpan<'d, S>) -> Self {
        let heading = SpanHeading::from(&mspan);
        let syslines = mspan.system_lines();

        let mut body = Vec::new();

        let (span, level) = match mspan {
            MaybeResolvedSpan::Resolved(rspan, oslabel) => {
                let span = rspan.unresolved_span();
                let src = rspan.into_lines();

                let (level, mut olabel) = match oslabel {
                    Some(SpanLabel(level, label)) => (level, Some(label)),
                    None => (Default::default(), None),
                };

                let nlines = src.len();

                body.extend(src.into_iter().enumerate().map(|(i, srcline)| {
                    let col = srcline.column();

                    SectionLine::SourceLine(SectionSourceLine {
                        src: srcline,
                        mark: LineMark {
                            col,
                            level,
                            label: if i == nlines - 1 {
                                olabel.take()
                            } else {
                                None
                            },
                        },
                    })
                }));

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
        }
    }
}

impl<'d> Display for Section<'d> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  {heading}\n", heading = self.heading)?;

        for line in self.body.iter() {
            // Let each line have control over its own newline so that it
            //   can fully suppress itself if it's not relevant.
            line.fmt(f)?;
        }

        Ok(())
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
        write!(f, "   = {level}: {label}")
    }
}

/// A possibly-annotated line of output.
///
/// Note that a section line doesn't necessarily correspond to a single line
///   of output on a terminal;
///     lines are likely to be annotated.
#[derive(Debug, PartialEq, Eq)]
enum SectionLine<'d> {
    SourceLine(SectionSourceLine<'d>),
    Footnote(SpanLabel<'d>),
}

impl<'d> SectionLine<'d> {
    fn into_footnote(self) -> Option<Self> {
        match self {
            Self::SourceLine(SectionSourceLine {
                mark: LineMark { level, label, .. },
                ..
            }) => label.map(|l| Self::Footnote(SpanLabel(level, l))),

            Self::Footnote(..) => Some(self),
        }
    }
}

impl<'d> Display for SectionLine<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SourceLine(line) => line.fmt(f),
            Self::Footnote(label) => write!(f, "{label}\n"),
        }
    }
}

/// A line representing possibly-annotated source code.
#[derive(Debug, PartialEq, Eq)]
struct SectionSourceLine<'d> {
    src: SourceLine,
    mark: LineMark<'d>,
}

impl<'d> Display for SectionSourceLine<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "   |\n")?;
        write!(f, "   | {src}\n", src = self.src)?;
        write!(f, "{}", self.mark)
    }
}

/// A type of line annotation that marks columns and provides labels,
///   if available.
#[derive(Debug, PartialEq, Eq)]
struct LineMark<'d> {
    level: Level,
    col: Option<Column>,
    label: Option<Label<'d>>,
}

impl<'d> Display for LineMark<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "   |")?;

        if let Some(col) = self.col {
            let underline = self
                .level
                .mark_char()
                .to_string()
                .repeat((col.end().get() - col.start().get()) as usize + 1);

            let lpad = col.start().get() as usize - 1;

            write!(f, " {:lpad$}{underline}", "")?;
        } else {
            write!(f, "\n")?;
        }

        if let Some(label) = self.label.as_ref() {
            if self.col.is_none() {
                // Render as a footnote.
                write!(f, "   =")?;
            }

            // TODO: If the span is at the end of a long line,
            //   this is more likely to wrap on the user's terminal and be
            //   unpleasant to read.
            write!(f, " {level}: {label}", level = self.level)?;
        }

        write!(f, "\n")
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
mod test {
    use super::*;
    use crate::{
        convert::ExpectInto,
        diagnose::resolver::Column,
        span::{DUMMY_CONTEXT, DUMMY_SPAN},
    };
    use std::{io, num::NonZeroU32};

    mod integration;

    #[derive(Default)]
    struct StubResolvedSpan {
        span: Option<Span>,
        first_line_span: Option<Span>,
        line_num: Option<NonZeroU32>,
        col_num: Option<Column>,
        context: Option<Context>,
        src_lines: Option<Vec<SourceLine>>,
    }

    impl ResolvedSpanData for StubResolvedSpan {
        fn line_num(&self) -> NonZeroU32 {
            self.line_num.expect("missing stub line_num")
        }

        fn col_num(&self) -> Option<Column> {
            self.col_num
        }

        fn first_line_span(&self) -> Span {
            self.first_line_span.expect("missing stub first_line_span")
        }

        fn context(&self) -> Context {
            self.context.expect("missing stub ctx")
        }

        fn unresolved_span(&self) -> Span {
            self.span.expect("missing stub unresolved span")
        }

        fn into_lines(self) -> Vec<SourceLine> {
            self.src_lines.unwrap_or_default()
        }
    }

    #[test]
    fn header_col_with_available_col() {
        let sut = HeadingColNum::Resolved(Column::Endpoints(
            5.unwrap_into(),
            // Second endpoint is ignored.
            6.unwrap_into(),
        ));

        assert_eq!(":5", format!("{}", sut));
    }

    #[test]
    fn header_col_without_available_col() {
        let sut = HeadingColNum::Unresolved {
            unresolved_span: DUMMY_CONTEXT.span(5, 2),
            first_line_span: DUMMY_CONTEXT.span(3, 7),
        };

        assert_eq!(" bytes 2--4", format!("{}", sut));
    }

    // Note that line is coupled with `HeadingColNum`,
    //   tested above.
    // The coupling is not ideal,
    //   but it keeps it simple and we don't concretely benefit from the
    //   decoupling for now.
    #[test]
    fn line_with_resolved_span() {
        let sut = HeadingLineNum::Resolved(
            5.unwrap_into(),
            HeadingColNum::Resolved(Column::Endpoints(
                3.unwrap_into(),
                3.unwrap_into(),
            )),
        );

        assert_eq!(":5:3", format!("{}", sut));
    }

    // Does _not_ use `HeadingColNum`,
    //   unlike the above,
    //   because the line was not resolved.
    #[test]
    fn line_with_unresolved_span_without_resolved_col() {
        let sut = HeadingLineNum::Unresolved(DUMMY_CONTEXT.span(3, 4));

        assert_eq!(" offset 3--7", format!("{}", sut));
    }

    // Whether you call this a unit or integration test depends on your
    //   perspective---it's
    //     either an integration test,
    //       or we're testing privates.
    // Neither are ideal,
    //   but decoupling isn't worth the type burden that results.
    #[test]
    fn span_heading() {
        let ctx = "header".unwrap_into();
        let sut = SpanHeading(
            ctx,
            HeadingLineNum::Resolved(
                2.unwrap_into(),
                HeadingColNum::Resolved(Column::Endpoints(
                    6.unwrap_into(),
                    6.unwrap_into(),
                )),
            ),
        );

        assert_eq!("--> header:2:6", format!("{}", sut));
    }

    #[test]
    fn section_from_mspan_resolved() {
        let ctx = Context::from("mspan/sec");
        let span = ctx.span(2, 3);

        let col_1 = Column::Endpoints(2.unwrap_into(), 3.unwrap_into());
        let col_2 = Column::Endpoints(1.unwrap_into(), 4.unwrap_into());

        let src_lines = vec![
            SourceLine::new_stub(
                1.unwrap_into(),
                Some(col_1),
                span,
                "line 1".into(),
            ),
            SourceLine::new_stub(
                2.unwrap_into(),
                Some(col_2),
                span,
                "line 2".into(),
            ),
        ];

        assert_eq!(
            Section::from(MaybeResolvedSpan::Resolved(
                StubResolvedSpan {
                    context: Some(ctx),
                    line_num: Some(1.unwrap_into()),
                    col_num: Some(col_1),
                    first_line_span: Some(DUMMY_SPAN),
                    span: Some(span),
                    src_lines: Some(src_lines.clone()),
                },
                Some(SpanLabel(Level::Note, "test label".into())),
            )),
            Section {
                heading: SpanHeading(
                    ctx,
                    HeadingLineNum::Resolved(
                        1.unwrap_into(),
                        HeadingColNum::Resolved(Column::Endpoints(
                            2.unwrap_into(),
                            3.unwrap_into()
                        ))
                    )
                ),
                span,
                // Derived from label.
                level: Level::Note,
                body: vec![
                    SectionLine::SourceLine(SectionSourceLine {
                        src: src_lines[0].clone(),
                        mark: LineMark {
                            level: Level::Note,
                            col: Some(col_1),
                            // Label goes on the last source line.
                            label: None,
                        }
                    }),
                    SectionLine::SourceLine(SectionSourceLine {
                        src: src_lines[1].clone(),
                        mark: LineMark {
                            level: Level::Note,
                            col: Some(col_2),
                            // Label at last source line
                            label: Some("test label".into()),
                        }
                    }),
                ],
            }
        );
    }

    #[test]
    fn section_from_mspan_resolved_no_label() {
        let ctx = Context::from("mspan/sec-no-label");
        let span = ctx.span(3, 4);

        assert_eq!(
            Section::from(MaybeResolvedSpan::Resolved(
                StubResolvedSpan {
                    context: Some(ctx),
                    line_num: Some(2.unwrap_into()),
                    col_num: Some(Column::Endpoints(
                        1.unwrap_into(),
                        2.unwrap_into()
                    )),
                    first_line_span: Some(DUMMY_SPAN),
                    span: Some(span),
                    src_lines: None,
                },
                None,
            )),
            Section {
                heading: SpanHeading(
                    ctx,
                    HeadingLineNum::Resolved(
                        2.unwrap_into(),
                        HeadingColNum::Resolved(Column::Endpoints(
                            1.unwrap_into(),
                            2.unwrap_into()
                        ))
                    )
                ),
                span,
                // Level is normally derived from the label,
                //   so in this case it gets defaulted.
                level: Level::default(),
                body: vec![],
            }
        );
    }

    #[test]
    fn section_from_mspan_unresolved() {
        let ctx = Context::from("mspan/sec-unresolved");
        let span = ctx.span(2, 3);

        let mspan = MaybeResolvedSpan::Unresolved::<StubResolvedSpan>(
            span,
            Some(SpanLabel(Level::Note, "test label".into())),
            SpanResolverError::Io(io::ErrorKind::NotFound),
        );

        assert_eq!(
            Section::from(mspan),
            Section {
                heading: SpanHeading(ctx, HeadingLineNum::Unresolved(span),),
                span,
                level: Level::Note,
                body: vec![
                    SectionLine::Footnote(SpanLabel(
                        Level::Note,
                        "test label".into()
                    )),
                    SectionLine::Footnote(SpanLabel(
                        Level::Help,
                        // This hard-coding is not ideal,
                        //   as it makes the test fragile.
                        format!(
                            "an error occurred while trying to look up \
                             information about this span: {}",
                            io::ErrorKind::NotFound
                        )
                        .into()
                    )),
                ],
            }
        );
    }

    #[test]
    fn section_footnote_into_footnote() {
        assert_eq!(
            SectionLine::Footnote(SpanLabel(
                Level::Note,
                "test footnote".into()
            ))
            .into_footnote(),
            Some(SectionLine::Footnote(SpanLabel(
                Level::Note,
                "test footnote".into()
            ))),
        );
    }

    #[test]
    fn section_src_line_with_label_into_footnote() {
        assert_eq!(
            SectionLine::SourceLine(SectionSourceLine {
                src: SourceLine::new_stub(
                    1.unwrap_into(),
                    None,
                    DUMMY_SPAN,
                    "discarded".into()
                ),
                mark: LineMark {
                    level: Level::Help,
                    col: None,
                    label: Some("kept label".into())
                }
            })
            .into_footnote(),
            Some(SectionLine::Footnote(SpanLabel(
                Level::Help,
                "kept label".into()
            ))),
        );
    }

    #[test]
    fn section_src_line_without_label_into_footnote() {
        assert_eq!(
            SectionLine::SourceLine(SectionSourceLine {
                src: SourceLine::new_stub(
                    1.unwrap_into(),
                    None,
                    DUMMY_SPAN,
                    "discarded".into()
                ),
                mark: LineMark {
                    level: Level::Help,
                    col: None,
                    label: None,
                }
            })
            .into_footnote(),
            None
        );
    }

    // TODO: Section squashing is currently only covered by integration
    //   tests!

    // TODO: Most `Display::fmt` only covered by integration tests!
}
