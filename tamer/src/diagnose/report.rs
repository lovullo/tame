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
    resolver::{Column, ResolvedSpanData, SpanResolver, SpanResolverError},
    AnnotatedSpan, Diagnostic, Label, Level,
};
use crate::span::{Context, Span, UNKNOWN_SPAN};
use std::{
    fmt::{self, Display},
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
    fn render<'l>(&mut self, diagnostic: &'l impl Diagnostic) -> Report<'l>;
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
    fn render<'l>(&mut self, diagnostic: &'l impl Diagnostic) -> Report<'l> {
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

        let message = Message(diagnostic.to_string());

        let mut report = Report::empty(message);
        report.extend(mspans.into_iter().map(Into::into));
        report
    }
}

#[derive(Debug)]
pub struct Report<'l> {
    msg: Message,
    secs: Vec<Section<'l>>,
    level: Level,
}

impl<'l> Report<'l> {
    fn empty(msg: Message) -> Self {
        Self {
            msg,
            secs: Vec::new(),
            level: Level::default(),
        }
    }
}

impl<'l> Extend<Section<'l>> for Report<'l> {
    fn extend<T: IntoIterator<Item = Section<'l>>>(&mut self, secs: T) {
        for sec in secs {
            self.level = self.level.min(sec.level());
            self.secs.push(sec.consider_squash(self.secs.last()));
        }
    }
}

impl<'l> Display for Report<'l> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{level}: {msg}\n", level = self.level, msg = self.msg)?;
        self.secs.iter().try_for_each(|sec| sec.fmt(f))
    }
}

#[derive(Debug)]
struct Message(String);

impl Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
enum Section<'l> {
    /// Section is delimited from surrounding sections with a heading.
    Delimited(SpanHeading, SystemLabels, Option<SpanLabel<'l>>, Span),

    /// Section is squashed into the preceding section by eliding its
    ///   heading.
    ///
    /// This term originates from `git rebase` for a similar operation.
    Squashed(Option<SpanLabel<'l>>, Span),
}

impl<'s, 'l> Section<'l> {
    fn level(&self) -> Level {
        match self {
            Self::Delimited(_, _, olabel, _) | Self::Squashed(olabel, _) => {
                olabel
                    .as_ref()
                    .map(|label| label.level())
                    .unwrap_or(Level::default())
            }
        }
    }

    fn unresolved_span(&self) -> Span {
        match self {
            Self::Delimited(.., span) | Self::Squashed(.., span) => *span,
        }
    }

    fn consider_squash(self, prev: Option<&Self>) -> Self {
        match (prev, self) {
            (Some(prev), Self::Delimited(.., olabel, span))
                if prev.unresolved_span() == span =>
            {
                Self::Squashed(olabel, span)
            }
            (_, orig) => orig,
        }
    }
}

impl<'l, 'a, S> From<MaybeResolvedSpan<'l, S>> for Section<'l>
where
    S: ResolvedSpanData,
{
    fn from(mspan: MaybeResolvedSpan<'l, S>) -> Self {
        let heading = SpanHeading::from(&mspan);
        let syslabels = mspan.system_labels();

        match mspan {
            MaybeResolvedSpan::Resolved(rspan, label) => Section::Delimited(
                heading,
                syslabels,
                label,
                rspan.unresolved_span(),
            ),

            MaybeResolvedSpan::Unresolved(span, label, _) => {
                Section::Delimited(heading, syslabels, label, span)
            }
        }
    }
}

impl<'l> Display for Section<'l> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let olabel = match self {
            Self::Delimited(heading, syslabels, olabel, _) => {
                write!(f, "  {heading}\n")?;
                syslabels.fmt(f)?;
                olabel
            }
            Self::Squashed(olabel, _) => olabel,
        };

        if let Some(label) = olabel {
            write!(f, "{label}\n")?;
        }

        Ok(())
    }
}

#[derive(Debug)]
struct SystemLabels(Vec<SpanLabel<'static>>);

impl Display for SystemLabels {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().try_for_each(|label| write!(f, "{label}\n"))
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
enum MaybeResolvedSpan<'l, S: ResolvedSpanData> {
    Resolved(S, Option<SpanLabel<'l>>),
    Unresolved(Span, Option<SpanLabel<'l>>, SpanResolverError),
}

impl<'l, S: ResolvedSpanData> MaybeResolvedSpan<'l, S> {
    /// We should never mask an error with our own;
    ///   the diagnostic system is supposed to _help_ the user in diagnosing
    ///   problems,
    ///     not hinder them by masking it.
    fn system_labels(&self) -> SystemLabels {
        SystemLabels(match self {
            Self::Resolved(rspan, _) if rspan.col_num().is_none() => vec![
                SpanLabel(
                    Level::Help,
                    "unable to calculate columns because the line is \
                        not a valid UTF-8 string"
                        .into(),
                ),
                SpanLabel(
                    Level::Help,
                    "you have been provided with 0-indexed \
                        line-relative inclusive byte offsets"
                        .into(),
                ),
            ],

            Self::Unresolved(_, _, e) => {
                vec![SpanLabel(
                    Level::Help,
                    format!(
                        "there was an error trying to look up \
                         information about this span: {e}"
                    )
                    .into(),
                )]
            }

            _ => vec![],
        })
    }
}

/// Heading describing the context of a (hopefully resolved) span.
///
/// The ideal header contains the context along with the line, and column
///   numbers,
///     visually distinguishable from surrounding lines to allow the user to
///     quickly skip between reports.
#[derive(Debug)]
struct SpanHeading(Context, HeadingLineNum);

impl Display for SpanHeading {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ctx, line) = self;
        write!(f, "--> {ctx}{line}")
    }
}

impl<'s, 'l, S> From<&'s MaybeResolvedSpan<'l, S>> for SpanHeading
where
    S: ResolvedSpanData,
{
    /// Span header containing the (hopefully resolved) context.
    fn from(mspan: &'s MaybeResolvedSpan<'l, S>) -> Self {
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
#[derive(Debug)]
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
#[derive(Debug)]
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
struct SpanLabel<'l>(Level, Label<'l>);

impl<'l> SpanLabel<'l> {
    fn level(&self) -> Level {
        self.0
    }
}

impl<'l> Display for SpanLabel<'l> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(level, label) = self;
        write!(f, "      {level}: {label}")
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
    use std::num::NonZeroU32;

    mod integration;

    #[derive(Default)]
    struct StubResolvedSpan {
        span: Option<Span>,
        first_line_span: Option<Span>,
        line_num: Option<NonZeroU32>,
        col_num: Option<Column>,
        context: Option<Context>,
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
    fn span_heading_from_mspan() {
        let ctx = Context::from("mspan/header");

        assert_eq!(
            format!(
                "{}",
                SpanHeading::from(&MaybeResolvedSpan::Resolved(
                    StubResolvedSpan {
                        context: Some(ctx),
                        line_num: Some(1.unwrap_into()),
                        col_num: Some(Column::Endpoints(
                            2.unwrap_into(),
                            3.unwrap_into()
                        )),
                        first_line_span: Some(DUMMY_SPAN),
                        span: Some(DUMMY_SPAN),
                    },
                    None
                ))
            ),
            "--> mspan/header:1:2",
        );

        assert_eq!(
            format!(
                "{}",
                SpanHeading::from(
                    &MaybeResolvedSpan::<StubResolvedSpan>::Unresolved(
                        ctx.span(0, 0),
                        None,
                        SpanResolverError::OutOfRange(0),
                    )
                )
            ),
            "--> mspan/header offset 0--0",
        );
    }
}
