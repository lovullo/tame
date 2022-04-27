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
    fmt::{self, Display, Write},
    num::NonZeroU32,
};

pub trait Reporter {
    /// Render diagnostic report.
    ///
    /// Please be mindful of where this report is being rendered `to`.
    /// For example,
    ///   if rendering to standard out,
    ///   it is a good idea to buffer the entire report before flushing to
    ///     stdout,
    ///       otherwise the report may become interleaved with other
    ///       concurrent processes
    ///         (e.g. if TAMER is being invoked using `make -jN`).
    fn render(
        &mut self,
        diagnostic: &impl Diagnostic,
        to: &mut impl Write,
    ) -> Result<(), fmt::Error>;

    /// Render a diagnostic report into an owned [`String`].
    ///
    /// This invokes [`Reporter::render`] on a newly allocated [`String`].
    fn render_to_string(
        &mut self,
        diagnostic: &impl Diagnostic,
    ) -> Result<String, fmt::Error> {
        let mut str = String::new();
        self.render(diagnostic, &mut str)?;
        Ok(str)
    }
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
    // _TODO: This is a proof-of-concept._
    fn render(
        &mut self,
        diagnostic: &impl Diagnostic,
        to: &mut impl Write,
    ) -> fmt::Result {
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
        let mut report = DefaultReport::empty(message);

        report.extend(mspans.iter().map(Into::into));

        write!(to, "{}", report)
    }
}

type DefaultReport<'s, 'l> = Report<'s, 'l, HeadingLineNum>;

#[derive(Debug)]
struct Report<'s, 'l, L: Display> {
    msg: Message,
    secs: Vec<Section<'s, 'l, L>>,
    level: Level,
}

impl<'s, 'l, L: Display> Report<'s, 'l, L> {
    fn empty(msg: Message) -> Self {
        Self {
            msg,
            secs: Vec::new(),
            level: Level::default(),
        }
    }
}

impl<'s, 'l, L: Display> Extend<Section<'s, 'l, L>> for Report<'s, 'l, L> {
    fn extend<T: IntoIterator<Item = Section<'s, 'l, L>>>(&mut self, secs: T) {
        for sec in secs {
            self.level = self.level.min(sec.level());
            self.secs.push(sec.consider_squash(self.secs.last()));
        }
    }
}

impl<'s, 'l, L: Display> Display for Report<'s, 'l, L> {
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
enum Section<'s, 'l, L: Display> {
    /// Section is delimited from surrounding sections with a heading.
    Delimited(
        SpanHeading<L>,
        SystemLabels,
        Option<&'s SpanLabel<'l>>,
        Span,
    ),

    /// Section is squashed into the preceding section by eliding its
    ///   heading.
    ///
    /// This term originates from `git rebase` for a similar operation.
    Squashed(Option<&'s SpanLabel<'l>>, Span),
}

impl<'s, 'l, L: Display> Section<'s, 'l, L> {
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

impl<'s, 'l, 'a, L, S> From<&'s MaybeResolvedSpan<'l, S>> for Section<'s, 'l, L>
where
    L: Display + From<&'s MaybeResolvedSpan<'l, S>>,
    S: ResolvedSpanData,
{
    fn from(mspan: &'s MaybeResolvedSpan<'l, S>) -> Self {
        Section::Delimited(
            SpanHeading::from(mspan),
            mspan.system_labels(),
            mspan.label(),
            mspan.unresolved_span(),
        )
    }
}

impl<'s, 'l, L: Display> Display for Section<'s, 'l, L> {
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
    fn unresolved_span(&self) -> Span {
        match self {
            Self::Resolved(rspan, ..) => rspan.unresolved_span(),
            Self::Unresolved(span, ..) => *span,
        }
    }

    fn label(&self) -> Option<&SpanLabel<'l>> {
        match self {
            Self::Resolved(_, olabel) | Self::Unresolved(_, olabel, _) => {
                olabel.as_ref()
            }
        }
    }

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
struct SpanHeading<L: Display>(Context, L);

impl<L: Display> Display for SpanHeading<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ctx, line) = self;
        write!(f, "--> {ctx}{line}")
    }
}

impl<'s, 'l, S, L> From<&'s MaybeResolvedSpan<'l, S>> for SpanHeading<L>
where
    S: ResolvedSpanData,
    L: Display + From<&'s MaybeResolvedSpan<'l, S>>,
{
    /// Span header containing the (hopefully resolved) context.
    fn from(mspan: &'s MaybeResolvedSpan<'l, S>) -> Self {
        match mspan {
            MaybeResolvedSpan::Resolved(rspan, _) => {
                SpanHeading(rspan.context(), L::from(mspan))
            }

            MaybeResolvedSpan::Unresolved(span, _, _) => {
                SpanHeading(span.context(), L::from(mspan))
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
    Resolved {
        line_num: NonZeroU32,
        col_num: Option<Column>,
        first_line_span: Span,
        unresolved_span: Span,
    },

    Unresolved(Span),
}

impl Display for HeadingLineNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Resolved {
                line_num,
                col_num,
                first_line_span,
                unresolved_span,
            } => {
                let col =
                    col_num.map(HeadingColNum::Resolved).unwrap_or_else(|| {
                        HeadingColNum::Unresolved {
                            unresolved_span: *unresolved_span,
                            first_line_span: *first_line_span,
                        }
                    });

                write!(f, ":{}{col}", line_num)
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

impl<'s, 'l, S: ResolvedSpanData> From<&'s MaybeResolvedSpan<'l, S>>
    for HeadingLineNum
{
    fn from(mspan: &'s MaybeResolvedSpan<S>) -> Self {
        match mspan {
            MaybeResolvedSpan::Resolved(rspan, _) => Self::Resolved {
                line_num: rspan.line_num(),
                col_num: rspan.col_num(),
                first_line_span: rspan.first_line_span(),
                unresolved_span: rspan.unresolved_span(),
            },

            MaybeResolvedSpan::Unresolved(span, _, _) => {
                Self::Unresolved(*span)
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
        convert::ExpectInto, diagnose::resolver::Column, span::DUMMY_CONTEXT,
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
        let sut = HeadingLineNum::Resolved {
            line_num: 5.unwrap_into(),
            col_num: Some(Column::Endpoints(3.unwrap_into(), 3.unwrap_into())),
            first_line_span: UNKNOWN_SPAN,
            unresolved_span: UNKNOWN_SPAN,
        };

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

    #[test]
    fn span_heading() {
        struct StubLine;

        impl Display for StubLine {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "[:stub line]")
            }
        }

        let ctx = "header".unwrap_into();
        let sut = SpanHeading(ctx, StubLine);

        assert_eq!("--> header[:stub line]", format!("{}", sut));
    }

    #[test]
    fn span_heading_from_mspan() {
        struct StubLine(String);

        impl Display for StubLine {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "[:stub {}]", self.0)
            }
        }

        impl<'s, 'l, S: ResolvedSpanData> From<&'s MaybeResolvedSpan<'l, S>>
            for StubLine
        {
            fn from(mspan: &'s MaybeResolvedSpan<S>) -> Self {
                match mspan {
                    MaybeResolvedSpan::Resolved(..) => Self("resolved".into()),
                    MaybeResolvedSpan::Unresolved(..) => {
                        Self("unresolved".into())
                    }
                }
            }
        }

        let ctx = Context::from("mspan/header");

        assert_eq!(
            format!(
                "{}",
                SpanHeading::<StubLine>::from(&MaybeResolvedSpan::Resolved(
                    StubResolvedSpan {
                        context: Some(ctx),
                        ..Default::default()
                    },
                    None
                ))
            ),
            "--> mspan/header[:stub resolved]",
        );

        assert_eq!(
            format!(
                "{}",
                SpanHeading::<StubLine>::from(&MaybeResolvedSpan::<
                    StubResolvedSpan,
                >::Unresolved(
                    ctx.span(0, 0),
                    None,
                    SpanResolverError::OutOfRange(0),
                ))
            ),
            "--> mspan/header[:stub unresolved]",
        );
    }
}
