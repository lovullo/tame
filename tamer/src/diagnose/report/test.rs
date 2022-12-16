// Diagnostic system rendering tests
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

use super::*;
use crate::{
    convert::ExpectInto,
    diagnose::resolve::Column,
    span::dummy::{DUMMY_CONTEXT, DUMMY_SPAN},
};
use std::{io, num::NonZeroU32};

// NB: Integration tests exist in this module,
//   and test against exact output of this system;
//     you should use them for prototyping changes to be sure you understand
//     how output will be considered in various circumstances.
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
            Level::Note,
            Some("test label".into()),
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
                SectionLine::SourceLinePadding,
                SectionLine::SourceLine(src_lines[0].clone().into()),
                SectionLine::SourceLineMark(LineMark {
                    level: Level::Note,
                    col: col_1,
                    // Label goes on the last source line.
                    label: None,
                }),
                SectionLine::SourceLinePadding,
                // Will be stripped during finalization
                SectionLine::SourceLinePadding,
                SectionLine::SourceLine(src_lines[1].clone().into()),
                SectionLine::SourceLineMark(LineMark {
                    level: Level::Note,
                    col: col_2,
                    // Label at last source line
                    label: Some("test label".into()),
                }),
                // Will be stripped during finalization
                SectionLine::SourceLinePadding,
            ],
            line_max: 2.unwrap_into(),
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
            Level::Note,
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
            level: Level::Note,
            body: vec![],
            // Line comes from `src_lines`.
            line_max: 1.unwrap_into(),
        }
    );
}

#[test]
fn section_from_mspan_unresolved() {
    let ctx = Context::from("mspan/sec-unresolved");
    let span = ctx.span(2, 3);

    let mspan = MaybeResolvedSpan::Unresolved::<StubResolvedSpan>(
        span,
        Level::Note,
        Some("test label".into()),
        SpanResolverError::Io(io::ErrorKind::NotFound),
    );

    assert_eq!(
        Section::from(mspan),
        Section {
            heading: SpanHeading(ctx, HeadingLineNum::Unresolved(span),),
            span,
            level: Level::Note,
            body: vec![
                SectionLine::Footnote(Level::Note, "test label".into()),
                SectionLine::Footnote(
                    Level::Help,
                    // This hard-coding is not ideal,
                    //   as it makes the test fragile.
                    format!(
                        "an error occurred while trying to look up \
                         information about this span: {}",
                        io::ErrorKind::NotFound
                    )
                    .into()
                ),
            ],
            line_max: 1.unwrap_into(),
        }
    );
}

#[test]
fn section_footnote_into_footnote() {
    assert_eq!(
        SectionLine::Footnote(Level::Note, "test footnote".into())
            .into_footnote(),
        Some(SectionLine::Footnote(Level::Note, "test footnote".into())),
    );
}

#[test]
fn section_src_line_into_footnote() {
    assert_eq!(
        SectionLine::SourceLine(
            SourceLine::new_stub(
                1.unwrap_into(),
                None,
                DUMMY_SPAN,
                "discarded".into()
            )
            .into()
        )
        .into_footnote(),
        None
    );
}

#[test]
fn section_mark_with_label_into_footnote() {
    assert_eq!(
        SectionLine::SourceLineMark(LineMark {
            level: Level::Help,
            col: Column::Before(1.unwrap_into()),
            label: Some("kept label".into())
        })
        .into_footnote(),
        Some(SectionLine::Footnote(Level::Help, "kept label".into())),
    );
}

#[test]
fn section_mark_without_label_into_footnote() {
    assert_eq!(
        SectionLine::SourceLineMark(LineMark {
            level: Level::Help,
            col: Column::Before(1.unwrap_into()),
            label: None,
        })
        .into_footnote(),
        None
    );
}

// TODO: Section squashing is currently only covered by integration
//   tests!

// TODO: Most `Display::fmt` only covered by integration tests!
