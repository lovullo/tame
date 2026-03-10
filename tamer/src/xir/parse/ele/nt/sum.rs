// XIR element parser generator sum nonterminal
//
//  Copyright (C) 2014-2026 Ryan Specialty, LLC.
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

use super::{Nt, NtExpectKind};

use crate::{
    fmt::{DisplayFn, TtQuote},
    parse::prelude::*,
    span::Span,
    xir::{
        EleSpan, OpenSpan, QName,
        flat::{Depth, RefinedText, XirfToken},
        parse::{SuperState, SuperStateContext},
    },
};

use std::{fmt::Display, marker::PhantomData};

/// Sum nonterminal.
pub trait SumNt: Nt {
    fn fmt_matches_top(f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

/// States for sum nonterminals.
///
/// Sum NTs act like a sum type,
///   transitioning to the appropriate inner NT based on the next token of
///   input.
/// Sum NTs have order-based precedence when faced with ambiguity,
///   like a PEG.
///
/// This is expected to be wrapped by a newtype for each Sum NT,
///   and does not implement [`ParseState`] itself.
#[derive(Debug, PartialEq, Eq)]
pub enum SumNtState<NT: SumNt> {
    /// Expecting an opening tag for an element.
    Expecting(NtExpectKind),

    /// Recovery state ignoring all remaining tokens for this
    ///   element.
    RecoverEleIgnore(QName, OpenSpan, Depth, PhantomData<NT>),
}

impl<NT: SumNt> SumNtState<NT> {
    /// Whether the parser is in a state that can tolerate
    ///   superstate node preemption.
    pub fn can_preempt_node(&self) -> bool {
        use SumNtState::*;

        match self {
            Expecting(preempt) => preempt.can_preempt_node(),

            // Preemption during recovery would cause tokens to
            //   be parsed when they ought to be ignored,
            //     so we must process all tokens during recovery.
            RecoverEleIgnore(..) => false,
        }
    }
}

impl<NT: SumNt> Display for SumNtState<NT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SumNtState::*;

        match self {
            Expecting(preempt) => {
                write!(f, "expecting {preempt} ")?;
                NT::fmt_matches_top(f)
            }

            RecoverEleIgnore(name, _, _, _) => {
                write!(
                    f,
                    "attempting to recover by ignoring element \
                    with unexpected name {given} \
                    (expected",
                    given = TtQuote::wrap(name),
                )?;

                NT::fmt_matches_top(f)?;
                f.write_str(")")
            }
        }
    }
}

impl<NT: SumNt> ParseState for SumNtState<NT>
where
    Self: Into<NT::NtSuper>,
    <NT as Nt>::ParseError: From<SumNtError<NT>>,
{
    type Token = XirfToken<RefinedText>;
    type Object = <Self::Super as ParseState>::Object;
    type Error = <NT as Nt>::ParseError;
    type Context = SuperStateContext<NT::NtSuper>;
    type Super = <NT as Nt>::NtSuper;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NtExpectKind::*;
        use SumNtState::*;

        match (self, tok) {
            (
                Expecting(NonPreemptable),
                tok @ XirfToken::Open(qname, span, depth),
            ) => {
                NT::matches(qname)
                    .map(|nt| {
                        stack.transfer_with_ret(
                            Transition(Expecting(Preemptable)),
                            // Propagate non-preemption status,
                            //   otherwise we'll provide a lookback of
                            //   the original token and end up recursing
                            //   until we hit the `stack` limit.
                            Transition(nt.expect_non_preemptable())
                                .incomplete()
                                .with_lookahead(tok),
                        )
                    })
                    .unwrap_or_else(|| {
                        // Since we're non-preemptable,
                        //   we're expected to be able to process this token
                        //   or fail trying.
                        Transition(RecoverEleIgnore(
                            qname,
                            span,
                            depth,
                            Default::default(),
                        ))
                        .err(
                            // Use name span rather than full `OpenSpan`
                            //   since it's specifically the name that was
                            //   unexpected,
                            //     not the fact that it's an element.
                            SumNtError::UnexpectedEle(
                                qname,
                                span.name_span(),
                                Default::default(),
                            ),
                        )
                    })
            }

            (Expecting(Preemptable), tok @ XirfToken::Open(qname, ..)) => {
                NT::matches(qname)
                    .map(|nt| {
                        stack.transfer_with_ret(
                            Transition(Expecting(Preemptable)),
                            // note: this clone is just because the
                            //   borrow checker can't prove a single use
                            //   between this closure and below; it should
                            //   optimize away
                            Transition(nt)
                                .incomplete()
                                .with_lookahead(tok.clone()),
                        )
                    })
                    .unwrap_or_else(
                        // An unexpected token ends repetition
                        //   and should not result in an error.
                        || Transition(Expecting(Preemptable)).dead(tok),
                    )
            }

            // An unexpected token when repeating ends repetition
            //   and should not result in an error.
            (Expecting(..), tok) => {
                Transition(Expecting(Preemptable)).dead(tok)
            }

            // XIRF ensures that the closing tag matches the opening,
            //   so we need only check depth.
            (
                RecoverEleIgnore(_, _, depth_open, _),
                XirfToken::Close(_, _, depth_close),
            ) if depth_open == depth_close => {
                Transition(Expecting(Preemptable)).incomplete()
            }

            (st @ RecoverEleIgnore(..), _) => Transition(st).incomplete(),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, SumNtState::Expecting(..))
    }
}

/// Error during parsing of a sum nonterminal.
#[derive(Debug, PartialEq)]
pub enum SumNtError<NT: SumNt> {
    UnexpectedEle(QName, Span, PhantomData<NT>),
}

impl<NT: SumNt> Error for SumNtError<NT> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl<NT: SumNt> Display for SumNtError<NT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::xir::fmt::TtOpenXmlEle;

        match self {
            Self::UnexpectedEle(qname, _, _) => {
                write!(f, "unexpected {}", TtOpenXmlEle::wrap(qname))
            }
        }
    }
}

impl<NT: SumNt> Diagnostic for SumNtError<NT> {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan<'_>> {
        // Note that we should place expected values in the help
        //   footnote rather than the span label because it can
        //   get rather long.
        // Maybe in the future the diagnostic renderer can be
        //   smart about that based on the terminal width and
        //   automatically move into the footer.
        match self {
            Self::UnexpectedEle(qname, span, _) => span
                .error(format!(
                    "element {name} cannot appear here",
                    name = TtQuote::wrap(qname),
                ))
                .with_help(format!(
                    "expecting {}",
                    DisplayFn(NT::fmt_matches_top)
                ))
                .into(),
        }
    }
}
