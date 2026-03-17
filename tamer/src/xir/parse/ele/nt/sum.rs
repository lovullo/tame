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

use super::{Nt, NtExpectKind, NtMeta};

use crate::{
    fmt::{DisplayFn, TtQuote},
    parse::prelude::*,
    span::Span,
    xir::{
        EleSpan, QName,
        flat::{RefinedText, XirfToken},
        parse::{SuperState, SuperStateContext},
    },
};

use std::{fmt::Display, marker::PhantomData};

/// Sum nonterminal.
pub trait SumNt: Nt {
    /// Format all constituent matchers for display.
    fn fmt_matches_all(f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Self::fmt_matches(Self::matches_n().saturating_sub(1), &mut 0, f)
    }
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
    RecoverEleIgnore(NtMeta, PhantomData<NT>),

    /// Closing tag of the NT we delegated to has been found and the parsing
    ///   of the NT is complete;
    ///     this parser must not parse another element.
    ///
    /// This indicates that the parser was used in an ephemeral context for
    ///   parsing of a single element and must yield to the prior state.
    ClosedEphemeral,
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

            // Upon completion of an ephemeral parse,
            //   we want to force ourselves to handle the next token so that
            //   we can produce a dead state to trigger a stack return.
            //
            // To understand this,
            //   assume that we have just completed a preemption parse,
            //   and the superstate just encountered another preemption
            //   token.
            // The superstate consults this function to see if preemption is
            //   permitted within this context.
            // If we were to return `true`,
            //   then every sibling preemption would continue to grow the
            //   stack;
            //     if there are enough siblings,
            //       we would eventually hit the parsing stack limit and
            //       fail.
            ClosedEphemeral => false,
        }
    }
}

impl<NT: SumNt> Display for SumNtState<NT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SumNtState::*;

        match self {
            Expecting(preempt) => {
                write!(f, "expecting {preempt} ")?;
                NT::fmt_matches_all(f)
            }

            RecoverEleIgnore(meta, _) => {
                write!(
                    f,
                    "attempting to recover by ignoring element \
                    with unexpected name {given} \
                    (expected",
                    given = TtQuote::wrap(meta.qname),
                )?;

                NT::fmt_matches_all(f)?;
                f.write_str(")")
            }

            ClosedEphemeral => {
                write!(f, "done parsing single element (ephemeral) of ")?;
                NT::fmt_matches_all(f)
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
                Expecting(kind @ (NonPreemptable | PreemptedParsing)),
                tok @ XirfToken::Open(qname, ospan, depth),
            ) => {
                NT::matches(qname)
                    .map(|nt| {
                        stack.transfer_with_ret(
                            // Preemptions trigger an ephemeral parse
                            //   (we stop after a single match)
                            match kind {
                                PreemptedParsing => Transition(ClosedEphemeral),
                                _ => Transition(Expecting(Preemptable)),
                            },
                            // Propagate non-preemption status,
                            //   otherwise we'll provide a lookback of
                            //   the original token and end up recursing
                            //   until we hit the `stack` limit.
                            Transition(nt.expect_kind(kind))
                                .incomplete()
                                .with_lookahead(tok),
                        )
                    })
                    .unwrap_or_else(|| {
                        // Since we're non-preemptable,
                        //   we're expected to be able to process this token
                        //   or fail trying.
                        Transition(RecoverEleIgnore(
                            NtMeta {
                                qname,
                                ospan,
                                depth,
                                caused_preemption: kind.into(),
                            },
                            Default::default(),
                        ))
                        .err(
                            // Use name span rather than full `OpenSpan`
                            //   since it's specifically the name that was
                            //   unexpected,
                            //     not the fact that it's an element.
                            SumNtError::UnexpectedEle(
                                qname,
                                ospan.name_span(),
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
                            Transition(nt.expect_kind(Preemptable))
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
            // The explicit match of each variant is intentional to force
            //   attention on this parser when variants change.
            (
                Expecting(NonPreemptable | Preemptable | PreemptedParsing),
                tok,
            ) => Transition(Expecting(Preemptable)).dead(tok),

            // Ephemeral parses must conclude after a single element.
            (st @ ClosedEphemeral, tok) => Transition(st).dead(tok),

            // XIRF ensures that the closing tag matches the opening,
            //   so we need only check depth.
            (
                RecoverEleIgnore(
                    NtMeta {
                        depth: depth_open, ..
                    },
                    _,
                ),
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
                    DisplayFn(NT::fmt_matches_all)
                ))
                .into(),
        }
    }
}
