// Lower NIR into AIR
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

//! Lower [NIR](super) into [AIR](crate::asg::air).

use super::Nir;
use crate::{
    asg::air::Air,
    diagnose::{Annotate, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::prelude::*,
    span::Span,
};
use arrayvec::ArrayVec;
use std::{error::Error, fmt::Display};

// These are also used by the `test` module which imports `super`.
#[cfg(feature = "wip-asg-derived-xmli")]
use crate::{
    asg::ExprOp,
    nir::{Nir::*, NirEntity::*},
    sym::{st::raw::U_TRUE, SymbolId},
};

#[derive(Debug, PartialEq, Eq, Default)]
pub enum NirToAir {
    #[default]
    Ready,

    /// Predicate opened but its subject is not yet known.
    PredOpen(Span),

    /// A predicate has been partially applied to its subject,
    ///   but we do not yet know its function or comparison value.
    PredPartial(Span, SPair),

    /// Processing a metavariable.
    Meta(Span),
}

impl Display for NirToAir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAir::*;

        match self {
            Ready => write!(f, "ready to lower NIR to AIR"),
            PredOpen(_) => {
                write!(f, "awaiting information about open predicate")
            }
            PredPartial(_, name) => write!(
                f,
                "waiting to determine type of predicate for identifier {}",
                TtQuote::wrap(name),
            ),
            Meta(_) => write!(f, "parsing metavariable definition"),
        }
    }
}

/// Stack of [`Air`] objects to yield on subsequent iterations.
type ObjStack = ArrayVec<Air, 2>;

/// The symbol to use when lexically expanding shorthand notations to
///   compare against values of `1`.
#[cfg(feature = "wip-asg-derived-xmli")]
pub const SYM_TRUE: SymbolId = U_TRUE;

impl ParseState for NirToAir {
    type Token = Nir;
    type Object = Air;
    type Error = NirToAirError;
    type Context = ObjStack;

    #[cfg(not(feature = "wip-asg-derived-xmli"))]
    fn parse_token(
        self,
        tok: Self::Token,
        _queue: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;

        let _ = tok; // prevent `unused_variables` warning
        Transition(Ready).ok(Air::Todo(crate::span::UNKNOWN_SPAN))
    }

    #[cfg(feature = "wip-asg-derived-xmli")]
    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;
        use NirToAirError::*;

        if let Some(obj) = stack.pop() {
            return Transition(Ready).ok(obj).with_lookahead(tok);
        }

        // TODO: The intent is to refactor this monstrosity once we're far
        //     enough along that a clear pattern emerges.
        //   Part of this process has been deriving appropriate
        //     responsibilities betwen XIR->NIR, NIR->AIR, and AIR->ASG.
        match (self, tok) {
            (Ready, Open(Package, span)) => {
                // TODO: Package name needs to be generated and provided to us;
                //   this is transitionary.
                Transition(Ready)
                    .ok(Air::PkgStart(span, SPair("/TODO".into(), span)))
            }

            (Ready, Close(Package, span)) => {
                Transition(Ready).ok(Air::PkgEnd(span))
            }

            (Ready, Open(Rate | Sum, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Sum, span))
            }
            (Ready, Open(Product, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Product, span))
            }
            (Ready, Open(Ceil, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Ceil, span))
            }
            (Ready, Open(Floor, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Floor, span))
            }
            (Ready, Open(Classify | All, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Conj, span))
            }
            (Ready, Open(Any, span)) => {
                Transition(Ready).ok(Air::ExprStart(ExprOp::Disj, span))
            }

            // Match
            (Ready, Open(Match, span)) => {
                Transition(PredOpen(span)).incomplete()
            }
            (PredOpen(ospan), RefSubject(on)) => {
                Transition(PredPartial(ospan, on)).incomplete()
            }
            (PredPartial(ospan, on), Ref(value)) => {
                stack.push(Air::RefIdent(value));
                stack.push(Air::RefIdent(on));
                Transition(Ready).ok(Air::ExprStart(ExprOp::Eq, ospan))
            }
            (PredPartial(ospan, on), Close(Match, cspan)) => {
                stack.push(Air::RefIdent(SPair(SYM_TRUE, ospan)));
                stack.push(Air::RefIdent(on));
                Transition(Ready)
                    .ok(Air::ExprStart(ExprOp::Eq, ospan))
                    .with_lookahead(Close(Match, cspan))
            }
            // Special case of the general error below,
            //   since recovery here involves discarding the nonsense match.
            (PredOpen(ospan), Close(Match, span)) => Transition(Ready)
                .err(MatchSubjectExpected(ospan, Close(Match, span))),
            (PredOpen(ospan), tok) => Transition(PredOpen(ospan))
                .err(MatchSubjectExpected(ospan, tok)),
            (Ready, Close(Match, cspan)) => {
                Transition(Ready).ok(Air::ExprEnd(cspan))
            }
            (PredPartial(ospan, on), tok) => {
                // TODO: Until match body is supported,
                //   error and discard tokens.
                Transition(PredPartial(ospan, on))
                    .err(TodoMatchBody(ospan, tok.span()))
            }

            (Ready, Open(Tpl, span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }
            (Ready, Close(Tpl, span)) => {
                Transition(Ready).ok(Air::TplEnd(span))
            }

            (Ready, Open(TplApply, span)) => {
                Transition(Ready).ok(Air::TplStart(span))
            }

            // Short-hand template application must be handled through
            //   desugaring as part of the lowering pipeline,
            //     so that it is converted to long form before getting here.
            (
                Ready,
                Open(TplApplyShort(..) | TplParamShort(..), span)
                | Close(TplApplyShort(..) | TplParamShort(..), span),
            ) => {
                // TODO: In the future maybe TAMER will have evolved its
                //   abstractions enough that there's an ROI for prohibiting
                //   this at the type level.
                diagnostic_panic!(
                    vec![
                        span.internal_error(
                            "attempted shorthand template application"
                        ),
                        span.help(
                            "TAMER must be compiled with support for \
                               shorthand template application by utilizing the \
                               nir::tplshort module in the lowering pipeline."
                        )
                    ],
                    "shortand template application is unsupported in this \
                         build of TAMER"
                )
            }
            (Ready, Close(TplApply, span)) => {
                Transition(Ready).ok(Air::TplEndRef(span))
            }

            (Ready, Open(TplParam, span)) => {
                Transition(Meta(span)).ok(Air::MetaStart(span))
            }
            (Meta(mspan), Text(lexeme)) => {
                Transition(Meta(mspan)).ok(Air::MetaLexeme(lexeme))
            }
            (Ready | Meta(_), Close(TplParam, span)) => {
                Transition(Ready).ok(Air::MetaEnd(span))
            }
            // Some of these will be permitted in the future.
            (
                Meta(mspan),
                tok @ (Open(..) | Close(..) | Ref(..) | RefSubject(..)
                | Desc(..)),
            ) => Transition(Meta(mspan))
                .err(NirToAirError::UnexpectedMetaToken(mspan, tok)),

            (Ready, Text(text)) => Transition(Ready).ok(Air::DocText(text)),

            (
                Ready,
                Close(
                    Rate | Sum | Product | Ceil | Floor | Classify | All | Any,
                    span,
                ),
            ) => Transition(Ready).ok(Air::ExprEnd(span)),

            (st @ (Ready | Meta(_)), BindIdent(spair)) => {
                Transition(st).ok(Air::BindIdent(spair))
            }
            (Ready, Ref(spair) | RefSubject(spair)) => {
                Transition(Ready).ok(Air::RefIdent(spair))
            }

            (Ready, Desc(clause)) => {
                Transition(Ready).ok(Air::DocIndepClause(clause))
            }

            (Ready, Import(namespec)) => {
                Transition(Ready).ok(Air::PkgImport(namespec))
            }

            // Shouldn't happen in  practice because nir::parse will not
            //   produce this.
            // This assumption is only valid so long as that's the only
            //   thing producing NIR.
            (st @ Meta(..), tok @ Import(_)) => Transition(st).dead(tok),

            (_, tok @ (Todo(..) | TodoAttr(..))) => {
                crate::diagnostic_todo!(
                    vec![tok.internal_error(
                        "this token is not yet supported in TAMER"
                    )],
                    "unsupported token: {tok}",
                )
            }

            (st, Noop(_)) => Transition(st).incomplete(),
        }
    }

    fn is_accepting(&self, stack: &Self::Context) -> bool {
        matches!(self, Self::Ready) && stack.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NirToAirError {
    /// Expected a match subject,
    ///   but encountered some other token.
    ///
    /// TODO: "match subject" is not familiar terminology to the user;
    ///   we'll want to introduce a layer permitting XML-specific augmenting
    ///   with `@on` when derived from an XML source.
    MatchSubjectExpected(Span, Nir),

    /// Match body is not yet supported.
    TodoMatchBody(Span, Span),

    /// The provided [`Nir`] token of input was unexpected for the body of a
    ///   metavariable that was opened at the provided [`Span`].
    UnexpectedMetaToken(Span, Nir),
}

impl Display for NirToAirError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAirError::*;

        match self {
            MatchSubjectExpected(_, nir) => {
                write!(f, "expected match subject, found {nir}")
            }

            TodoMatchBody(_, _) => {
                write!(f, "match body is not yet supported by TAMER")
            }

            UnexpectedMetaToken(_, tok) => {
                write!(
                    f,
                    "expected lexical token for metavariable, found {tok}"
                )
            }
        }
    }
}

impl Error for NirToAirError {}

// TODO: We need to be able to augment with useful context,
//   e.g. XML suggestions.
impl Diagnostic for NirToAirError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use NirToAirError::*;

        match self {
            MatchSubjectExpected(ospan, given) => vec![
                ospan.note("for this match"),
                given
                    .span()
                    .error("comparison value provided before subject"),
            ],

            TodoMatchBody(ospan, given) => vec![
                ospan.note("for this match"),
                given.error(
                    "tokens in match body are not yet supported by TAMER",
                ),
            ],

            // The user should have been preempted by the parent parser
            //   (e.g. XML->Nir),
            //     and so shouldn't see this.
            UnexpectedMetaToken(mspan, given) => vec![
                mspan.note("while parsing the body of this metavariable"),
                given.span().error("expected a lexical token here"),
            ],
        }
    }
}

#[cfg(all(test, feature = "wip-asg-derived-xmli"))]
mod test;
