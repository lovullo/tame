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
    span::{Span, UNKNOWN_SPAN},
    sym::{st::raw::U_TRUE, SymbolId},
};
use arrayvec::ArrayVec;
use std::{error::Error, fmt::Display};

// These are also used by the `test` module which imports `super`.
#[cfg(feature = "wip-asg-derived-xmli")]
use crate::{
    asg::ExprOp,
    nir::{Nir::*, NirEntity::*},
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
        }
    }
}

/// Stack of [`Air`] objects to yield on subsequent iterations.
type ObjStack = ArrayVec<Air, 2>;

/// The symbol to use when lexically expanding shorthand notations to
///   compare against values of `1`.
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
        Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN))
    }

    #[cfg(feature = "wip-asg-derived-xmli")]
    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;
        use NirToAirError::*;

        use crate::diagnostic_panic;

        if let Some(obj) = stack.pop() {
            return Transition(Ready).ok(obj).with_lookahead(tok);
        }

        match (self, tok) {
            (Ready, Open(Package, span)) => {
                Transition(Ready).ok(Air::PkgStart(span))
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
                Transition(Ready).ok(Air::TplMetaStart(span))
            }
            (Ready, Close(TplParam, span)) => {
                Transition(Ready).ok(Air::TplMetaEnd(span))
            }
            (Ready, Text(lexeme)) => {
                Transition(Ready).ok(Air::TplLexeme(lexeme))
            }

            (
                Ready,
                Close(
                    Rate | Sum | Product | Ceil | Floor | Classify | All | Any,
                    span,
                ),
            ) => Transition(Ready).ok(Air::ExprEnd(span)),

            (Ready, BindIdent(spair)) => {
                Transition(Ready).ok(Air::BindIdent(spair))
            }
            (Ready, Ref(spair) | RefSubject(spair)) => {
                Transition(Ready).ok(Air::RefIdent(spair))
            }

            (Ready, Desc(clause)) => {
                Transition(Ready).ok(Air::DocIndepClause(clause))
            }

            (Ready, Todo | TodoAttr(..)) => {
                Transition(Ready).ok(Air::Todo(UNKNOWN_SPAN))
            }
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
        }
    }
}

#[cfg(all(test, feature = "wip-asg-derived-xmli"))]
mod test;
