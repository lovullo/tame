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
    asg::{air::Air, ExprOp},
    fmt::TtQuote,
    nir::{Nir::*, NirEntity::*},
    parse::prelude::*,
    span::Span,
    sym::{st::raw::U_TRUE, SymbolId},
};
use arrayvec::ArrayVec;

/// Dynamic [`NirToAir`] parser configuration.
///
/// This acts as a runtime feature flag while this portions of TAMER is
///   under development.
#[derive(Debug, PartialEq, Eq)]
pub enum NirToAirParseType {
    /// Discard incoming tokens instead of lowering them.
    Noop,

    /// Lower known tokens,
    ///   but produce errors for everything else that is not yet supported.
    ///
    /// It is expected that this will fail on at least some packages;
    ///   this should be enabled only on packages known to compile with the
    ///   new system.
    LowerKnownErrorRest,
}

impl Display for NirToAirParseType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Noop => write!(f, "discarding all tokens (noop)"),
            Self::LowerKnownErrorRest => write!(
                f,
                "lowering only supported tokens and failing on all others"
            ),
        }
    }
}

impl From<NirToAirParseType> for (NirToAirParseType, ObjStack) {
    fn from(ty: NirToAirParseType) -> Self {
        (ty, Default::default())
    }
}

impl From<(NirToAirParseType, ObjStack)> for NirToAirParseType {
    fn from((ty, _): (NirToAirParseType, ObjStack)) -> Self {
        ty
    }
}

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
pub const SYM_TRUE: SymbolId = U_TRUE;

impl ParseState for NirToAir {
    type Token = Nir;
    type Object = Air;
    type Error = NirToAirError;
    type Context = (NirToAirParseType, ObjStack);
    type PubContext = NirToAirParseType;

    fn parse_token(
        self,
        tok: Self::Token,
        (parse_type, stack): &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NirToAir::*;
        use NirToAirError::*;

        match parse_type {
            NirToAirParseType::Noop => return Transition(Ready).incomplete(),
            NirToAirParseType::LowerKnownErrorRest => (),
        }

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
            (Meta(mspan), BindIdentMeta(spair)) => {
                Transition(Meta(mspan)).ok(Air::BindIdent(spair))
            }
            (Meta(mspan), Ref(spair)) => {
                Transition(Meta(mspan)).ok(Air::RefIdent(spair))
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
                tok @ (Open(..) | Close(..) | BindIdent(..) | RefSubject(..)),
            ) => Transition(Meta(mspan))
                .err(NirToAirError::ExpectedMetaToken(mspan, tok)),

            (Ready, Text(text)) => Transition(Ready).ok(Air::DocText(text)),

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
            (st @ (Ready | Meta(_)), BindIdentAbstract(spair)) => {
                Transition(st).ok(Air::BindIdentAbstract(spair))
            }
            (Ready, Ref(spair) | RefSubject(spair)) => {
                Transition(Ready).ok(Air::RefIdent(spair))
            }

            (st @ (Ready | Meta(_)), Desc(clause)) => {
                Transition(st).ok(Air::DocIndepClause(clause))
            }

            (Ready, Import(namespec)) => {
                Transition(Ready).ok(Air::PkgImport(namespec))
            }

            // Shouldn't happen in  practice because nir::parse will not
            //   produce this.
            // This assumption is only valid so long as that's the only
            //   thing producing NIR.
            (st @ Meta(..), tok @ Import(_)) => Transition(st).dead(tok),
            (st @ Ready, tok @ BindIdentMeta(_)) => Transition(st).dead(tok),

            // Unsupported tokens yield errors.
            // There _is_ a risk that this will put us in a wildly
            //   inconsistent state,
            //     yielding nonsense errors in the future.
            // This used to panic,
            //   but yielding errors allows compilation to continue and
            //   discover further problems,
            //     so that this new parser can be run on a given package
            //       (with e.g. `--emit xmlo-experimental`)
            //       to get some idea of what type of missing features may
            //       be needed to support the compilation of that package.
            // Note also that,
            //   at the time of writing,
            //   large numbers of diagnostic spans may be quite slow to
            //     output on large files because the system does not cache
            //     newline locations and requires re-calculating from the
            //     beginning of the file for earlier spans.
            (st, tok @ (Todo(..) | TodoAttr(..))) => {
                Transition(st).err(NirToAirError::UnsupportedToken(tok))
            }

            (st, Noop(_)) => Transition(st).incomplete(),
        }
    }

    fn is_accepting(&self, (_, stack): &Self::Context) -> bool {
        matches!(self, Self::Ready) && stack.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NirToAirError {
    /// The provided token is not yet supported by TAMER.
    ///
    /// This means that a token was recognized by NIR but it makes no
    ///   guarantees about _whether_ a token will be supported in the
    ///   future;
    ///     explicit rejection is still a future possibility.
    UnsupportedToken(Nir),

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
    ExpectedMetaToken(Span, Nir),
}

impl Display for NirToAirError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAirError::*;

        match self {
            UnsupportedToken(tok) => {
                write!(f, "unsupported token: {tok}")
            }

            MatchSubjectExpected(_, nir) => {
                write!(f, "expected match subject, found {nir}")
            }

            TodoMatchBody(_, _) => {
                write!(f, "match body is not yet supported by TAMER")
            }

            ExpectedMetaToken(_, tok) => {
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
            UnsupportedToken(tok) => vec![
                tok.span().internal_error("this token is not yet supported in TAMER"),
                tok.span().help(
                    "if this is unexpected, \
                        are you unintentionally using the `--emit xmlo-experimental` \
                        command line option?"
                ),
                tok.span().help(
                    "this package may also have a sibling `.experimental` file \
                        that triggers `xmlo-experimental`"
                ),
            ],

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
            ExpectedMetaToken(mspan, given) => vec![
                mspan.note("while parsing the body of this metavariable"),
                given.span().error("expected a lexical token here"),
            ],
        }
    }
}

#[cfg(test)]
mod test;
