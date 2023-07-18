// ASG IR template parsing
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

//! AIR template parser.
//!
//! See the [parent module](super) for more information.

use super::{
    super::{graph::object::Tpl, Asg, AsgError, ObjectIndex},
    ir::AirBindableTpl,
    AirAggregate, AirAggregateCtx,
};
use crate::{fmt::TtQuote, parse::prelude::*, span::Span};

/// Template parser and token aggregator.
///
/// A template consists of
///
///   - Metadata about the template,
///       including its parameters; and
///   - A collection of objects representing the body of the template that
///       will be expanded into the application site when the template is
///       applied.
///
/// The superstate is expected to preempt this parser for expression
///   parsing.
#[derive(Debug, PartialEq)]
pub enum AirTplAggregate {
    /// Ready for a template,
    ///   defined as part of the given package.
    ///
    /// This state also includes the template header;
    ///   unlike NIR,
    ///     AIR has no restrictions on when template header tokens are
    ///     provided,
    ///       which simplifies AIR generation.
    Ready,

    /// Toplevel template context.
    ///
    /// Conceptually,
    ///   tokens that are received in this state are interpreted as directly
    ///   applying to the template itself,
    ///     or creating an object directly owned by the template.
    Toplevel(TplState),

    /// A template has been completed.
    ///
    /// This is used to determine whether the next token of input ought to
    ///   result in a dead state transition or a transition back to
    ///   [`Self::Ready`].
    Done,
}

impl Display for AirTplAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready => write!(f, "ready for template definition"),
            Self::Toplevel(tpl) => write!(f, "building {tpl} at toplevel"),
            Self::Done => write!(f, "completed building template"),
        }
    }
}

/// The current reachability status of the template.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TplState {
    /// Template is dangling and cannot be referenced by anything else.
    Dangling(ObjectIndex<Tpl>),

    /// Template is anonymous and is not reachable by an identifier,
    ///   but is reachable in the current context.
    AnonymousReachable(ObjectIndex<Tpl>),

    /// Template is reachable via an identifier.
    ///
    /// This uses an [`SPair`] as evidence for that assertion rather than an
    ///   [`ObjectIndex`] so that it provides useful output via [`Display`]
    ///   in parser traces.
    Identified(ObjectIndex<Tpl>, SPair),
}

impl TplState {
    fn oi(&self) -> ObjectIndex<Tpl> {
        match self {
            TplState::Dangling(oi)
            | TplState::AnonymousReachable(oi)
            | TplState::Identified(oi, _) => *oi,
        }
    }

    fn identify(self, id: SPair) -> Self {
        Self::Identified(self.oi(), id)
    }

    fn anonymous_reachable(self) -> Self {
        Self::AnonymousReachable(self.oi())
    }

    /// Attempt to complete a template definition.
    ///
    /// If `self` is [`Self::Dangling`],
    ///   then an [`AsgError::DanglingTpl`] will be returned.
    ///
    /// This updates the span of the template to encompass the entire
    ///   definition,
    ///     even if an error occurs.
    fn close(self, asg: &mut Asg, close_span: Span) -> Result<(), AsgError> {
        let oi = self.oi().close(asg, close_span);

        match self {
            Self::Dangling(_) => {
                Err(AsgError::DanglingTpl(oi.resolve(asg).span()))
            }
            Self::AnonymousReachable(..) | Self::Identified(..) => Ok(()),
        }
    }
}

impl Display for TplState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TplState::Dangling(_) => write!(f, "anonymous dangling template"),
            TplState::AnonymousReachable(_) => {
                write!(f, "anonymous reachable template")
            }
            TplState::Identified(_, id) => {
                write!(f, "identified template {}", TtQuote::wrap(id))
            }
        }
    }
}

impl ParseState for AirTplAggregate {
    type Token = AirBindableTpl;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;
    type Super = AirAggregate;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirDoc::*, AirTpl::*};
        use AirBindableTpl::*;
        use AirTplAggregate::*;

        match (self, tok) {
            (Ready | Done, AirTpl(TplStart(span))) => {
                let oi_tpl = ctx.asg_mut().create(Tpl::new(span));

                Transition(Toplevel(TplState::Dangling(oi_tpl))).incomplete()
            }

            (st @ Toplevel(..), tok @ AirTpl(TplStart(_))) => {
                ctx.ret_or_transfer(st, tok, AirTplAggregate::new())
            }

            (Toplevel(tpl), AirBind(BindIdent(id))) => ctx
                .defines_concrete(id)
                .and_then(|oi_ident| {
                    oi_ident.bind_definition(ctx.asg_mut(), id, tpl.oi())
                })
                .map(|_| ())
                .transition(Toplevel(tpl.identify(id))),

            (Toplevel(tpl), AirBind(BindIdentAbstract(meta_name))) => {
                diagnostic_todo!(
                    vec![
                        tpl.oi().note("for this template"),
                        meta_name.note(
                            "attempting to bind an abstract identifier with \
                                this metavariable"
                        ),
                    ],
                    "attempt to bind abstract identifier to template",
                )
            }

            (Toplevel(tpl), AirBind(RefIdent(name))) => {
                let tpl_oi = tpl.oi();
                let ref_oi = ctx.lookup_lexical_or_missing(name);

                tpl_oi.apply_named_tpl(ctx.asg_mut(), ref_oi, name.span());

                Transition(Toplevel(tpl)).incomplete()
            }

            (Toplevel(tpl), AirDoc(DocIndepClause(clause))) => {
                tpl.oi().add_desc_short(ctx.asg_mut(), clause);
                Transition(Toplevel(tpl)).incomplete()
            }

            (Toplevel(tpl), AirDoc(DocText(text))) => {
                tpl.oi().append_doc_text(ctx.asg_mut(), text);
                Transition(Toplevel(tpl)).incomplete()
            }

            (Toplevel(tpl), AirTpl(TplEnd(span))) => {
                tpl.close(ctx.asg_mut(), span).transition(Done)
            }

            (Toplevel(tpl), AirTpl(TplEndRef(span))) => {
                // Note that we utilize lookahead in either case,
                //   but in the case of an error,
                //   we are effectively discarding the ref and translating
                //     into a `TplEnd`.
                match ctx.expansion_oi() {
                    Some(oi_target) => {
                        tpl.oi().expand_into(ctx.asg_mut(), oi_target);

                        Transition(Toplevel(tpl.anonymous_reachable()))
                            .incomplete()
                    }
                    None => Transition(Toplevel(tpl))
                        .err(AsgError::InvalidExpansionContext(span)),
                }
                .with_lookahead(AirTpl(TplEnd(span)))
            }

            // If we just finished a template then this end may represent
            //   the closing of a parent template.
            (Done, tok @ AirTpl(TplEnd(_) | TplEndRef(_))) => {
                Transition(Done).dead(tok)
            }
            // Otherwise we have an unbalanced close
            //   (we just started parsing to receive an end token).
            (Ready, AirTpl(TplEnd(span) | TplEndRef(span))) => {
                Transition(Ready).err(AsgError::UnbalancedTpl(span))
            }

            (st @ (Ready | Done), tok @ (AirBind(..) | AirDoc(..))) => {
                Transition(st).dead(tok)
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready | Self::Done)
    }
}

impl AirTplAggregate {
    pub(super) fn new() -> Self {
        Self::Ready
    }

    pub(super) fn active_tpl_oi(&self) -> Option<ObjectIndex<Tpl>> {
        use AirTplAggregate::*;

        match self {
            Ready | Done => None,
            Toplevel(tplst) => Some(tplst.oi()),
        }
    }
}

#[cfg(test)]
mod test;
