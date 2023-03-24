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
    expr::AirExprAggregateStoreDangling,
    ir::AirTemplatable,
    AirExprAggregate,
};
use crate::{
    asg::{
        graph::object::{Meta, ObjectRelTo},
        Ident,
    },
    diagnose::Annotate,
    diagnostic_todo,
    fmt::{DisplayWrapper, TtQuote},
    parse::prelude::*,
    span::Span,
};

/// Template parser and token aggregator.
///
/// A template consists of
///
///   - Metadata about the template,
///       including its parameters; and
///   - A collection of [`AirTemplatable`] tokens representing the body of
///       the template that will be expanded into the application site when
///       the template is applied.
///
/// This contains an embedded [`AirExprAggregate`] parser for handling
///   expressions just the same as [`super::AirAggregate`] does with
///   packages.
#[derive(Debug, PartialEq)]
pub enum AirTplAggregate<OR, OT = OR>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    /// Ready for a template,
    ///   defined as part of the given package.
    ///
    /// This state also includes the template header;
    ///   unlike NIR,
    ///     AIR has no restrictions on when template header tokens are
    ///     provided,
    ///       which simplifies AIR generation.
    Ready(TplEnvCtx<OR, OT>),

    /// Toplevel template context.
    ///
    /// Conceptually,
    ///   tokens that are received in this state are interpreted as directly
    ///   applying to the template itself,
    ///     or creating an object directly owned by the template.
    Toplevel(
        TplEnvCtx<OR, OT>,
        TplState,
        AirExprAggregateStoreDangling<Tpl>,
    ),

    /// Defining a template metavariable.
    TplMeta(
        TplEnvCtx<OR, OT>,
        TplState,
        AirExprAggregateStoreDangling<Tpl>,
        ObjectIndex<Meta>,
    ),

    /// Aggregating tokens into a template.
    TplExpr(
        TplEnvCtx<OR, OT>,
        TplState,
        AirExprAggregateStoreDangling<Tpl>,
    ),
}

impl<OR, OT> Display for AirTplAggregate<OR, OT>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready(_) => write!(f, "ready for template definition"),

            Self::Toplevel(_, tpl, expr) | Self::TplExpr(_, tpl, expr) => {
                write!(f, "building {tpl} with {expr}")
            }

            Self::TplMeta(_, tpl, _, _) => {
                write!(f, "building {tpl} metavariable")
            }
        }
    }
}

/// Environment context for template application.
///
/// The _root_ `OR` represents a container for [`Ident`]s to named templates
///   produced by [`AirTplAggregate`].
///
/// The _expansion target_ `OT` is where template applications will expand
///   into when referenced.
/// Note that templates defined _within_ a template will receive a different
///   expansion target,
///     determined by [`AirTplAggregate``].
#[derive(Debug, PartialEq)]
pub struct TplEnvCtx<OR, OT>(ObjectIndex<OR>, ObjectIndex<OT>)
where
    Self: TplEnvCtxPair<OR, OT>;

// At the time of writing (2023-03),
//   deriving these macros places `Clone`/`Copy` trait bounds on each of
//   `OR` and `OT`,
//     which is nonsense.
impl<OR, OT> Clone for TplEnvCtx<OR, OT>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}
impl<OR, OT> Copy for TplEnvCtx<OR, OT> where Self: TplEnvCtxPair<OR, OT> {}

impl<OR, OT> TplEnvCtx<OR, OT>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    pub fn defines(self, asg: &mut Asg, oi: ObjectIndex<Ident>) -> Self {
        match self {
            Self(oi_root, oi_target) => {
                Self(oi_root.defines(asg, oi), oi_target)
            }
        }
    }

    pub fn oi_target(&self) -> ObjectIndex<OT> {
        match self {
            Self(_, oi_target) => *oi_target,
        }
    }
}

/// More concisely represent trait bounds for [`TplEnvCtx`].
pub trait TplEnvCtxPair<OR, OT> = Sized
where
    OR: ObjectRelTo<Ident>,
    OT: ObjectRelTo<Tpl>;

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

impl<OR, OT> ParseState for AirTplAggregate<OR, OT>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    type Token = AirTemplatable;
    type Object = ();
    type Error = AsgError;
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use super::ir::{AirBind::*, AirTpl::*};
        use AirTemplatable::*;
        use AirTplAggregate::*;

        match (self, tok) {
            (Ready(ois), AirTpl(TplStart(span))) => {
                let oi_tpl = asg.create(Tpl::new(span));

                Transition(Toplevel(
                    ois,
                    TplState::Dangling(oi_tpl),
                    AirExprAggregate::new_in(oi_tpl),
                ))
                .incomplete()
            }

            (Toplevel(..), AirTpl(TplStart(span))) => diagnostic_todo!(
                vec![span.note("for this template")],
                "nested tpl open"
            ),

            (Toplevel(ois, tpl, expr), AirBind(BindIdent(id))) => asg
                .lookup_global_or_missing(id)
                .bind_definition(asg, id, tpl.oi())
                .map(|oi_ident| ois.defines(asg, oi_ident))
                .map(|_| ())
                .transition(Toplevel(ois, tpl.identify(id), expr)),

            (Toplevel(ois, tpl, expr), AirBind(RefIdent(id))) => {
                tpl.oi().apply_named_tpl(asg, id);
                Transition(Toplevel(ois, tpl, expr)).incomplete()
            }

            (Toplevel(ois, tpl, expr), AirTpl(TplMetaStart(span))) => {
                let oi_meta = asg.create(Meta::new_required(span));
                Transition(TplMeta(ois, tpl, expr, oi_meta)).incomplete()
            }
            (TplMeta(ois, tpl, expr, oi_meta), AirTpl(TplMetaEnd(cspan))) => {
                oi_meta.close(asg, cspan);
                Transition(Toplevel(ois, tpl, expr)).incomplete()
            }

            (TplMeta(ois, tpl, expr, oi_meta), AirTpl(TplLexeme(lexeme))) => {
                Transition(TplMeta(
                    ois,
                    tpl,
                    expr,
                    oi_meta.assign_lexeme(asg, lexeme),
                ))
                .incomplete()
            }

            (TplMeta(ois, tpl, expr, oi_meta), AirBind(BindIdent(name))) => {
                oi_meta.identify_as_tpl_param(asg, tpl.oi(), name);
                Transition(TplMeta(ois, tpl, expr, oi_meta)).incomplete()
            }

            (TplMeta(..), tok @ AirBind(RefIdent(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "AirBind in metavar context (param-value)"
                )
            }

            (TplMeta(..), tok @ AirExpr(..)) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "AirExpr in metavar context (e.g. @values@)"
                )
            }

            (
                TplMeta(..),
                tok @ AirTpl(
                    TplStart(..) | TplMetaStart(..) | TplEnd(..)
                    | TplEndRef(..),
                ),
            ) => {
                diagnostic_todo!(vec![tok.note("this token")], "AirTpl variant")
            }

            (Toplevel(..), tok @ AirTpl(TplMetaEnd(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "unbalanced meta"
                )
            }

            (Toplevel(..), tok @ AirTpl(TplLexeme(..))) => {
                diagnostic_todo!(
                    vec![tok.note("this token")],
                    "err: TplLexeme outside of metavar"
                )
            }

            (Toplevel(ois, tpl, _expr_done), AirTpl(TplEnd(span))) => {
                tpl.close(asg, span).transition(Ready(ois))
            }

            (TplExpr(ois, tpl, expr), AirTpl(TplEnd(span))) => {
                // TODO: duplicated with AirAggregate
                if expr.is_accepting(asg) {
                    tpl.close(asg, span).transition(Ready(ois))
                } else {
                    Transition(TplExpr(ois, tpl, expr))
                        .err(AsgError::InvalidTplEndContext(span))
                }
            }

            (Toplevel(ois, tpl, expr_done), AirTpl(TplEndRef(span))) => {
                tpl.oi().expand_into(asg, ois.oi_target());

                Transition(Toplevel(ois, tpl.anonymous_reachable(), expr_done))
                    .incomplete()
                    .with_lookahead(AirTpl(TplEnd(span)))
            }

            (TplExpr(ois, tpl, expr_done), AirTpl(TplEndRef(span))) => {
                tpl.oi().expand_into(asg, ois.oi_target());

                Transition(TplExpr(ois, tpl.anonymous_reachable(), expr_done))
                    .incomplete()
                    .with_lookahead(AirTpl(TplEnd(span)))
            }

            (
                Toplevel(ois, tpl, expr) | TplExpr(ois, tpl, expr),
                AirExpr(etok),
            ) => Self::delegate_expr(asg, ois, tpl, expr, etok),

            (TplExpr(ois, tpl, expr), AirBind(etok)) => {
                Self::delegate_expr(asg, ois, tpl, expr, etok)
            }

            (TplExpr(..), AirTpl(TplStart(span))) => {
                diagnostic_todo!(
                    vec![span.note("for this token")],
                    "nested template (template-generated template)"
                )
            }

            (
                Ready(..) | TplExpr(..),
                tok @ AirTpl(TplMetaStart(..) | TplLexeme(..) | TplMetaEnd(..)),
            ) => {
                diagnostic_todo!(
                    vec![tok.note("for this token")],
                    "metasyntactic token in non-tpl-toplevel context: {tok:?}"
                )
            }

            (st @ Ready(..), AirTpl(TplEnd(span) | TplEndRef(span))) => {
                Transition(st).err(AsgError::UnbalancedTpl(span))
            }

            (st @ Ready(..), tok @ (AirExpr(..) | AirBind(..))) => {
                Transition(st).dead(tok)
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready(..))
    }
}

impl<OR, OT> AirTplAggregate<OR, OT>
where
    Self: TplEnvCtxPair<OR, OT>,
{
    pub(super) fn new(
        oi_root: ObjectIndex<OR>,
        oi_target: ObjectIndex<OT>,
    ) -> Self {
        Self::Ready(TplEnvCtx(oi_root, oi_target))
    }

    /// Delegate to the expression parser [`AirExprAggregate`].
    fn delegate_expr(
        asg: &mut <Self as ParseState>::Context,
        ois: TplEnvCtx<OR, OT>,
        tpl: TplState,
        expr: AirExprAggregateStoreDangling<Tpl>,
        etok: impl Into<<AirExprAggregateStoreDangling<Tpl> as ParseState>::Token>,
    ) -> TransitionResult<Self> {
        let tok = etok.into();

        expr.parse_token(tok, asg).branch_dead::<Self, _>(
            |expr, ()| Transition(Self::Toplevel(ois, tpl, expr)).incomplete(),
            |expr, result, ()| {
                result
                    .map(ParseStatus::reflexivity)
                    .transition(Self::TplExpr(ois, tpl, expr))
            },
            (),
        )
    }
}

#[cfg(test)]
mod test;
