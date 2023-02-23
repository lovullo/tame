// XML representation of graph objects
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

//! XML representation of graph objects via [XIR](crate::xir).
//!
//! Attempts to produce a representation of graph [`Object`]s that is
//!   familiar to those writing TAME's XML-based source language.
//!
//! _This representation will change over time_ as TAME's source language
//!   evolves.
//! There is no guarantee that this representation will stay over time;
//!   it was written for transitional purposes,
//!     but may be useful in the future for concrete code suggestions/fixes,
//!       or observing template expansions.

use super::object::{
    DynObjectRel, Expr, Object, ObjectIndex, ObjectRelTy, Pkg,
};
use crate::{
    asg::{
        visit::{Depth, TreeWalkRel},
        Asg, ExprOp,
    },
    diagnose::{panic::DiagnosticPanic, Annotate},
    diagnostic_panic, diagnostic_unreachable,
    parse::{prelude::*, util::SPair},
    span::{Span, UNKNOWN_SPAN},
    sym::{
        st::{URI_LV_CALC, URI_LV_RATER, URI_LV_TPL},
        UriStaticSymbolId,
    },
    xir::{
        flat::{Text, XirfToken},
        st::qname::*,
        OpenSpan, QName,
    },
};
use arrayvec::ArrayVec;
use std::{convert::Infallible, fmt::Display, marker::PhantomData};

#[derive(Debug, PartialEq, Eq)]
pub enum AsgTreeToXirf<'a> {
    Ready(PhantomData<&'a ()>),
}

impl<'a> Default for AsgTreeToXirf<'a> {
    fn default() -> Self {
        Self::Ready(PhantomData::default())
    }
}

impl<'a> Display for AsgTreeToXirf<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "generating XIRF sources from ASG tree")
    }
}

type Xirf = XirfToken<Text>;

impl<'a> ParseState for AsgTreeToXirf<'a> {
    type Token = TreeWalkRel;
    type Object = Xirf;
    type Error = Infallible;
    type Context = TreeContext<'a>;

    fn parse_token(
        self,
        tok: Self::Token,
        TreeContext(toks, asg): &mut TreeContext,
    ) -> TransitionResult<Self::Super> {
        if let Some(emit) = toks.pop() {
            return Transition(self).ok(emit).with_lookahead(tok);
        }

        let tok_span = tok.span();
        let TreeWalkRel(dyn_rel, depth) = tok;

        if depth == Depth(0) {
            return Transition(self).incomplete();
        }

        let obj = dyn_rel.target().resolve(asg);

        match obj {
            Object::Pkg(pkg) => {
                let span = pkg.span();

                toks.push(ns(QN_XMLNS_T, URI_LV_TPL, span));
                toks.push(ns(QN_XMLNS_C, URI_LV_CALC, span));
                toks.push(ns(QN_XMLNS, URI_LV_RATER, span));

                Transition(self).ok(package(pkg, depth))
            }

            // Identifiers will be considered in context;
            //   pass over it for now.
            Object::Ident(..) => Transition(self).incomplete(),

            Object::Expr(expr) => match dyn_rel.source_ty() {
                ObjectRelTy::Ident => {
                    // We were just told an ident exists,
                    //   so this should not fail.
                    let ident = dyn_rel
                        .must_narrow_into::<Expr>()
                        .ident(asg)
                        .diagnostic_unwrap(|| {
                            vec![expr.internal_error(
                                "missing ident for this expression",
                            )]
                        });

                    toks.push(yields(ident.name(), expr.span()));

                    Transition(self).ok(stmt(expr, depth))
                }
                _ => Transition(self).ok(expr_ele(expr, depth)),
            },

            Object::Root(_) => diagnostic_unreachable!(
                vec![tok_span.error("unexpected Root")],
                "tree walk is not expected to emit Root",
            ),
        }
    }

    fn is_accepting(&self, TreeContext(toks, _): &Self::Context) -> bool {
        toks.is_empty()
    }

    fn eof_tok(
        &self,
        TreeContext(toks, _): &Self::Context,
    ) -> Option<Self::Token> {
        // If the stack is not empty on EOF,
        //   yield a dummy token just to invoke `parse_token` to finish
        //   emptying it.
        (!toks.is_empty()).then_some(TreeWalkRel(
            DynObjectRel::new(
                ObjectRelTy::Root,
                ObjectRelTy::Root,
                ObjectIndex::new(0.into(), UNKNOWN_SPAN),
                None,
            ),
            // This is the only part that really matters;
            //   the tree walk will never yield a depth of 0.
            Depth(0),
        ))
    }
}

fn package(pkg: &Pkg, depth: Depth) -> Xirf {
    Xirf::open(QN_PACKAGE, OpenSpan::without_name_span(pkg.span()), depth)
}

fn ns(qname: QName, uri: UriStaticSymbolId, span: Span) -> Xirf {
    Xirf::attr(qname, uri, (span, span))
}

fn stmt(expr: &Expr, depth: Depth) -> Xirf {
    match expr.op() {
        ExprOp::Sum => {
            Xirf::open(QN_RATE, OpenSpan::without_name_span(expr.span()), depth)
        }

        _ => todo!("stmt: {expr:?}"),
    }
}

fn yields(name: SPair, span: Span) -> Xirf {
    Xirf::attr(QN_YIELDS, name, (span, name))
}

fn expr_ele(expr: &Expr, depth: Depth) -> Xirf {
    let qname = match expr.op() {
        ExprOp::Sum => QN_C_SUM,
        op => todo!("expr_ele qname: {op:?}"),
    };

    Xirf::open(qname, OpenSpan::without_name_span(expr.span()), depth)
}

pub struct TreeContext<'a>(TokenStack, &'a Asg);

// Custom `Debug` impl to omit ASG rendering,
//   since it's large and already included while rendering other parts of
//   the lowering pipeline.
// Of course,
//   that's assuming this is part of the lowering pipeline.
impl<'a> std::fmt::Debug for TreeContext<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("TreeContext")
            .field(&self.0)
            .field(&AsgElided)
            .finish()
    }
}

/// Used a placeholder for [`TreeContext`]'s [`Debug`].
#[derive(Debug)]
struct AsgElided;

impl<'a> From<&'a Asg> for TreeContext<'a> {
    fn from(asg: &'a Asg) -> Self {
        Self(Default::default(), asg)
    }
}

/// Size of the token stack.
///
/// See [`TokenStack`] for more information.
const TOK_STACK_SIZE: usize = 8;

/// Token stack to hold generated tokens between [`AsgTreeToXirf`]
///   iterations.
///
/// The token stack is used to avoid having to create separate states for
///   emitting each individual token.
/// It is populated by [`AsgTreeToXirf`] if more than a single [`XirfToken`]
///   needs to be emitted,
///     and tokens are removed on each subsequent iteration until empty.
///
/// This need only be big enough to accommodate [`AsgTreeToXirf`]'s
///   implementation;
///     the size is independent of user input.
#[derive(Debug, Default)]
struct TokenStack(ArrayVec<Xirf, TOK_STACK_SIZE>);

impl TokenStack {
    fn push(&mut self, tok: Xirf) {
        match self {
            Self(stack) => {
                if stack.is_full() {
                    diagnostic_panic!(
                        vec![tok.internal_error(
                            "while emitting a token for this object"
                        )],
                        "token stack exhausted (increase TOK_STACK_SIZE)",
                    )
                }

                stack.push(tok)
            }
        }
    }

    fn pop(&mut self) -> Option<Xirf> {
        match self {
            Self(stack) => stack.pop(),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self(stack) => stack.is_empty(),
        }
    }
}

// System tests covering this functionality can be found in
//   `tamer/tests/xir/`.
