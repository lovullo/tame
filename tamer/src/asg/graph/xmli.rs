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
    DynObjectRel, Expr, Object, ObjectIndex, ObjectRelTy, OiPairObjectInner,
    Pkg, Tpl,
};
use crate::{
    asg::{
        visit::{Depth, TreeWalkRel},
        Asg, ExprOp, Ident,
    },
    diagnose::Annotate,
    diagnostic_panic, diagnostic_unreachable,
    parse::{prelude::*, util::SPair, Transitionable},
    span::{Span, UNKNOWN_SPAN},
    sym::{
        st::{URI_LV_CALC, URI_LV_RATER, URI_LV_TPL},
        UriStaticSymbolId,
    },
    xir::{
        flat::{Text, XirfToken},
        st::qname::*,
        CloseSpan, OpenSpan, QName,
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
        TreeWalkRel(dyn_rel, depth): Self::Token,
        ctx: &mut TreeContext,
    ) -> TransitionResult<Self::Super> {
        match (ctx.pop(), depth) {
            // Empty the token stack before processing any further.
            // Note that we must yield the token as lookahead to ensure that
            //   we do eventually process it.
            (Some(emit), _) => Transition(self)
                .ok(emit)
                .with_lookahead(TreeWalkRel(dyn_rel, depth)),

            // Used by `eof_tok` only to empty the token stack,
            //   which we're now done with.
            // We consume the token by not yielding any lookahead.
            (None, Depth(0)) => Transition(self).incomplete(),

            // The stack is empty,
            //   so proceed with processing the provided relation.
            (None, depth) => ctx.derive_src(dyn_rel, depth).transition(self),
        }
    }

    fn is_accepting(&self, ctx: &Self::Context) -> bool {
        ctx.stack_is_empty()
    }

    fn eof_tok(&self, ctx: &Self::Context) -> Option<Self::Token> {
        // If the stack is not empty on EOF,
        //   yield a dummy token just to invoke `parse_token` to finish
        //   emptying it.
        (!ctx.stack_is_empty()).then_some(TreeWalkRel(
            DynObjectRel::new(
                ObjectRelTy::Root,
                ObjectRelTy::Root,
                ObjectIndex::new(0.into(), UNKNOWN_SPAN),
                ObjectIndex::new(0.into(), UNKNOWN_SPAN),
                None,
            ),
            // This is the only part that really matters;
            //   the tree walk will never yield a depth of 0.
            Depth(0),
        ))
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
type TokenStack = ArrayVec<Xirf, TOK_STACK_SIZE>;

pub struct TreeContext<'a> {
    stack: TokenStack,
    asg: &'a Asg,
}

impl<'a> TreeContext<'a> {
    /// Given a [`DynObjectRel`],
    ///   derive a legacy TAME representation that will faithfully represent
    ///   an equivalent program when compiled by the XSLT-based TAME
    ///   compiler.
    ///
    /// The [`TokenStack`] may be used to pre-generate [XIRF](Xirf) to be
    ///   yielded on subsequent iterations rather than having to introduce
    ///   [`AsgTreeToXirf`] states for each individual token.
    /// Adjust [`TOK_STACK_SIZE`] as necessary.
    ///
    /// The provided [`Depth`] represent the depth of the tree at the
    ///   position of the provided [`DynObjectRel`].
    /// See [`TreeWalkRel`] for more information.
    fn derive_src(
        &mut self,
        dyn_rel: DynObjectRel,
        depth: Depth,
    ) -> Option<Xirf> {
        // TODO: Verify that the binary does not perform unnecessary
        //   resolution in branches that do not utilize the source.
        let paired_rel = dyn_rel.resolve_oi_pairs(self.asg);

        match paired_rel.target() {
            Object::Pkg((pkg, _)) => self.emit_package(pkg, depth),

            // Identifiers will be considered in context;
            //   pass over it for now.
            // But we must not skip over its depth,
            //   otherwise we parent a following sibling at a matching
            //   depth;
            //     this close will force the auto-closing system to close
            //     any siblings in preparation for the object to follow.
            Object::Ident((ident, _)) => Some(Xirf::Close(
                None,
                CloseSpan::without_name_span(ident.span()),
                depth,
            )),

            Object::Expr((expr, _)) => {
                self.emit_expr(expr, paired_rel.source(), depth)
            }

            Object::Tpl((tpl, _)) => {
                self.emit_template(tpl, paired_rel.source(), depth)
            }

            Object::Root(..) => diagnostic_unreachable!(
                vec![],
                "tree walk is not expected to emit Root",
            ),
        }
    }

    /// Emit tokens representing the root package element.
    fn emit_package(&mut self, pkg: &Pkg, depth: Depth) -> Option<Xirf> {
        let span = pkg.span();

        self.push_all([
            ns(QN_XMLNS_T, URI_LV_TPL, span),
            ns(QN_XMLNS_C, URI_LV_CALC, span),
            ns(QN_XMLNS, URI_LV_RATER, span),
        ]);

        Some(package(pkg, depth))
    }

    /// Emit an expression as a legacy TAME statement or expression.
    ///
    /// Identified expressions must be represented using statements in
    ///   legacy TAME,
    ///     such as `<rate>`.
    /// Anonymous expressions are nested within statements.
    ///
    /// This system will emit statements and expressions that are compatible
    ///   with the information on the [ASG](crate::asg) and recognized by the
    ///   downstream XSLT-based compiler.
    /// There is no guarantee,
    ///   however,
    ///   that what is emitted is exactly representative of what the user
    ///     originally entered.
    ///
    /// Please ensure that the system matches your expectations using the system
    ///   tests in `:tamer/tests/xmli`.
    fn emit_expr(
        &mut self,
        expr: &Expr,
        src: &Object<OiPairObjectInner>,
        depth: Depth,
    ) -> Option<Xirf> {
        match src {
            Object::Ident((ident, _)) => {
                self.emit_expr_ident(expr, ident, depth)
            }
            _ => Some(expr_ele(expr, depth)),
        }
    }

    /// Emit an identified expression.
    ///
    /// Legacy TAME is only able to bind certain identifiers via statements
    ///   such as `rate` and `classify`.
    fn emit_expr_ident(
        &mut self,
        expr: &Expr,
        ident: &Ident,
        depth: Depth,
    ) -> Option<Xirf> {
        let (qname, ident_qname) = match expr.op() {
            ExprOp::Sum => (QN_RATE, QN_YIELDS),
            ExprOp::Conj => (QN_CLASSIFY, QN_AS),

            ExprOp::Product | ExprOp::Disj => todo!("stmt: {expr:?}"),
        };

        let ispan = ident.span();
        self.push(Xirf::attr(ident_qname, ident.name(), (ispan, ispan)));

        Some(Xirf::open(
            qname,
            OpenSpan::without_name_span(expr.span()),
            depth,
        ))
    }

    /// Emit a template definition.
    fn emit_template(
        &mut self,
        tpl: &Tpl,
        src: &Object<OiPairObjectInner>,
        depth: Depth,
    ) -> Option<Xirf> {
        match src {
            Object::Ident((ident, _)) => {
                self.push(attr_name(ident.name()));

                Some(Xirf::open(
                    QN_TEMPLATE,
                    OpenSpan::without_name_span(tpl.span()),
                    depth,
                ))
            }

            _ => todo!("emit_template: {src:?}"),
        }
    }

    fn push(&mut self, tok: Xirf) {
        if self.stack.is_full() {
            diagnostic_panic!(
                vec![tok
                    .internal_error("while emitting a token for this object")],
                "token stack exhausted (increase TOK_STACK_SIZE)",
            )
        }

        self.stack.push(tok)
    }

    fn pop(&mut self) -> Option<Xirf> {
        self.stack.pop()
    }

    fn stack_is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn push_all(&mut self, toks: impl IntoIterator<Item = Xirf>) {
        toks.into_iter().for_each(|x| self.push(x))
    }
}

// Custom `Debug` impl to omit ASG rendering,
//   since it's large and already included while rendering other parts of
//   the lowering pipeline.
// Of course,
//   that's assuming this is part of the lowering pipeline.
impl<'a> std::fmt::Debug for TreeContext<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TreeContext")
            .field("stack", &self.stack)
            .finish_non_exhaustive()
    }
}

impl<'a> From<&'a Asg> for TreeContext<'a> {
    fn from(asg: &'a Asg) -> Self {
        TreeContext {
            stack: Default::default(),
            asg,
        }
    }
}

fn package(pkg: &Pkg, depth: Depth) -> Xirf {
    Xirf::open(QN_PACKAGE, OpenSpan::without_name_span(pkg.span()), depth)
}

fn ns(qname: QName, uri: UriStaticSymbolId, span: Span) -> Xirf {
    Xirf::attr(qname, uri, (span, span))
}

fn attr_name(name: SPair) -> Xirf {
    Xirf::attr(QN_NAME, name, (name.span(), name.span()))
}

fn expr_ele(expr: &Expr, depth: Depth) -> Xirf {
    use ExprOp::*;

    let qname = match expr.op() {
        Sum => QN_C_SUM,
        Product => QN_C_PRODUCT,
        Conj => QN_ALL,
        Disj => QN_ANY,
    };

    Xirf::open(qname, OpenSpan::without_name_span(expr.span()), depth)
}

// System tests covering this functionality can be found in
//   `:tamer/tests/xir/`.
