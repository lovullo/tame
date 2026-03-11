// Tests for ASG IR template parsing
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

mod apply;

use super::*;
use crate::asg::air::test::{
    Sut, as_pkg_body, expect_ident_obj, expect_ident_oi,
};
use crate::convert::ExpectInto;
use crate::span::dummy::*;
use crate::{
    asg::{
        Expr, ExprOp, Ident,
        air::{
            Air::*,
            expr::test::collect_subexprs,
            test::{
                air_ctx_from_pkg_body_toks, air_ctx_from_toks,
                parse_as_pkg_body, pkg_expect_ident_obj, pkg_expect_ident_oi,
                pkg_lookup,
            },
        },
        graph::object::{Doc, Meta, ObjectRel, expr::MetaState, tpl::TplShape},
    },
    parse::util::spair,
};

// A template is defined by the package containing it,
//   like an expression.
#[test]
fn tpl_defining_pkg() {
    let id_tpl = spair("_tpl_", S3);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, spair("/pkg", S1)),
          // This also tests tpl as a transition away from the package header.
          TplStart(S2),
            BindIdent(id_tpl),
          TplEnd(S4),
        PkgEnd(S5),
    ];

    let ctx = air_ctx_from_toks(toks);
    let asg = ctx.asg_ref();

    let tpl = pkg_expect_ident_obj::<Tpl>(&ctx, id_tpl);
    assert_eq!(S2.merge(S4).unwrap(), tpl.span());
    assert_eq!(TplShape::Empty, tpl.shape());

    let oi_id_tpl = pkg_lookup(&ctx, id_tpl).unwrap();
    assert_eq!(
        S1.merge(S5),
        oi_id_tpl.src_pkg(&asg).map(|pkg| pkg.resolve(&asg).span()),
    );
}

#[test]
fn tpl_after_expr() {
    let id_expr = spair("expr", S3);
    let id_tpl = spair("_tpl_", S6);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, spair("/pkg", S1)),
          // This expression is incidental to this test;
          //   it need only parse.
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_expr),
          ExprEnd(S4),

          // Open after an expression.
          TplStart(S5),
            BindIdent(id_tpl),
          TplEnd(S7),
        PkgEnd(S8),
    ];

    let ctx = air_ctx_from_toks(toks);

    let tpl = pkg_expect_ident_obj::<Tpl>(&ctx, id_tpl);
    assert_eq!(S5.merge(S7).unwrap(), tpl.span());
    assert_eq!(TplShape::Empty, tpl.shape());
}

// Templates within expressions are permitted by NIR at the time of writing
//   (and so cannot be observed in system tests using TAME's source
//     language),
//   but it _is_ permitted by AIR,
//     to simplify lowering, desugaring, and template expansion.
//
// This test is a rather important one,
//   since it ensures that expression context is properly restored
//   regardless of whether a template is encountered.
// This context includes the entire active expression stack.
#[test]
fn tpl_within_expr() {
    let id_expr = spair("expr", S3);
    let id_tpl = spair("_tpl_", S7);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, spair("/pkg", S1)),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_expr),

            // Child expression before the template to ensure that the
            //   context is properly restored after template parsing.
            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),

            // Template _within_ an expression.
            // This will not be present in the final expression,
            //   as if it were hoisted out.
            TplStart(S6),
              BindIdent(id_tpl),
            TplEnd(S8),

            // Child expression _after_ the template for the same reason.
            ExprStart(ExprOp::Sum, S9),
            ExprEnd(S10),
          ExprEnd(S11),
        PkgEnd(S12),
    ];

    let ctx = air_ctx_from_toks(toks);
    let asg = ctx.asg_ref();

    // The inner template.
    let tpl = pkg_expect_ident_obj::<Tpl>(&ctx, id_tpl);
    assert_eq!(S6.merge(S8).unwrap(), tpl.span());
    assert_eq!(TplShape::Empty, tpl.shape());

    // The expression that was produced on the graph ought to be equivalent
    //   to the expression without the template being present at all
    //     (noting that the spans are of course not adjusted).
    let oi_expr = pkg_expect_ident_oi::<Expr>(&ctx, id_expr);
    let expr = oi_expr.resolve(&asg);
    assert_eq!(S2.merge(S11).unwrap(), expr.span());
    assert_eq!(
        #[rustfmt::skip]
        vec![
            S4.merge(S5).unwrap(),
            S9.merge(S10).unwrap(),
        ],
        collect_subexprs(&asg, oi_expr)
            .iter()
            .map(|(_, expr)| expr.span())
            .collect::<Vec<_>>(),
    );
}

#[test]
fn close_tpl_without_open() {
    let id_tpl = spair("_tpl_", S3);

    #[rustfmt::skip]
    let toks = [
        TplEnd(S1),
        // RECOVERY: Try again.
        TplStart(S2),
          BindIdent(id_tpl),
        TplEnd(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Err(ParseError::StateError(AsgError::UnbalancedTpl(S1))),
              // RECOVERY
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // BindIdent
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

#[test]
fn tpl_with_reachable_expression() {
    let id_tpl = spair("_tpl_", S2);
    let id_expr_a = spair("expra", S4);
    let id_expr_b = spair("exprb", S7);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),

          ExprStart(ExprOp::Sum, S3),
            // Must not be cached in the global env.
            BindIdent(id_expr_a),
          ExprEnd(S5),

          ExprStart(ExprOp::Sum, S6),
            // Must not be cached in the global env.
            BindIdent(id_expr_b),
          ExprEnd(S8),
        TplEnd(S9),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let tpl = oi_tpl.resolve(&asg);
    assert_eq!(S1.merge(S9).unwrap(), tpl.span());

    // Because the above expressions were bound to identifiers,
    //   they will not be inlined into the application site
    //     (they'll be hoisted to the nearest container,
    //        which might be the same as the application site,
    //        but it's still not inlining an expression).
    assert_eq!(TplShape::Empty, tpl.shape());

    // The inner expressions are reachable,
    //   but the intent is to expand them into the template's eventual
    //   application site.
    // They have identifiers,
    //   but those identifiers _must not_ be cached in the global
    //   environment;
    //     such a determination will be made at expansion-time.
    // Given that,
    //   they should be defined by the template...
    assert_eq!(
        vec![
            // At the time of writing,
            //   this is implemented using the same `edges_filtered`,
            //   but the point is that we want to ensure that the
            //     identifiers bound to this template are only these.
            oi_tpl.lookup_local_linear(&asg, id_expr_b),
            oi_tpl.lookup_local_linear(&asg, id_expr_a),
        ],
        oi_tpl
            .edges_filtered::<Ident>(&asg)
            .map(Some)
            .collect::<Vec<_>>()
    );

    // ...but not by the package containing the template.
    let oi_pkg = pkg_lookup(&ctx, id_tpl).unwrap().src_pkg(&asg).unwrap();
    assert_eq!(
        vec![
            // The only identifier on the package should be the template itself.
            pkg_lookup(&ctx, id_tpl).unwrap(),
        ],
        oi_pkg.edges_filtered::<Ident>(&asg).collect::<Vec<_>>()
    );

    // Verify the above claim that these are not cached in the global
    //   environment.
    assert_eq!(None, pkg_lookup(&ctx, id_expr_a));
    assert_eq!(None, pkg_lookup(&ctx, id_expr_b));
}

// Templates can expand into many contexts,
//   including other expressions,
//   and so must be able to contain expressions that,
//     while dangling now,
//     will become reachable in its expansion context.
#[test]
fn tpl_holds_dangling_expressions() {
    let id_tpl = spair("_tpl_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),

          // Dangling expression.
          ExprStart(ExprOp::Sum, S3),
          ExprEnd(S4),
        TplEnd(S5),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let tpl = oi_tpl.resolve(&asg);

    // The above `Expr` would be inlined at an application site,
    //   and so this changes the shape of the template.
    assert_eq!(TplShape::Expr(S3.merge(S4).unwrap()), tpl.shape());

    assert_eq!(
        vec![S3.merge(S4).unwrap()],
        oi_tpl
            .edges_filtered::<Expr>(&asg)
            .map(ObjectIndex::cresolve(&asg))
            .map(Expr::span)
            .collect::<Vec<_>>()
    );
}

// As of TAMER,
//   a new restriction is that templates can't just inline whatever they
//   want into their expression expansion context.
#[test]
fn multi_dangling_invalid_expr_shape() {
    let id_tpl = spair("_tpl_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),

          // We have an expression to be expanded inline.
          // We cannot have another.
          ExprStart(ExprOp::Sum, S3),
          ExprEnd(S4),

          // ...but we're provided another!
          // This should error.
          ExprStart(ExprOp::Sum, S5),
          ExprEnd(S6),
        TplEnd(S7),
    ];

    let mut sut = Sut::parse(as_pkg_body(toks));

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // BindIdent

                // This one's okay.
                Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // ExprEnd

                // We start out okay,
                //   because an edge for a dangling expression is not added
                //   until after the expression ends
                //     (we have until then to bind an identifier).
                Ok(Parsed::Incomplete), // ExprStart
                // But the ending token will trigger the error.
                Err(ParseError::StateError(
                    AsgError::TplShapeExprMulti(
                        Some(id_tpl),
                        S5.merge(S6).unwrap(),
                        S3.merge(S4).unwrap()
                    )
                )),

                // RECOVERY: We do not add the edge,
                //   but the object otherwise continues to exist on the
                //   graph,
                //     unreachable.
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let tpl = oi_tpl.resolve(&asg);

    // The error above should be evidence for this,
    //   but let's make sure the error didn't somehow cause the shape to
    //   change.
    assert_eq!(TplShape::Expr(S3.merge(S4).unwrap()), tpl.shape());

    // The template should have only one inline expression;
    //   it should have discarded the other.
    assert_eq!(
        vec![S3.merge(S4).unwrap()],
        oi_tpl
            .edges_filtered::<Expr>(&asg)
            .map(ObjectIndex::cresolve(&asg))
            .map(Expr::span)
            .collect::<Vec<_>>()
    );
}

#[test]
fn close_tpl_mid_open() {
    let id_tpl = spair("_tpl_", S2);
    let id_expr = spair("expr", S4);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),

          ExprStart(ExprOp::Sum, S3),
            BindIdent(id_expr),
        // This is misplaced.
        TplEnd(S5),
          // RECOVERY: Close the expression and try again.
          ExprEnd(S6),
        TplEnd(S7),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // BindIdent
                Ok(Parsed::Incomplete), // ExprStart
                  Ok(Parsed::Incomplete), // BindIdent
              Err(ParseError::StateError(
                  AsgError::UnbalancedTpl(S5))
              ),
                // RECOVERY
                Ok(Parsed::Incomplete), // ExprEnd
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

// If a template is ended with `TplEnd` and was not assigned a name,
//   then it isn't reachable on the graph.
//
// ...that's technically not entirely true in a traversal sense
//   (see following test),
//   but the context would be all wrong.
// It _is_ true from a practical sense,
//   with how NIR and AIR have been constructed at the time of writing,
//   but may not be true in the future.
#[test]
fn unreachable_anonymous_tpl() {
    let id_ok = spair("_tpl_", S4);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          // No BindIdent
        TplEnd(S2),

        // Recovery should ignore the above template
        //   (it's lost to the void)
        //   and allow continuing.
        TplStart(S3),
          BindIdent(id_ok),
        TplEnd(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // TplStart
              Err(ParseError::StateError(AsgError::DanglingTpl(
                  S1.merge(S2).unwrap()
              ))),
              // RECOVERY
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // TplBindIdent
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // Let's make sure that the template created after recovery succeeded.
    pkg_expect_ident_obj::<Tpl>(&ctx, id_ok);
}

#[test]
fn tpl_with_param() {
    let id_tpl = spair("_tpl_", S2);

    let id_param1 = spair("@param1@", S4);
    let pval1 = spair("value1", S5);
    let id_param2 = spair("@param2@", S8);
    let param_desc = spair("param desc", S9);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),

          // Metavariable with a value.
          MetaStart(S3),
            BindIdent(id_param1),
            MetaLexeme(pval1),
          MetaEnd(S6),

          // Required metavariable (no value).
          MetaStart(S7),
            BindIdent(id_param2),
            DocIndepClause(param_desc),
          MetaEnd(S10),

          // Metavariables must be used by something,
          //   otherwise we'll receive an error.
          ExprStart(ExprOp::Sum, S11),
            RefIdent(id_param1),
            RefIdent(id_param2),
          ExprEnd(S14),
        TplEnd(S15),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let tpl = oi_tpl.resolve(&asg);

    assert_eq!(TplShape::Expr(S11.merge(S14).unwrap()), tpl.shape());

    // The template should have an edge to each identifier for each
    //   metavariable.
    let params = [id_param1, id_param2]
        .iter()
        .map(|id| {
            oi_tpl
                .lookup_local_linear(&asg, *id)
                .and_then(|oi| oi.edges_filtered::<Meta>(&asg).next())
                .map(|oi| (oi, oi.resolve(&asg)))
        })
        .collect::<Vec<_>>();

    assert_eq!(
        params[0].unwrap().1,
        &Meta::Lexeme(S3.merge(S6).unwrap(), pval1)
    );
    assert_eq!(
        params[1].unwrap().1,
        &Meta::Required(S7.merge(S10).unwrap())
    );

    // The second param should have a description.
    let doc = params[1]
        .unwrap()
        .0
        .edges_filtered::<Doc>(&asg)
        .last()
        .map(ObjectIndex::cresolve(&asg));

    assert_eq!(doc, Some(&Doc::new_indep_clause(param_desc)));
}

// A template definition nested within another creates a
//   template-defining-template.
// The inner template and its identifier should be entirely contained within
//   the outer (parent) template so that it will expand into a template
//   definition in the context of the expansion site.
#[test]
fn tpl_nested() {
    let id_tpl_outer = spair("_tpl-outer_", S2);
    let id_tpl_inner = spair("_tpl-inner_", S4);

    #[rustfmt::skip]
    let toks = vec![
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template
          TplStart(S3),
            BindIdent(id_tpl_inner),
          TplEnd(S5),
        TplEnd(S6),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    // The outer template should be defined globally,
    //   but not the inner,
    //   since it hasn't been expanded yet.
    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);
    assert_eq!(None, pkg_lookup(&ctx, id_tpl_inner));
    assert_eq!(S1.merge(S6).unwrap(), oi_tpl_outer.resolve(&asg).span());

    // The identifier for the inner template should be local to the outer
    //   template.
    let oi_tpl_inner_ident =
        oi_tpl_outer.lookup_local_linear(&asg, id_tpl_inner);
    let tpl_inner = oi_tpl_inner_ident
        .and_then(|oi| oi.definition_narrow::<Tpl>(&asg))
        .map(ObjectIndex::cresolve(&asg));

    assert_eq!(S3.merge(S5), tpl_inner.map(Tpl::span));

    let tpl_outer = oi_tpl_outer.resolve(&asg);

    // The inner template has no body and so is empty.
    assert_eq!(TplShape::Empty, tpl_inner.unwrap().shape());

    // The outer template defines an inner template but has nothing to
    //   inline,
    //     and so its shape is also empty.
    assert_eq!(TplShape::Empty, tpl_outer.shape());
}

#[test]
fn tpl_doc_short_desc() {
    let id_tpl = spair("foo", S2);
    let clause = spair("short desc", S3);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl),
          DocIndepClause(clause),
        TplEnd(S4),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let oi_docs = oi_tpl
        .edges_filtered::<Doc>(&asg)
        .map(ObjectIndex::cresolve(&asg));

    assert_eq!(
        vec![&Doc::new_indep_clause(clause)],
        oi_docs.collect::<Vec<_>>(),
    );

    // The documentation does not contribute to expansion and therefore does
    //   not influence the shape of the template.
    assert_eq!(TplShape::Empty, oi_tpl.resolve(&asg).shape());
}

// While NIR does not accept metavariables (params) within expressions that
//   are the body of a template,
//     metavariable interpolation does create them.
// Hoisting them out of the expression and into the template context is a
//   fairly simple thing to do for AIR,
//     but would be vastly more complicated for NIR,
//       especially for a streaming parser,
//         as it'd have to hold tokens until it was sure that no
//         interpolation would occur.
// We therefore adopt the simple rule that metavariables are hoisted into
//   the context of the parent template.
// This also gives much greater flexibility to any other (AIR) code
//   generators.
#[test]
fn metavars_within_exprs_hoisted_to_parent_tpl() {
    let id_tpl_outer = spair("_tpl-outer_", S2);
    let id_tpl_inner = spair("_tpl-inner_", S9);

    let id_param_outer = spair("@param_outer@", S5);
    let id_param_inner = spair("@param_inner@", S12);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // This expression begins the body of the template.
          // NIR would not allow params past this point,
          //   but desugaring may produce this.
          ExprStart(ExprOp::Sum, S3),
            // Expresions are not containers and so this metavariable should
            //   be hoisted to the parent container context.
            // That container must be a valid meta context.
            MetaStart(S4),
              BindIdent(id_param_outer),
            MetaEnd(S6),

            RefIdent(id_param_outer),
          ExprEnd(S7),

          // Nested template
          TplStart(S8),
            BindIdent(id_tpl_inner),

            ExprStart(ExprOp::Sum, S10),
              // Hoisting should be relative to the innermost template.
              MetaStart(S11),
                BindIdent(id_param_inner),
              MetaEnd(S13),

              RefIdent(id_param_inner),
            ExprEnd(S14),
          TplEnd(S15),
        TplEnd(S16),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);

    let span_outer = ctx
        .env_scope_lookup::<Ident>(oi_outer, id_param_outer)
        .expect("missing id_param_outer Ident")
        .definition_narrow::<Meta>(asg)
        .expect("missing id_param_outer definition")
        .resolve(asg)
        .span();

    assert_eq!(S4.merge(S6).unwrap(), span_outer);

    let oi_inner = ctx
        .env_scope_lookup::<Ident>(oi_outer, id_tpl_inner)
        .expect("could not locate inner Tpl's Ident")
        .definition_narrow::<Tpl>(asg)
        .expect("missing inner Tpl");

    let span_inner = ctx
        .env_scope_lookup::<Ident>(oi_inner, id_param_inner)
        .expect("missing id_param_inner Ident")
        .definition_narrow::<Meta>(asg)
        .expect("missing id_param_inner definition")
        .resolve(asg)
        .span();

    assert_eq!(S11.merge(S13).unwrap(), span_inner);

    // The template would expand into an expression,
    //   since it otherwise dangling.
    assert_eq!(
        TplShape::Expr(S3.merge(S7).unwrap()),
        oi_outer.resolve(&asg).shape(),
    );
}

#[test]
fn expr_abstract_bind_produces_cross_edge_from_ident_to_meta() {
    let id_tpl = spair("_tpl_", S2);
    let id_meta = spair("@foo@", S4);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          // This identifier is concrete;
          //   the abstract identifier will be the _expression_.
          BindIdent(id_tpl),

          ExprStart(ExprOp::Sum, S3),
            // This expression is bound to an _abstract_ identifier,
            //   which will be expanded at a later time.
            // This does _not_ change the dangling status,
            //   and so can only occur within an expression that acts as a
            //   container for otherwise-dangling expressions.
            BindIdentAbstract(id_meta),
          ExprEnd(S5),
        TplEnd(S6),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    // The expression cannot be indexed by a named (concrete) identifier
    //   and so we must find it through a traversal.
    // The abstract identifier should be bound to our rooting context,
    //   which is the template,
    //   despite not having a name;
    //     this mirrors the same structure that the template body will
    //     assume upon instantiation.
    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl);
    let oi_ident = oi_tpl
        .edges_filtered::<Ident>(asg)
        .next()
        .expect("abstract identifier is not rooted in template");

    assert_eq!(
        None,
        oi_ident.resolve(asg).name(),
        "abstract identifier must not have a concrete name",
    );

    // The metavariable referenced by the abstract identifier
    assert_eq!(id_meta, oi_ident.name_or_meta(asg));

    // The identifier should be bound to the expression.
    let expr = oi_ident
        .definition_narrow::<Expr>(asg)
        .expect("abstract identifier did not bind to Expr")
        .resolve(asg);

    assert_eq!(S3.merge(S5).unwrap(), expr.span());

    // The abstract identifier references the expression,
    //   but the expression does not contain it.
    // Consequently,
    //   the expression is still concrete.
    assert_eq!(expr.meta_state(), MetaState::Concrete);

    // Finally,
    //   the expression should not be considered dangling and so we should
    //   not have an edge directly from the template to the Expr.
    // If that were to happen,
    //   then we'd end up duplicating the expression.
    assert!(
        oi_tpl.edges_filtered::<Expr>(asg).next().is_none(),
        "Tpl must not have an edge directly to Expr \
           (is it considered dangling?)",
    );

    // Because the expression _will be_ bound to an identifier during
    //   instantiation,
    //     it'll be hoisted upon expansion,
    //     and so our shape is still empty.
    // This is the same result as if we had a concrete identifier;
    //   it all ends up expanding into the same thing in the end.
    assert_eq!(TplShape::Empty, oi_tpl.resolve(&asg).shape());
}

// Just because an expression is defined within a template does not mean
//   that it abstract;
//     expressions without metavariable references are concrete.
#[test]
fn expressions_within_tpls_without_metavars_are_concrete() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          // This expression should be concrete,
          //   since it references no metavariables.
          ExprStart(ExprOp::Sum, S3),
            BindIdent(spair("expr", S4)),
          ExprEnd(S5),
        TplEnd(S7),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S7));

    let expr = expect_ident_obj::<Expr>(&ctx, oi_tpl, spair("expr", S8));
    assert_eq!(expr.meta_state(), MetaState::Concrete);
}

#[test]
fn expressions_referencing_metavars_are_abstract() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          // TODO: At the time of writing,
          //     we do not yet notify objects when a
          //     missing Ident has received a definition.
          //   Until that time,
          //     this will remain unresolved.
          ExprStart(ExprOp::Sum, S3),
            BindIdent(spair("expr_pre", S4)),

            // This has yet to be defined,
            //   and so is `Missing`.
            RefIdent(spair("@param@", S5)),               // --.
          ExprEnd(S6),                                    //   |
                                                          //   |
          MetaStart(S7),                                  //   |
            BindIdent(spair("@param@", S8)),              // <-:
                                                          //   |
            // We'll give this metavar a value just to    //   |
            //   show that it doesn't matter that we      //   |
            //   _could_ make the expression concrete;    //   |
            //     expansion is never performed until     //   |
            //     it is explicitly requested.            //   |
            MetaLexeme(spair("value", S9)),               //   |
          MetaEnd(S10),                                   //   |
                                                          //   |
          ExprStart(ExprOp::Sum, S11),                    //   |
            BindIdent(spair("expr_post", S12)),           // <-+--.
                                                          //   |  |
            // This reference causes the parent           //   |  |
            //   expression to become abstract since      //   |  |
            //   it requires expansion.                   //   |  |
            RefIdent(spair("@param@", S13)),              // --'  |
          ExprEnd(S14),                                   //      |
                                                          //      |
          ExprStart(ExprOp::Sum, S15),                    //      |
            BindIdent(spair("expr_conc", S16)),           //      |
                                                          //      |
            // Even though we reference an abstract       //      |
            //   expression,                              //      |
            //     that expression is not our child       //      |
            //     and therefore does not affect          //      |
            //     whether this expression is abstract.   //      |
            RefIdent(spair("expr_post", S17)),            // -----'
          ExprEnd(S18),
        TplEnd(S19),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S20));

    // Both `expr_pre` and `expr_post` expressions are abstract,
    //   containing a reference to a metavariable.
    // Intuitively,
    //   we don't know what we're referencing until that metavariable is
    //   replaced with a lexical value during template application.
    let expr_post =
        expect_ident_obj::<Expr>(&ctx, oi_tpl, spair("expr_post", S21));
    assert_eq!(expr_post.meta_state(), MetaState::Abstract);

    // ...but in the case of expr_pre,
    //   we don't yet notify the object on Ident resolution and so we do not
    //   yet know that it ought to be abstract.
    let expr_pre =
        expect_ident_obj::<Expr>(&ctx, oi_tpl, spair("expr_pre", S22));
    assert_eq!(
        expr_pre.meta_state(),
        MetaState::MaybeConcrete(1_u16.unwrap_into())
    );

    // But an expression that _references_ that abstract expression does not
    //   itself become abstract.
    // That is:
    //   we depend on `expr_post` having been computed before we can
    //   reference it,
    //     but that dependency is not affected by whether `expr_post` is concrete
    //     or abstract.
    // It is certainly required that `expr_post` be made concrete before it
    //   can be lowered into the target,
    //     but provided that occurs
    //       (and there would be a compilation failure if it didn't),
    //       we are unaffected.
    let expr_conc =
        expect_ident_obj::<Expr>(&ctx, oi_tpl, spair("expr_conc", S23));
    assert_eq!(expr_conc.meta_state(), MetaState::Concrete);
}

// If we know of at least _one_ abstract reference,
//   then it does not matter if we have missing references---
//     we are abstract.
#[test]
fn expression_referencing_abstract_with_missing_is_abstract() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          MetaStart(S3),
            BindIdent(spair("@param@", S4)),              // <-.
          MetaEnd(S5),                                    //   |
                                                          //   |
          ExprStart(ExprOp::Sum, S7),                     //   |
            BindIdent(spair("expr", S8)),                 //   |
                                                          //   |
            // This reference causes the parent           //   |
            //   expression to become abstract since      //   |
            //   it requires expansion.                   //   |
            RefIdent(spair("@param@", S9)),               // --'

            // This reference is unknown,
            //   but we're still just abstract,
            //   since it takes only a single abstract reference to make
            //   that determination.
            RefIdent(spair("missing", S10)),
          ExprEnd(S11),
        TplEnd(S12),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S20));

    let expr = expect_ident_obj::<Expr>(&ctx, oi_tpl, spair("expr", S21));
    assert_eq!(expr.meta_state(), MetaState::Abstract);
}

// Independent clauses representing descriptions can also reference
//   metavariables,
//     allowing for documentation generation.
// Because such a clause requires expansion,
//   and because it is owned by the object that it describes,
//   it must cause the parent object to also be interpreted as abstract.
#[test]
fn abstract_doc_clause_binds_to_ref() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          MetaStart(S3),
            BindIdent(spair("@param@", S4)),              // <-.
          MetaEnd(S5),                                    //   |
                                                          //   |
          ExprStart(ExprOp::Sum, S7),                     //   |
            BindIdent(spair("expr", S8)),                 //   |
                                                          //   |
            // This reference causes the parent           //   |
            //   expression to become abstract since      //   |
            //   it requires expansion.                   //   |
            DocIndepClauseRef(spair("@param@", S9)),      // --'
          ExprEnd(S11),
        TplEnd(S12),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S20));

    // Even though the _calculation_ portion is concrete,
    //   the Doc object causes this expression to be abstract.
    let oi_expr = expect_ident_oi::<Expr>(&ctx, oi_tpl, spair("expr", S21));
    let expr = oi_expr.resolve(asg);
    assert_eq!(expr.meta_state(), MetaState::Abstract);

    let oi_doc = oi_expr
        .edges_filtered::<Doc>(asg)
        .next()
        .expect("missing Doc object on Expr");
    let doc = oi_doc.resolve(asg);

    assert_eq!(&Doc::AbstractIndepClause(S9), doc);

    let oi_ident = oi_doc
        .edges_filtered::<Ident>(asg)
        .next()
        .expect("missing abstract doc ident");
    assert_eq!(S4, oi_ident.resolve(asg).span());
}

// Independent clauses representing descriptions can also reference
//   metavariables,
//     allowing for documentation generation.
// Because such a clause requires expansion,
//   and because it is owned by the object that it describes,
//   it must cause the parent object to also be interpreted as abstract.
//
// It is not expected that the user will encounter this error in practice
//   when using the source XML language,
//     because interpolation ensures that documentation is either a literal
//     or a proper metavariable reference.
// With that said,
//   the intent is to have other IRs lower into AIR in the future.
#[test]
fn abstract_doc_clause_cannot_bind_to_non_meta_ref() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          ExprStart(ExprOp::Sum, S3),
            BindIdent(spair("notmeta", S4)),              // <-.
          ExprEnd(S5),                                    //   |
                                                          //   |
          ExprStart(ExprOp::Sum, S7),                     //   |
            BindIdent(spair("expr", S8)),                 //   |
                                                          //   |
            // This references another expression.        //   |
            DocIndepClauseRef(spair("notmeta", S9)),      // --'
          ExprEnd(S11),
        TplEnd(S12),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // BindIdent

                Ok(Parsed::Incomplete), // ExprStart
                  Ok(Parsed::Incomplete), // BindIdent
                Ok(Parsed::Incomplete), // ExprEnd

                Ok(Parsed::Incomplete), // ExprStart
                  Ok(Parsed::Incomplete), // BindIdent

                  Err(ParseError::StateError(
                    AsgError::InvalidDocRef(S9, S4)
                  )),

                  // RECOVERY: We build the expression,
                  //   omitting the documentation string.
                Ok(Parsed::Incomplete), // ExprEnd
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();
    let asg = ctx.asg_ref();

    // Let's make sure that the template created after recovery succeeded.
    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S20));

    // We should have kept the expression despite the error.
    assert!(
        oi_tpl
            .edges_filtered::<Ident>(asg)
            .filter_map(|oi_ident| oi_ident.definition_narrow::<Expr>(asg))
            .any(|oi| oi.resolve(asg).span() == S7.merge(S11).unwrap())
    );
}

/// All template metavariables (params) must hold at least one reference.
/// For rationale,
///   see the parent module's documentation.
#[test]
fn unused_metavars_at_definition_tpl_close_produce_error() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl", S2)),

          MetaStart(S3),
            BindIdent(spair("@used@", S4)),      // <-.
          MetaEnd(S5),                           //   |
                                                 //   |
          MetaStart(S6),                         //   |
            BindIdent(spair("@unused_a@", S7)),  //   |
          MetaEnd(S8),                           //   |
                                                 //   |
          MetaStart(S9),                         //   |
            BindIdent(spair("@unused_b@", S10)), //   |
          MetaEnd(S11),                          //   |
                                                 //   |
          ExprStart(ExprOp::Sum, S12),           //   |
            RefIdent(spair("@used@", S13)),      // --'
          ExprEnd(S14),
        TplEnd(S15),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Err(ParseError::StateError(AsgError::TplUnusedParams(
                Some(spair("_tpl", S2)),
                vec![
                    spair("@unused_a@", S7),
                    spair("@unused_b@", S10),
                ],
            ))),
        ],
        sut.by_ref().filter(Result::is_err).collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // The template should not have been defined.
    // This is admittedly driven by implementation details:
    //   no other part of AIR's parsing continues with an operation despite
    //   an error at the time of writing.
    // TODO: Should we allow the template to be defined,
    //   so that the user doesn't get errors saying the template doesn't
    //   exist?
    assert_eq!(None, pkg_lookup(&ctx, spair("_tpl_", S20)));
}
