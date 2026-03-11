// Tests for AIR template application
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

use super::*;

#[test]
fn tpl_apply_within_expr() {
    let id_expr = spair("expr", S3);
    let id_tpl = spair("_tpl_", S5);
    let ref_tpl = spair("_tpl_", S8);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S2),
          BindIdent(id_expr),

          // This will not be present in the final expression,
          //   as if it were hoisted out.
          TplStart(S4),
            BindIdent(id_tpl),
          TplEnd(S6),

          // But the application will remain.
          TplStart(S7),
            RefIdent(ref_tpl),
          TplEndRef(S9),
        ExprEnd(S10),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let tpl = pkg_expect_ident_obj::<Tpl>(&ctx, id_tpl);
    assert_eq!(S4.merge(S6).unwrap(), tpl.span());
    assert_eq!(TplShape::Empty, tpl.shape());

    // The expression that was produced on the graph ought to be equivalent
    //   to the expression without the template being present at all,
    //     but retaining the _application_.
    let oi_expr = pkg_expect_ident_oi::<Expr>(&ctx, id_expr);
    let expr = oi_expr.resolve(&asg);
    assert_eq!(S2.merge(S10).unwrap(), expr.span());
    assert_eq!(
        #[rustfmt::skip]
        vec![
            S7.merge(S9).unwrap(),
        ],
        oi_expr
            .edges(&asg)
            .map(|rel| rel.widen().resolve(&asg).span())
            .collect::<Vec<_>>()
    );
}

// Normally we cannot reference objects without an identifier using AIR
//   (at the time of writing at least),
//   but `TplEndRef` is an exception.
#[test]
fn anonymous_tpl_immediate_ref() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          // No BindIdent
        // But ended with `TplEndRef`,
        //   so the missing identifier is okay.
        // This would fail if it were `TplEnd`.
        TplEndRef(S2),
    ];

    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));
}

// A template application within another template can be interpreted as
//   either applying the template as much as possible into the body of the
//   template definition,
//     or as expanding the template application into the expansion site and
//     _then_ expanding the inner template at the expansion site.
// Both will yield equivalent results,
//   but in either case,
//   it all starts the same.
#[test]
fn tpl_apply_nested() {
    let id_tpl_outer = spair("_tpl-outer_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template application
          TplStart(S3),
          TplEndRef(S4),
        TplEnd(S5),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);
    assert_eq!(S1.merge(S5).unwrap(), oi_tpl_outer.resolve(&asg).span());

    // The inner template,
    //   being a template application,
    //   should be a direct child of the outer template.
    let inners = oi_tpl_outer
        .edges_filtered::<Tpl>(&asg)
        .map(|oi| oi.resolve(&asg).span());

    assert_eq!(vec![S3.merge(S4).unwrap()], inners.collect::<Vec<_>>());

    // Since the inner template is empty,
    //   so too should the outer.
    let tpl_outer = oi_tpl_outer.resolve(&asg);
    assert_eq!(TplShape::Empty, tpl_outer.shape());
}

// Template application should resolve all the same regardless of order of
//   ref/def.
#[test]
fn tpl_apply_nested_missing() {
    let id_tpl_outer = spair("_tpl-outer_", S2);

    let tpl_inner = "_tpl-inner_";
    let id_tpl_inner = spair(tpl_inner, S7);
    let ref_tpl_inner_pre = spair(tpl_inner, S4);
    let ref_tpl_inner_post = spair(tpl_inner, S10);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template application (Missing)
          TplStart(S3),
            RefIdent(ref_tpl_inner_pre),               // -.
          TplEndRef(S5),                               //  |
                                                       //  |
          // Define the template above                 //  |
          TplStart(S6),                                //  |
            BindIdent(id_tpl_inner),                   // <:
          TplEnd(S8),                                  //  |
                                                       //  |
          // Apply again,                              //  |
          //   this time _after_ having been defined.  //  |
          TplStart(S9),                                //  |
            RefIdent(ref_tpl_inner_post),              // -'
          TplEndRef(S11),
        TplEnd(S12),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);
    assert_eq!(S1.merge(S12).unwrap(), oi_tpl_outer.resolve(&asg).span());

    // TODO: It should be the case that we apply two templates,
    //     both of which are empty,
    //     and so the outer shape is still empty.
    //   But until we notify templates of `Missing` ident resolution,
    //     we don't know the shape of the template by the time we reach the
    //     first reference.
    //   Consequently,
    //     `TplShape::Unknown` must take precedence to reflect this
    //     uncertainty.
    let tpl_outer = oi_tpl_outer.resolve(&asg);
    assert_eq!(TplShape::Unknown(S3.merge(S5).unwrap()), tpl_outer.shape());

    // The inner template should be contained within the outer and so not
    //   globally resolvable.
    assert!(pkg_lookup(&ctx, id_tpl_inner).is_none());

    // But it is accessible as a local on the outer template.
    let oi_tpl_inner = oi_tpl_outer
        .lookup_local_linear(&asg, id_tpl_inner)
        .expect("could not locate inner template as a local")
        .definition_narrow::<Tpl>(&asg)
        .expect("could not resolve inner template ref to Tpl");

    // We should have two inner template applications.
    let inners = oi_tpl_outer.edges_filtered::<Tpl>(&asg).collect::<Vec<_>>();
    assert_eq!(
        vec![S9.merge(S11).unwrap(), S3.merge(S5).unwrap()],
        inners
            .iter()
            .map(|oi| oi.resolve(&asg).span())
            .collect::<Vec<_>>(),
    );

    // Each of those inner template applications should have resolved to the
    //   same template,
    //     despite their varying ref/def ordering.
    assert_eq!(
        vec![oi_tpl_inner, oi_tpl_inner],
        inners
            .iter()
            .flat_map(|oi| oi.edges_filtered::<Ident>(&asg))
            .filter_map(|oi| oi.definition_narrow(&asg))
            .collect::<Vec<_>>(),
    );
}

#[test]
fn tpl_inner_apply_inherit_shape() {
    let id_tpl_outer = spair("_tpl-outer_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template application has an Expr shape.
          TplStart(S3),
            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),
          TplEndRef(S6),
        TplEnd(S7),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);
    let oi_tpl_inner = oi_tpl_outer.edges_filtered::<Tpl>(&asg).next().unwrap();

    // The inner template's expression should determine its shape.
    assert_eq!(
        TplShape::Expr(S4.merge(S5).unwrap()),
        oi_tpl_inner.resolve(&asg).shape()
    );

    // Since the outer template applies the inner,
    //   it inherits the inner's shape.
    // (Imagine pasting the body of the inner template inline.)
    assert_eq!(
        TplShape::Expr(S3.merge(S6).unwrap()),
        oi_tpl_outer.resolve(&asg).shape()
    );
}

// It's okay to have sibling template applications that aren't Exprs.
#[test]
fn tpl_inner_apply_expr_alongside_empty() {
    let id_tpl_outer = spair("_tpl-outer_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template application has an Expr shape.
          TplStart(S3),
            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),
          TplEndRef(S6),

          // But this is empty.
          TplStart(S7),
          TplEndRef(S8),
        TplEnd(S9),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);

    // The Expr shape takes precedence.
    assert_eq!(
        TplShape::Expr(S3.merge(S6).unwrap()),
        oi_tpl_outer.resolve(&asg).shape()
    );
}

// Template applications with Expr shapes yield the same errors as if the
//   bodies were just pasted inline,
//     with the exception of the spans that are reported.
// The spans we provide to the user should be in the context of the template
//   being defined;
//     if we inherit the shape from the template,
//       then the span would be for the expression _inside_ that template,
//         which would compose to the innermost application's expression,
//           recursively.
// And while that would certainly make for an impressive display if you
//   understood what was going on,
//     that breaks every layer of abstraction that was built and is not what
//     we want to provide.
#[test]
fn tpl_inner_apply_expr_alongside_another_apply_expr() {
    let id_tpl_outer = spair("_tpl-outer_", S2);

    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(id_tpl_outer),

          // Inner template application has an Expr shape.
          TplStart(S3),
            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),
          TplEndRef(S6),

          // As does this one,
          //   which should produce an error.
          TplStart(S7),
            ExprStart(ExprOp::Sum, S8),
            ExprEnd(S9),
          TplEndRef(S10),
        TplEnd(S11),
    ];

    let mut sut = Sut::parse(as_pkg_body(toks));

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // TplStart
                Ok(Parsed::Incomplete), // BindIdent

                // This one's okay and changes the template's shape.
                Ok(Parsed::Incomplete), // TplStart
                  Ok(Parsed::Incomplete), // ExprStart
                  Ok(Parsed::Incomplete), // ExprEnd
                Ok(Parsed::Incomplete), // TplEnd

                // But this one runs afoul of that new shape.
                Ok(Parsed::Incomplete), // TplStart
                  Ok(Parsed::Incomplete), // ExprStart
                  Ok(Parsed::Incomplete), // ExprEnd
                Err(ParseError::StateError(
                    // These spans are relative to the application itself.
                    // This is important to present the appropriate level of
                    //   abstraction;
                    //     see the comment for this test case.
                    AsgError::TplShapeExprMulti(
                        Some(id_tpl_outer),
                        S7.merge(S10).unwrap(),
                        S3.merge(S6).unwrap()
                    )
                )),
                // RECOVERY: We ignore the template by not adding the edge.
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();
    let asg = ctx.asg_ref();

    let oi_tpl_outer = pkg_expect_ident_oi::<Tpl>(&ctx, id_tpl_outer);

    assert_eq!(
        TplShape::Expr(S3.merge(S6).unwrap()),
        oi_tpl_outer.resolve(&asg).shape()
    );

    // The second template application should have been omitted.
    assert_eq!(
        vec![S3.merge(S6).unwrap()],
        oi_tpl_outer
            .edges_filtered::<Tpl>(&asg)
            .map(ObjectIndex::cresolve(&asg))
            .map(Tpl::span)
            .collect::<Vec<_>>()
    );
}

// A simpler version of the above test that asserts against the explicit
//   case of sibling expressions in a template body,
//     with no wrapping or indirection.
#[test]
fn tpl_multi_expr() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl_", S2)),

          // First one is okay.
          ExprStart(ExprOp::Sum, S3),
          ExprEnd(S4),

          // But the second is in error and will be dropped.
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

                // This one's okay and changes the template's shape.
                Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // ExprEnd

                // But this one runs afoul of that new shape.
                Ok(Parsed::Incomplete), // ExprStart
                Err(ParseError::StateError(
                    AsgError::TplShapeExprMulti(
                        Some(spair("_tpl_", S2)),
                        S5.merge(S6).unwrap(),
                        S3.merge(S4).unwrap(),
                    )
                )),
                // RECOVERY: We ignore the expression by not adding its
                //   edge.
              Ok(Parsed::Incomplete), // TplEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();
    let asg = ctx.asg_ref();

    let oi_tpl = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl_", S8));

    assert_eq!(
        TplShape::Expr(S3.merge(S4).unwrap()),
        oi_tpl.resolve(&asg).shape()
    );

    // The second Expr should have been omitted.
    assert_eq!(
        vec![S3.merge(S4).unwrap()],
        oi_tpl
            .edges_filtered::<Expr>(&asg)
            .map(ObjectIndex::cresolve(&asg))
            .map(Expr::span)
            .collect::<Vec<_>>()
    );
}

// Identifiers are a layer of indirection,
//   but the object that they identify should influence the shape of the
//   template all the same.
#[test]
fn tpl_ref_expr_shape() {
    #[rustfmt::skip]
    let toks = [
        TplStart(S1),
          BindIdent(spair("_tpl-pre_", S2)),

          RefIdent(spair("expr", S3)),                // --,
        TplEnd(S4),                                   //   |
                                                      //   | U
        ExprStart(ExprOp::Sum, S5),                   //   |
          BindIdent(spair("expr", S6)),               // <=:
        ExprEnd(S7),                                  //   |
                                                      //   | E
        TplStart(S8),                                 //   |
          BindIdent(spair("_tpl-post_", S9)),         // <-+-.
                                                      //   | |
          RefIdent(spair("expr", S10)),               // --' |
        TplEnd(S11),                                  //     |
                                                      //     |
        // Similarly,                                 //     |
        //   if we have a template that references    //     |
        //   one of the above templates,              //     |
        //     it too should have the same            //     | E
        //     Expr shape,                            //     |
        //       since the reference represents       //     |
        //       application.                         //     |
        TplStart(S12),                                //     |
          BindIdent(spair("_tpl-post-tpl-ref_", S13)),//     |
                                                      //     |
          RefIdent(spair("_tpl-post_", S14)),         // ----'
        TplEnd(S14),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_tpl_pre = pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl-pre_", S15));
    let oi_tpl_post =
        pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl-post_", S16));
    let oi_tpl_post_tpl_ref =
        pkg_expect_ident_oi::<Tpl>(&ctx, spair("_tpl-post-tpl-ref_", S17));

    // TODO: We haven't yet handled notification when an identifier has been
    //   resolved,
    //     so the shape will be unknown until then.
    assert_eq!(TplShape::Unknown(S3), oi_tpl_pre.resolve(&asg).shape(),);

    // But the template defined _after_ the expression will be immediately
    //   resolved and its shape known.
    assert_eq!(
        // Note that the span is that of the _reference_,
        //   since that is what appears within the template body and that
        //   which we want to emphasize in diagnostics.
        TplShape::Expr(S10),
        oi_tpl_post.resolve(&asg).shape(),
    );

    // And the final template applies the preceding one,
    //   and should inherit its shape since.
    assert_eq!(
        // Just as above,
        //   the span is the topmost _reference_,
        //     which is the application.
        TplShape::Expr(S14),
        oi_tpl_post_tpl_ref.resolve(&asg).shape(),
    );
}
