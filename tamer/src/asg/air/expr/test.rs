// Tests for ASG IR expression parsing
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
use crate::span::dummy::*;
use crate::{
    asg::{
        air::{
            test::{
                air_ctx_from_pkg_body_toks, air_ctx_from_toks,
                parse_as_pkg_body, pkg_expect_ident_obj, pkg_expect_ident_oi,
                pkg_lookup,
            },
            Air::*,
            AirAggregate,
        },
        graph::object::{expr::ExprRel, Doc, ObjectRel},
        ExprOp, Ident,
    },
    parse::util::spair,
};
use std::assert_matches::assert_matches;

type Sut = AirAggregate;

pub fn collect_subexprs(
    asg: &Asg,
    oi: ObjectIndex<Expr>,
) -> Vec<(ObjectIndex<Expr>, &Expr)> {
    oi.edges(&asg)
        .filter_map(|rel| rel.narrow::<Expr>())
        .map(|oi| (oi, oi.resolve(&asg)))
        .collect::<Vec<_>>()
}

#[test]
fn expr_empty_ident() {
    let id = spair("foo", S2);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id),
        ExprEnd(S3),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);

    // The expression should have been bound to this identifier so that
    //   we're able to retrieve it from the graph by name.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());
}

#[test]
fn expr_without_pkg() {
    let toks = [
        // No package
        //   (because we're not parsing with `parse_as_pkg_body` below)
        ExprStart(ExprOp::Sum, S1),
        // RECOVERY
        PkgStart(S2, spair("/pkg", S2)),
        PkgEnd(S3),
    ];

    assert_eq!(
        vec![
            Err(ParseError::StateError(AsgError::PkgExpected(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgStart
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

// Note that this can't happen in e.g. NIR / TAME's source XML.
#[test]
fn close_pkg_mid_expr() {
    let id = spair("foo", S4);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, spair("/pkg", S1)),
          ExprStart(ExprOp::Sum, S2),
        PkgEnd(S3),
            // RECOVERY: Let's finish the expression first...
            BindIdent(id),
          ExprEnd(S5),
        // ...and then try to close again.
        PkgEnd(S6),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // ExprStart
            Err(ParseError::StateError(AsgError::InvalidPkgEndContext(S3))),
                // RECOVERY: We should be able to close the package if we
                //   just finish the expression first,
                //     demonstrating that recovery properly maintains all
                //     state.
                Ok(Parsed::Incomplete), // BindIdent
              Ok(Parsed::Incomplete), // ExprEnd
            // Successful close here.
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn open_pkg_mid_expr() {
    let pkg_a = spair("/pkg", S1);
    let pkg_nested = spair("/pkg-nested", S3);
    let id = spair("foo", S4);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, pkg_a),
          ExprStart(ExprOp::Sum, S2),
        PkgStart(S3, pkg_nested),
            // RECOVERY: We should still be able to complete successfully.
            BindIdent(id),
          ExprEnd(S5),
        // Closes the _original_ package.
        PkgEnd(S6),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // ExprStart
            Err(ParseError::StateError(AsgError::NestedPkgStart(
                (S3, pkg_nested),
                (S1, pkg_a),
            ))),
                // RECOVERY: Ignore the open and continue.
                //   Of course,
                //     this means that any identifiers would be defined in a
                //     different package than was likely intended,
                //       but at least we'll be able to keep processing.
                Ok(Parsed::Incomplete), // BindIdent
              Ok(Parsed::Incomplete), // ExprEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_non_empty_ident_root() {
    let id_a = spair("foo", S2);
    let id_b = spair("bar", S2);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // Identifier while still empty...
          BindIdent(id_a),

          ExprStart(ExprOp::Sum, S3),
            // (note that the inner expression _does not_ have an ident
            //   binding)
          ExprEnd(S4),

          // ...and an identifier non-empty.
          BindIdent(id_b),
        ExprEnd(S6),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);

    let expr_a = pkg_expect_ident_obj::<Expr>(&ctx, id_a);
    assert_eq!(expr_a.span(), S1.merge(S6).unwrap());

    // Identifiers should reference the same expression.
    let expr_b = pkg_expect_ident_obj::<Expr>(&ctx, id_b);
    assert_eq!(expr_a, expr_b);
}

// Binding an identifier after a child expression means that the parser is
//   creating an expression that is a child of a dangling expression,
//     which only becomes reachable at the end.
#[test]
fn expr_non_empty_bind_only_after() {
    let id = spair("foo", S2);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // Expression root is still dangling at this point.
          ExprStart(ExprOp::Sum, S2),
          ExprEnd(S3),

          // We only bind an identifier _after_ we've created the expression,
          //   which should cause the still-dangling root to become
          //   reachable.
          BindIdent(id),
        ExprEnd(S5),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);

    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id);
    assert_eq!(expr.span(), S1.merge(S5).unwrap());
}

// Danging expressions are unreachable and therefore not useful
//   constructions.
// Prohibit them,
//   since they're either mistakes or misconceptions.
#[test]
fn expr_dangling_no_subexpr() {
    let toks = [
        ExprStart(ExprOp::Sum, S1),
        // No `BindIdent`,
        //   so this expression is dangling.
        ExprEnd(S2),
    ];

    // The error span should encompass the entire expression.
    let full_span = S1.merge(S2).unwrap();

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete),
              Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr() {
    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // Expression root is still dangling at this point.
          ExprStart(ExprOp::Sum, S2),
          ExprEnd(S3),
        // Still no ident binding,
        //   so root should still be dangling.
        ExprEnd(S4),
    ];

    let full_span = S1.merge(S4).unwrap();

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete),     // PkgStart
              Ok(Parsed::Incomplete),   // ExprStart
                Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // ExprEnd
              Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr_ident() {
    let id = spair("foo", S3);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // Expression root is still dangling at this point.
          ExprStart(ExprOp::Sum, S2),
            // The _inner_ expression receives an identifier,
            //   but that should have no impact on the dangling status of
            //   the root,
            //     especially given that subexpressions are always reachable
            //     anyway.
            BindIdent(id),
          ExprEnd(S4),
          // But the root still has no ident binding,
          //   and so should still be dangling.
        ExprEnd(S5),
    ];

    let full_span = S1.merge(S5).unwrap();

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete),       // PkgStart
              Ok(Parsed::Incomplete),     // ExprStart
                Ok(Parsed::Incomplete),   // ExprStart
                  Ok(Parsed::Incomplete), // BindIndent
                Ok(Parsed::Incomplete),   // ExprEnd
              Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

// Ensure that the parser correctly recognizes dangling expressions after
//   having encountered a reachable expression.
// Ideally the parser will have been written to make this impossible,
//   but this also protects against potential future breakages.
#[test]
fn expr_reachable_subsequent_dangling() {
    let id = spair("foo", S2);

    #[rustfmt::skip]
    let toks = [
        // Reachable
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id),
        ExprEnd(S3),

        // Dangling
        ExprStart(ExprOp::Sum, S4),
        ExprEnd(S5),
    ];

    // The error span should encompass the entire expression.
    // TODO: ...let's actually have something inside this expression.
    let second_span = S4.merge(S5).unwrap();

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              // Reachable
              Ok(Parsed::Incomplete),
                Ok(Parsed::Incomplete),
              Ok(Parsed::Incomplete),
              // Dangling
              Ok(Parsed::Incomplete),
              Err(ParseError::StateError(AsgError::DanglingExpr(second_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

// Recovery from dangling expression.
#[test]
fn recovery_expr_reachable_after_dangling() {
    let id = spair("foo", S4);

    #[rustfmt::skip]
    let toks = [
        // Dangling
        ExprStart(ExprOp::Sum, S1),
        ExprEnd(S2),

        // Reachable, after error from dangling.
        ExprStart(ExprOp::Sum, S3),
          BindIdent(id),
        ExprEnd(S5),
    ];

    // The error span should encompass the entire expression.
    let err_span = S1.merge(S2).unwrap();

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete),
              Err(ParseError::StateError(AsgError::DanglingExpr(err_span))),

              // RECOVERY: continue at this point with the next expression.
              Ok(Parsed::Incomplete),
                Ok(Parsed::Incomplete),
              Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // Let's make sure that we _actually_ added it to the graph,
    //   despite the previous error.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id);
    assert_eq!(expr.span(), S3.merge(S5).unwrap());

    // The dangling expression may or may not be on the graph,
    //   but it doesn't matter;
    //     we cannot reference it
    //       (unless we break abstraction and walk the underlying graph).
    // Let's leave this undefined so that we have flexibility in what we
    //   decide to do in the future.
    // So we end here.
}

#[test]
fn expr_close_unbalanced() {
    let id = spair("foo", S3);

    #[rustfmt::skip]
    let toks = [
        // Close before _any_ open.
        ExprEnd(S1),

        // Should recover,
        //   allowing for a normal expr.
        ExprStart(ExprOp::Sum, S2),
          BindIdent(id),
        ExprEnd(S4),

        // And now an extra close _after_ a valid expr.
        ExprEnd(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Err(ParseError::StateError(AsgError::UnbalancedExpr(S1))),

              // RECOVERY
              Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // BindIdent
              Ok(Parsed::Incomplete), // ExprEnd

              // Another error after a successful expression.
              Err(ParseError::StateError(AsgError::UnbalancedExpr(S5))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // Just verify that the expression was successfully added after recovery.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id);
    assert_eq!(expr.span(), S2.merge(S4).unwrap());
}

// Subexpressions should not only have edges to their parent,
//   but those edges ought to be ordered,
//   allowing TAME to handle non-commutative expressions.
// We must further understand the relative order in which edges are stored
//   for non-associative expressions.
#[test]
fn sibling_subexprs_have_ordered_edges_to_parent() {
    let id_root = spair("root", S1);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // Identify the root so that it is not dangling.
          BindIdent(id_root),

          // Sibling A
          ExprStart(ExprOp::Sum, S3),
          ExprEnd(S4),

          // Sibling B
          ExprStart(ExprOp::Sum, S5),
          ExprEnd(S6),

          // Sibling C
          ExprStart(ExprOp::Sum, S7),
          ExprEnd(S8),
        ExprEnd(S9),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    // The root is the parent expression that should contain edges to each
    //   subexpression
    //     (the siblings above).
    // Note that we retrieve its _index_,
    //   not the object itself.
    let oi_root = pkg_expect_ident_oi::<Expr>(&ctx, id_root);

    let siblings = oi_root
        .edges_filtered::<Expr>(&asg)
        .map(ObjectIndex::cresolve(&asg))
        .collect::<Vec<_>>();

    // The reversal here is an implementation detail with regards to how
    //   Petgraph stores its edges as effectively linked lists,
    //     using node indices instead of pointers.
    // It is very important that we understand this behavior.
    assert_eq!(siblings.len(), 3);
    assert_eq!(siblings[2].span(), S3.merge(S4).unwrap());
    assert_eq!(siblings[1].span(), S5.merge(S6).unwrap());
    assert_eq!(siblings[0].span(), S7.merge(S8).unwrap());
}

#[test]
fn nested_subexprs_related_to_relative_parent() {
    let id_root = spair("root", S1);
    let id_suba = spair("suba", S2);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1), // 0
          BindIdent(id_root),

          ExprStart(ExprOp::Sum, S2), // 1
            BindIdent(id_suba),

            ExprStart(ExprOp::Sum, S3), // 2
            ExprEnd(S4),
          ExprEnd(S5),
        ExprEnd(S6),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_0 = pkg_expect_ident_oi::<Expr>(&ctx, id_root);
    let subexprs_0 = collect_subexprs(&asg, oi_0);

    // Subexpr 1
    assert_eq!(subexprs_0.len(), 1);
    let (oi_1, subexpr_1) = subexprs_0[0];
    assert_eq!(subexpr_1.span(), S2.merge(S5).unwrap());

    let subexprs_1 = collect_subexprs(&asg, oi_1);

    // Subexpr 2
    assert_eq!(subexprs_1.len(), 1);
    let (_, subexpr_2) = subexprs_1[0];
    assert_eq!(subexpr_2.span(), S3.merge(S4).unwrap());
}

#[test]
fn expr_redefine_ident() {
    // Same identifier but with different spans
    //   (which would be the case in the real world).
    let id_first = spair("foo", S2);
    let id_dup = spair("foo", S3);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id_first),

          ExprStart(ExprOp::Sum, S3),
            BindIdent(id_dup),
          ExprEnd(S4),
        ExprEnd(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // BindIdent (first)
                Ok(Parsed::Incomplete), // ExprStart
                  Err(ParseError::StateError(AsgError::IdentRedefine(
                      id_first,
                      id_dup.span(),
                  ))),
                // RECOVERY: Ignore the attempt to redefine and continue.
                Ok(Parsed::Incomplete), // ExprEnd
              Ok(Parsed::Incomplete), // ExprEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // The identifier should continue to reference the first expression.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id_first);
    assert_eq!(expr.span(), S1.merge(S5).unwrap());
}

// Similar to the above test,
//   but with two entirely separate expressions,
//   such that a failure to identify an expression ought to leave it in an
//     unreachable state.
#[test]
fn expr_still_dangling_on_redefine() {
    // Same identifier but with different spans
    //   (which would be the case in the real world).
    let id_first = spair("foo", S2);
    let id_dup = spair("foo", S5);
    let id_dup2 = spair("foo", S8);
    let id_second = spair("bar", S9);

    #[rustfmt::skip]
    let toks = [
        // First expr (OK)
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id_first),
        ExprEnd(S3),

        // Second expr should still dangle due to use of duplicate
        //   identifier
        ExprStart(ExprOp::Sum, S4),
          BindIdent(id_dup),
        ExprEnd(S6),

        // Third expr will error on redefine but then be successful.
        // This probably won't happen in practice with TAME's original
        //   source language,
        //     but could happen at e.g. a REPL.
        ExprStart(ExprOp::Sum, S7),
          BindIdent(id_dup2),   // fail
          BindIdent(id_second), // succeed
        ExprEnd(S10),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // ExprStart
              Ok(Parsed::Incomplete), // BindIdent (first)
              Ok(Parsed::Incomplete), // ExprEnd

              // Beginning of second expression
              Ok(Parsed::Incomplete), // ExprStart
                Err(ParseError::StateError(AsgError::IdentRedefine(
                    id_first,
                    id_dup.span(),
                ))),
              // RECOVERY: Ignore the attempt to redefine and continue.
              // ...but then immediately fail _again_ because we've closed a
              //   dangling expression.
              Err(ParseError::StateError(AsgError::DanglingExpr(
                  S4.merge(S6).unwrap()
              ))),

              // RECOVERY: But we'll continue onto one final expression,
              //   which we will fail to define but then subsequently define
              //   successfully.
              Ok(Parsed::Incomplete), // ExprStart
                Err(ParseError::StateError(AsgError::IdentRedefine(
                    id_first,
                    id_dup2.span(),
                ))),
                // RECOVERY: Despite the initial failure,
                //   we can now re-attempt to bind with a unique id.
                Ok(Parsed::Incomplete), // BindIdent (second)
              Ok(Parsed::Incomplete), // ExprEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // The identifier should continue to reference the first expression.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id_first);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());

    // There's nothing we can do using the ASG's public API at the time of
    //   writing to try to reference the dangling expression.

    // The second identifier should have been successfully bound despite the
    //   failed initial attempt.
    let expr = pkg_expect_ident_obj::<Expr>(&ctx, id_second);
    assert_eq!(expr.span(), S7.merge(S10).unwrap());
}

#[test]
fn expr_ref_to_ident() {
    let id_foo = spair("foo", S2);
    let id_bar = spair("bar", S6);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id_foo),

          // Reference to an as-of-yet-undefined id (okay),
          //   with a different span than `id_bar`.
          RefIdent(spair("bar", S3)),
        ExprEnd(S4),

        //
        // Another expression to reference the first
        //   (we don't handle cyclic references until a topological sort,
        //     so no point in referencing ourselves;
        //       it'd work just fine here.)
        ExprStart(ExprOp::Sum, S5),
          BindIdent(id_bar),
        ExprEnd(S7),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_foo = pkg_expect_ident_oi::<Expr>(&ctx, id_foo);

    let mut foo_rels = oi_foo
        .edges(&asg)
        .filter_map(ExprRel::narrows_into::<Ident>)
        .collect::<Vec<_>>();

    // We should have only a single reference (to `id_bar`).
    assert_eq!(foo_rels.len(), 1);

    let oi_ident_bar =
        foo_rels.pop().and_then(ExprRel::narrow::<Ident>).unwrap();
    let ident_bar = oi_ident_bar.resolve(&asg);

    // The identifier will have originally been `Missing`,
    //   since it did not exist at the point of reference.
    // But it should now properly identify the other expression.
    assert_matches!(ident_bar, Ident::Transparent(..));

    // The span of the identifier must be updated with the defining
    //   `BindIdent`,
    //     otherwise it'll be the location of the `RefIdent` that originally
    //     added it as `Missing`.
    assert_eq!(ident_bar.span(), id_bar.span());

    let oi_expr_bar = pkg_expect_ident_oi::<Expr>(&ctx, id_bar);
    assert!(oi_ident_bar.is_bound_to(&asg, oi_expr_bar));
}

#[test]
fn idents_share_defining_pkg() {
    let id_foo = spair("foo", S3);
    let id_bar = spair("bar", S5);
    let id_baz = spair("baz", S6);

    // An expression nested within another.
    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, spair("/pkg", S1)),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_foo),

            ExprStart(ExprOp::Sum, S4),
              BindIdent(id_bar),
              RefIdent(id_baz),
            ExprEnd(S7),
          ExprEnd(S8),
        PkgEnd(S9),
    ];

    let ctx = air_ctx_from_toks(toks);
    let asg = ctx.asg_ref();

    let oi_foo = pkg_lookup(&ctx, id_foo).unwrap();
    let oi_bar = pkg_lookup(&ctx, id_bar).unwrap();

    assert_eq!(oi_foo.src_pkg(asg).unwrap(), oi_bar.src_pkg(asg).unwrap());

    // Missing identifiers should not have a source package,
    //   since we don't know what defined it yet.
    let oi_baz = pkg_lookup(&ctx, id_baz).unwrap();
    assert_eq!(None, oi_baz.src_pkg(asg));

    // The package span should encompass the entire definition.
    assert_eq!(
        S1.merge(S9),
        oi_foo.src_pkg(asg).map(|pkg| pkg.resolve(asg).span())
    )
}

#[test]
fn expr_doc_short_desc() {
    let id_expr = spair("foo", S2);
    let clause = spair("short desc", S3);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          BindIdent(id_expr),
          DocIndepClause(clause),
        ExprEnd(S4),
    ];

    let ctx = air_ctx_from_pkg_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_expr = pkg_expect_ident_oi::<Expr>(&ctx, id_expr);
    let oi_docs = oi_expr
        .edges_filtered::<Doc>(&asg)
        .map(ObjectIndex::cresolve(&asg));

    assert_eq!(
        vec![&Doc::new_indep_clause(clause)],
        oi_docs.collect::<Vec<_>>(),
    );
}

// Binding an abstract identifier to an expression means that the expression
//   may _eventually_ be reachable after expansion,
//     but it is not yet.
// They must therefore only be utilized within the context of a container
//   that supports dangling expressions,
//     like a template.
#[test]
fn abstract_bind_without_dangling_container() {
    let id_meta = spair("@foo@", S2);
    let id_ok = spair("concrete", S5);

    #[rustfmt::skip]
    let toks = [
        ExprStart(ExprOp::Sum, S1),
          // This expression is bound to an _abstract_ identifier,
          //   which will be expanded at a later time.
          // Consequently,
          //   this expression is still dangling.
          BindIdentAbstract(id_meta),

        // Since the expression is still dangling,
        //   attempting to close it will produce an error.
        ExprEnd(S3),

        // RECOVERY: Since an attempt at identification has been made,
        //   we shouldn't expect that another attempt will be made.
        // The sensible thing to do is to move on to try to find other
        //   errors,
        //     leaving the expression alone and unreachable.
        ExprStart(ExprOp::Sum, S4),
          // This is intended to demonstrate that we can continue on to the
          //   next expression despite the prior error.
          BindIdent(id_ok),
        ExprEnd(S6),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgStart
              Ok(Parsed::Incomplete), // ExprStart

              // This provides an _abstract_ identifier,
              //   which is not permitted in this context.
              Err(ParseError::StateError(AsgError::InvalidAbstractBindContext(
                  id_meta,
                  Some(S1), // Pkg
              ))),

              // RECOVERY: Ignore the bind and move to close.
              // The above identifier was rejected and so we are still dangling.
              Err(ParseError::StateError(AsgError::DanglingExpr(
                  S1.merge(S3).unwrap()
              ))),

              // RECOVERY: This observes that we're able to continue parsing
              //   the package after the above identification problem.
              Ok(Parsed::Incomplete), // ExprStart
                Ok(Parsed::Incomplete), // BindIdent (ok)
              Ok(Parsed::Incomplete), // ExprEnd
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let _ = sut.finalize().unwrap();
}
