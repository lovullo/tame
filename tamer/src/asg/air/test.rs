// Tests for ASG IR
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

//! These are tested as if they are another API directly atop of the ASG,
//!   since that is how they are used.

use super::super::Ident;
use super::*;
use crate::asg::graph::object::ObjectRel;
use crate::parse::Parser;
use crate::{
    parse::{ParseError, Parsed},
    span::dummy::*,
};
use std::assert_matches::assert_matches;

type Sut = AirAggregate;

#[test]
fn ident_decl() {
    let id = SPair("foo".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl".into()),
        ..Default::default()
    };

    let toks = vec![Air::IdentDecl(id, kind.clone(), src.clone())].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg.lookup(id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare the same identifier.
    let bad_toks =
        vec![Air::IdentDecl(SPair(id.symbol(), S2), kind, src)].into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_extern_decl() {
    let id = SPair("foo".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl-extern".into()),
        ..Default::default()
    };

    let toks =
        vec![Air::IdentExternDecl(id, kind.clone(), src.clone())].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg.lookup(id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id).extern_(S1, kind, src.clone()).as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare with a different kind.
    let different_kind = IdentKind::Meta;
    let bad_toks = vec![Air::IdentExternDecl(
        SPair(id.symbol(), S2),
        different_kind,
        src,
    )]
    .into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_dep() {
    let id = SPair("foo".into(), S1);
    let dep = SPair("dep".into(), S2);

    let toks = vec![Air::IdentDep(id, dep)].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg.lookup(id).expect("identifier was not added to graph");
    let dep_node = asg.lookup(dep).expect("dep was not added to graph");

    assert!(asg.has_dep(ident_node, dep_node));
}

#[test]
fn ident_fragment() {
    let id = SPair("frag".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/frag".into()),
        ..Default::default()
    };
    let frag = "fragment text".into();

    let toks = vec![
        // Identifier must be declared before it can be given a
        //   fragment.
        Air::IdentDecl(id, kind.clone(), src.clone()),
        Air::IdentFragment(id, frag),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentFragment

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg.lookup(id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
            .and_then(|resolved| resolved.set_fragment(frag))
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   re-set the fragment.
    let bad_toks = vec![Air::IdentFragment(id, frag)].into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

// Adding a root before the identifier exists should add a
//   `Ident::Missing`.
#[test]
fn ident_root_missing() {
    let id = SPair("toroot".into(), S1);

    let toks = vec![Air::IdentRoot(id)].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg
        .lookup(id)
        .expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The identifier did not previously exist,
    //   and so a missing node is created as a placeholder.
    assert_eq!(&Ident::Missing(id), ident);

    // And that missing identifier should be rooted.
    assert!(asg.is_rooted(ident_node));
}

#[test]
fn ident_root_existing() {
    let id = SPair("toroot".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/root-existing".into()),
        ..Default::default()
    };

    // Ensure that it won't auto-root based on the kind,
    //   otherwise we won't be testing the right thing.
    assert!(!kind.is_auto_root());

    let toks = vec![
        Air::IdentDecl(id, kind.clone(), src.clone()),
        Air::IdentRoot(SPair(id.symbol(), S2)),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentRoot

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg
        .lookup(id)
        .expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The previously-declared identifier...
    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
            .as_ref()
    );

    // ...should have been subsequently rooted.
    assert!(asg.is_rooted(ident_node));
}

#[test]
fn pkg_is_rooted() {
    let toks = vec![Air::PkgOpen(S1), Air::PkgClose(S2)];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let oi_root = asg.root(S3);
    let pkg = oi_root
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("missing rooted package")
        .resolve(&asg);

    assert_eq!(pkg.span(), S1);
}

#[test]
fn close_pkg_without_open() {
    let toks = vec![
        Air::PkgClose(S1),
        // RECOVERY: Try again.
        Air::PkgOpen(S2),
        Air::PkgClose(S3),
    ];

    assert_eq!(
        vec![
            Err(ParseError::StateError(AsgError::InvalidPkgCloseContext(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // PkgClose
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn nested_open_pkg() {
    let toks = vec![
        Air::PkgOpen(S1),
        Air::PkgOpen(S2),
        // RECOVERY
        Air::PkgClose(S3),
    ];

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Err(ParseError::StateError(AsgError::NestedPkgOpen(S2, S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

/// Parse using [`Sut`] when the test does not care about the outer package.
fn parse_as_pkg_body<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Parser<Sut, impl Iterator<Item = Air> + Debug>
where
    <I as IntoIterator>::IntoIter: Debug,
{
    use std::iter;

    Sut::parse(
        iter::once(Air::PkgOpen(S1))
            .chain(toks.into_iter())
            .chain(iter::once(Air::PkgClose(S1))),
    )
}

#[test]
fn expr_empty_ident() {
    let id = SPair("foo".into(), S2);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id),
        Air::ExprClose(S3),
    ];

    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    // The expression should have been bound to this identifier so that
    //   we're able to retrieve it from the graph by name.
    let expr = asg.expect_ident_obj::<Expr>(id);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());
}

#[test]
fn expr_without_pkg() {
    let toks = vec![
        // No package
        //   (because we're not parsing with `parse_as_pkg_body` below)
        Air::ExprOpen(ExprOp::Sum, S1),
        // RECOVERY
        Air::PkgOpen(S2),
        Air::PkgClose(S3),
    ];

    assert_eq!(
        vec![
            Err(ParseError::StateError(AsgError::InvalidExprContext(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // PkgClose
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

// Note that this can't happen in e.g. NIR / TAME's source XML.
#[test]
fn close_pkg_mid_expr() {
    let id = SPair("foo".into(), S4);

    let toks = vec![
        Air::PkgOpen(S1),
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::PkgClose(S3),
        // RECOVERY: Let's finish the expression first...
        Air::ExprIdent(id),
        Air::ExprClose(S5),
        // ...and then try to close again.
        Air::PkgClose(S6),
    ];

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // ExprOpen
            Err(ParseError::StateError(AsgError::InvalidPkgCloseContext(S3))),
            // RECOVERY: We should be able to close the package if we just
            //   finish the expression first,
            //     demonstrating that recovery properly maintains all state.
            Ok(Parsed::Incomplete), // ExprIdent
            Ok(Parsed::Incomplete), // ExprClose
            // Successful close here.
            Ok(Parsed::Incomplete), // PkgClose
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn open_pkg_mid_expr() {
    let id = SPair("foo".into(), S4);

    let toks = vec![
        Air::PkgOpen(S1),
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::PkgOpen(S3),
        // RECOVERY: We should still be able to complete successfully.
        Air::ExprIdent(id),
        Air::ExprClose(S5),
        // Closes the _original_ package.
        Air::PkgClose(S6),
    ];

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // ExprOpen
            Err(ParseError::StateError(AsgError::NestedPkgOpen(S3, S1))),
            // RECOVERY: Ignore the open and continue.
            //   Of course,
            //     this means that any identifiers would be defined in a
            //     different package than was likely intended,
            //       but at least we'll be able to keep processing.
            Ok(Parsed::Incomplete), // ExprIdent
            Ok(Parsed::Incomplete), // ExprClose
            Ok(Parsed::Incomplete), // PkgClose
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_non_empty_ident_root() {
    let id_a = SPair("foo".into(), S2);
    let id_b = SPair("bar".into(), S2);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // Identifier while still empty...
        Air::ExprIdent(id_a),
        Air::ExprOpen(ExprOp::Sum, S3),
        // (note that the inner expression _does not_ have an ident binding)
        Air::ExprClose(S4),
        // ...and an identifier non-empty.
        Air::ExprIdent(id_b),
        Air::ExprClose(S6),
    ];

    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let expr_a = asg.expect_ident_obj::<Expr>(id_a);
    assert_eq!(expr_a.span(), S1.merge(S6).unwrap());

    // Identifiers should reference the same expression.
    let expr_b = asg.expect_ident_obj::<Expr>(id_b);
    assert_eq!(expr_a, expr_b);
}

// Binding an identifier after a child expression means that the parser is
//   creating an expression that is a child of a dangling expression,
//     which only becomes reachable at the end.
#[test]
fn expr_non_empty_bind_only_after() {
    let id = SPair("foo".into(), S2);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::ExprClose(S3),
        // We only bind an identifier _after_ we've created the expression,
        //   which should cause the still-dangling root to become
        //   reachable.
        Air::ExprIdent(id),
        Air::ExprClose(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let expr = asg.expect_ident_obj::<Expr>(id);
    assert_eq!(expr.span(), S1.merge(S5).unwrap());
}

// Danging expressions are unreachable and therefore not useful
//   constructions.
// Prohibit them,
//   since they're either mistakes or misconceptions.
#[test]
fn expr_dangling_no_subexpr() {
    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // No `ExprIdent`,
        //   so this expression is dangling.
        Air::ExprClose(S2),
    ];

    // The error span should encompass the entire expression.
    let full_span = S1.merge(S2).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr() {
    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::ExprClose(S3),
        // Still no ident binding,
        //   so root should still be dangling.
        Air::ExprClose(S4),
    ];

    let full_span = S1.merge(S4).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr_ident() {
    let id = SPair("foo".into(), S3);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::ExprOpen(ExprOp::Sum, S2),
        // The _inner_ expression receives an identifier,
        //   but that should have no impact on the dangling status of the
        //   root,
        //     especially given that subexpressions are always reachable
        //     anyway.
        Air::ExprIdent(id),
        Air::ExprClose(S4),
        // But the root still has no ident binding,
        //   and so should still be dangling.
        Air::ExprClose(S5),
    ];

    let full_span = S1.merge(S5).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
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
    let id = SPair("foo".into(), S2);
    let toks = vec![
        // Reachable
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id),
        Air::ExprClose(S3),
        // Dangling
        Air::ExprOpen(ExprOp::Sum, S4),
        Air::ExprClose(S5),
    ];

    // The error span should encompass the entire expression.
    // TODO: ...let's actually have something inside this expression.
    let second_span = S4.merge(S5).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(second_span))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}

// Recovery from dangling expression.
#[test]
fn recovery_expr_reachable_after_dangling() {
    let id = SPair("foo".into(), S4);
    let toks = vec![
        // Dangling
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprClose(S2),
        // Reachable, after error from dangling.
        Air::ExprOpen(ExprOp::Sum, S3),
        Air::ExprIdent(id),
        Air::ExprClose(S5),
    ];

    // The error span should encompass the entire expression.
    let err_span = S1.merge(S2).unwrap();

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(err_span))),
            // RECOVERY: continue at this point with the next expression.
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // Let's make sure that we _actually_ added it to the graph,
    //   despite the previous error.
    let expr = asg.expect_ident_obj::<Expr>(id);
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
    let id = SPair("foo".into(), S3);

    let toks = vec![
        // Close before _any_ open.
        Air::ExprClose(S1),
        // Should recover,
        //   allowing for a normal expr.
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::ExprIdent(id),
        Air::ExprClose(S4),
        // And now an extra close _after_ a valid expr.
        Air::ExprClose(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Err(ParseError::StateError(AsgError::UnbalancedExpr(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // ExprOpen
            Ok(Parsed::Incomplete), // ExprIdent
            Ok(Parsed::Incomplete), // ExprClose
            // Another error after a successful expression.
            Err(ParseError::StateError(AsgError::UnbalancedExpr(S5))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // Just verify that the expression was successfully added after recovery.
    let expr = asg.expect_ident_obj::<Expr>(id);
    assert_eq!(expr.span(), S2.merge(S4).unwrap());
}

#[test]
fn expr_bind_to_empty() {
    let id_noexpr_a = SPair("noexpr_a".into(), S1);
    let id_good = SPair("noexpr".into(), S3);
    let id_noexpr_b = SPair("noexpr_b".into(), S5);

    let toks = vec![
        // No open expression to bind to.
        Air::ExprIdent(id_noexpr_a),
        // Post-recovery create an expression.
        Air::ExprOpen(ExprOp::Sum, S2),
        Air::ExprIdent(id_good),
        Air::ExprClose(S4),
        // Once again we have nothing to bind to.
        Air::ExprIdent(id_noexpr_b),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Err(ParseError::StateError(AsgError::InvalidExprBindContext(
                id_noexpr_a
            ))),
            // RECOVERY
            Ok(Parsed::Incomplete), // ExprOpen
            Ok(Parsed::Incomplete), // ExprIdent
            Ok(Parsed::Incomplete), // ExprClose
            // Another error after a successful expression.
            Err(ParseError::StateError(AsgError::InvalidExprBindContext(
                id_noexpr_b
            ))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // Neither of the identifiers outside of expressions should exist on the
    //   graph.
    assert_eq!(None, asg.get_ident_obj::<Expr>(id_noexpr_a));
    assert_eq!(None, asg.get_ident_obj::<Expr>(id_noexpr_b));

    // Verify that the expression was successfully added after recovery.
    let expr = asg.expect_ident_obj::<Expr>(id_good);
    assert_eq!(expr.span(), S2.merge(S4).unwrap());
}

// Subexpressions should not only have edges to their parent,
//   but those edges ought to be ordered,
//   allowing TAME to handle non-commutative expressions.
// We must further understand the relative order in which edges are stored
//   for non-associative expressions.
#[test]
fn sibling_subexprs_have_ordered_edges_to_parent() {
    let id_root = SPair("root".into(), S1);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        // Identify the root so that it is not dangling.
        Air::ExprIdent(id_root),
        // Sibling A
        Air::ExprOpen(ExprOp::Sum, S3),
        Air::ExprClose(S4),
        // Sibling B
        Air::ExprOpen(ExprOp::Sum, S5),
        Air::ExprClose(S6),
        // Sibling C
        Air::ExprOpen(ExprOp::Sum, S7),
        Air::ExprClose(S8),
        Air::ExprClose(S9),
    ];

    let asg = asg_from_toks(toks);

    // The root is the parent expression that should contain edges to each
    //   subexpression
    //     (the siblings above).
    // Note that we retrieve its _index_,
    //   not the object itself.
    let oi_root = asg.expect_ident_oi::<Expr>(id_root);

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
    let id_root = SPair("root".into(), S1);
    let id_suba = SPair("suba".into(), S2);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1), // 0
        Air::ExprIdent(id_root),
        Air::ExprOpen(ExprOp::Sum, S2), // 1
        Air::ExprIdent(id_suba),
        Air::ExprOpen(ExprOp::Sum, S3), // 2
        Air::ExprClose(S4),
        Air::ExprClose(S5),
        Air::ExprClose(S6),
    ];

    let asg = asg_from_toks(toks);

    let oi_0 = asg.expect_ident_oi::<Expr>(id_root);
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
    let id_first = SPair("foo".into(), S2);
    let id_dup = SPair("foo".into(), S3);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id_first),
        Air::ExprOpen(ExprOp::Sum, S3),
        Air::ExprIdent(id_dup),
        Air::ExprClose(S4),
        Air::ExprClose(S5),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // ExprOpen
            Ok(Parsed::Incomplete), // ExprIdent (first)
            Ok(Parsed::Incomplete), // ExprOpen
            Err(ParseError::StateError(AsgError::IdentRedefine(
                id_first,
                id_dup.span(),
            ))),
            // RECOVERY: Ignore the attempt to redefine and continue.
            Ok(Parsed::Incomplete), // ExprClose
            Ok(Parsed::Incomplete), // ExprClose
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // The identifier should continue to reference the first expression.
    let expr = asg.expect_ident_obj::<Expr>(id_first);
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
    let id_first = SPair("foo".into(), S2);
    let id_dup = SPair("foo".into(), S5);
    let id_dup2 = SPair("foo".into(), S8);
    let id_second = SPair("bar".into(), S9);

    let toks = vec![
        // First expr (OK)
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id_first),
        Air::ExprClose(S3),
        // Second expr should still dangle due to use of duplicate
        //   identifier
        Air::ExprOpen(ExprOp::Sum, S4),
        Air::ExprIdent(id_dup),
        Air::ExprClose(S6),
        // Third expr will error on redefine but then be successful.
        // This probably won't happen in practice with TAME's original
        //   source language,
        //     but could happen at e.g. a REPL.
        Air::ExprOpen(ExprOp::Sum, S7),
        Air::ExprIdent(id_dup2),   // fail
        Air::ExprIdent(id_second), // succeed
        Air::ExprClose(S10),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Ok(Parsed::Incomplete), // ExprOpen
            Ok(Parsed::Incomplete), // ExprIdent (first)
            Ok(Parsed::Incomplete), // ExprClose
            // Beginning of second expression
            Ok(Parsed::Incomplete), // ExprOpen
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
            Ok(Parsed::Incomplete), // ExprOpen
            Err(ParseError::StateError(AsgError::IdentRedefine(
                id_first,
                id_dup2.span(),
            ))),
            // RECOVERY: Despite the initial failure,
            //   we can now re-attempt to bind with a unique id.
            Ok(Parsed::Incomplete), // ExprIdent (second)
            Ok(Parsed::Incomplete), // ExprClose
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // The identifier should continue to reference the first expression.
    let expr = asg.expect_ident_obj::<Expr>(id_first);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());

    // There's nothing we can do using the ASG's public API at the time of
    //   writing to try to reference the dangling expression.

    // The second identifier should have been successfully bound despite the
    //   failed initial attempt.
    let expr = asg.expect_ident_obj::<Expr>(id_second);
    assert_eq!(expr.span(), S7.merge(S10).unwrap());
}

#[test]
fn expr_ref_to_ident() {
    let id_foo = SPair("foo".into(), S2);
    let id_bar = SPair("bar".into(), S6);

    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id_foo),
        // Reference to an as-of-yet-undefined id (okay)
        Air::ExprRef(id_bar),
        Air::ExprClose(S4),
        //
        // Another expression to reference the first
        //   (we don't handle cyclic references until a topological sort,
        //     so no point in referencing ourselves;
        //       it'd work just fine here.)
        Air::ExprOpen(ExprOp::Sum, S5),
        Air::ExprIdent(id_bar),
        Air::ExprClose(S7),
    ];

    let asg = asg_from_toks(toks);

    let oi_foo = asg.expect_ident_oi::<Expr>(id_foo);

    let foo_refs = oi_foo.edges_filtered::<Ident>(&asg).collect::<Vec<_>>();

    // We should have only a single reference (to `id_bar`).
    assert_eq!(foo_refs.len(), 1);

    let oi_ident_bar = foo_refs[0];
    let ident_bar = oi_ident_bar.resolve(&asg);
    assert_eq!(ident_bar.span(), id_bar.span());

    // The identifier will have originally been `Missing`,
    //   since it did not exist at the point of reference.
    // But it should now properly identify the other expression.
    assert_matches!(ident_bar, Ident::Transparent(..));

    let oi_expr_bar = asg.expect_ident_oi::<Expr>(id_bar);
    assert!(oi_ident_bar.is_bound_to(&asg, oi_expr_bar));
}

#[test]
fn expr_ref_outside_of_expr_context() {
    let id_foo = SPair("foo".into(), S2);

    let toks = vec![
        // This will fail since we're not in an expression context.
        Air::ExprRef(id_foo),
        // RECOVERY: Simply ignore the above.
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id_foo),
        Air::ExprClose(S3),
    ];

    let mut sut = parse_as_pkg_body(toks);

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Err(ParseError::StateError(AsgError::InvalidExprRefContext(
                id_foo
            ))),
            // RECOVERY: Proceed as normal
            Ok(Parsed::Incomplete), // ExprOpen
            Ok(Parsed::Incomplete), // ExprIdent
            Ok(Parsed::Incomplete), // ExprClose
            Ok(Parsed::Incomplete), // PkgClose
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // Verify that the identifier was bound just to have some confidence in
    //   the recovery.
    let expr = asg.expect_ident_obj::<Expr>(id_foo);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());
}

#[test]
fn idents_share_defining_pkg() {
    let id_foo = SPair("foo".into(), S2);
    let id_bar = SPair("bar".into(), S4);
    let id_baz = SPair("baz".into(), S5);

    // An expression nested within another.
    let toks = vec![
        Air::ExprOpen(ExprOp::Sum, S1),
        Air::ExprIdent(id_foo),
        Air::ExprOpen(ExprOp::Sum, S3),
        Air::ExprIdent(id_bar),
        Air::ExprRef(id_baz),
        Air::ExprClose(S6),
        Air::ExprClose(S7),
    ];

    let asg = asg_from_toks(toks);

    let oi_foo = asg.lookup(id_foo).unwrap();
    let oi_bar = asg.lookup(id_bar).unwrap();

    assert_eq!(oi_foo.src_pkg(&asg).unwrap(), oi_bar.src_pkg(&asg).unwrap());

    // Missing identifiers should not have a source package,
    //   since we don't know what defined it yet.
    let oi_baz = asg.lookup(id_baz).unwrap();
    assert_eq!(None, oi_baz.src_pkg(&asg));
}

fn asg_from_toks<I: IntoIterator<Item = Air>>(toks: I) -> Asg
where
    I::IntoIter: Debug,
{
    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));
    sut.finalize().unwrap().into_context()
}

fn collect_subexprs(
    asg: &Asg,
    oi: ObjectIndex<Expr>,
) -> Vec<(ObjectIndex<Expr>, &Expr)> {
    oi.edges(&asg)
        .filter_map(|rel| rel.narrow::<Expr>())
        .map(|oi| (oi, oi.resolve(&asg)))
        .collect::<Vec<_>>()
}
