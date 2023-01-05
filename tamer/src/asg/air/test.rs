// Tests for ASG IR
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

use super::*;
use crate::{
    asg::Ident,
    parse::{ParseError, Parsed},
    span::dummy::*,
};
use std::assert_matches::assert_matches;

type Sut = AirAggregate;

#[test]
fn ident_decl() {
    let sym = "foo".into();
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl".into()),
        ..Default::default()
    };

    let toks = vec![Air::IdentDecl(SPair(sym, S1), kind.clone(), src.clone())]
        .into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        asg.lookup(sym).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(SPair(sym, S1))
            .resolve(S1, kind.clone(), src.clone())
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare the same identifier.
    let bad_toks = vec![Air::IdentDecl(SPair(sym, S2), kind, src)].into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_extern_decl() {
    let sym = "foo".into();
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl-extern".into()),
        ..Default::default()
    };

    let toks = vec![Air::IdentExternDecl(
        SPair(sym, S1),
        kind.clone(),
        src.clone(),
    )]
    .into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        asg.lookup(sym).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(SPair(sym, S1))
            .extern_(S1, kind, src.clone())
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare with a different kind.
    let different_kind = IdentKind::Meta;
    let bad_toks =
        vec![Air::IdentExternDecl(SPair(sym, S2), different_kind, src)]
            .into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_dep() {
    let ident = "foo".into();
    let dep = "dep".into();

    let toks =
        vec![Air::IdentDep(SPair(ident, S1), SPair(dep, S2))].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg
        .lookup(ident)
        .expect("identifier was not added to graph");
    let dep_node = asg.lookup(dep).expect("dep was not added to graph");

    assert!(asg.has_dep(ident_node, dep_node));
}

#[test]
fn ident_fragment() {
    let sym = "frag".into();
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/frag".into()),
        ..Default::default()
    };
    let frag = "fragment text".into();

    let toks = vec![
        // Identifier must be declared before it can be given a
        //   fragment.
        Air::IdentDecl(SPair(sym, S1), kind.clone(), src.clone()),
        Air::IdentFragment(SPair(sym, S1), frag),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentFragment

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        asg.lookup(sym).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(SPair(sym, S1))
            .resolve(S1, kind.clone(), src.clone())
            .and_then(|resolved| resolved.set_fragment(frag))
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   re-set the fragment.
    let bad_toks = vec![Air::IdentFragment(SPair(sym, S1), frag)].into_iter();
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
    let sym = "toroot".into();

    let toks = vec![Air::IdentRoot(SPair(sym, S1))].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg
        .lookup(sym)
        .expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The identifier did not previously exist,
    //   and so a missing node is created as a placeholder.
    assert_eq!(&Ident::Missing(SPair(sym, S1)), ident);

    // And that missing identifier should be rooted.
    assert!(asg.is_rooted(ident_node));
}

#[test]
fn ident_root_existing() {
    let sym = "toroot".into();
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/root-existing".into()),
        ..Default::default()
    };

    // Ensure that it won't auto-root based on the kind,
    //   otherwise we won't be testing the right thing.
    assert!(!kind.is_auto_root());

    let toks = vec![
        Air::IdentDecl(SPair(sym, S1), kind.clone(), src.clone()),
        Air::IdentRoot(SPair(sym, S2)),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentRoot

    let asg = sut.finalize().unwrap().into_context();

    let ident_node = asg
        .lookup(sym)
        .expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The previously-declared identifier...
    assert_eq!(
        Ok(ident),
        Ident::declare(SPair(sym, S1))
            .resolve(S1, kind.clone(), src.clone())
            .as_ref()
    );

    // ...should have been subsequently rooted.
    assert!(asg.is_rooted(ident_node));
}

#[test]
fn expr_empty() {
    let id = SPair("foo".into(), S2);

    let toks = vec![
        Air::OpenExpr(ExprOp::Sum, S1),
        Air::IdentExpr(id),
        Air::CloseExpr(S3),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    // The expression should have been bound to this identifier so that
    //   we're able to retrieve it from the graph by name.
    let expr = asg.expect_ident_obj::<Expr>(id);
    assert_eq!(expr.span(), S1.merge(S3).unwrap());
}

#[test]
fn expr_non_empty() {
    let id_a = SPair("foo".into(), S2);
    let id_b = SPair("bar".into(), S2);

    let toks = vec![
        Air::OpenExpr(ExprOp::Sum, S1),
        // Identifier while still empty...
        Air::IdentExpr(id_a),
        Air::OpenExpr(ExprOp::Sum, S3),
        // (note that the inner expression _does not_ have an ident binding)
        Air::CloseExpr(S4),
        // ...and an identifier non-empty.
        Air::IdentExpr(id_b),
        Air::CloseExpr(S6),
    ];

    let mut sut = Sut::parse(toks.into_iter());
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
        Air::OpenExpr(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::OpenExpr(ExprOp::Sum, S2),
        Air::CloseExpr(S3),
        // We only bind an identifier _after_ we've created the expression,
        //   which should cause the still-dangling root to become
        //   reachable.
        Air::IdentExpr(id),
        Air::CloseExpr(S5),
    ];

    let mut sut = Sut::parse(toks.into_iter());
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
        Air::OpenExpr(ExprOp::Sum, S1),
        // No `IdentExpr`,
        //   so this expression is dangling.
        Air::CloseExpr(S2),
    ];

    // The error span should encompass the entire expression.
    let full_span = S1.merge(S2).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span)))
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr() {
    let toks = vec![
        Air::OpenExpr(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::OpenExpr(ExprOp::Sum, S2),
        Air::CloseExpr(S3),
        // Still no ident binding,
        //   so root should still be dangling.
        Air::CloseExpr(S4),
    ];

    let full_span = S1.merge(S4).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span)))
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn expr_dangling_with_subexpr_ident() {
    let id = SPair("foo".into(), S3);

    let toks = vec![
        Air::OpenExpr(ExprOp::Sum, S1),
        // Expression root is still dangling at this point.
        Air::OpenExpr(ExprOp::Sum, S2),
        // The _inner_ expression receives an identifier,
        //   but that should have no impact on the dangling status of the
        //   root,
        //     especially given that subexpressions are always reachable
        //     anyway.
        Air::IdentExpr(id),
        Air::CloseExpr(S4),
        // But the root still has no ident binding,
        //   and so should still be dangling.
        Air::CloseExpr(S5),
    ];

    let full_span = S1.merge(S5).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span)))
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
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
        Air::OpenExpr(ExprOp::Sum, S1),
        Air::IdentExpr(id),
        Air::CloseExpr(S3),
        // Dangling
        Air::OpenExpr(ExprOp::Sum, S4),
        Air::CloseExpr(S5),
    ];

    // The error span should encompass the entire expression.
    // TODO: ...let's actually have something inside this expression.
    let second_span = S4.merge(S5).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(second_span)))
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

// Recovery from dangling expression.
#[test]
fn recovery_expr_reachable_after_dangling() {
    let id = SPair("foo".into(), S4);
    let toks = vec![
        // Dangling
        Air::OpenExpr(ExprOp::Sum, S1),
        Air::CloseExpr(S2),
        // Reachable, after error from dangling.
        Air::OpenExpr(ExprOp::Sum, S3),
        Air::IdentExpr(id),
        Air::CloseExpr(S5),
    ];

    // The error span should encompass the entire expression.
    let err_span = S1.merge(S2).unwrap();

    let mut sut = Sut::parse(toks.into_iter());

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(err_span))),
            // Recovery allows us to continue at this point with the next
            //   expression.
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
            Ok(Parsed::Incomplete),
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
