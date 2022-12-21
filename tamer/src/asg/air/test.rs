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

use std::assert_matches::assert_matches;

use crate::{
    asg::{Ident, Object},
    parse::{ParseError, Parsed},
    span::dummy::*,
};

use super::*;

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
            .map(Object::Ident)
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
            .map(Object::Ident)
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
            .map(Object::Ident)
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
    assert_eq!(&Object::Ident(Ident::Missing(SPair(sym, S1))), ident);

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
            .map(Object::Ident)
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

    let mut asg = sut.finalize().unwrap().into_context();

    // The expression should have been bound to this identifier so that
    //   we're able to retrieve it from the graph by name.
    asg.mut_map_obj_by_ident::<Expr>(id, |expr| {
        assert_eq!(expr.span(), S1.merge(S3).unwrap());
        expr
    });
}

// Danging expressions are unreachable and therefore not useful
//   constructions.
// Prohibit them,
//   since they're either mistakes or misconceptions.
#[test]
fn expr_dangling() {
    let toks = vec![
        Air::OpenExpr(ExprOp::Sum, S1),
        // No `IdentExpr`,
        //   so this expression is dangling.
        Air::CloseExpr(S2),
    ];

    // The error span should encompass the entire expression.
    // TODO: ...let's actually have something inside this expression.
    let full_span = S1.merge(S2).unwrap();

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete),
            Err(ParseError::StateError(AsgError::DanglingExpr(full_span)))
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );

    // TODO: recovery, which will probably mean that we need to have some
    // successful tests first to support it
}
