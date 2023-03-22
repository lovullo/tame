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

use super::*;
use crate::asg::{
    air::{
        expr::test::collect_subexprs,
        test::{asg_from_toks, parse_as_pkg_body},
        Air, AirAggregate,
    },
    Expr, ExprOp, Ident,
};
use crate::span::dummy::*;

type Sut = AirAggregate;

// A template is defined by the package containing it,
//   like an expression.
#[test]
fn tpl_defining_pkg() {
    let id_tpl = SPair("_tpl_".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        Air::PkgStart(S1),
          // This also tests tpl as a transition away from the package header.
          Air::TplStart(S2),
            Air::BindIdent(id_tpl),
          Air::TplEnd(S4),
        Air::PkgEnd(S5),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));
    let asg = sut.finalize().unwrap().into_context();

    let tpl = asg.expect_ident_obj::<Tpl>(id_tpl);
    assert_eq!(S2.merge(S4).unwrap(), tpl.span());

    let oi_id_tpl = asg.lookup(id_tpl).unwrap();
    assert_eq!(
        S1.merge(S5),
        oi_id_tpl.src_pkg(&asg).map(|pkg| pkg.resolve(&asg).span()),
    );
}

#[test]
fn tpl_after_expr() {
    let id_expr = SPair("expr".into(), S3);
    let id_tpl = SPair("_tpl_".into(), S6);

    #[rustfmt::skip]
    let toks = vec![
        Air::PkgStart(S1),
          // This expression is incidental to this test;
          //   it need only parse.
          Air::ExprStart(ExprOp::Sum, S2),
            Air::BindIdent(id_expr),
          Air::ExprEnd(S4),

          // Open after an expression.
          Air::TplStart(S5),
            Air::BindIdent(id_tpl),
          Air::TplEnd(S7),
        Air::PkgEnd(S8),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));
    let asg = sut.finalize().unwrap().into_context();

    let tpl = asg.expect_ident_obj::<Tpl>(id_tpl);
    assert_eq!(S5.merge(S7).unwrap(), tpl.span());
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
    let id_expr = SPair("expr".into(), S3);
    let id_tpl = SPair("_tpl_".into(), S7);

    #[rustfmt::skip]
    let toks = vec![
        Air::PkgStart(S1),
          Air::ExprStart(ExprOp::Sum, S2),
            Air::BindIdent(id_expr),

            // Child expression before the template to ensure that the
            //   context is properly restored after template parsing.
            Air::ExprStart(ExprOp::Sum, S4),
            Air::ExprEnd(S5),

            // Template _within_ an expression.
            // This will not be present in the final expression,
            //   as if it were hoisted out.
            Air::TplStart(S6),
              Air::BindIdent(id_tpl),
            Air::TplEnd(S8),

            // Child expression _after_ the template for the same reason.
            Air::ExprStart(ExprOp::Sum, S9),
            Air::ExprEnd(S10),
          Air::ExprEnd(S11),
        Air::PkgEnd(S12),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));
    let asg = sut.finalize().unwrap().into_context();

    // The inner template.
    let tpl = asg.expect_ident_obj::<Tpl>(id_tpl);
    assert_eq!(S6.merge(S8).unwrap(), tpl.span());

    // The expression that was produced on the graph ought to be equivalent
    //   to the expression without the template being present at all
    //     (noting that the spans are of course not adjusted).
    let oi_expr = asg.expect_ident_oi::<Expr>(id_expr);
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
            .rev()
            .collect::<Vec<_>>(),
    );
}

#[test]
fn close_tpl_without_open() {
    let id_tpl = SPair("_tpl_".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplEnd(S1),
        // RECOVERY: Try again.
        Air::TplStart(S2),
          Air::BindIdent(id_tpl),
        Air::TplEnd(S4),
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
    let id_tpl = SPair("_tpl_".into(), S2);
    let id_expr_a = SPair("expra".into(), S4);
    let id_expr_b = SPair("exprb".into(), S7);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplStart(S1),
          Air::BindIdent(id_tpl),

          Air::ExprStart(ExprOp::Sum, S3),
            Air::BindIdent(id_expr_a),
          Air::ExprEnd(S5),

          Air::ExprStart(ExprOp::Sum, S6),
            Air::BindIdent(id_expr_b),
          Air::ExprEnd(S8),
        Air::TplEnd(S9),
    ];

    let asg = asg_from_toks(toks);

    let oi_tpl = asg.expect_ident_oi::<Tpl>(id_tpl);
    let tpl = oi_tpl.resolve(&asg);
    assert_eq!(S1.merge(S9).unwrap(), tpl.span());

    // The inner expressions are reachable,
    //   but the intent is to expand them into the template's eventual
    //   application site.
    // Given that,
    //   they should be defined by the template...
    assert_eq!(
        vec![
            asg.lookup(id_expr_b).unwrap(),
            asg.lookup(id_expr_a).unwrap(),
        ],
        oi_tpl.edges_filtered::<Ident>(&asg).collect::<Vec<_>>()
    );

    // ...but not by the package containing the template.
    let oi_pkg = asg.lookup(id_tpl).unwrap().src_pkg(&asg).unwrap();
    assert_eq!(
        vec![
            // The only identifier on the package should be the template itself.
            asg.lookup(id_tpl).unwrap(),
        ],
        oi_pkg.edges_filtered::<Ident>(&asg).collect::<Vec<_>>()
    );
}

// Templates can expand into many contexts,
//   including other expressions,
//   and so must be able to contain expressions that,
//     while dangling now,
//     will become reachable in its expansion context.
#[test]
fn tpl_holds_dangling_expressions() {
    let id_tpl = SPair("_tpl_".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplStart(S1),
          Air::BindIdent(id_tpl),

          // Dangling
          Air::ExprStart(ExprOp::Sum, S3),
          Air::ExprEnd(S4),

          // Dangling
          Air::ExprStart(ExprOp::Sum, S5),
          Air::ExprEnd(S6),
        Air::TplEnd(S7),
    ];

    let asg = asg_from_toks(toks);
    let oi_tpl = asg.expect_ident_oi::<Tpl>(id_tpl);

    assert_eq!(
        vec![S5.merge(S6).unwrap(), S3.merge(S4).unwrap(),],
        oi_tpl
            .edges_filtered::<Expr>(&asg)
            .map(ObjectIndex::cresolve(&asg))
            .map(Expr::span)
            .collect::<Vec<_>>()
    );
}

#[test]
fn close_tpl_mid_open() {
    let id_tpl = SPair("_tpl_".into(), S2);
    let id_expr = SPair("expr".into(), S4);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplStart(S1),
          Air::BindIdent(id_tpl),

          Air::ExprStart(ExprOp::Sum, S3),
            Air::BindIdent(id_expr),
        // This is misplaced.
        Air::TplEnd(S5),
          // RECOVERY: Close the expression and try again.
          Air::ExprEnd(S6),
        Air::TplEnd(S7),
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
                  AsgError::InvalidTplEndContext(S5))
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
    let id_ok = SPair("_tpl_".into(), S4);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplStart(S1),
          // No BindIdent
        Air::TplEnd(S2),

        // Recovery should ignore the above template
        //   (it's lost to the void)
        //   and allow continuing.
        Air::TplStart(S3),
          Air::BindIdent(id_ok),
        Air::TplEnd(S5),
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

    let asg = sut.finalize().unwrap().into_context();

    // Let's make sure that the template created after recovery succeeded.
    asg.expect_ident_obj::<Tpl>(id_ok);
}

// Normally we cannot reference objects without an identifier using AIR
//   (at the time of writing at least),
//   but `TplEndRef` is an exception.
#[test]
fn anonymous_tpl_immediate_ref() {
    #[rustfmt::skip]
    let toks = vec![
        Air::TplStart(S1),
          // No BindIdent
        // But ended with `TplEndRef`,
        //   so the missing identifier is okay.
        // This would fail if it were `TplEnd`.
        Air::TplEndRef(S2),
    ];

    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));

    // TODO: More to come.
}
