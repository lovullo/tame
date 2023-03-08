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

    let toks = vec![
        Air::PkgOpen(S1),
        // This also tests tpl as a transition away from the package header.
        Air::TplOpen(S2),
        Air::BindIdent(id_tpl),
        Air::TplClose(S4),
        Air::PkgClose(S5),
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
        Air::PkgOpen(S1),
          // This expression is incidental to this test;
          //   it need only parse.
          Air::ExprOpen(ExprOp::Sum, S2),
            Air::BindIdent(id_expr),
          Air::ExprClose(S4),

          // Open after an expression.
          Air::TplOpen(S5),
            Air::BindIdent(id_tpl),
          Air::TplClose(S7),
        Air::PkgClose(S8),
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
        Air::PkgOpen(S1),
          Air::ExprOpen(ExprOp::Sum, S2),
            Air::BindIdent(id_expr),

            // Child expression before the template to ensure that the
            //   context is properly restored after template parsing.
            Air::ExprOpen(ExprOp::Sum, S4),
            Air::ExprClose(S5),

            // Template _within_ an expression.
            // This will not be present in the final expression,
            //   as if it were hoisted out.
            Air::TplOpen(S6),
              Air::BindIdent(id_tpl),
            Air::TplClose(S8),

            // Child expression _after_ the template for the same reason.
            Air::ExprOpen(ExprOp::Sum, S9),
            Air::ExprClose(S10),
          Air::ExprClose(S11),
        Air::PkgClose(S12),
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
    let toks = vec![
        Air::TplClose(S1),
        // RECOVERY: Try again.
        Air::TplOpen(S2),
        Air::TplClose(S3),
    ];

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
            Err(ParseError::StateError(AsgError::UnbalancedTpl(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // TplOpen
            Ok(Parsed::Incomplete), // TplClose
            Ok(Parsed::Incomplete), // PkgClose
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
        Air::TplOpen(S1),
          Air::BindIdent(id_tpl),

          Air::ExprOpen(ExprOp::Sum, S3),
            Air::BindIdent(id_expr_a),
          Air::ExprClose(S5),

          Air::ExprOpen(ExprOp::Sum, S6),
            Air::BindIdent(id_expr_b),
          Air::ExprClose(S8),
        Air::TplClose(S9),
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

#[test]
fn close_tpl_mid_open() {
    let id_expr = SPair("expr".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        Air::TplOpen(S1),
          Air::ExprOpen(ExprOp::Sum, S2),
          Air::BindIdent(id_expr),
        // This is misplaced.
        Air::TplClose(S4),
          // RECOVERY: Close the expression and try again.
          Air::ExprClose(S5),
        Air::TplClose(S6),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Parsed::Incomplete), // PkgOpen
              Ok(Parsed::Incomplete), // TplOpen
                Ok(Parsed::Incomplete), // ExprOpen
                  Ok(Parsed::Incomplete), // BindIdent
              Err(ParseError::StateError(
                  AsgError::InvalidTplCloseContext(S4))
              ),
                // RECOVERY
                Ok(Parsed::Incomplete), // ExprClose
              Ok(Parsed::Incomplete), // TplClose
            Ok(Parsed::Incomplete), // PkgClose
        ],
        parse_as_pkg_body(toks).collect::<Vec<_>>(),
    );
}
