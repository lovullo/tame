// Test lowering NIR into AIR
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
use crate::{parse::util::SPair, span::dummy::*};

type Sut = NirToAir;

use Parsed::Object as O;

#[test]
fn package_to_pkg() {
    let toks = vec![Open(Package, S1), Close(Package, S2)];

    assert_eq!(
        Ok(vec![O(Air::PkgStart(S1)), O(Air::PkgEnd(S2)),]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn rate_to_sum_expr() {
    let id = SPair("foo".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Rate, S1),
          BindIdent(id),
        Close(Rate, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::ExprStart(ExprOp::Sum, S1)),
              O(Air::BindIdent(id)),
            O(Air::ExprEnd(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn calc_exprs() {
    #[rustfmt::skip]
    let toks = vec![
        Open(Sum, S1),
          Open(Product, S2),
          Close(Product, S3),
        Close(Sum, S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::ExprStart(ExprOp::Sum, S1)),
              O(Air::ExprStart(ExprOp::Product, S2)),
              O(Air::ExprEnd(S3)),
            O(Air::ExprEnd(S4)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn classify_to_conj_expr() {
    let id = SPair("always".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Classify, S1),
          BindIdent(id),
        Close(Classify, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::ExprStart(ExprOp::Conj, S1)),
              O(Air::BindIdent(id)),
            O(Air::ExprEnd(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn logic_exprs() {
    #[rustfmt::skip]
    let toks = vec![
        Open(All, S1),
          Open(Any, S2),
          Close(Any, S3),
        Close(All, S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::ExprStart(ExprOp::Conj, S1)),
              O(Air::ExprStart(ExprOp::Disj, S2)),
              O(Air::ExprEnd(S3)),
            O(Air::ExprEnd(S4)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn tpl_with_name() {
    let name = SPair("_tpl_name_".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Tpl, S1),
          BindIdent(name),
        Close(Tpl, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::TplStart(S1)),
              O(Air::BindIdent(name)),
            O(Air::TplEnd(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Long form takes the actual underscore-padded template name without any
//   additional processing.
#[test]
fn apply_template_long_form_nullary() {
    let name = SPair("_tpl-name_".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(TplApply, S1),
          Ref(name),
        Close(TplApply, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::TplStart(S1)),
              O(Air::RefIdent(name)),
            O(Air::TplEndRef(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn apply_template_long_form_args() {
    let name = SPair("_tpl-name_".into(), S2);
    let p1 = SPair("@p1@".into(), S4);
    let v1 = SPair("value1".into(), S5);
    let p2 = SPair("@p2@".into(), S8);
    let v2 = SPair("value2".into(), S9);

    #[rustfmt::skip]
    let toks = vec![
        Open(TplApply, S1),
          Ref(name),

          Open(TplParam, S3),
            BindIdent(p1),
            Text(v1),
          Close(TplParam, S6),

          Open(TplParam, S7),
            BindIdent(p2),
            Text(v2),
          Close(TplParam, S10),
        Close(TplApply, S11),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::TplStart(S1)),
              O(Air::RefIdent(name)),

              O(Air::TplMetaStart(S3)),
                O(Air::BindIdent(p1)),
                O(Air::TplLexeme(v1)),
              O(Air::TplMetaEnd(S6)),

              O(Air::TplMetaStart(S7)),
                O(Air::BindIdent(p2)),
                O(Air::TplLexeme(v2)),
              O(Air::TplMetaEnd(S10)),
            O(Air::TplEndRef(S11)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}
