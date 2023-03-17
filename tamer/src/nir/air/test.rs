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
use crate::{convert::ExpectInto, parse::util::SPair, span::dummy::*};

type Sut = NirToAir;

use Parsed::Object as O;

#[test]
fn package_to_pkg() {
    let toks = vec![
        Nir::Open(NirEntity::Package, S1),
        Nir::Close(NirEntity::Package, S2),
    ];

    assert_eq!(
        Ok(vec![O(Air::PkgStart(S1)), O(Air::PkgEnd(S2)),]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn rate_to_sum_expr() {
    let id = SPair("foo".into(), S2);

    let toks = vec![
        Nir::Open(NirEntity::Rate, S1),
        Nir::BindIdent(id),
        Nir::Close(NirEntity::Rate, S3),
    ];

    assert_eq!(
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
    let toks = vec![
        Nir::Open(NirEntity::Sum, S1),
        Nir::Open(NirEntity::Product, S2),
        Nir::Close(NirEntity::Product, S3),
        Nir::Close(NirEntity::Sum, S4),
    ];

    assert_eq!(
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

    let toks = vec![
        Nir::Open(NirEntity::Classify, S1),
        Nir::BindIdent(id),
        Nir::Close(NirEntity::Classify, S3),
    ];

    assert_eq!(
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
    let toks = vec![
        Nir::Open(NirEntity::All, S1),
        Nir::Open(NirEntity::Any, S2),
        Nir::Close(NirEntity::Any, S3),
        Nir::Close(NirEntity::All, S4),
    ];

    assert_eq!(
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

    let toks = vec![
        Nir::Open(NirEntity::Tpl, S1),
        Nir::BindIdent(name),
        Nir::Close(NirEntity::Tpl, S3),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Air::TplStart(S1)),
            O(Air::BindIdent(name)),
            O(Air::TplEnd(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// This is the form everyone uses.
// It applies a template much more concisely and without the underscore
//   padding,
//     making it look much like a language primitive
//       (with the exception of the namespace prefix).
#[test]
fn short_hand_tpl_apply() {
    // Shorthand converts `t:tpl-name` into `_tpl-name_`.
    let qname = ("t", "tpl-name").unwrap_into();
    let name = SPair("_tpl-name_".into(), S1);

    let toks = vec![
        Nir::Open(NirEntity::TplApply(Some(qname)), S1),
        Nir::Close(NirEntity::TplApply(None), S2),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Air::TplStart(S1)),
              // The span associated with the name of the template to be
              //   applied is the span of the entire QName from NIR.
              // The reason for this is that `t:foo` is transformed into
              //   `_foo_`,
              //     and so the `t:` is a necessary part of the
              //     representation of the name of the template;
              //       `foo` is not in itself a valid template name at the
              //       time of writing.
              O(Air::RefIdent(name)),
            O(Air::TplEndRef(S2)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Long form takes the actual underscore-padded template name without any
//   additional processing.
#[test]
fn apply_template_long_form() {
    let name = SPair("_tpl-name_".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Nir::Open(NirEntity::TplApply(None), S1),
          Nir::Ref(name),
        Nir::Close(NirEntity::TplApply(None), S3),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Air::TplStart(S1)),
              O(Air::RefIdent(name)),
            O(Air::TplEndRef(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}
