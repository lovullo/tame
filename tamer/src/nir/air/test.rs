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
use crate::{
    parse::{util::SPair, Parser},
    span::dummy::*,
};

type Sut = NirToAir;

use Parsed::{Incomplete, Object as O};

fn sut_parse<I: Iterator<Item = Nir>>(toks: I) -> Parser<Sut, I> {
    Sut::parse_with_context(
        toks.into_iter(),
        NirToAirParseType::LowerKnownErrorRest,
    )
}

#[test]
fn ignores_input_when_parse_type_is_noop() {
    let toks = vec![Open(Package, S1), Close(Package, S2)];

    assert_eq!(
        Ok(vec![Incomplete, Incomplete,]),
        Sut::parse_with_context(toks.into_iter(), NirToAirParseType::Noop)
            .collect(),
    );
}

#[test]
fn package_to_pkg() {
    let toks = vec![Open(Package, S1), Close(Package, S2)];

    assert_eq!(
        Ok(vec![
            O(Air::PkgStart(S1, SPair("/TODO".into(), S1))),
            O(Air::PkgEnd(S2)),
        ]),
        sut_parse(toks.into_iter()).collect(),
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
        sut_parse(toks.into_iter()).collect(),
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
        sut_parse(toks.into_iter()).collect(),
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
        sut_parse(toks.into_iter()).collect(),
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
        sut_parse(toks.into_iter()).collect(),
    );
}

// @desc becomes an independent clause,
//   intended for short summary documentation.
#[test]
fn desc_as_indep_clause() {
    let id = SPair("class".into(), S2);
    let desc = SPair("class desc".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        Open(Classify, S1),
          BindIdent(id),
          Desc(desc),
        Close(Classify, S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::ExprStart(ExprOp::Conj, S1)),
              O(Air::BindIdent(id)),
              O(Air::DocIndepClause(desc)),
            O(Air::ExprEnd(S4)),
        ]),
        sut_parse(toks.into_iter()).collect(),
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
        sut_parse(toks.into_iter()).collect(),
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
          RefSubject(name),
        Close(TplApply, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::TplStart(S1)),
              O(Air::RefIdent(name)),
            O(Air::TplEndRef(S3)),
        ]),
        sut_parse(toks.into_iter()).collect(),
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
          RefSubject(name),

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

              O(Air::MetaStart(S3)),
                O(Air::BindIdent(p1)),
                O(Air::MetaLexeme(v1)),
              O(Air::MetaEnd(S6)),

              O(Air::MetaStart(S7)),
                O(Air::BindIdent(p2)),
                O(Air::MetaLexeme(v2)),
              O(Air::MetaEnd(S10)),
            O(Air::TplEndRef(S11)),
        ]),
        sut_parse(toks.into_iter()).collect(),
    );
}

// Short-hand matches with no value desugar to an equality check
//   against `TRUE`.
#[test]
fn match_short_no_value() {
    let name = SPair("matchOn".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Match, S1),
          RefSubject(name),  // @on
        Close(Match, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            // When first encountering a `Match`,
            //   we do not know what predicate needs to be emitted for AIR.
            Incomplete,
            // Nor do we know when encountering an identifier,
            //   which serves as the first argument to the yet-to-be-known
            //   predicate.
            Incomplete,
            // Once closing,
            //   we default to an equality check against `TRUE`.
            O(Air::ExprStart(ExprOp::Eq, S1)),
            O(Air::RefIdent(name)),
            O(Air::RefIdent(SPair(SYM_TRUE, S1))),
            O(Air::ExprEnd(S3)),
        ]),
        sut_parse(toks.into_iter()).collect(),
    );
}

// Same as above but _not_ defaulted to `TRUE`.
#[test]
fn match_short_with_value() {
    let name = SPair("matchOn".into(), S2);
    let value = SPair("value".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        Open(Match, S1),
          RefSubject(name),  // @on
          Ref(value),        // @value
        Close(Match, S4),
    ];

    // See notes from `match_short_no_value`,
    //   which are not duplicated here.
    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete,
            Incomplete,
            O(Air::ExprStart(ExprOp::Eq, S1)),
            O(Air::RefIdent(name)),
            // Rather than defaulting to `SYM_TRUE` as above,
            //   we use the _user-provided_ value.
            O(Air::RefIdent(value)),
            O(Air::ExprEnd(S4)),
        ]),
        sut_parse(toks.into_iter()).collect(),
    );
}

// Equivalently stated in XML: `match/@value` before `match/@on`;
//   NIR imposes ordering,
//     such that the `@on` must come first.
#[test]
fn match_short_value_before_subject_err() {
    let name = SPair("matchOn".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Match, S1),
          Ref(name),  // @value, not @on

          // RECOVERY: Provide the subject
          RefSubject(name),
        Close(Match, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete),  // Open
            // We were expecting RefSubject (@on) but got Ref (@value)
            Err(ParseError::StateError(
                NirToAirError::MatchSubjectExpected(S1, Ref(name))
            )),
            // RECOVERY: Subject is provided.
            Ok(Incomplete),
            // And we then close as an eq match on TRUE,
            //   because no value has been provided
            //     (rather the value was consumed in the error).
            Ok(O(Air::ExprStart(ExprOp::Eq, S1))),
            Ok(O(Air::RefIdent(name))),
            Ok(O(Air::RefIdent(SPair(SYM_TRUE, S1)))),
            Ok(O(Air::ExprEnd(S3))),
        ],
        sut_parse(toks.into_iter()).collect::<Vec<Result<_, _>>>(),
    );
}

// Closing a match before providing any arguments.
#[test]
fn match_no_args_err() {
    #[rustfmt::skip]
    let toks = vec![
        Open(Match, S1),
          // No refs.
        Close(Match, S2),
        // RECOVERY: We have no choice but to discard the above match.
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete),  // Open
            // We were expecting RefSubject (@on) but closed instead.
            Err(ParseError::StateError(
                NirToAirError::MatchSubjectExpected(S1, Close(Match, S2))
            )),
            // RECOVERY: Useless match above discarded.
        ],
        sut_parse(toks.into_iter()).collect::<Vec<Result<_, _>>>(),
    );
}

// Sibling text (e.g. mixed content in XML) becomes arbitrary
//   documentation,
//     intended to be written in a literate style.
#[test]
fn text_as_arbitrary_doc() {
    let text = SPair("foo bar baz".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(Package, S1),
          Text(text),
        Close(Package, S3),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
          O(Air::PkgStart(S1, SPair("/TODO".into(), S1))),
            O(Air::DocText(text)),
          O(Air::PkgEnd(S3)),
        ]),
        sut_parse(toks.into_iter()).collect(),
    );
}
