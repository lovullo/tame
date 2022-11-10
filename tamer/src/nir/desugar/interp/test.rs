// Interpolation parser for desugaring NIR
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

use super::*;
use crate::{
    nir::PlainNirSymbol,
    parse::Parsed,
    span::dummy::{DUMMY_CONTEXT as DC, *},
    sym::GlobalSymbolResolve,
};
use std::assert_matches::assert_matches;
use Parsed::*;

type Sut = InterpState;

// While it'd be semantically valid to desugar a literal into a template
//   param,
//     it'd certainly be wasteful
//       (and would only be optimized away by a future lowering operation).
// Best to just leave it be.
#[test]
fn does_not_desugar_literal_only() {
    // `@bar@` is a metavariable,
    //   but it's also a literal because it's not enclosed in braces.
    for literal in ["foo", "@bar@"] {
        let sym = literal.into();
        let toks = vec![SPair(sym, S1)];

        assert_eq!(
            Ok(vec![Object(DoneExpanding(SPair(sym, S1)))]),
            Sut::parse(toks.into_iter()).collect(),
            "literal `{literal}` must not desugar",
        );
    }
}

// When ending with an interpolated variable,
//   the parser should recognize that we've returned to the outer literal
//   context and permit successful termination of the specification string.
#[test]
fn desugars_literal_with_ending_var() {
    let given_val = "foo{@bar@}";
    //               [-] [---]|
    //               0 2 4   8|
    //               |B    C  |
    //               [--------]
    //               0        9
    //                   A

    // Non-zero span offset ensures that derived spans properly consider
    //   parent offset.
    let a = DC.span(10, 10);
    let b = DC.span(10, 3);
    let c = DC.span(14, 5);

    let given_sym = SPair(given_val.into(), a);
    let toks = vec![given_sym];

    let GenIdentSymbolId(expect_name) = gen_tpl_param_ident_at_offset(a);
    let expect_dfn = PlainNirSymbol::Todo(expect_name.into(), a);
    let expect_text = PlainNirSymbol::Todo("foo".into(), b);
    let expect_param = PlainNirSymbol::Todo("@bar@".into(), c);

    let mut sut = Sut::parse(toks.into_iter());

    // This is the template param generated from the interpolated string.
    // The generated string is not interned,
    //   so we cannot match on its symbol,
    //   but that's okay since we don't entirely care what it says beyond
    //    containing the original string that it was derived from to provide
    //    helpful information to a human reader.
    assert_matches!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamOpen(
            dfn,
            PlainNirSymbol::Todo(desc_str, desc_span)
        ))))) if dfn == expect_dfn
            && desc_str.lookup_str().contains(given_val)
            && desc_span == a
    );

    // Note how the span associated with this is `B`,
    //   which is derived from the relevant portion of the original
    //   specification string.
    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamText(expect_text)))))
    );

    // This is the actual metavariable reference,
    //   pulled out of the interpolated portion of the given value.
    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamValue(expect_param))))),
    );

    // This is an object generated from user input,
    //   so the closing span has to identify what were generated from.
    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamClose(a)))))
    );

    // Finally,
    //   we replace the original provided attribute
    //     (the interpolation specification)
    //   with a metavariable reference to the generated parameter.
    assert_matches!(
        sut.next(),
        Some(Ok(Object(DoneExpanding(SPair(given_replace, given_span)))))
            if given_replace == expect_name && given_span == a
    );

    assert_eq!(sut.next(), None);
}

// This is largely the same as the above test,
//   with the literal and interpolation var reversed.
//
// Explanations above are omitted here.
#[test]
fn desugars_var_with_ending_literal() {
    let given_val = "{@foo@}bar";
    //               |[---] [-]
    //               |1   5 7 9
    //               |  B    C|
    //               [--------]
    //               0        9
    //                   A

    let a = DC.span(20, 10);
    let b = DC.span(21, 5);
    let c = DC.span(27, 3);

    let given_sym = SPair(given_val.into(), a);
    let toks = vec![given_sym];

    let GenIdentSymbolId(expect_name) = gen_tpl_param_ident_at_offset(a);
    let expect_dfn = PlainNirSymbol::Todo(expect_name.into(), a);
    let expect_param = PlainNirSymbol::Todo("@foo@".into(), b);
    let expect_text = PlainNirSymbol::Todo("bar".into(), c);

    let mut sut = Sut::parse(toks.into_iter());

    //
    // See above test for explanations that are not repeated here.
    //

    assert_matches!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamOpen(
            dfn,
            PlainNirSymbol::Todo(desc_str, desc_span)
        ))))) if dfn == expect_dfn
            && desc_str.lookup_str().contains(given_val)
            && desc_span == a
    );

    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamValue(expect_param))))),
    );

    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamText(expect_text)))))
    );

    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamClose(a)))))
    );

    assert_matches!(
        sut.next(),
        Some(Ok(Object(DoneExpanding(SPair(given_replace, given_span)))))
            if given_replace == expect_name && given_span == a
    );

    assert_eq!(sut.next(), None);
}

// Combination of the above two tests.
//
// Explanations above are omitted here.
#[test]
fn desugars_many_vars_and_literals() {
    let given_val = "foo{@bar@}baz{@quux@}";
    //               [-] [---] [-] [----]|
    //               0 2 4   8 10  14  19|
    //               |B    C    D    E   |
    //               [-------------------]
    //               0                  20
    //                          A

    let a = DC.span(30, 21);
    let b = DC.span(30, 3);
    let c = DC.span(34, 5);
    let d = DC.span(40, 3);
    let e = DC.span(44, 6);

    let given_sym = SPair(given_val.into(), a);
    let toks = vec![given_sym];

    let GenIdentSymbolId(expect_name) = gen_tpl_param_ident_at_offset(a);
    let expect_dfn = PlainNirSymbol::Todo(expect_name.into(), a);
    let expect_text1 = PlainNirSymbol::Todo("foo".into(), b);
    let expect_param1 = PlainNirSymbol::Todo("@bar@".into(), c);
    let expect_text2 = PlainNirSymbol::Todo("baz".into(), d);
    let expect_param2 = PlainNirSymbol::Todo("@quux@".into(), e);

    let mut sut = Sut::parse(toks.into_iter());

    //
    // See above tests for explanations that are not repeated here.
    //

    assert_matches!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamOpen(
            dfn,
            PlainNirSymbol::Todo(desc_str, desc_span)
        ))))) if dfn == expect_dfn
            && desc_str.lookup_str().contains(given_val)
            && desc_span == a
    );

    assert_eq!(
        Ok(vec![
            // These two are the as previous tests.
            Object(Expanded(PlainNir::TplParamText(expect_text1))),
            Object(Expanded(PlainNir::TplParamValue(expect_param1))),
            // This pair repeats literals and vars further into the pattern
            //   to ensure that the parser is able to handle returning to
            //   previous states and is able to handle inputs at different
            //   offsets.
            Object(Expanded(PlainNir::TplParamText(expect_text2))),
            Object(Expanded(PlainNir::TplParamValue(expect_param2))),
        ]),
        sut.by_ref().take(4).collect(),
    );

    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamClose(a)))))
    );

    assert_matches!(
        sut.next(),
        Some(Ok(Object(DoneExpanding(SPair(given_replace, given_span)))))
            if given_replace == expect_name && given_span == a
    );

    assert_eq!(sut.next(), None);
}

// Adjacent vars with empty literal between them.
#[test]
fn desugars_adjacent_interpolated_vars() {
    let given_val = "{@foo@}{@bar@}{@baz@}";
    //               |[---]  [---]  [---]|
    //               |1   5  8  12  15 19|
    //               |  B      C      D  |
    //               [-------------------]
    //               0                  20
    //                          A

    let a = DC.span(40, 21);
    let b = DC.span(41, 5);
    let c = DC.span(48, 5);
    let d = DC.span(55, 5);

    let given_sym = SPair(given_val.into(), a);
    let toks = vec![given_sym];

    let GenIdentSymbolId(expect_name) = gen_tpl_param_ident_at_offset(a);
    let expect_dfn = PlainNirSymbol::Todo(expect_name.into(), a);
    let expect_param1 = PlainNirSymbol::Todo("@foo@".into(), b);
    let expect_param2 = PlainNirSymbol::Todo("@bar@".into(), c);
    let expect_param3 = PlainNirSymbol::Todo("@baz@".into(), d);

    let mut sut = Sut::parse(toks.into_iter());

    //
    // See above tests for explanations that are not repeated here.
    //

    assert_matches!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamOpen(
            dfn,
            PlainNirSymbol::Todo(desc_str, desc_span)
        ))))) if dfn == expect_dfn
            && desc_str.lookup_str().contains(given_val)
            && desc_span == a
    );

    // These are the three adjacent vars.
    assert_eq!(
        Ok(vec![
            Object(Expanded(PlainNir::TplParamValue(expect_param1))),
            Object(Expanded(PlainNir::TplParamValue(expect_param2))),
            Object(Expanded(PlainNir::TplParamValue(expect_param3))),
        ]),
        sut.by_ref().take(3).collect(),
    );

    assert_eq!(
        sut.next(),
        Some(Ok(Object(Expanded(PlainNir::TplParamClose(a)))))
    );

    assert_matches!(
        sut.next(),
        Some(Ok(Object(DoneExpanding(SPair(given_replace, given_span)))))
            if given_replace == expect_name && given_span == a
    );

    assert_eq!(sut.next(), None);
}
