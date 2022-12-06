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
    nir::NirEntity,
    parse::{Parsed, Parser},
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

        // Arbitrary token type that supports symbols
        let toks = vec![Nir::Ref(SPair(sym, S1))];

        assert_eq!(
            Ok(vec![Object(Nir::Ref(SPair(sym, S1)))]),
            Sut::parse(toks.into_iter()).collect(),
            "literal `{literal}` must not desugar",
        );
    }
}

// ...not that it could.
#[test]
fn does_not_desugar_tokens_without_symbols() {
    let toks = vec![Nir::Close(S1)];

    assert_eq!(
        Ok(vec![Object(Nir::Close(S1))]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

fn expect_expanded_header(
    sut: &mut Parser<InterpState, std::vec::IntoIter<Nir>>,
    given_val: &str,
    span: Span,
) -> SymbolId {
    let GenIdentSymbolId(expect_name) = gen_tpl_param_ident_at_offset(span);
    let expect_name_sym = expect_name.into();

    // This is the template param generated from the interpolated string.
    // The generated string is not interned,
    //   so we cannot match on its symbol,
    //   but that's okay since we don't entirely care what it says beyond
    //    containing the original string that it was derived from to provide
    //    helpful information to a human reader.
    assert_eq!(
        sut.next(),
        Some(Ok(Object(Nir::Open(NirEntity::TplParam, span)))),
    );
    assert_eq!(
        sut.next(),
        Some(Ok(Object(Nir::BindIdent(SPair(expect_name_sym, span))))),
    );
    assert_matches!(
        sut.next(),
        Some(Ok(Object(Nir::Desc(SPair(desc_str, desc_span)))))
            if desc_str.lookup_str().contains(given_val)
                && desc_span == span
    );

    expect_name_sym
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

    let given_sym = Nir::Ref(SPair(given_val.into(), a));
    let toks = vec![given_sym];

    let mut sut = Sut::parse(toks.into_iter());

    let expect_name = expect_expanded_header(&mut sut, given_val, a);

    assert_eq!(
        Ok(vec![
            // Note how the span associated with this is `B`,
            //   which is derived from the relevant portion of the original
            //   specification string.
            Object(Nir::Text(SPair("foo".into(), b))),
            // This is the actual metavariable reference, pulled out of the
            //   interpolated portion of the given value.
            Object(Nir::Ref(SPair("@bar@".into(), c))),
            // This is an object generated from user input, so the closing
            //   span has to identify what were generated from.
            Object(Nir::Close(a)),
            // Finally,
            //   we replace the original provided attribute
            //     (the interpolation specification)
            //   with a metavariable reference to the generated parameter.
            Object(Nir::Ref(SPair(expect_name, a))),
        ]),
        sut.collect(),
    );
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

    let given_sym = Nir::Ref(SPair(given_val.into(), a));
    let toks = vec![given_sym];

    let mut sut = Sut::parse(toks.into_iter());

    let expect_name = expect_expanded_header(&mut sut, given_val, a);

    assert_eq!(
        Ok(vec![
            Object(Nir::Ref(SPair("@foo@".into(), b))),
            Object(Nir::Text(SPair("bar".into(), c))),
            Object(Nir::Close(a)),
            Object(Nir::Ref(SPair(expect_name, a))),
        ]),
        sut.collect(),
    );
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

    let given_sym = Nir::Ref(SPair(given_val.into(), a));
    let toks = vec![given_sym];

    let mut sut = Sut::parse(toks.into_iter());

    let expect_name = expect_expanded_header(&mut sut, given_val, a);

    assert_eq!(
        Ok(vec![
            // These two are the as previous tests.
            Object(Nir::Text(SPair("foo".into(), b))),
            Object(Nir::Ref(SPair("@bar@".into(), c))),
            // This pair repeats literals and vars further into the pattern
            //   to ensure that the parser is able to handle returning to
            //   previous states and is able to handle inputs at different
            //   offsets.
            Object(Nir::Text(SPair("baz".into(), d))),
            Object(Nir::Ref(SPair("@quux@".into(), e))),
            Object(Nir::Close(a)),
            Object(Nir::Ref(SPair(expect_name, a))),
        ]),
        sut.collect(),
    );
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

    let given_sym = Nir::Ref(SPair(given_val.into(), a));
    let toks = vec![given_sym];

    let mut sut = Sut::parse(toks.into_iter());

    let expect_name = expect_expanded_header(&mut sut, given_val, a);

    assert_eq!(
        Ok(vec![
            Object(Nir::Ref(SPair("@foo@".into(), b))),
            Object(Nir::Ref(SPair("@bar@".into(), c))),
            Object(Nir::Ref(SPair("@baz@".into(), d))),
            Object(Nir::Close(a)),
            Object(Nir::Ref(SPair(expect_name, a))),
        ]),
        sut.collect(),
    );
}
