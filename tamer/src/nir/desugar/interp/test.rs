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

use super::{super::super::NirSymbolTy::*, *};
use crate::{
    nir::{PlainNirSymbol, SugaredNirSymbol},
    parse::Parsed,
    span::dummy::{DUMMY_CONTEXT as DC, *},
    sym::GlobalSymbolResolve,
};
use std::assert_matches::assert_matches;

use Parsed::*;

type Sut<const TY: NirSymbolTy> = InterpState<TY>;

// While it'd be semantically valid to desugar a literal into a template
//   param,
//     it'd certainly be wasteful
//       (and would only be optimized away by a future lowering operation).
// Best to just leave it be.
#[test]
fn does_not_desugar_literal_only() {
    let sym = "foo".into();
    let toks = vec![SugaredNirSymbol::<{ StringLiteral }>(sym, S1)];

    assert_eq!(
        Ok(vec![Object(ReplaceSym(PlainNirSymbol::Todo(sym, S1)))]),
        Sut::parse(toks.into_iter()).collect(),
    );
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

    let a = DC.span(0, 10);
    let b = DC.span(0, 3);
    let c = DC.span(4, 5);

    let given_sym = SugaredNirSymbol::<{ StringLiteral }>(given_val.into(), a);
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
        Some(Ok(Object(ReplaceSym(PlainNirSymbol::Todo(given_replace, given_span)))))
            if given_replace == expect_name && given_span == a
    );

    assert_eq!(sut.next(), None);
}
