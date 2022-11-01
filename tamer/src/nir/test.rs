// Base tests for NIR
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

use super::NirSymbolTy::*;
use crate::{
    nir::{NirSymbolTy, SugaredNirSymbol},
    span::dummy::*,
};

/// A type for testing that can hold any [`SymbolId`] without worry of type
///   validations
///     (so that [`NirSymbolTy`] can be ignored to test other logic).
type AnySugaredNirSymbol = SugaredNirSymbol<{ StringLiteral }>;

/// Sugared NIR should recognize when there will be no need for desugaring
///   (by consequence of it detecting when there _is_ such a need).
#[test]
fn from_pair_plain_string() {
    // No sugar added.
    let sym = "foo".into();

    assert_eq!(Ok(AnySugaredNirSymbol::Todo(sym, S1)), (sym, S1).try_into(),);
}

/// Strings requiring interpolation should be detected,
///   but not yet parsed.
/// This means that we detect strings that contain the interpolation
///   character `{` and mark them for further processing _even if it is not
///   balanced_.
///
/// A separate test checks whether type parsing is deferred.
#[test]
fn from_pair_interpolation_string() {
    let tests = [
        // This is the form that we'd expect.
        "foo{@bar@}baz",
        // This doesn't make sense,
        //   but we don't know that yet;
        //     it still requires interpolation to parse.
        "foo{bar}baz",
        // This is not even valid syntax,
        //   but hey,
        //   we still have to mark it so that we can find that out when we
        //   go to interpolate during desugaring.
        "foo{",
        // This will be a trivial replacement,
        //   but it's still interpolation.
        "{@foo@}",
        // Absolute nonsense,
        //   but you get the drill.
        "{",
    ];

    tests.into_iter().map(Into::into).for_each(|sym| {
        assert_eq!(
            Ok(AnySugaredNirSymbol::Interpolate(sym, S1)),
            (sym, S1).try_into(),
            "must recognize `{sym}` as needing interpolation",
        );
    });
}

// We cannot possibly validate whether a string can be parsed into its
//   target type until we've interpolated it.
#[test]
fn from_pair_interpolation_delays_type_validation() {
    // This is the type we're hoping to parse into,
    const DEST_TY: NirSymbolTy = NumLiteral;
    // but we cannot know yet because interpolation is needed.
    let sym = "{@maybe_a_number@}".into();

    assert_eq!(
        Ok(SugaredNirSymbol::<{ DEST_TY }>::Interpolate(sym, S2)),
        (sym, S2).try_into(),
    );
}
