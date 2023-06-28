// Test abstract binding translation for NIR
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
use crate::span::dummy::{DUMMY_CONTEXT as DC, *};

type Sut = AbstractBindTranslate;
use Parsed::Object as O;

#[test]
fn already_abstract_bind_ignored_despite_metavar_naming() {
    // This is named as a metavariable.
    let name = "@foo@".into();

    assert_tok_translate(
        // Identifier is already abstract...
        BindIdentAbstract(SPair(name, S1)),
        // ...so this acts as an identity operation.
        BindIdentAbstract(SPair(name, S1)),
    );
}

#[test]
fn concrete_bind_without_metavar_naming_ignored_non_meta() {
    // This is _not_ named as a metavariable.
    let name = "concrete".into();

    assert_tok_translate(
        // Identifier is concrete and the name does not follow a
        //   metavariable naming convention...
        BindIdent(SPair(name, S1)),
        // ...and so this acts as an identity operation.
        BindIdent(SPair(name, S1)),
    );
}

#[test]
fn non_meta_concrete_bind_with_metavar_naming_translated_to_abstract_bind() {
    // This is named as a metavariable.
    let name = "@foo@".into();

    assert_tok_translate(
        // Identifier is concrete...
        BindIdent(SPair(name, S1)),
        // ...and so gets translated into an abstract binding.
        // Its data are otherwise the same.
        BindIdentAbstract(SPair(name, S1)),
    );
}

// Metavariable definitions must be left concrete since they produce the
//   identifiers that are utilized by other abstract identifiers.
#[test]
fn meta_concrete_bind_with_metavar_naming_ignored() {
    // This is named as a metavariable.
    let name = "@param@".into();

    assert_tok_translate(
        // This identifier utilizes a metavariable naming convention,
        //   but we're in a metavariable definition context.
        BindIdentMeta(SPair(name, S2)),
        // And so the bind stays concrete.
        BindIdentMeta(SPair(name, S2)),
    );
}

// This lowering operation utilizes a naming convention to infer user intent
//   and lift the requirement for curly braces;
//     they go hand-in-hand.
// To utilize this feature,
//   we must also require adherence to the naming convention so that we know
//   that our assumptions hold.
//
// We can't check for the opposite---
//   that non-meta identifiers must _not_ follow that convention---
//   because we interpet such occurrences as abstract identifiers.
// In practice,
//   users will get an error because the conversion into a reference will
//   yield an error when the metavariable does not exist already as a
//   reference,
//     or a duplicate definition error if it was already defined.
#[test]
fn rejects_metavariable_without_naming_convention() {
    let name_a = "@missing-end".into();
    //            |          |
    //            |         11
    //            |          B
    //            [----------]
    //            0         11
    //                 A

    let a_a = DC.span(10, 12);
    let a_b = DC.span(22, 0); // _after_ last char

    let name_b = "missing-start@".into();
    //            |            |
    //            0            |
    //            A            |
    //            [------------]
    //            0           13
    //                  A
    let b_a = DC.span(10, 14);
    let b_b = DC.span(10, 0); // _before_ first char

    let name_c = "missing-both".into();
    //            |          |
    //            0          |
    //            B          |
    //            [----------]
    //            0         11
    //                 A
    let c_a = DC.span(10, 12);
    let c_b = DC.span(10, 0); // _before_ first char

    // Each of these will result in slightly different failures.
    #[rustfmt::skip]
    let toks = [
        BindIdentMeta(SPair(name_a, a_a)),
        BindIdentMeta(SPair(name_b, b_a)),
        BindIdentMeta(SPair(name_c, c_a)),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Err(ParseError::StateError(
                AbstractBindTranslateError::MetaNamePadMissing(
                    SPair(name_a, a_a),
                    a_b,
                ),
            )),
            Err(ParseError::StateError(
                AbstractBindTranslateError::MetaNamePadMissing(
                    SPair(name_b, b_a),
                    b_b,
                ),
            )),
            Err(ParseError::StateError(
                AbstractBindTranslateError::MetaNamePadMissing(
                    SPair(name_c, c_a),
                    c_b,
                ),
            )),
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<Result<_, _>>>(),
    );
}

fn assert_tok_translate(tok: Nir, expect: Nir) {
    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![O(expect)]),
        Sut::parse([tok].into_iter()).collect()
    );
}
