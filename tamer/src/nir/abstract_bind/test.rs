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
use crate::span::dummy::*;

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
    let name_outside = "@foo@".into();

    #[rustfmt::skip]
    let toks = [
        Open(TplParam, S1),
          // This identifier utilizes a metavariable naming convention,
          //   but we're in a template parameter context,
          //   which defines a metavariable.
          BindIdent(SPair(name, S2)),
        Close(TplParam, S3),

        // Back outside of a param context.
        BindIdent(SPair(name_outside, S4)),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(TplParam, S1)),
              // Since we are in a metavariable definition context,
              //   the bind _stays concrete_.
              O(BindIdent(SPair(name, S2))),
            O(Close(TplParam, S3)),

            // This one is now outside the metavariable context,
            //   and so we should go back to translating to abstract.
            O(BindIdentAbstract(SPair(name_outside, S4))),
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

fn assert_tok_translate(tok: Nir, expect: Nir) {
    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![O(expect)]),
        Sut::parse([tok].into_iter()).collect()
    );
}
