// Tests shorthand template application desugaring for NIR
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
use crate::{convert::ExpectInto, span::dummy::*};

use Parsed::Object as O;

type Sut = TplShortDesugar;

#[test]
fn desugars_nullary() {
    // Shorthand converts `t:tpl-name` into `_tpl-name_`.
    let qname = ("t", "tpl-name").unwrap_into();
    let tpl = "_tpl-name_".into();

    let toks = [
        Nir::Open(NirEntity::TplApply(Some(qname)), S1),
        Nir::Close(NirEntity::TplApply(None), S2),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Nir::Open(NirEntity::TplApply(None), S1)),
              // The span associated with the name of the template to be
              //   applied is the span of the entire QName from NIR.
              // The reason for this is that `t:foo` is transformed into
              //   `_foo_`,
              //     and so the `t:` is a necessary part of the
              //     representation of the name of the template;
              //       `foo` is not in itself a valid template name at the
              //       time of writing.
              O(Nir::Ref(SPair(tpl, S1))),
            O(Nir::Close(NirEntity::TplApply(None), S2)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn desugars_unary() {
    // Shorthand converts `t:tpl-name` into `_tpl-name_`.
    let qname = ("t", "tpl-name").unwrap_into();
    let name = SPair("_tpl-name_".into(), S1);

    let aname = SPair("foo".into(), S3);
    let pval = SPair("foo value".into(), S4);

    // The attribute name gets padded with '@',
    //   much like the template does with underscores.
    let pname = SPair("@foo@".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        // <t:qname
        Nir::Open(NirEntity::TplApply(Some(qname)), S1),
          // foo="foo value"
          Nir::Open(NirEntity::TplParam(Some((aname, pval))), S2),
          // Implicit close.
        // />
        Nir::Close(NirEntity::TplApply(None), S6),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Nir::Open(NirEntity::TplApply(None), S1)),
              O(Nir::Ref(name)),

              O(Nir::Open(NirEntity::TplParam(None), S2)),
                // Derived from `aname` (by padding)
                O(Nir::BindIdent(pname)),
                // The value is left untouched.
                O(Nir::Text(pval)),
              // Close is derived from open.
              O(Nir::Close(NirEntity::TplParam(None), S2)),
            O(Nir::Close(NirEntity::TplApply(None), S6)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Don't parse what we desugar into!
#[test]
fn does_not_desugar_long_form() {
    let name = SPair("_tpl-name_".into(), S2);
    let pname = SPair("@param".into(), S4);
    let pval = SPair("value".into(), S5);

    #[rustfmt::skip]
    let toks = [
        Nir::Open(NirEntity::TplApply(None), S1),
          Nir::BindIdent(name),

          Nir::Open(NirEntity::TplParam(None), S3),
            Nir::BindIdent(pname),
            Nir::Text(pval),
          Nir::Close(NirEntity::TplParam(None), S6),
        Nir::Close(NirEntity::TplApply(None), S7),
    ];

    #[rustfmt::skip]
    assert_eq!(
        // We avoid #[derive(Clone)] on Nir so that we have confidence that
        //   we're not doing anything suspicious with tokens.
        // So this is a duplicate of the above,
        //   mapped over `O`.
        Ok(vec![
            O(Nir::Open(NirEntity::TplApply(None), S1)),
              O(Nir::BindIdent(name)),

              O(Nir::Open(NirEntity::TplParam(None), S3)),
                O(Nir::BindIdent(pname)),
                O(Nir::Text(pval)),
              O(Nir::Close(NirEntity::TplParam(None), S6)),
            O(Nir::Close(NirEntity::TplApply(None), S7)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}
