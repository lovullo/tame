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
        Open(TplApplyShort(qname), S1),
        Close(TplApplyShort(qname), S2),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(TplApply, S1)),
              // The span associated with the name of the template to be
              //   applied is the span of the entire QName from NIR.
              // The reason for this is that `t:foo` is transformed into
              //   `_foo_`,
              //     and so the `t:` is a necessary part of the
              //     representation of the name of the template;
              //       `foo` is not in itself a valid template name at the
              //       time of writing.
              O(Ref(SPair(tpl, S1))),
            O(Close(TplApply, S2)),
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
        Open(TplApplyShort(qname), S1),
          // foo="foo value"
          Open(TplParamShort(aname, pval), S2),
          // Implicit close.
        // />
        Close(TplApplyShort(qname), S6),
    ];

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(TplApply, S1)),
              O(Ref(name)),

              O(Open(TplParam, S2)),
                // Derived from `aname` (by padding)
                O(BindIdent(pname)),
                // The value is left untouched.
                O(Text(pval)),
              // Close is derived from open.
              O(Close(TplParam, S2)),
            O(Close(TplApply, S6)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Body of shorthand is desugared into `@values@` param.
#[test]
fn desugars_body_into_tpl_with_ref_in_values_param() {
    // Shorthand converts `t:tpl-name` into `_tpl-name_`.
    let qname = ("t", "short").unwrap_into();
    let name = SPair("_short_".into(), S1);

    #[rustfmt::skip]
    let toks = vec![
        // <t:qname>
        Open(TplApplyShort(qname), S1),
          // Body to desugar into own template (@values@).
          Open(Sum, S2),
            Open(Product, S3),
            Close(Product, S4),
          Close(Sum, S5),

          // Body can contain siblings.
          Open(Product, S6),
          Close(Product, S7),
        // </t:qname>
        Close(TplApplyShort(qname), S8),
    ];

    // The name of the generated template.
    // This test is a bit too friendly with implementation details,
    //   but it does allow us to be perfectly precise in the output
    //   assertion.
    let gen_name = gen_tpl_name_at_offset(S1);

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(TplApply, S1)),
              O(Ref(name)),

              // @values@ remains lexical by referencing the name of a
              //   template we're about to generate.
              O(Open(TplParam, S1)),
                O(BindIdent(SPair(L_TPLP_VALUES, S1))),
                O(Text(SPair(gen_name, S1))),      //:-.
              O(Close(TplParam, S1)),              //   |
            O(Close(TplApply, S1)),                //   |
                                                   //   |
            // Generate a template to hold the     //   |
            //   body of `@values@`.               //   |
            // It is closed and so expandable.     //   |
            O(Open(Tpl, S1)),                      //  /
              O(BindIdent(SPair(gen_name, S1))),   //<`

              // And here we have the body of the above
              //   shorthand application.
              O(Open(Sum, S2)),
                O(Open(Product, S3)),
                O(Close(Product, S4)),
              O(Close(Sum, S5)),

              O(Open(Product, S6)),
              O(Close(Product, S7)),
            O(Close(Tpl, S8)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Shorthand within a shorthand.
#[test]
fn desugar_nested_apply() {
    let qname_outer = ("t", "outer").unwrap_into();
    let name_outer = SPair("_outer_".into(), S1);

    let qname_inner = ("t", "inner").unwrap_into();
    let name_inner = SPair("_inner_".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Open(TplApplyShort(qname_outer), S1),
          // Body is a second shorthand template application.
          Open(TplApplyShort(qname_inner), S2),
          Close(TplApplyShort(qname_inner), S3),
        Close(TplApplyShort(qname_outer), S4),
    ];

    let gen_name_outer = gen_tpl_name_at_offset(S1);

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(TplApply, S1)),
              O(Ref(name_outer)),

              // @values@
              O(Open(TplParam, S1)),
                O(BindIdent(SPair(L_TPLP_VALUES, S1))),
                O(Text(SPair(gen_name_outer, S1))),    //:-.
              O(Close(TplParam, S1)),                  //   |
            O(Close(TplApply, S1)),                    //   |
                                                       //   |
            O(Open(Tpl, S1)),                          //  /
              O(BindIdent(SPair(gen_name_outer, S1))), //<`

              // And within this template,
              //   we generate another application.
              // This one has no body and so no `@values@`.
              O(Open(TplApply, S2)),
                O(Ref(name_inner)),
              O(Close(TplApply, S3)),
            O(Close(Tpl, S4)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Don't parse what we desugar into!
#[test]
fn does_not_desugar_long_form() {
    let name = SPair("_tpl-name_".into(), S2);
    let pname = SPair("@param@".into(), S4);
    let pval = SPair("value".into(), S5);

    #[rustfmt::skip]
    let toks = [
        Open(TplApply, S1),
          BindIdent(name),

          Open(TplParam, S3),
            BindIdent(pname),
            Text(pval),
          Close(TplParam, S6),
        Close(TplApply, S7),
    ];

    #[rustfmt::skip]
    assert_eq!(
        // We avoid #[derive(Clone)] on Nir so that we have confidence that
        //   we're not doing anything suspicious with tokens.
        // So this is a duplicate of the above,
        //   mapped over `O`.
        Ok(vec![
            O(Open(TplApply, S1)),
              O(BindIdent(name)),

              O(Open(TplParam, S3)),
                O(BindIdent(pname)),
                O(Text(pval)),
              O(Close(TplParam, S6)),
            O(Close(TplApply, S7)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}
