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
