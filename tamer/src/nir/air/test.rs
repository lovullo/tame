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
use crate::{parse::util::SPair, span::dummy::*};

type Sut = NirToAir;

use Parsed::Object as O;

#[test]
fn rate_to_sum_expr() {
    let id = SPair("foo".into(), S2);

    let toks = vec![
        Nir::Open(NirEntity::Rate, S1),
        Nir::BindIdent(id),
        Nir::Close(S3),
    ];

    assert_eq!(
        Ok(vec![
            O(Air::ExprOpen(ExprOp::Sum, S1)),
            O(Air::ExprIdent(id)),
            O(Air::ExprClose(S3)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}