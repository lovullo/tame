// Tests for NIR desugaring
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
use crate::parse::Parsed;

type Sut = DesugarNir;

// Given the simplicity,
//   this just really ensures that the parser terminates.
#[test]
fn maps_plain_nir_todo() {
    let toks = vec![SugaredNir::Todo];

    use Parsed::*;
    assert_eq!(
        Ok(vec![Object(PlainNir::Todo)]),
        Sut::parse(toks.into_iter()).collect(),
    );
}
