// Tests for XML frontend
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

type Sut<B> = XmlFrontendParser<B>;

#[test]
fn emits_eof() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    let result = sut.parse_next();

    assert!(matches!(result, Ok(FrontendEvent::Eof)));
}
