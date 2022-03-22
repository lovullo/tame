// Tests xmlo object file reader
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

use std::assert_matches::assert_matches;

use super::*;
use crate::{
    convert::ExpectInto,
    parse::{ParseError, ParseState},
    span::DUMMY_SPAN as DS,
    xir::{
        attr::Attr,
        flat::{Depth, Object as Xirf},
    },
};

type Sut = XmloReader;

#[test]
fn fails_on_invalid_root() {
    let mut sut = Sut::parse(
        [Xirf::Open(
            "not-a-valid-package-node".unwrap_into(),
            DS,
            Depth(0),
        )]
        .into_iter(),
    );

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(XmloError::UnexpectedRoot)))
    );
}

//#[test]
fn _parses_package_attrs() {
    let name = "pkgroot".into();
    let relroot = "../../".into();
    let elig = "elig-class-yields".into();

    let mut sut = Sut::parse(
        [
            Xirf::Open("package".unwrap_into(), DS, Depth(0)),
            Xirf::Attr(Attr::new("name".unwrap_into(), name, (DS, DS))),
            Xirf::Attr(Attr::new(
                "__rootpath".unwrap_into(),
                relroot,
                (DS, DS),
            )),
            Xirf::Attr(Attr::new(
                "program".unwrap_into(),
                crate::sym::st::raw::L_TRUE,
                (DS, DS),
            )),
            Xirf::Attr(Attr::new(
                ("preproc", "elig-class-yields").unwrap_into(),
                elig,
                (DS, DS),
            )),
        ]
        .into_iter(),
    );

    let _result = sut.next();

    todo!()
}

// The linker does not [yet] parse namespaces.
//#[test]
fn _parses_package_attrs_with_ns_prefix() {
    let name = "pkgrootns".into();

    let mut sut = Sut::parse(
        [
            Xirf::Open(("lv", "package").unwrap_into(), DS, Depth(0)),
            Xirf::Attr(Attr::new("name".unwrap_into(), name, (DS, DS))),
        ]
        .into_iter(),
    );

    let _result = sut.next();

    todo!()
}
