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
use crate::{convert::ExpectInto, span::DUMMY_SPAN as DS, xir::Token};

type Sut<B> = XmloReader<B>;

#[test]
fn fail_unexpected_eof() {
    let mut sut = Sut::from_reader([].into_iter());
    assert_matches!(sut.next(), Some(Err(XmloError::UnexpectedEof)));
}

#[test]
fn fails_on_invalid_root() {
    let mut sut = Sut::from_reader(
        [Token::Open("not-a-valid-package-node".unwrap_into(), DS)].into_iter(),
    );

    assert_matches!(sut.next(), Some(Err(XmloError::UnexpectedRoot)));
}

#[test]
fn parses_package_attrs() {
    let name = "pkgroot".into();
    let relroot = "../../".into();
    let elig = "elig-class-yields".into();

    let mut sut = Sut::from_reader(
        [
            Token::Open("package".unwrap_into(), DS),
            Token::AttrName("name".unwrap_into(), DS),
            Token::AttrValue(name, DS),
            Token::AttrName("__rootpath".unwrap_into(), DS),
            Token::AttrValue(relroot, DS),
            Token::AttrName("program".unwrap_into(), DS),
            Token::AttrValue(raw::L_TRUE, DS),
            Token::AttrName(("preproc", "elig-class-yields").unwrap_into(), DS),
            Token::AttrValue(elig, DS),
            Token::AttrEnd,
        ]
        .into_iter(),
    );

    let result = sut.next();

    assert_eq!(
        Some(Ok(XmloEvent::Package(PackageAttrs {
            name: Some(name),
            relroot: Some(relroot),
            program: true,
            elig: Some(elig),
        }))),
        result
    );
}

// The linker does not [yet] parse namespaces.
#[test]
fn parses_package_attrs_with_ns_prefix() {
    let name = "pkgrootns".into();

    let mut sut = Sut::from_reader(
        [
            Token::Open(("lv", "package").unwrap_into(), DS),
            Token::AttrName("name".unwrap_into(), DS),
            Token::AttrValue(name, DS),
            Token::AttrEnd,
        ]
        .into_iter(),
    );

    let result = sut.next();

    // NB: This also tests defaults and non-program.
    assert_eq!(
        Some(Ok(XmloEvent::Package(PackageAttrs {
            name: Some(name),
            ..Default::default()
        }))),
        result
    );
}
