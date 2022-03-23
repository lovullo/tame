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
    parse::{ParseError, ParseState, Parsed},
    span::{Span, DUMMY_SPAN},
    xir::{
        attr::Attr,
        flat::{Depth, Object as Xirf},
        QName,
    },
};

const S1: Span = DUMMY_SPAN;
const S2: Span = S1.offset_add(1).unwrap();
const S3: Span = S2.offset_add(1).unwrap();
const S4: Span = S3.offset_add(1).unwrap();

type Sut = XmloReader;

#[test]
fn fails_on_invalid_root() {
    let mut sut = Sut::parse(
        [Xirf::Open(
            "not-a-valid-package-node".unwrap_into(),
            S1,
            Depth(0),
        )]
        .into_iter(),
    );

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(XmloError::UnexpectedRoot)))
    );
}

fn common_parses_package_attrs(package: QName) {
    let name = "pkgroot".into();
    let relroot = "../../".into();
    let elig = "elig-class-yields".into();

    let toks = [
        Xirf::Open(package, S1, Depth(0)),
        Xirf::Attr(Attr::new("name".unwrap_into(), name, (S2, S3))),
        Xirf::Attr(Attr::new("__rootpath".unwrap_into(), relroot, (S2, S3))),
        Xirf::Attr(Attr::new(
            "program".unwrap_into(),
            crate::sym::st::raw::L_TRUE,
            (S3, S4),
        )),
        Xirf::Attr(Attr::new(
            ("preproc", "elig-class-yields").unwrap_into(),
            elig,
            (S3, S4),
        )),
        Xirf::Close(Some(package), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,
            Parsed::Object(XmloEvent::PkgName(name)),
            Parsed::Object(XmloEvent::PkgRootPath(relroot)),
            Parsed::Object(XmloEvent::PkgProgramFlag),
            Parsed::Object(XmloEvent::PkgEligClassYields(elig)),
            Parsed::Incomplete,
        ]),
        sut.collect(),
    );
}

// The linker does not [yet] parse namespaces.
#[test]
fn parses_package_attrs_without_ns_prefix() {
    common_parses_package_attrs("package".unwrap_into());
}

// The linker does not [yet] parse namespaces.
#[test]
fn parses_package_attrs_with_ns_prefix() {
    common_parses_package_attrs(("lv", "package").unwrap_into());
}

// Maintains BC with existing system,
//   but this ought to reject in the future.
#[test]
fn ignores_unknown_package_attr() {
    let package = "package".unwrap_into();
    let name = "pkgroot".into();

    let toks = [
        Xirf::Open(package, S1, Depth(0)),
        Xirf::Attr(Attr::new("name".unwrap_into(), name, (S2, S3))),
        // This is ignored.
        Xirf::Attr(Attr::new("unknown".unwrap_into(), name, (S2, S3))),
        Xirf::Close(Some(package), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,
            Parsed::Object(XmloEvent::PkgName(name)),
            Parsed::Incomplete, // The unknown attribute
            Parsed::Incomplete,
        ]),
        sut.collect(),
    );
}
