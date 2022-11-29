// XIR attribute parser generator tests
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

use super::super::{AttrParseError, AttrParseState};
use crate::{
    diagnose::{AnnotatedSpan, Diagnostic},
    parse::{self, util::SPair, ParseError, Parsed, Parser, TokenStream},
    span::dummy::*,
    xir::{
        attr::{Attr, AttrSpan},
        flat::XirfToken,
        st::qname::*,
        OpenSpan, QName,
    },
};
use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display},
    iter,
};

use Parsed::Object;

const SE: OpenSpan = OpenSpan(S1.offset_add(100).unwrap(), 0);

// Random choice of QName for tests.
const QN_ELE: QName = QN_YIELDS;

fn sut_parse<S: AttrParseState, I: TokenStream<S::Token>>(
    toks: I,
) -> Parser<S, I>
where
    S: AttrParseState,
    S::Context: Default,
{
    Parser::with_state(S::with_element(QN_ELE, SE), toks)
}

#[derive(Debug, PartialEq, Eq)]
enum Foo {
    A(SPair),
    B(SPair),
    Unused(SPair),
}

impl parse::Object for Foo {}

impl<E> From<Foo> for Result<Foo, E> {
    fn from(value: Foo) -> Self {
        Ok(value)
    }
}

// Remember: we only describe what is _permissable_,
//   not what is required or what order it must appear in.
// That is the responsibility of parsers lower in the pipeline.
#[test]
fn attrs_any_order_and_optional() {
    attr_parse_stream! {
        type Object = Foo;
        type ValueError = Infallible;

        ValuesState {
            QN_NAME => Foo::A,

            // The above is the same as this longer form:
            QN_YIELDS => |spair| Foo::B(spair),

            // No value will be provided for this one,
            //   which is okay since all are implicitly optional.
            QN_INDEX => Foo::Unused,
        }
    }

    let name = "val_name".into();
    let yields = "val_value".into();
    let attr_name = Attr(QN_NAME, name, AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, yields, AttrSpan(S2, S3));

    // @yields then @name just to emphasize that order does not matter.
    let toks = vec![XirfToken::Attr(attr_yields), XirfToken::Attr(attr_name)];

    assert_eq!(
        // Simply parses back out the attributes;
        //   see following tests further value parsing.
        // Note that we omit one of the attributes declared above.
        Ok(vec![
            Object(Foo::B(SPair(yields, S3))),
            Object(Foo::A(SPair(name, S2)))
        ]),
        sut_parse::<ValuesState, _>(toks.into_iter()).collect(),
    );
}

// Since all are optional,
//   the attribute list can be empty.
#[test]
fn attrs_empty() {
    attr_parse_stream! {
        type Object = Foo;
        type ValueError = Infallible;

        ValuesState {
            // We will not provide a value for this.
            QN_NAME => Foo::Unused,
        }
    }

    assert_eq!(
        // Simply parses back out the attributes;
        //   see following tests further value parsing.
        Ok(vec![]),
        sut_parse::<ValuesState, _>(iter::empty()).collect(),
    );
}

#[test]
fn attr_value_error() {
    impl TryFrom<SPair> for Foo {
        type Error = FooError;

        fn try_from(spair: SPair) -> Result<Self, Self::Error> {
            Err(FooError(spair))
        }
    }

    #[derive(Debug, PartialEq)]
    struct FooError(SPair);

    impl Error for FooError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }

    impl Display for FooError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "test FooError")
        }
    }

    impl Diagnostic for FooError {
        fn describe(&self) -> Vec<AnnotatedSpan> {
            vec![]
        }
    }

    impl Into<Result<Foo, FooError>> for FooError {
        fn into(self) -> Result<Foo, FooError> {
            Err(self)
        }
    }

    attr_parse_stream! {
        type Object = Foo;
        type ValueError = FooError;

        ValueTryIntoState {
            // Explicit form:
            QN_NAME => |s| Err(FooError(s)),

            // Conversion using `Into`:
            QN_YIELDS => FooError,
        }
    }

    let val_name = "val_name".into();
    let val_yields = "val_yields".into();
    let attr_name = Attr(QN_NAME, val_name, AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, val_yields, AttrSpan(S2, S3));

    let toks = vec![
        XirfToken::Attr(attr_name.clone()),
        XirfToken::Attr(attr_yields.clone()),
    ];

    let mut sut = sut_parse::<ValueTryIntoState, _>(toks.into_iter());

    assert_eq!(
        Some(Err(ParseError::StateError(AttrParseError::InvalidValue(
            FooError(SPair(val_name, S2)),
            QN_ELE
        )))),
        sut.next(),
    );

    // TryInto on `Option` inner type.
    assert_eq!(
        Some(Err(ParseError::StateError(AttrParseError::InvalidValue(
            FooError(SPair(val_yields, S3)),
            QN_ELE
        )))),
        sut.next(),
    );
}

#[test]
fn unexpected_attr_with_recovery() {
    attr_parse_stream! {
        type Object = Foo;
        type ValueError = Infallible;

        UnexpectedState {
            QN_NAME => Foo::A,
            QN_SRC => Foo::B,
        }
    }

    let name = "val_name".into();
    let src = "val_src".into();
    let attr_name = Attr(QN_NAME, name, AttrSpan(S1, S2));
    let attr_unexpected = Attr(QN_TYPE, "unexpected".into(), AttrSpan(S1, S2));
    let attr_src = Attr(QN_SRC, src, AttrSpan(S2, S3));

    let toks = vec![
        // This is expected:
        XirfToken::Attr(attr_name),
        // NOT expected (produce an error):
        XirfToken::Attr(attr_unexpected.clone()),
        // <Recovery must take place here.>
        // This is expected after recovery:
        XirfToken::Attr(attr_src),
    ];

    let mut sut = Parser::with_state(
        UnexpectedState::with_element(QN_ELE, SE),
        toks.into_iter(),
    );

    assert_eq!(sut.next(), Some(Ok(Object(Foo::A(SPair(name, S2))))));

    // This will fail at the unknown attribute,
    //   and must then remain in a state where parsing can be resumed.
    // This simply means ignoring the provided attribute,
    //   which in XIRF is discarding a single token of input,
    //   rather than having to continue parsing the attribute to then
    //     discard.
    assert_eq!(
        sut.next(),
        Some(Err(ParseError::StateError(AttrParseError::UnexpectedAttr(
            attr_unexpected,
            QN_ELE,
        )))),
    );

    assert_eq!(sut.next(), Some(Ok(Object(Foo::B(SPair(src, S3))))));
}
