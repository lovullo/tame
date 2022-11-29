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
    parse::{self, ParseError, Parsed, Parser, TokenStream},
    span::{dummy::*, Span},
    sym::SymbolId,
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

// Remember: we only describe what is _permissable_,
//   not what is required or what order it must appear in.
// That is the responsibility of parsers lower in the pipeline.
#[test]
fn attrs_any_order_and_optional() {
    attr_parse_stream! {
        type Object = Attr;
        type ValueError = Infallible;

        ValuesState {
            QN_NAME => Attr,
            QN_YIELDS => Attr,

            // No value will be provided for this one,
            //   which is okay since all are implicitly optional.
            QN_INDEX => Attr,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, "val_value".into(), AttrSpan(S2, S3));

    // @yields then @name just to emphasize that order does not matter.
    let toks = vec![
        XirfToken::Attr(attr_yields.clone()),
        XirfToken::Attr(attr_name.clone()),
    ];

    assert_eq!(
        // Simply parses back out the attributes;
        //   see following tests further value parsing.
        // Note that we omit one of the attributes declared above.
        Ok(vec![Object(attr_yields), Object(attr_name),]),
        sut_parse::<ValuesState, _>(toks.into_iter()).collect(),
    );
}

// Since all are optional,
//   the attribute list can be empty.
#[test]
fn attrs_empty() {
    attr_parse_stream! {
        type Object = Attr;
        type ValueError = Infallible;

        ValuesState {
            // We will not provide a value for this.
            QN_NAME => Attr,
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
fn attr_value_into() {
    // Yes, this is like SPair,
    //   but the point of this test is to be useful in isolation,
    //   so please do not couple this with SPair.
    #[derive(Debug, PartialEq, Eq)]
    struct Foo(SymbolId, Span);

    impl From<Attr> for Foo {
        fn from(attr: Attr) -> Self {
            Foo(attr.value(), attr.attr_span().value_span())
        }
    }

    impl parse::Object for Foo {}

    attr_parse_stream! {
        type Object = Foo;
        type ValueError = Infallible;

        ValueIntoState {
            QN_NAME => Foo,
            QN_YIELDS => Foo,
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

    assert_eq!(
        Ok(vec![Object(Foo(val_name, S2)), Object(Foo(val_yields, S3))]),
        sut_parse::<ValueIntoState, _>(toks.into_iter()).collect(),
    );
}

// This test would fail at compile time.
#[test]
fn attr_value_error() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo;

    impl TryFrom<Attr> for Foo {
        type Error = FooError;

        fn try_from(attr: Attr) -> Result<Self, Self::Error> {
            Err(FooError(attr.value()))
        }
    }

    impl parse::Object for Foo {}

    #[derive(Debug, PartialEq)]
    struct FooError(SymbolId);

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

    attr_parse_stream! {
        type Object = Foo;
        type ValueError = FooError;

        ValueTryIntoState {
            QN_NAME => Foo,
            QN_YIELDS => Foo,
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
            FooError(val_name),
            QN_ELE
        )))),
        sut.next(),
    );

    // TryInto on `Option` inner type.
    assert_eq!(
        Some(Err(ParseError::StateError(AttrParseError::InvalidValue(
            FooError(val_yields),
            QN_ELE
        )))),
        sut.next(),
    );
}

#[test]
fn unexpected_attr_with_recovery() {
    attr_parse_stream! {
        type Object = Attr;
        type ValueError = Infallible;

        UnexpectedState {
            QN_NAME => Attr,
            QN_SRC => Attr,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_unexpected = Attr(QN_TYPE, "unexpected".into(), AttrSpan(S1, S2));
    let attr_src = Attr(QN_SRC, "val_src".into(), AttrSpan(S2, S3));

    let toks = vec![
        // This is expected:
        XirfToken::Attr(attr_name.clone()),
        // NOT expected (produce an error):
        XirfToken::Attr(attr_unexpected.clone()),
        // <Recovery must take place here.>
        // This is expected after recovery:
        XirfToken::Attr(attr_src.clone()),
    ];

    let mut sut = Parser::with_state(
        UnexpectedState::with_element(QN_ELE, SE),
        toks.into_iter(),
    );

    assert_eq!(sut.next(), Some(Ok(Object(attr_name))));

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

    assert_eq!(sut.next(), Some(Ok(Object(attr_src))));
}
