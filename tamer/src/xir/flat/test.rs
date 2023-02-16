// Test XIRF representation
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

//! Integration tests for XIRF parser.
//!
//! These tests take place within the context of the XIR parsing framework,
//!   so they are one layer of abstraction away from unit tests.

use std::assert_matches::assert_matches;

use super::*;
use crate::convert::ExpectInto;
use crate::parse::{FinalizeError, ParseError, Parsed};
use crate::span::dummy::*;
use crate::sym::GlobalSymbolIntern;
use crate::xir::test::{
    close as xir_close, close_empty as xir_close_empty, open as xir_open,
};
use std::fmt::Debug;

/// Hastily and lazily produce a [`XirfToken::Open`].
///
/// This function is not suitable for production use as it does not produce
///   a complete [`OpenSpan`].
pub fn open<Q: TryInto<QName>, S: Into<OpenSpan>, T: TextType>(
    qname: Q,
    span: S,
    depth: Depth,
) -> XirfToken<T>
where
    <Q as TryInto<QName>>::Error: Debug,
{
    XirfToken::Open(qname.unwrap_into(), span.into(), depth)
}

/// Hastily and lazily produce a [`XirfToken::Close`] for an empty tag.
///
/// This is [`close`] with the omission of the `qname` argument;
///   the type parameter `Q` cannot be inferred if the value is [`None`].
///
/// This function is not suitable for production use as it does not produce
///   a complete [`OpenSpan`].
pub fn close_empty<S: Into<CloseSpan>, T: TextType>(
    span: S,
    depth: Depth,
) -> XirfToken<T> {
    XirfToken::Close(None, span.into(), depth)
}

/// Hastily and lazily produce a [`XirfToken::Close`].
///
/// See also [`close_empty`] if `Q` cannot be inferred.
///
/// This function is not suitable for production use as it does not produce
///   a complete [`OpenSpan`].
pub fn close<Q: TryInto<QName>, S: Into<CloseSpan>, T: TextType>(
    qname: Option<Q>,
    span: S,
    depth: Depth,
) -> XirfToken<T>
where
    <Q as TryInto<QName>>::Error: Debug,
{
    XirfToken::Close(qname.map(ExpectInto::unwrap_into), span.into(), depth)
}

#[test]
fn empty_element_self_close() {
    let name = ("ns", "elem");

    let toks = [xir_open(name, S1), xir_close_empty(S2)].into_iter();

    let sut = parse::<1, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(name, S1, Depth(0))),
            Parsed::Object(close_empty(S2, Depth(0))),
        ]),
        sut.collect(),
    );
}

// Same as above test, but with balanced closing instead of self
// closing.
#[test]
fn empty_element_balanced_close() {
    let name = ("ns", "openclose");

    let toks = [xir_open(name, S1), xir_close(Some(name), S2)].into_iter();

    let sut = parse::<1, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(name, S1, Depth(0))),
            Parsed::Object(close(Some(name), S2, Depth(0))),
        ]),
        sut.collect(),
    );
}

// More closing tags than opening.
//
// We cannot keep the token and throw our own error because this tag may be
//   part of a parent context.
#[test]
fn extra_closing_tag() {
    let name = ("ns", "openclose");
    let toks = [
        // We need an opening tag to actually begin document parsing.
        xir_open(name, S1),
        xir_close(Some(name), S2),
        xir_close(Some(name), S3),
    ]
    .into_iter();

    let sut = parse::<1, Text>(toks);

    assert_matches!(
        sut.collect::<Result<Vec<Parsed<_>>, _>>(),
        Err(ParseError::UnexpectedToken(
            XirToken::Close(Some(given_name), given_span),
            _
        )) if given_name == name.unwrap_into() && given_span == S3.into()
    );
}

// This should never happen, but let's operate in a sane way in case it ever
// does, since that's not the user's fault (that is, we shouldn't have
// gotten to XIRF).
#[test]
fn extra_self_closing_tag() {
    let name = ("ns", "openclose");
    let toks = [
        // We need an opening tag to actually begin document parsing.
        xir_open(name, S1),
        xir_close_empty(S2),
        xir_close_empty(S3),
    ]
    .into_iter();

    let sut = parse::<1, Text>(toks);

    assert_matches!(
        sut.collect::<Result<Vec<Parsed<_>>, _>>(),
        Err(ParseError::UnexpectedToken(XirToken::Close(None, given_span), _))
            if given_span == S3.into(),
    );
}

// Unbalanced should result in error.  This does not test what happens
// _after_ the error.
#[test]
fn empty_element_unbalanced_close() {
    let open_name = "open".unwrap_into();
    let close_name = "unbalanced_name".unwrap_into();

    let toks =
        [xir_open(open_name, S1), xir_close(Some(close_name), S2)].into_iter();

    let mut sut = parse::<1, Text>(toks);

    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(open(open_name, S1, Depth(0)))))
    );
    assert_eq!(
        sut.next(),
        Some(Err(ParseError::StateError(XirToXirfError::UnbalancedTag {
            open: (open_name, S1),
            close: (close_name, S2),
        })))
    );
}

// Testing depth value.
#[test]
fn single_empty_child() {
    let name = ("ns", "openclose");
    let child = "child";

    let toks = [
        xir_open(name, S1),
        xir_open(child, S2),
        xir_close_empty(S3),
        xir_close(Some(name), S4),
    ]
    .into_iter();

    let sut = parse::<2, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(name, S1, Depth(0))),
            Parsed::Object(open(child, S2, Depth(1))),
            Parsed::Object(close_empty(S3, Depth(1))),
            Parsed::Object(close(Some(name), S4, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn depth_exceeded() {
    let name = ("ns", "openclose");
    let exceed = "exceed".unwrap_into();

    let toks = [
        xir_open(name, S1),
        // This one exceeds the max depth, ...
        xir_open(exceed, S2),
    ]
    .into_iter();

    // ...which is set here: MAX_DEPTH here is 1
    let mut sut = parse::<1, Text>(toks);

    assert_eq!(
        Some(Ok(Parsed::Object(open(name, S1, Depth(0))))),
        sut.next()
    );
    assert_eq!(
        Some(Err(ParseError::StateError(
            XirToXirfError::MaxDepthExceeded {
                open: (exceed, S2),
                max: Depth(1),
            }
        ))),
        sut.next()
    );
}

#[test]
fn empty_element_with_attrs() {
    let name = ("ns", "elem");
    let attr1 = "a".unwrap_into();
    let attr2 = "b".unwrap_into();
    let val1 = "val1".intern();
    let val2 = "val2".intern();

    let toks = [
        xir_open(name, S1),
        XirToken::AttrName(attr1, S2),
        XirToken::AttrValue(val1, S3),
        XirToken::AttrName(attr2, S3),
        XirToken::AttrValue(val2, S4),
        xir_close_empty(S4),
    ]
    .into_iter();

    let sut = parse::<2, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(name, S1, Depth(0))),
            Parsed::Incomplete,
            Parsed::Object(XirfToken::Attr(Attr::new(attr1, val1, (S2, S3)))),
            Parsed::Incomplete,
            Parsed::Object(XirfToken::Attr(Attr::new(attr2, val2, (S3, S4)))),
            Parsed::Object(close_empty(S4, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn child_element_after_attrs() {
    let name = ("ns", "elem");
    let child = "child";
    let attr = "a".unwrap_into();
    let val = "val".intern();

    let toks = [
        xir_open(name, S1),
        XirToken::AttrName(attr, S1),
        XirToken::AttrValue(val, S2),
        xir_open(child, S1),
        xir_close_empty(S2),
        xir_close(Some(name), S3),
    ]
    .into_iter();

    let sut = parse::<2, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(name, S1, Depth(0))),
            Parsed::Incomplete,
            Parsed::Object(XirfToken::Attr(Attr::new(attr, val, (S1, S2)))),
            Parsed::Object(open(child, S1, Depth(1))),
            Parsed::Object(close_empty(S2, Depth(1))),
            Parsed::Object(close(Some(name), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn element_with_empty_sibling_children() {
    let parent = "parent";
    let childa = "childa";
    let childb = "childb";

    let toks = [
        xir_open(parent, S1),
        xir_open(childa, S2),
        xir_close_empty(S3),
        xir_open(childb, S2),
        xir_close_empty(S3),
        xir_close(Some(parent), S2),
    ]
    .into_iter();

    let sut = parse::<2, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(parent, S1, Depth(0))),
            Parsed::Object(open(childa, S2, Depth(1))),
            Parsed::Object(close_empty(S3, Depth(1))),
            Parsed::Object(open(childb, S2, Depth(1))),
            Parsed::Object(close_empty(S3, Depth(1))),
            Parsed::Object(close(Some(parent), S2, Depth(0))),
        ]),
        sut.collect(),
    );
}

// Ensures that attributes do not cause the parent context to be lost.
#[test]
fn element_with_child_with_attributes() {
    let parent = "parent";
    let child = "child";
    let attr = "attr".unwrap_into();
    let value = "attr value".intern();

    let toks = [
        xir_open(parent, S1),
        xir_open(child, S1),
        XirToken::AttrName(attr, S1),
        XirToken::AttrValue(value, S2),
        xir_close_empty(S3),
        xir_close(Some(parent), S3),
    ]
    .into_iter();

    let sut = parse::<2, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(parent, S1, Depth(0))),
            Parsed::Object(open(child, S1, Depth(1))),
            Parsed::Incomplete,
            Parsed::Object(XirfToken::Attr(Attr::new(attr, value, (S1, S2)))),
            Parsed::Object(close_empty(S3, Depth(1))),
            Parsed::Object(close(Some(parent), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn element_with_text() {
    let parent = "parent";
    let text = "inner text".into();

    let toks = [
        xir_open(parent, S1),
        XirToken::Text(text, S2),
        xir_close(Some(parent), S3),
    ]
    .into_iter();

    let sut = parse::<1, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(open(parent, S1, Depth(0))),
            Parsed::Object(XirfToken::Text(Text(text, S2), Depth(1))),
            Parsed::Object(close(Some(parent), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn not_accepting_state_if_element_open() {
    let name = "unclosed";
    let toks = [xir_open(name, S1)].into_iter();

    let mut sut = parse::<1, Text>(toks);

    assert_eq!(
        Some(Ok(Parsed::Object(open(name, S1, Depth(0))))),
        sut.next()
    );

    // Element was not closed.
    assert_matches!(
        sut.next(),
        Some(Err(ParseError::FinalizeError(
            FinalizeError::UnexpectedEof(..)
        )))
    );
}

// XML permits comment nodes before and after the document root element.
#[test]
fn comment_before_or_after_root_ok() {
    let name = "root";
    let cstart = "start comment".intern();
    let cend = "end comment".intern();

    let toks = [
        XirToken::Comment(cstart, S1),
        xir_open(name, S2),
        xir_close_empty(S3),
        XirToken::Comment(cend, S4),
    ]
    .into_iter();

    let sut = parse::<1, Text>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(XirfToken::Comment(cstart, S1, Depth(0))),
            Parsed::Object(open(name, S2, Depth(0))),
            Parsed::Object(close_empty(S3, Depth(0))),
            Parsed::Object(XirfToken::Comment(cend, S4, Depth(0))),
        ]),
        sut.collect(),
    );
}

// Similar to above,
//   but with whitespace.
#[test]
fn whitespace_before_or_after_root_ok() {
    let name = "root";
    let ws = "  ".unwrap_into();

    let toks = [
        XirToken::Text(ws, S1),
        xir_open(name, S2),
        xir_close_empty(S3),
        XirToken::Text(ws, S4),
    ]
    .into_iter();

    let sut = parse::<1, RefinedText>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,
            Parsed::Object(open(name, S2, Depth(0))),
            Parsed::Object(close_empty(S3, Depth(0))),
            Parsed::Incomplete,
        ]),
        sut.collect(),
    );
}

// But there must be no content at the end of the document after the closing
//   root node.
// This does not test every applicable token;
//   you can easily verify the actual implementation at a glance.
//
// This is just a dead parser state,
//   since it's possible for XIRF to be composed and we want to return to
//   the parent parser.
#[test]
fn content_after_root_close_error() {
    let name = "root".unwrap_into();

    let toks = [
        xir_open(name, S1),
        xir_close_empty(S2),
        // Document ends here
        xir_open(name, S3),
    ]
    .into_iter();

    let sut = parse::<1, Text>(toks);

    assert_matches!(
        sut.collect(),
        Result::<Vec<Parsed<_>>, _>::Err(ParseError::UnexpectedToken(
            XirToken::Open(given_name, given_span),
        _)) if given_name == name && given_span == S3.into()
    );
}

// Non-comment nodes cannot appear before the opening root tag.
#[test]
fn content_before_root_open_error() {
    let text = "foo".intern();

    let toks = [XirToken::Text(text, S1)].into_iter();

    let sut = parse::<1, Text>(toks);

    assert_eq!(
        Result::<Vec<Parsed<_>>, _>::Err(ParseError::StateError(
            XirToXirfError::RootOpenExpected(XirToken::Text(text, S1))
        )),
        sut.collect()
    );
}

#[test]
fn whitespace_refinement() {
    // Nothing exhaustive;
    //   just check some notable examples.
    vec![
        ("".into(), true),
        (" ".into(), true),
        ("\n".into(), true),
        ("\n\n\t    ".into(), true),
        ("   foo   ".into(), false),
        ("\n         .".into(), false),
        (".\n         ".into(), false),
    ]
    .into_iter()
    .for_each(|(given, expected)| {
        let mut sut = parse::<1, RefinedText>(
            vec![xir_open("root", S1), XirToken::Text(given, S1)].into_iter(),
        );

        let _ = sut.next(); // discard root

        match sut.next().unwrap().unwrap() {
            Parsed::Object(XirfToken::Text(
                RefinedText::Whitespace(Whitespace(Text(ws, span))),
                Depth(1),
            )) => {
                assert_eq!(ws, given);
                assert_eq!(span, S1);
                assert!(expected == true)
            }

            Parsed::Object(XirfToken::Text(
                RefinedText::Unrefined(Text(text, span)),
                Depth(1),
            )) => {
                assert_eq!(text, given);
                assert_eq!(span, S1);
                assert!(expected == false)
            }

            unexpected => panic!("unexpected token: {unexpected:?}"),
        }
    });
}

// Basic sanity check;
//   the implementation is simple enough to verify almost at a glance,
//     but the attribute deconstruction with lookahead could be missed so
//     it's worth just testing an example.
#[test]
fn xirf_to_xir() {
    use crate::parse::Lower;

    let xir_toks = vec![
        XirToken::Open("a".unwrap_into(), S1.into()),
        XirToken::AttrName("attr".unwrap_into(), S2),
        XirToken::AttrValue("value".into(), S3),
        XirToken::Comment("comment".into(), S4),
        XirToken::Text("text".into(), S5),
        XirToken::CData("cdata".into(), S6),
        XirToken::Close(Some("a".unwrap_into()), S7.into()),
    ];

    // This type incantation
    //   (a) is a sorry mess because at the time of writing the lowering
    //         pipeline is still in need of further abstraction; and
    //   (b) simply parses XIR -> XirToXirf -> XirfToXir -> XIR and asserts
    //         that the result is the same as what was originally provided.
    //
    // It really does make sense if you approach it slowly and offer it food.
    assert_eq!(
        Ok(xir_toks.clone().into_iter().map(Parsed::Object).collect()),
        Lower::<XirToXirf<1, Text>, XirfToXir<Text>, _>::lower(
            &mut parse::<1, Text>(xir_toks.into_iter()),
            |out| out
                .filter(|x| !matches!(x, Ok(Parsed::Incomplete)))
                .collect::<Result<Vec<_>, _>>()
        )
    );

    // The lowering pipeline above requires compatible errors.
    impl From<ParseError<XirfToken<Text>, Infallible>>
        for ParseError<XirToken, XirToXirfError>
    {
        fn from(_value: ParseError<XirfToken<Text>, Infallible>) -> Self {
            unreachable!()
        }
    }
}
