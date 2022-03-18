// Test XIRF representation
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

//! Integration tests for XIRF parser.
//!
//! These tests take place within the context of the XIR parsing framework,
//!   so they are one layer of abstraction away from unit tests.

use super::super::parse::ParseError;
use super::*;
use crate::convert::ExpectInto;
use crate::span::DUMMY_SPAN;
use crate::sym::GlobalSymbolIntern;
use crate::xir::parse::Parsed;

const S: Span = DUMMY_SPAN;
const S2: Span = S.offset_add(1).unwrap();
const S3: Span = S2.offset_add(1).unwrap();
const S4: Span = S3.offset_add(1).unwrap();

#[test]
fn empty_element_self_close() {
    let name = ("ns", "elem").unwrap_into();

    let toks = [Token::Open(name, S), Token::Close(None, S2)].into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(name, S, Depth(0))),
            Parsed::Object(Object::Close(None, S2, Depth(0))),
        ]),
        sut.collect(),
    );
}

// Same as above test, but with balanced closing instead of self
// closing.
#[test]
fn empty_element_balanced_close() {
    let name = ("ns", "openclose").unwrap_into();

    let toks = [Token::Open(name, S), Token::Close(Some(name), S2)].into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(name, S, Depth(0))),
            Parsed::Object(Object::Close(Some(name), S2, Depth(0))),
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
    let name = ("ns", "openclose").unwrap_into();
    let toks = [
        // We need an opening tag to actually begin document parsing.
        Token::Open(name, S),
        Token::Close(Some(name), S2),
        Token::Close(Some(name), S3),
    ]
    .into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Err(ParseError::UnexpectedToken(Token::Close(Some(name), S3),)),
        sut.collect::<Result<Vec<Parsed<Object>>, _>>()
    );
}

// This should never happen, but let's operate in a sane way in case it ever
// does, since that's not the user's fault (that is, we shouldn't have
// gotten to XIRF).
#[test]
fn extra_self_closing_tag() {
    let name = ("ns", "openclose").unwrap_into();
    let toks = [
        // We need an opening tag to actually begin document parsing.
        Token::Open(name, S),
        Token::Close(None, S2),
        Token::Close(None, S3),
    ]
    .into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Err(ParseError::UnexpectedToken(Token::Close(None, S3),)),
        sut.collect::<Result<Vec<Parsed<Object>>, _>>()
    );
}

// Unbalanced should result in error.  This does not test what happens
// _after_ the error.
#[test]
fn empty_element_unbalanced_close() {
    let open_name = "open".unwrap_into();
    let close_name = "unbalanced_name".unwrap_into();

    let toks = [
        Token::Open(open_name, S),
        Token::Close(Some(close_name), S2),
    ]
    .into_iter();

    let mut sut = parse::<1>(toks);

    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(Object::Open(open_name, S, Depth(0)))))
    );
    assert_eq!(
        sut.next(),
        Some(Err(ParseError::StateError(StateError::UnbalancedTag {
            open: (open_name, S),
            close: (close_name, S2),
        })))
    );
}

// Testing depth value.
#[test]
fn single_empty_child() {
    let name = ("ns", "openclose").unwrap_into();
    let child = "child".unwrap_into();

    let toks = [
        Token::Open(name, S),
        Token::Open(child, S2),
        Token::Close(None, S3),
        Token::Close(Some(name), S4),
    ]
    .into_iter();

    let sut = parse::<2>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(name, S, Depth(0))),
            Parsed::Object(Object::Open(child, S2, Depth(1))),
            Parsed::Object(Object::Close(None, S3, Depth(1))),
            Parsed::Object(Object::Close(Some(name), S4, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn depth_exceeded() {
    let name = ("ns", "openclose").unwrap_into();
    let exceed = "exceed".unwrap_into();

    let toks = [
        Token::Open(name, S),
        // This one exceeds the max depth, ...
        Token::Open(exceed, S2),
    ]
    .into_iter();

    // ...which is set here: MAX_DEPTH here is 1
    let mut sut = parse::<1>(toks);

    assert_eq!(
        Some(Ok(Parsed::Object(Object::Open(name, S, Depth(0))))),
        sut.next()
    );
    assert_eq!(
        Some(Err(ParseError::StateError(StateError::MaxDepthExceeded {
            open: (exceed, S2),
            max: Depth(1),
        }))),
        sut.next()
    );
}

#[test]
fn empty_element_with_attrs() {
    let name = ("ns", "elem").unwrap_into();
    let attr1 = "a".unwrap_into();
    let attr2 = "b".unwrap_into();
    let val1 = "val1".intern();
    let val2 = "val2".intern();

    let toks = [
        Token::Open(name, S),
        Token::AttrName(attr1, S2),
        Token::AttrValue(val1, S3),
        Token::AttrName(attr2, S3),
        Token::AttrValue(val2, S4),
        Token::Close(None, S4),
    ]
    .into_iter();

    let sut = parse::<2>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(name, S, Depth(0))),
            Parsed::Incomplete,
            Parsed::Object(Object::Attr(Attr::new(attr1, val1, (S2, S3)))),
            Parsed::Incomplete,
            Parsed::Object(Object::Attr(Attr::new(attr2, val2, (S3, S4)))),
            Parsed::Object(Object::Close(None, S4, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn child_element_after_attrs() {
    let name = ("ns", "elem").unwrap_into();
    let child = "child".unwrap_into();
    let attr = "a".unwrap_into();
    let val = "val".intern();

    let toks = [
        Token::Open(name, S),
        Token::AttrName(attr, S),
        Token::AttrValue(val, S2),
        Token::Open(child, S),
        Token::Close(None, S2),
        Token::Close(Some(name), S3),
    ]
    .into_iter();

    let sut = parse::<2>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(name, S, Depth(0))),
            Parsed::Incomplete,
            Parsed::Object(Object::Attr(Attr::new(attr, val, (S, S2)))),
            Parsed::Object(Object::Open(child, S, Depth(1))),
            Parsed::Object(Object::Close(None, S2, Depth(1))),
            Parsed::Object(Object::Close(Some(name), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn element_with_empty_sibling_children() {
    let parent = "parent".unwrap_into();
    let childa = "childa".unwrap_into();
    let childb = "childb".unwrap_into();

    let toks = [
        Token::Open(parent, S),
        Token::Open(childa, S2),
        Token::Close(None, S3),
        Token::Open(childb, S2),
        Token::Close(None, S3),
        Token::Close(Some(parent), S2),
    ]
    .into_iter();

    let sut = parse::<2>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(parent, S, Depth(0))),
            Parsed::Object(Object::Open(childa, S2, Depth(1))),
            Parsed::Object(Object::Close(None, S3, Depth(1))),
            Parsed::Object(Object::Open(childb, S2, Depth(1))),
            Parsed::Object(Object::Close(None, S3, Depth(1))),
            Parsed::Object(Object::Close(Some(parent), S2, Depth(0))),
        ]),
        sut.collect(),
    );
}

// Ensures that attributes do not cause the parent context to be lost.
#[test]
fn element_with_child_with_attributes() {
    let parent = "parent".unwrap_into();
    let child = "child".unwrap_into();
    let attr = "attr".unwrap_into();
    let value = "attr value".intern();

    let toks = [
        Token::Open(parent, S),
        Token::Open(child, S),
        Token::AttrName(attr, S),
        Token::AttrValue(value, S2),
        Token::Close(None, S3),
        Token::Close(Some(parent), S3),
    ]
    .into_iter();

    let sut = parse::<2>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(parent, S, Depth(0))),
            Parsed::Object(Object::Open(child, S, Depth(1))),
            Parsed::Incomplete,
            Parsed::Object(Object::Attr(Attr::new(attr, value, (S, S2)))),
            Parsed::Object(Object::Close(None, S3, Depth(1))),
            Parsed::Object(Object::Close(Some(parent), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn element_with_text() {
    let parent = "parent".unwrap_into();
    let text = "inner text".into();

    let toks = [
        Token::Open(parent, S),
        Token::Text(text, S2),
        Token::Close(Some(parent), S3),
    ]
    .into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Open(parent, S, Depth(0))),
            Parsed::Object(Object::Text(text, S2)),
            Parsed::Object(Object::Close(Some(parent), S3, Depth(0))),
        ]),
        sut.collect(),
    );
}

#[test]
fn not_accepting_state_if_element_open() {
    let name = "unclosed".unwrap_into();
    let toks = [Token::Open(name, S)].into_iter();

    let mut sut = parse::<1>(toks);

    assert_eq!(
        Some(Ok(Parsed::Object(Object::Open(name, S, Depth(0))))),
        sut.next()
    );

    // Element was not closed.
    assert_eq!(Some(Err(ParseError::UnexpectedEof(Some(S)))), sut.next());
}

// XML permits comment nodes before and after the document root element.
#[test]
fn comment_before_or_after_root_ok() {
    let name = "root".unwrap_into();
    let cstart = "start comment".intern();
    let cend = "end comment".intern();

    let toks = [
        Token::Comment(cstart, S),
        Token::Open(name, S2),
        Token::Close(None, S3),
        Token::Comment(cend, S4),
    ]
    .into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Object(Object::Comment(cstart, S)),
            Parsed::Object(Object::Open(name, S2, Depth(0))),
            Parsed::Object(Object::Close(None, S3, Depth(0))),
            Parsed::Object(Object::Comment(cend, S4)),
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
        Token::Open(name, S),
        Token::Close(None, S2),
        // Document ends here
        Token::Open(name, S3),
    ]
    .into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Result::<Vec<Parsed<Object>>, _>::Err(ParseError::UnexpectedToken(
            Token::Open(name, S3)
        )),
        sut.collect()
    );
}

// Non-comment nodes cannot appear before the opening root tag.
#[test]
fn content_before_root_open_error() {
    let text = "foo".intern();

    let toks = [Token::Text(text, S)].into_iter();

    let sut = parse::<1>(toks);

    assert_eq!(
        Result::<Vec<Parsed<Object>>, _>::Err(ParseError::StateError(
            StateError::RootOpenExpected(Token::Text(text, S))
        )),
        sut.collect()
    );
}
