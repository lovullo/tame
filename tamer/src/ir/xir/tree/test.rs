// Test XIR tree representation
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
use crate::convert::ExpectInto;
use crate::sym::GlobalSymbolIntern;

type Ix = u16;

lazy_static! {
    static ref S: Span =
        Span::from_byte_interval((0, 0), "test case, 1".intern());
    static ref S2: Span =
        Span::from_byte_interval((0, 0), "test case, 2".intern());
    static ref S3: Span =
        Span::from_byte_interval((0, 0), "test case, 3".intern());
}

mod tree {
    use super::*;

    #[test]
    fn element_from_tree() {
        let ele = Element::<Ix> {
            name: "foo".unwrap_into(),
            attrs: AttrList::new(),
            children: vec![],
            span: (*S, *S2),
        };

        let tree = Tree::Element(ele.clone());

        assert_eq!(Some(ele), tree.element());
    }
}

#[test]
fn empty_element_self_close_from_toks() {
    let name = ("ns", "elem").unwrap_into();

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(name, *S),
        Token::<Ix>::Close(None, *S2),
    ]);

    let expected = Element {
        name,
        attrs: AttrList::new(),
        children: vec![],
        span: (*S, *S2),
    };

    let mut sut = toks.scan(ParserState::new(), parse);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(Tree::Element(expected))))
    );
    assert_eq!(sut.next(), None);
}

// Same as above test, but with balanced closing instead of self
// closing.
#[test]
fn empty_element_balanced_close_from_toks() {
    let name = ("ns", "openclose").unwrap_into();

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(name, *S),
        Token::<Ix>::Close(Some(name), *S2),
    ]);

    let expected = Element {
        name,
        attrs: AttrList::new(),
        children: vec![],
        span: (*S, *S2),
    };

    let mut sut = toks.scan(ParserState::new(), parse);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(Tree::Element(expected))))
    );
    assert_eq!(sut.next(), None);
}

// Unbalanced should result in error.  This does not test what happens
// _after_ the error.
#[test]
fn empty_element_unbalanced_close_from_toks() {
    let open_name = "open".unwrap_into();
    let close_name = "unbalanced_name".unwrap_into();

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(open_name, *S),
        Token::<Ix>::Close(Some(close_name), *S2),
    ]);

    let mut sut = toks.scan(ParserState::new(), parse);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
    assert_eq!(
        sut.next(),
        Some(Err(ParseError::UnbalancedTag {
            open: (open_name, *S),
            close: (close_name, *S2),
        }))
    );

    // TODO: We need to figure out how to best implement recovery before
    // continuing with this design.
}

#[test]
fn empty_element_with_attrs_from_toks() {
    let name = ("ns", "elem").unwrap_into();
    let attr1 = "a".unwrap_into();
    let attr2 = "b".unwrap_into();
    let val1 = AttrValue::Escaped("val1".intern());
    let val2 = AttrValue::Escaped("val2".intern());

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(name, *S),
        Token::AttrName(attr1, *S),
        Token::AttrValue(val1, *S2),
        Token::AttrName(attr2, *S),
        Token::AttrValue(val2, *S2),
        Token::Close(None, *S2),
    ]);

    let expected = Element {
        name,
        attrs: AttrList::from(vec![
            Attr {
                name: attr1,
                value: val1,
                span: (*S, *S2),
            },
            Attr {
                name: attr2,
                value: val2,
                span: (*S, *S2),
            },
        ]),
        children: vec![],
        span: (*S, *S2),
    };

    let mut sut = toks.scan(ParserState::new(), parse);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Open
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrName
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrValue
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrName
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrValue
    assert_eq!(
        sut.next(),
        Some(Ok(Parsed::Object(Tree::Element(expected))))
    );
    assert_eq!(sut.next(), None);
}

#[test]
fn element_with_empty_sibling_children() {
    let parent = "parent".unwrap_into();
    let childa = "childa".unwrap_into();
    let childb = "childb".unwrap_into();

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(parent, *S),
        Token::<Ix>::Open(childa, *S),
        Token::<Ix>::Close(None, *S2),
        Token::<Ix>::Open(childb, *S),
        Token::<Ix>::Close(None, *S2),
        Token::<Ix>::Close(Some(parent), *S2),
    ]);

    let expected = Element {
        name: parent,
        attrs: AttrList::new(),
        children: vec![
            Tree::Element(Element {
                name: childa,
                attrs: AttrList::new(),
                children: vec![],
                span: (*S, *S2),
            }),
            Tree::Element(Element {
                name: childb,
                attrs: AttrList::new(),
                children: vec![],
                span: (*S, *S2),
            }),
        ],
        span: (*S, *S2),
    };

    let mut sut = parser_from(toks);

    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}

// Ensures that attributes do not cause the parent context to be lost.
#[test]
fn element_with_child_with_attributes() {
    let parent = "parent".unwrap_into();
    let child = "child".unwrap_into();
    let attr = "attr".unwrap_into();
    let value = AttrValue::Escaped("attr value".into());

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(parent, *S),
        Token::<Ix>::Open(child, *S),
        Token::<Ix>::AttrName(attr, *S),
        Token::<Ix>::AttrValue(value, *S2),
        Token::<Ix>::Close(None, *S3),
        Token::<Ix>::Close(Some(parent), *S3),
    ]);

    let expected = Element {
        name: parent,
        attrs: AttrList::new(),
        children: vec![Tree::Element(Element {
            name: child,
            attrs: AttrList::from([Attr {
                name: attr,
                value,
                span: (*S, *S2),
            }]),
            children: vec![],
            span: (*S, *S3),
        })],
        span: (*S, *S3),
    };

    let mut sut = parser_from(toks);

    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}

#[test]
fn parser_from_filters_incomplete() {
    let name = ("ns", "elem").unwrap_into();
    let attr = "a".unwrap_into();
    let val = AttrValue::Escaped("val1".intern());

    let toks = std::array::IntoIter::new([
        Token::<Ix>::Open(name, *S),
        Token::AttrName(attr, *S),
        Token::AttrValue(val, *S2),
        Token::Close(None, *S2),
    ]);

    let expected = Element {
        name,
        attrs: AttrList::from([Attr {
            name: attr,
            value: val,
            span: (*S, *S2),
        }]),
        children: vec![],
        span: (*S, *S2),
    };

    let mut sut = parser_from(toks);

    // Unlike the previous tests, we should filter out all the
    // `Parsed::Incomplete` and yield only when we have a fully parsed
    // object.
    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}
