// Test XIR tree representation
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

use std::assert_matches::assert_matches;

use super::*;
use crate::convert::ExpectInto;
use crate::parse::ParseError;
use crate::span::dummy::*;
use crate::sym::GlobalSymbolIntern;
use crate::xir::test::{close, close_empty, open};

mod tree {
    use super::*;

    #[test]
    fn element_from_tree() {
        let ele = Element {
            name: "foo".unwrap_into(),
            attrs: AttrList::new(),
            children: vec![],
            span: (S1, S2),
        };

        let tree = Tree::Element(ele.clone());

        assert_eq!(Some(&ele), tree.as_element());
        assert_eq!(None, Into::<Option<SymbolId>>::into(tree));
    }

    #[test]
    fn text_from_tree() {
        let text = "foo".intern();
        let tree = Tree::Text(text, S1);

        assert!(!tree.is_element());
        assert_eq!(None, tree.as_element());
        assert_eq!(None, tree.clone().into_element());

        assert_eq!(Some(text), tree.into());
    }
}

mod attrs {
    use super::*;

    #[test]
    fn linear_search_for_attr_name_in_list() {
        let a = "a".unwrap_into();
        let b = "b".unwrap_into();

        let attra = Attr::new(a, "a value".intern(), (S1, S2));
        let attrb = Attr::new(b, "b value".intern(), (S1, S2));

        let attrs = AttrList::from([attra.clone(), attrb.clone()]);

        assert_eq!(attrs.find(a), Some(&attra));
        assert_eq!(attrs.find(b), Some(&attrb));

        assert_eq!(attrs.find("unknown".unwrap_into()), None);
    }
}

#[test]
fn empty_element_self_close_from_toks() {
    let name = ("ns", "elem").unwrap_into();

    let toks = [open(name, S1), close_empty(S2)].into_iter();

    let expected = Element {
        name,
        attrs: AttrList::new(),
        children: vec![],
        span: (S1, S2),
    };

    let mut sut = parse(toks);

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

    let toks = [open(name, S1), close(Some(name), S2)].into_iter();

    let expected = Element {
        name,
        attrs: AttrList::new(),
        children: vec![],
        span: (S1, S2),
    };

    let mut sut = parse(toks);

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

    let toks = [open(open_name, S1), close(Some(close_name), S2)].into_iter();

    let mut sut = parse(toks);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
    assert_eq!(
        sut.next(),
        Some(Err(ParseError::StateError(StackError::UnbalancedTag {
            open: (open_name, S1),
            close: (close_name, S2),
        })))
    );

    // TODO: We need to figure out how to best implement recovery before
    // continuing with this design.
}

#[test]
fn empty_element_with_attrs_from_toks() {
    let name = ("ns", "elem").unwrap_into();
    let attr1 = "a".unwrap_into();
    let attr2 = "b".unwrap_into();
    let val1 = "val1".intern();
    let val2 = "val2".intern();

    let toks = [
        open(name, S1),
        Token::AttrName(attr1, S1),
        Token::AttrValue(val1, S2),
        Token::AttrName(attr2, S1),
        Token::AttrValue(val2, S3),
        close_empty(S2),
    ]
    .into_iter();

    let expected = Element {
        name,
        attrs: AttrList::from(vec![
            Attr::new(attr1, val1, (S1, S2)),
            Attr::new(attr2, val2, (S1, S3)),
        ]),
        children: vec![],
        span: (S1, S2),
    };

    let mut sut = parse(toks);

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
fn child_element_after_attrs() {
    let name = ("ns", "elem").unwrap_into();
    let child = "child".unwrap_into();
    let attr = "a".unwrap_into();
    let val = "val".intern();

    let toks = [
        open(name, S1),
        Token::AttrName(attr, S1),
        Token::AttrValue(val, S2),
        open(child, S1),
        close_empty(S2),
        close(Some(name), S3),
    ]
    .into_iter();

    let expected = Element {
        name,
        attrs: AttrList::from(vec![Attr::new(attr, val, (S1, S2))]),
        children: vec![Tree::Element(Element {
            name: child,
            attrs: AttrList::new(),
            children: vec![],
            span: (S1, S2),
        })],
        span: (S1, S3),
    };

    let mut sut = parse(toks);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Open
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrName
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrValue
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Open
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Close
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

    let toks = [
        open(parent, S1),
        open(childa, S1),
        close_empty(S2),
        open(childb, S1),
        close_empty(S2),
        close(Some(parent), S2),
    ]
    .into_iter();

    let expected = Element {
        name: parent,
        attrs: AttrList::new(),
        children: vec![
            Tree::Element(Element {
                name: childa,
                attrs: AttrList::new(),
                children: vec![],
                span: (S1, S2),
            }),
            Tree::Element(Element {
                name: childb,
                attrs: AttrList::new(),
                children: vec![],
                span: (S1, S2),
            }),
        ],
        span: (S1, S2),
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
    let value = "attr value".intern();

    let toks = [
        open(parent, S1),
        open(child, S1),
        Token::AttrName(attr, S1),
        Token::AttrValue(value, S2),
        close_empty(S3),
        close(Some(parent), S3),
    ]
    .into_iter();

    let expected = Element {
        name: parent,
        attrs: AttrList::new(),
        children: vec![Tree::Element(Element {
            name: child,
            attrs: AttrList::from([Attr::new(attr, value, (S1, S2))]),
            children: vec![],
            span: (S1, S3),
        })],
        span: (S1, S3),
    };

    let mut sut = parser_from(toks);

    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}

#[test]
fn element_with_text() {
    let parent = "parent".unwrap_into();
    let text = "inner text".into();

    let toks = [
        open(parent, S1),
        Token::Text(text, S2),
        close(Some(parent), S3),
    ]
    .into_iter();

    let expected = Element {
        name: parent,
        attrs: AttrList::new(),
        children: vec![Tree::Text(text, S2)],
        span: (S1, S3),
    };

    let mut sut = parser_from(toks);

    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}

#[test]
fn parser_from_filters_incomplete() {
    let name = ("ns", "elem").unwrap_into();
    let attr = "a".unwrap_into();
    let val = "val1".intern();

    let toks = [
        open(name, S1),
        Token::AttrName(attr, S1),
        Token::AttrValue(val, S2),
        close_empty(S2),
    ]
    .into_iter();

    let expected = Element {
        name,
        attrs: AttrList::from([Attr::new(attr, val, (S1, S2))]),
        children: vec![],
        span: (S1, S2),
    };

    let mut sut = parser_from(toks);

    // Unlike the previous tests, we should filter out all the
    // `Parsed::Incomplete` and yield only when we have a fully parsed
    // object.
    assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
    assert_eq!(sut.next(), None);
}

#[test]
fn attr_parser_with_non_attr_token() {
    let name = "unexpected".unwrap_into();
    let mut toks = [open(name, S1)].into_iter();

    let mut sut = attr_parser_from(&mut toks);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::UnexpectedToken(Token::Open(given_name, given_span), _)))
            if given_name == name && given_span == S1.into()
    );
}

#[test]
fn parser_attr_multiple() {
    let attr1 = "one".unwrap_into();
    let attr2 = "two".unwrap_into();
    let val1 = "val1".intern();
    let val2 = "val2".intern();

    let mut toks = [
        Token::AttrName(attr1, S1),
        Token::AttrValue(val1, S2),
        Token::AttrName(attr2, S2),
        Token::AttrValue(val2, S3),
        // Token that we should _not_ hit.
        Token::Text("nohit".into(), S1),
    ]
    .into_iter();

    let mut sut = attr_parser_from(&mut toks);

    assert_eq!(sut.next(), Some(Ok(Attr::new(attr1, val1, (S1, S2)))));
    assert_eq!(sut.next(), Some(Ok(Attr::new(attr2, val2, (S2, S3)))));

    // Parsing must stop after the last attribute,
    //   after which some other parser can continue on the same token
    //   stream
    //     (using this token as a lookahead).
    assert_matches!(
        sut.next(),
        Some(Err(ParseError::UnexpectedToken(Token::Text(
            given_name,
            given_span,
        ), _))) if given_name == "nohit".into() && given_span == S1
    );
}
