// XIR reader tests
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
use crate::{
    convert::ExpectInto,
    ir::xir::{AttrValue, Text, Token},
    span::DUMMY_SPAN,
};

/// These tests use [`quick_xml`] directly,
///   rather than mocking it,
///   because parsing XML isn't a simple matter and we want to be sure that
///     our assumptions of how `quick_xml` performs its parsing is accurate.
/// Consequently,
///   these act more like integration tests than unit tests.
///
/// This means that `quick_xml` breakages will break these tests,
///   and that is (unlike with unit tests) exactly what we want to happen
///   here;
///     we _complement_ the behavior of quick-xml,
///       both by reimplementing certain functionality
///         (like namespace management)
///         and by relying on certain parsing behavior to eliminate
///           redundant checks.

type Sut<B> = XmlXirReader<B>;

/// A byte that will be invalid provided that there is either no following
///   UTF-8 byte,
///     or if it's followed by another byte that is invalid in that
///     position.
const INVALID_UTF8_BYTE: u8 = 0b11000000u8;

// SAFETY: We want an invalid UTF-8 str for tests.
//   (We can use raw bytes and avoid `unsafe`,
//     but this is more convenient.)
const INVALID_STR: &str =
    unsafe { std::str::from_utf8_unchecked(&[INVALID_UTF8_BYTE]) };

#[test]
fn empty_node_without_prefix_or_attributes() {
    let sut = Sut::new("<empty-node />".as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("empty-node".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
        ],
    );
}

// Resolving namespaces is not the concern of XIR.
#[test]
fn does_not_resolve_xmlns() {
    let sut = Sut::new(r#"<no-ns xmlns="noresolve" />"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("no-ns".unwrap_into(), DUMMY_SPAN),
            // Since we didn't parse @xmlns, it's still an attribute.
            Token::AttrName("xmlns".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(
                AttrValue::Escaped("noresolve".into()),
                DUMMY_SPAN
            ),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
        ],
    );
}

// Resolving namespaces is not the concern of XIR.
#[test]
fn empty_node_with_prefix_without_attributes_unresolved() {
    let sut = Sut::new(r#"<x:empty-node xmlns:x="noresolve" />"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    // Should be the QName, _unresolved_.
    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open(("x", "empty-node").unwrap_into(), DUMMY_SPAN),
            Token::AttrName(("xmlns", "x").unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(
                AttrValue::Escaped("noresolve".into()),
                DUMMY_SPAN
            ),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
        ],
    );
}

// TODO: Enough information for error recovery and reporting.
#[test]
fn prefix_with_empty_local_name_invalid_qname() {
    // No local name (trailing colon).
    let sut = Sut::new(r#"<x: xmlns:x="testns" />"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(given) => {
            assert_eq!(Error::InvalidQName("x:".into()), given);
        }
    }
}

// The order of attributes must be retained.
#[test]
fn multiple_attrs_ordered() {
    let sut = Sut::new(r#"<ele foo="a" bar="b" b:baz="c" />"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("ele".unwrap_into(), DUMMY_SPAN),
            Token::AttrName("foo".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("a".into()), DUMMY_SPAN),
            Token::AttrName("bar".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("b".into()), DUMMY_SPAN),
            Token::AttrName(("b", "baz").unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("c".into()), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
        ],
    );
}

// Contrary to the specification, but this is the responsibility of XIRT; we
// need to allow it to support e.g. recovery, code formatting, and LSPs.
#[test]
fn permits_duplicate_attrs() {
    let sut = Sut::new(r#"<dup attr="a" attr="b" />"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("dup".unwrap_into(), DUMMY_SPAN),
            Token::AttrName("attr".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("a".into()), DUMMY_SPAN),
            Token::AttrName("attr".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("b".into()), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
        ],
    );
}

#[test]
fn child_node_self_closing() {
    let sut = Sut::new(r#"<root><child /></root>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Close(Some("root".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn sibling_nodes() {
    let sut = Sut::new(r#"<root><child /><child /></root>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Close(Some("root".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn child_node_with_attrs() {
    let sut = Sut::new(r#"<root><child foo="bar" /></root>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrName("foo".unwrap_into(), DUMMY_SPAN),
            Token::AttrValue(AttrValue::Escaped("bar".into()), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Close(Some("root".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn child_text() {
    let sut = Sut::new(r#"<text>foo bar</text>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("text".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Text(Text::Escaped("foo bar".into()), DUMMY_SPAN),
            Token::Close(Some("text".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn mixed_child_content() {
    let sut = Sut::new(r#"<text>foo<em>bar</em></text>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("text".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Text(Text::Escaped("foo".into()), DUMMY_SPAN),
            Token::Open("em".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Text(Text::Escaped("bar".into()), DUMMY_SPAN),
            Token::Close(Some("em".unwrap_into()), DUMMY_SPAN),
            Token::Close(Some("text".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

// This is how XML is typically written; people don't perceive it as mixed,
// even though it is.  This intentionally adds newlines before and after the
// opening and closing tags of the root node.
#[test]
fn mixed_child_content_with_newlines() {
    let sut = Sut::new(
        r#"
<root>
  <child />
</root>
"#
        .as_bytes(),
    );

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Text(Text::Escaped("\n".into()), DUMMY_SPAN),
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Text(Text::Escaped("\n  ".into()), DUMMY_SPAN),
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Text(Text::Escaped("\n".into()), DUMMY_SPAN),
            Token::Close(Some("root".unwrap_into()), DUMMY_SPAN),
            Token::Text(Text::Escaped("\n".into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn child_cdata() {
    let sut = Sut::new(r#"<cd><![CDATA[<foo />]]></cd>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("cd".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            // Escaped by quick_xml.
            Token::Text(Text::Escaped("&lt;foo /&gt;".into()), DUMMY_SPAN),
            Token::Close(Some("cd".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn mixed_child_text_and_cdata() {
    let sut = Sut::new(r#"<cd>foo<bar/><![CDATA[<baz/>]]></cd>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("cd".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Text(Text::Escaped("foo".into()), DUMMY_SPAN),
            Token::Open("bar".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            // Escaped by quick_xml.
            Token::Text(Text::Escaped("&lt;baz/&gt;".into()), DUMMY_SPAN),
            Token::Close(Some("cd".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn comment() {
    let sut = Sut::new(r#"<!--root--><root><!--<child>--></root>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Comment(Text::Unescaped("root".into()), DUMMY_SPAN),
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Comment(Text::Unescaped("<child>".into()), DUMMY_SPAN),
            Token::Close(Some("root".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

#[test]
fn comment_multiline() {
    let sut = Sut::new(
        r#"<mult><!--comment
on multiple
lines-->
</mult>"#
            .as_bytes(),
    );

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("mult".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Comment(
                Text::Unescaped("comment\non multiple\nlines".into()),
                DUMMY_SPAN
            ),
            Token::Text(Text::Escaped("\n".into()), DUMMY_SPAN),
            Token::Close(Some("mult".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

// XIRT handles mismatch errors; XIR must explicitly support them.
#[test]
fn permits_mismatched_tags() {
    let sut = Sut::new(r#"<root><child /></mismatch>"#.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    assert_eq!(
        result.expect("parsing failed"),
        vec![
            Token::Open("root".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Open("child".unwrap_into(), DUMMY_SPAN),
            Token::AttrEnd,
            Token::Close(None, DUMMY_SPAN),
            Token::Close(Some("mismatch".unwrap_into()), DUMMY_SPAN),
        ],
    );
}

// TODO: Enough information for error recovery and reporting.
#[test]
fn node_name_invalid_utf8() {
    let bytes: &[u8] = &[b'<', INVALID_UTF8_BYTE, b'/', b'>'];
    let sut = Sut::new(bytes);

    let result = sut.collect::<Result<Vec<_>>>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(Error::InvalidUtf8(_, bytes)) => {
            assert_eq!(bytes, &[INVALID_UTF8_BYTE]);
        }
        _ => panic!("unexpected failure"),
    }
}

// TODO: Enough information for error recovery and reporting.
#[test]
fn attr_name_invalid_utf8() {
    let mut s = String::from("<a ");
    s.push_str(INVALID_STR);
    s.push_str(r#"="value"/>"#);

    let sut = Sut::new(s.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(Error::InvalidUtf8(_, bytes)) => {
            assert_eq!(bytes, &[INVALID_UTF8_BYTE]);
        }
        _ => panic!("unexpected failure"),
    }
}

// TODO: Enough information for error recovery and reporting.
#[test]
fn attr_value_invalid_utf8() {
    let mut s = String::from(r#"<a attr="bad"#);
    s.push_str(INVALID_STR);
    s.push_str(r#""/>"#);

    let sut = Sut::new(s.as_bytes());

    let result = sut.collect::<Result<Vec<_>>>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(Error::InvalidUtf8(_, bytes)) => {
            assert_eq!(bytes, &[b'b', b'a', b'd', INVALID_UTF8_BYTE]);
        }
        _ => panic!("unexpected failure"),
    }
}
