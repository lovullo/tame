// XIR reader tests
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

use super::*;
use crate::parse::UnknownToken;
use crate::sym::GlobalSymbolIntern;
use crate::{
    convert::ExpectInto,
    parse::{
        ParseError::StateError as PE,
        Parsed::{self, Object as O},
    },
    span::DUMMY_CONTEXT as DC,
    xir::{Error, Token},
};
use std::borrow::Cow;

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

type Sut<'a, B, S> = XmlXirReader<'a, B, S>;
type SutResultCollect =
    result::Result<Vec<Parsed<Token>>, ParseError<UnknownToken, Error>>;

#[derive(Debug, Default)]
struct MockEscaper {}

// Simply adds ":UNESC" as a suffix to the provided byte slice.
impl Escaper for MockEscaper {
    fn escape_bytes(_: &[u8]) -> Cow<[u8]> {
        unreachable!("Reader should not be escaping!")
    }

    fn unescape_bytes(
        value: &[u8],
    ) -> result::Result<Cow<[u8]>, SpanlessError> {
        let mut unesc = value.to_owned();
        unesc.extend_from_slice(b":UNESC");

        Ok(Cow::Owned(unesc))
    }
}

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

macro_rules! new_sut {
    ($sut:ident = $data:expr) => {
        new_sut!(b $sut = $data.as_bytes())
    };

    (b $sut:ident = $data:expr) => {
        let escaper = MockEscaper::default();
        let $sut = Sut::new($data, &escaper, DC);
    };
}

#[test]
fn empty_node_without_prefix_or_attributes_or_whitespace() {
    new_sut!(sut = "<empty-node/>");
    //              [---------][]
    //              0        10
    //                   A     B

    let a = DC.span(0, 11);
    let b = DC.span(11, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("empty-node".unwrap_into(), a)),
            O(Token::Close(None, b)),
        ]),
        sut.collect(),
    );
}

#[test]
fn empty_node_without_prefix_or_attributes() {
    new_sut!(sut = "<empty-node   />");
    //              [---------]   []
    //              0        10  14
    //                   A        B
    //
    //            (extra WS intentional to test
    //             how it accommodates with spans)

    let a = DC.span(0, 11);
    let b = DC.span(14, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("empty-node".unwrap_into(), a)),
            O(Token::Close(None, b)),
        ]),
        sut.collect(),
    );
}

// Resolving namespaces is not the concern of XIR.
#[test]
fn does_not_resolve_xmlns() {
    new_sut!(sut = r#"<no-ns xmlns="noresolve" />"#);
    //                [----] [---]  [-------]  []
    //                0    5 7   11 14     22  25
    //                  A      B        C      D

    let a = DC.span(0, 6);
    let b = DC.span(7, 5);
    let c = DC.span(14, 9);
    let d = DC.span(25, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("no-ns".unwrap_into(), a)),
            // Since we didn't parse @xmlns, it's still an attribute.
            O(Token::AttrName("xmlns".unwrap_into(), b)),
            O(Token::AttrValue("noresolve:UNESC".intern(), c)),
            O(Token::Close(None, d)),
        ]),
        sut.collect(),
    );
}

// Resolving namespaces is not the concern of XIR.
#[test]
fn empty_node_with_prefix_without_attributes_unresolved() {
    new_sut!(sut = r#"<x:empty-node xmlns:x="noresolve" />"#);
    //                [-----------] [-----]  [-------]  []
    //                0          12 14   20  23     31  34
    //                      A          B         C      D

    let a = DC.span(0, 13);
    let b = DC.span(14, 7);
    let c = DC.span(23, 9);
    let d = DC.span(34, 2);

    // Should be the QName, _unresolved_.
    assert_eq!(
        Ok(vec![
            O(Token::Open(("x", "empty-node").unwrap_into(), a)),
            O(Token::AttrName(("xmlns", "x").unwrap_into(), b)),
            O(Token::AttrValue("noresolve:UNESC".intern(), c)),
            O(Token::Close(None, d)),
        ]),
        sut.collect(),
    );
}

// TODO: Enough information for error recovery and reporting.
#[test]
fn prefix_with_empty_local_name_invalid_qname() {
    // No local name (trailing colon).
    new_sut!(sut = r#"<x: xmlns:x="testns" />"#);
    //                 []
    //                 1
    //                 A

    let a = DC.span(1, 2);

    let result = sut.collect::<SutResultCollect>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(given) => {
            assert_eq!(PE(Error::InvalidQName("x:".into(), a)), given);
        }
    }
}

// The order of attributes must be retained.
#[test]
fn multiple_attrs_ordered() {
    new_sut!(sut = r#"<ele foo="a" bar="b" b:baz="c" />"#);
    //                [--] [-]  |  [-]  |  [---]  |  []
    //                0  3 5 7  10 13   18 21 25 28  31
    //                 A    B   C  D    E    F    G  H

    let a = DC.span(0, 4);
    let b = DC.span(5, 3);
    let c = DC.span(10, 1);
    let d = DC.span(13, 3);
    let e = DC.span(18, 1);
    let f = DC.span(21, 5);
    let g = DC.span(28, 1);
    let h = DC.span(31, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("ele".unwrap_into(), a)),
            O(Token::AttrName("foo".unwrap_into(), b)),
            O(Token::AttrValue("a:UNESC".intern(), c)),
            O(Token::AttrName("bar".unwrap_into(), d)),
            O(Token::AttrValue("b:UNESC".intern(), e)),
            O(Token::AttrName(("b", "baz").unwrap_into(), f)),
            O(Token::AttrValue("c:UNESC".intern(), g)),
            O(Token::Close(None, h)),
        ]),
        sut.collect(),
    );
}

#[test]
fn empty_attr_value() {
    new_sut!(sut = r#"<ele empty="" />"#);
    //                [--] [---]  | []
    //                0  3 5   9 12 14
    //                 A    B     C D
    //                           /
    //              zero-length span, where
    //               the value _would_ be

    let a = DC.span(0, 4);
    let b = DC.span(5, 5);
    let c = DC.span(12, 0);
    let d = DC.span(14, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("ele".unwrap_into(), a)),
            O(Token::AttrName("empty".unwrap_into(), b)),
            O(Token::AttrValue(":UNESC".intern(), c)),
            O(Token::Close(None, d)),
        ]),
        sut.collect(),
    );
}

// Contrary to the specification, but this is the responsibility of another
// parsing layer; we need to allow it to support e.g. recovery, code
// formatting, and LSPs.
#[test]
fn permits_duplicate_attrs() {
    new_sut!(sut = r#"<dup attr="a" attr="b" />"#);
    //                [--] [--]  |  [--]  |  []
    //                0  3 5  8  11 14 17 20 23
    //                 A    B    C    D   E  F

    let a = DC.span(0, 4);
    let b = DC.span(5, 4);
    let c = DC.span(11, 1);
    let d = DC.span(14, 4);
    let e = DC.span(20, 1);
    let f = DC.span(23, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("dup".unwrap_into(), a)),
            O(Token::AttrName("attr".unwrap_into(), b)),
            O(Token::AttrValue("a:UNESC".intern(), c)),
            O(Token::AttrName("attr".unwrap_into(), d)),
            O(Token::AttrValue("b:UNESC".intern(), e)),
            O(Token::Close(None, f)),
        ]),
        sut.collect(),
    );
}

#[test]
fn open_close_no_child() {
    new_sut!(sut = r#"<nochild></nochild>"#);
    //                [-------][--------]
    //                0       8`9      18
    //                    A        B
    //                  /
    //    note that this includes '>' when there are no attrs,
    //       since that results in a more intuitive span

    let a = DC.span(0, 9);
    let b = DC.span(9, 10);

    assert_eq!(
        Ok(vec![
            O(Token::Open("nochild".unwrap_into(), a)),
            O(Token::Close(Some("nochild".unwrap_into()), b)),
        ]),
        sut.collect(),
    );
}

// Whitespace is permitted after opening tags
//   (`STag` in the XML spec).
#[test]
fn open_close_no_child_open_tag_whitespace() {
    new_sut!(sut = r#"<nochild   ></nochild>"#);
    //                [----------][--------]
    //                0         11`12     21
    //                      A         B

    let a = DC.span(0, 12);
    let b = DC.span(12, 10);

    assert_eq!(
        Ok(vec![
            O(Token::Open("nochild".unwrap_into(), a)),
            O(Token::Close(Some("nochild".unwrap_into()), b)),
        ]),
        sut.collect(),
    );
}

// Space after end tags is explicitly permitted by the XML spec
//   (`ETag`).
#[test]
fn open_close_no_child_close_tag_whitespace() {
    new_sut!(sut = r#"<nochild></nochild   >"#);
    //                [-------][-----------]
    //                0       8`9         21
    //                    A          B

    let a = DC.span(0, 9);
    let b = DC.span(9, 13);

    assert_eq!(
        Ok(vec![
            O(Token::Open("nochild".unwrap_into(), a)),
            O(Token::Close(Some("nochild".unwrap_into()), b)),
        ]),
        sut.collect(),
    );
}

#[test]
fn child_node_self_closing() {
    new_sut!(sut = r#"<root><child /></root>"#);
    //                [----][----] [][-----]
    //                0    5`6  11 13`15  21
    //                   A    B    C    D
    //                  /
    //    note that this includes '>' when there are no attrs,
    // since that results in a more intuitive span (subject to change)

    let a = DC.span(0, 6);
    let b = DC.span(6, 6);
    let c = DC.span(13, 2);
    let d = DC.span(15, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("root".unwrap_into(), a)),
            O(Token::Open("child".unwrap_into(), b)),
            O(Token::Close(None, c)),
            O(Token::Close(Some("root".unwrap_into()), d)),
        ]),
        sut.collect(),
    );
}

#[test]
fn sibling_nodes() {
    new_sut!(sut = r#"<root><child /><child /></root>"#);
    //                [----][----] [][----] [][-----]
    //                0    5`6  11 13`15 20 22`24  30
    //                  A      B   C    D   E    F

    let a = DC.span(0, 6);
    let b = DC.span(6, 6);
    let c = DC.span(13, 2);
    let d = DC.span(15, 6);
    let e = DC.span(22, 2);
    let f = DC.span(24, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("root".unwrap_into(), a)),
            O(Token::Open("child".unwrap_into(), b)),
            O(Token::Close(None, c)),
            O(Token::Open("child".unwrap_into(), d)),
            O(Token::Close(None, e)),
            O(Token::Close(Some("root".unwrap_into()), f)),
        ]),
        sut.collect(),
    );
}

#[test]
fn child_node_with_attrs() {
    new_sut!(sut = r#"<root><child foo="bar" /></root>"#);
    //                [----][----] [-]  [-]  [][-----]
    //                0    5`6  11 13  18 20 23`25  31
    //                  A     B     C    D   E    F

    let a = DC.span(0, 6);
    let b = DC.span(6, 6);
    let c = DC.span(13, 3);
    let d = DC.span(18, 3);
    let e = DC.span(23, 2);
    let f = DC.span(25, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("root".unwrap_into(), a)),
            O(Token::Open("child".unwrap_into(), b)),
            O(Token::AttrName("foo".unwrap_into(), c)),
            O(Token::AttrValue("bar:UNESC".intern(), d)),
            O(Token::Close(None, e)),
            O(Token::Close(Some("root".unwrap_into()), f)),
        ]),
        sut.collect(),
    );
}

#[test]
fn child_text() {
    new_sut!(sut = r#"<text>foo bar</text>"#);
    //                [----][-----][-----]
    //                0    5`6   12`13  19
    //                  A      B      C

    let a = DC.span(0, 6);
    let b = DC.span(6, 7);
    let c = DC.span(13, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("text".unwrap_into(), a)),
            O(Token::Text("foo bar:UNESC".into(), b)),
            O(Token::Close(Some("text".unwrap_into()), c)),
        ]),
        sut.collect(),
    );
}

#[test]
fn mixed_child_content() {
    new_sut!(sut = r#"<text>foo<em>bar</em></text>"#);
    //                [----][-][--][-][---][-----]
    //                0    5`6 9 12`13`16  21   27
    //                  A    B  C   D   E     F

    let a = DC.span(0, 6);
    let b = DC.span(6, 3);
    let c = DC.span(9, 4);
    let d = DC.span(13, 3);
    let e = DC.span(16, 5);
    let f = DC.span(21, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("text".unwrap_into(), a)),
            O(Token::Text("foo:UNESC".into(), b)),
            O(Token::Open("em".unwrap_into(), c)),
            O(Token::Text("bar:UNESC".into(), d)),
            O(Token::Close(Some("em".unwrap_into()), e)),
            O(Token::Close(Some("text".unwrap_into()), f)),
        ]),
        sut.collect(),
    );
}

// This is how XML is typically written; people don't perceive it as mixed,
// even though it is.  This intentionally adds newlines before and after the
// opening and closing tags of the root node.
#[test]
fn mixed_child_content_with_newlines() {
    new_sut!(
        sut = r#"
<root>
  <child />
</root>
"#
    );
    // \n<root>\n  <child />\n</root>\n
    // [][----][ -][----] [][][-----][]
    // 0 1    6`7 9`10 15 17| `20  26`27
    //                      19
    // A   B     C   D    E F    G    H

    let a = DC.span(0, 1);
    let b = DC.span(1, 6);
    let c = DC.span(7, 3);
    let d = DC.span(10, 6);
    let e = DC.span(17, 2);
    let f = DC.span(19, 1);
    let g = DC.span(20, 7);
    let h = DC.span(27, 1);

    assert_eq!(
        Ok(vec![
            O(Token::Text("\n:UNESC".into(), a)),
            O(Token::Open("root".unwrap_into(), b)),
            O(Token::Text("\n  :UNESC".into(), c)),
            O(Token::Open("child".unwrap_into(), d)),
            O(Token::Close(None, e)),
            O(Token::Text("\n:UNESC".into(), f)),
            O(Token::Close(Some("root".unwrap_into()), g)),
            O(Token::Text("\n:UNESC".into(), h)),
        ]),
        sut.collect(),
    );
}

#[test]
fn comment() {
    new_sut!(sut = r#"<!--root--><root><!--<child>--></root>"#);
    //                [---------][----][------------][-----]
    //                0        10`11 16`17         30`31  37
    //                     A       B          C         D

    let a = DC.span(0, 11);
    let b = DC.span(11, 6);
    let c = DC.span(17, 14);
    let d = DC.span(31, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Comment("root".into(), a)),
            O(Token::Open("root".unwrap_into(), b)),
            O(Token::Comment("<child>".into(), c)),
            O(Token::Close(Some("root".unwrap_into()), d)),
        ]),
        sut.collect(),
    );
}

#[test]
fn comment_multiline() {
    new_sut!(
        sut = r#"<mult><!--comment
on multiple
lines-->
</mult>"#
    );
    // <mult><!--comment\non multiple\nlines-->\n</mult>
    // [----][----------- ------------ -------][][-----]
    // 0    5`6                             37'38`39  45
    //   A                     B               C    D

    let a = DC.span(0, 6);
    let b = DC.span(6, 32);
    let c = DC.span(38, 1);
    let d = DC.span(39, 7);

    assert_eq!(
        Ok(vec![
            O(Token::Open("mult".unwrap_into(), a)),
            O(Token::Comment("comment\non multiple\nlines".into(), b)),
            O(Token::Text("\n:UNESC".into(), c)),
            O(Token::Close(Some("mult".unwrap_into()), d)),
        ]),
        sut.collect(),
    );
}

// XIRF handles mismatch errors; XIR must explicitly support them.
#[test]
fn permits_mismatched_tags() {
    new_sut!(sut = r#"<root><child /></mismatch>"#);
    //                [----][----] [][---------]
    //                0    5`6  11 13`15      25
    //                  A      B   C      D

    let a = DC.span(0, 6);
    let b = DC.span(6, 6);
    let c = DC.span(13, 2);
    let d = DC.span(15, 11);

    assert_eq!(
        Ok(vec![
            O(Token::Open("root".unwrap_into(), a)),
            O(Token::Open("child".unwrap_into(), b)),
            O(Token::Close(None, c)),
            O(Token::Close(Some("mismatch".unwrap_into()), d)),
        ]),
        sut.collect(),
    );
}

#[test]
fn node_name_invalid_utf8() {
    let bytes: &[u8] = &[b'<', INVALID_UTF8_BYTE, b'/', b'>'];
    new_sut!(b sut = bytes);

    // We report at the QName, not the start tag.
    let span = DC.span(1, 1);

    let result = sut.collect::<SutResultCollect>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(PE(Error::InvalidUtf8(_, bytes, given_span))) => {
            assert_eq!(bytes, &[INVALID_UTF8_BYTE]);
            assert_eq!(span, given_span);
        }
        _ => panic!("unexpected failure"),
    }
}

#[test]
fn attr_name_invalid_utf8() {
    let mut s = String::from("<a ");
    s.push_str(INVALID_STR);
    s.push_str(r#"="value"/>"#);

    new_sut!(sut = s);

    let span = DC.span(3, 1);

    let result = sut.collect::<SutResultCollect>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(PE(Error::InvalidUtf8(_, bytes, given_span))) => {
            assert_eq!(bytes, &[INVALID_UTF8_BYTE]);
            assert_eq!(span, given_span);
        }
        _ => panic!("unexpected failure"),
    }
}

#[test]
fn attr_value_invalid_utf8() {
    let mut s = String::from(r#"<a attr="bad"#);
    s.push_str(INVALID_STR);
    s.push_str(r#""/>"#);

    new_sut!(sut = s);

    let span = DC.span(9, 4);

    let result = sut.collect::<SutResultCollect>();

    match result {
        Ok(_) => panic!("expected failure"),
        Err(PE(Error::InvalidUtf8(_, bytes, given_span))) => {
            // Doesn't make it to the Escaper.
            assert_eq!(bytes, &[b'b', b'a', b'd', INVALID_UTF8_BYTE]);
            assert_eq!(span, given_span);
        }
        _ => panic!("unexpected failure"),
    }
}

#[test]
fn valid_xml_decl_no_encoding() {
    new_sut!(sut = r#"<?xml version="1.0"?><root />"#);
    //                                     [---] []
    //                                     21 25 27
    //                                       A   B
    //  We do not yet emit a token for
    //       XML declarations

    let a = DC.span(21, 5);
    let b = DC.span(27, 2);

    assert_eq!(
        Ok(vec![
            O(Token::Open("root".unwrap_into(), a)),
            O(Token::Close(None, b)),
        ]),
        sut.collect()
    );
}

#[test]
fn valid_xml_decl_with_encoding_lower() {
    new_sut!(sut = r#"<?xml version="1.0" encoding="utf-8"?>"#);

    assert_eq!(Ok(vec![]), sut.collect());
}

#[test]
fn valid_xml_decl_with_encoding_upper() {
    new_sut!(sut = r#"<?xml version="1.0" encoding="UTF-8"?>"#);

    assert_eq!(Ok(vec![]), sut.collect());
}

// Only 1.0 supported.
#[test]
fn invalid_xml_decl_version() {
    new_sut!(sut = r#"<?xml version="1.1"?>"#);
    //                               [-]
    //                              15 17

    // Unlike above, we do actually calculate a span here.
    let span = DC.span(15, 3);

    assert_eq!(
        Err(PE(Error::UnsupportedXmlVersion("1.1".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

// Only UTF-8 supported.
#[test]
fn invalid_xml_encoding() {
    new_sut!(sut = r#"<?xml version="1.0" encoding="latin-1"?>"#);
    //                                              [-----]
    //                                             30    37

    let span = DC.span(30, 7);

    assert_eq!(
        Err(PE(Error::UnsupportedEncoding("latin-1".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

//
// quick-xml parser shortcomings
//
// There are certain problems quick-xml does not catch that we will catch
//   ourselves.
// This is not intended to be comprehensive,
//   but is intended to catch what might be common-enough errors that they
//   deserve friendly output from the compiler,
//     rather than resulting in more obscure errors down the line after
//     we've left the context of XML.
//
// Ultimately,
//   the writing may be on the wall for quick-xml,
//   but at the time of writing there are more important things to focus on,
//     so we will make do for now.
//

// quick-xml's behavior here is alarming---it simply doesn't see it as an
//   attribute at all and skips the tokens.
// We must detect this on our own.
#[test]
fn attr_single_no_value_no_eq() {
    new_sut!(sut = r#" <foo attr>"#);
    //           ____/          |
    //         /               10
    //       /     where the `="value"` should be
    //      |
    //   WS to make sure we add the file-relative pos
    // and not just have the span relative to the element

    let span = DC.span(10, 0);

    assert_eq!(
        Err(PE(Error::AttrValueExpected(None, span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn attr_single_no_value_with_eq() {
    new_sut!(sut = r#" <foo attr=>"#);
    //                           |
    //                           11
    //             where the `"value"` should be

    let span = DC.span(11, 0);

    assert_eq!(
        Err(PE(Error::AttrValueExpected(None, span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn attr_multi_no_value_no_eq() {
    new_sut!(sut = r#" <foo attr another>"#);
    //                          |
    //                          10
    //             where the `="value"` should be

    let span = DC.span(10, 0);

    assert_eq!(
        // quick-xml doesn't provide the name
        Err(PE(Error::AttrValueExpected(None, span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn attr_multi_no_value_with_eq() {
    new_sut!(sut = r#" <foo attr= another=>"#);
    //                           |
    //                           11
    //   quick-xml interprets this as an unquoted value

    // TODO: quick-xml does not give us the length so we'll have to figure
    //   it out ourselves.
    let span = DC.span(11, 0);

    assert_eq!(
        Err(PE(Error::AttrValueUnquoted(None, span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn attr_multiple_no_value_no_eq_then_good() {
    new_sut!(sut = r#" <foo attr good="value">"#);
    //                          |
    //                          10
    //             where the `="value"` should be

    let span = DC.span(10, 0);

    assert_eq!(
        // quick-xml doesn't provide the name
        Err(PE(Error::AttrValueExpected(None, span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn empty_element_qname_no_attrs() {
    new_sut!(sut = r#"<>"#);
    //                 |
    //                 1
    //        where the QName should be

    let span = DC.span(1, 0);

    assert_eq!(
        Err(PE(Error::InvalidQName("".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn empty_element_qname_with_space_no_attrs() {
    new_sut!(sut = r#"<  >"#);
    //                 |
    //                 1
    //        where the QName should be

    let span = DC.span(1, 0);

    assert_eq!(
        Err(PE(Error::InvalidQName("".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn empty_element_qname_with_attr() {
    new_sut!(sut = r#"<foo="bar">"#);
    //                 [-------]
    //                 1      10

    let span = DC.span(1, 9);

    assert_eq!(
        Err(PE(Error::InvalidQName("foo=\"bar\"".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

#[test]
fn empty_element_qname_with_space_with_attr() {
    new_sut!(sut = r#"<  foo="bar">"#);
    //                 |
    //                 1
    //   quick-xml interprets the space as a "" QName

    let span = DC.span(1, 0);

    assert_eq!(
        Err(PE(Error::InvalidQName("".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}

// Same as above test except that we have no attrs.
// We just want to be sure that we can't have a QName that starts with
//   whitespace.
#[test]
fn space_before_element_name() {
    new_sut!(sut = r#"< foo />"#);
    //                 |
    //                 1
    //   quick-xml interprets the space as a "" QName

    let span = DC.span(1, 0);

    assert_eq!(
        Err(PE(Error::InvalidQName("".intern(), span))),
        sut.collect::<SutResultCollect>()
    );
}
