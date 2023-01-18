// XIR element parser generator tests
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

//! Element parser generator tests.
//!
//! It is expected to be understood for these tests that `ele_parse!`
//!   directly invokes `attr_parse_stream!` to perform all attribute
//!   parsing,
//!     and so testing of that parsing is not duplicated here.
//! A brief visual inspection of the implementation of `ele_parse`
//!   should suffice to verify this claim.
//!
//! [`Parser`] is configured to output a parse trace to stderr for tests,
//!   which is visible when a test fails;
//!     this aids in debugging and study.
//! To force it to output on a successful test to observe the behavior of
//!   the system,
//!     simply force the test to panic at the end.

use std::{
    assert_matches::assert_matches, convert::Infallible, error::Error,
    fmt::Display,
};

use crate::{
    convert::ExpectInto,
    diagnose::{AnnotatedSpan, Diagnostic},
    parse::{
        util::SPair, FinalizeError, Object, ParseError, ParseState, Parsed,
        ParsedResult,
    },
    span::{dummy::*, Span},
    sym::SymbolId,
    xir::{
        attr::{Attr, AttrSpan},
        flat::{Depth, RefinedText, Text, Whitespace, XirfToken},
        st::{prefix::*, qname::*},
        CloseSpan, EleNameLen, EleSpan, OpenSpan, QName,
    },
};

// Some number (value does not matter).
const N: EleNameLen = 10;

#[test]
fn empty_element_no_attrs_no_close() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo;
    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo,
        };
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo), // [Root] Open
            Parsed::Incomplete,  // [Root] Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Same as above,
//   but with an object emitted on Close rather than Incomplete.
#[test]
fn empty_element_no_attrs_with_close() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Attr,
        Close,
    }

    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::Attr,
            / => Foo::Close,
        };
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo::Attr),  // [Root] Open
            Parsed::Object(Foo::Close), // [Root] Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Same as above,
//   but also with opening and closing spans.
#[test]
fn empty_element_no_attrs_with_close_with_spans() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Attr(OpenSpan),
        Close(CloseSpan),
    }

    impl crate::parse::Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE(_, ospan) {
            @ {} => Foo::Attr(ospan),
            /(cspan) => Foo::Close(cspan),
        };
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Attr(OpenSpan(S1, N))), // [Root] Open
            Object(Foo::Close(CloseSpan::empty(S2))), // [Root] Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Match on a namespace prefix rather than a static QName.
#[test]
fn empty_element_ns_prefix() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo(QName);
    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        // This matches `c:*`.
        Root := NS_C(qname, _) {
            @ {} => Foo(qname),
        };
    }

    let toks = vec![
        // Just some `c:*`.
        XirfToken::Open(QN_C_EQ, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo(QN_C_EQ)), // [Root] Open
            Parsed::Incomplete,           // [Root] Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn empty_element_ns_prefix_nomatch() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo;
    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        // This matches `c:*`.
        Root := NS_C {
            @ {} => Foo,
        };
    }

    let span = OpenSpan(S1, N);
    // Non `c:*` element.
    let unexpected = QN_PACKAGE;

    let toks = vec![
        XirfToken::Open(unexpected, span, Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::UnexpectedEle(
                unexpected,
                span.name_span()
            )
        )),
        err,
    );
}

// When a QName matches a namespace prefix,
//   that specific QName should be used in subsequent errors,
//   such as when expecting a closing tag.
#[test]
fn empty_element_ns_prefix_invalid_close_contains_matching_qname() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo;
    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        // This matches `c:*`.
        Root := NS_C {
            @ {} => Foo,
        };
    }

    let unexpected = QN_C_GT;
    let span_unexpected = OpenSpan(S2, N);

    let toks = vec![
        // Just some `c:*`.
        XirfToken::Open(QN_C_EQ, OpenSpan(S1, N), Depth(0)),
        // We're not expecting a child.
        XirfToken::Open(unexpected, span_unexpected, Depth(1)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The opening tag parses fine,
    //   and the unexpected tag successfully terminates attribute parsing.
    assert_eq!(sut.next(), Some(Ok(Parsed::Object(Foo)))); // [Root] Open

    // But then consuming the LA will produce an error,
    //   since we were not expecting a child.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::CloseExpected(
                // Verify that the error includes the QName that actually matched.
                QN_C_EQ,
                OpenSpan(S1, N),
                XirfToken::Open(unexpected, span_unexpected, Depth(1)),
            )
        )),
        err,
    );
}

// Merging of element stream with attributes.
#[test]
fn empty_element_with_attrs() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Ele,
        A(SPair),
        B(SPair),
    }

    impl Object for Foo {}

    impl<E> Into<Result<Foo, E>> for Foo {
        fn into(self) -> Result<Foo, E> {
            Ok(self)
        }
    }

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        // In practice we wouldn't actually use Attr
        //   (we'd use an appropriate newtype),
        //     but for the sake of this test we'll keep things simple.
        Root := QN_PACKAGE {
            @ {
                QN_NAME => Foo::A,
                QN_VALUE => Foo::B,
            } => Foo::Ele,
        };
    }

    let name_val = "bar".into();
    let value_val = "baz".into();

    let toks = vec![
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        // Purposefully out of order just to demonstrate that order does
        //   not matter.
        XirfToken::Attr(Attr(QN_VALUE, value_val, AttrSpan(S2, S3))),
        XirfToken::Attr(Attr(QN_NAME, name_val, AttrSpan(S4, S5))),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo::Ele),                     // Open
            Parsed::Object(Foo::B(SPair(value_val, S3))), // Attr
            Parsed::Object(Foo::A(SPair(name_val, S5))),  // Attr
            Parsed::Incomplete,                           // Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// This only tests one scenario under which attribute parsing may fail
//   (others are tested with `attr_parse!`).
// Failure to parse an attribute will ignore that attribute and continue.
//
// Historical Note
// ===============
// This strategy differs from the original design of this parser,
//   which would ignore the entire element,
//   with the intent that the type of token to yield would depend in part on
//   the element's attributes;
//     this has been walked back in favor of refinement lower in the
//     pipeline.
#[test]
fn element_with_failed_attr_parsing() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open,
        Close,
        Attr(SPair),
        Child,
    }

    impl crate::parse::Object for Foo {}

    impl<E> Into<Result<Foo, E>> for Foo {
        fn into(self) -> Result<Foo, E> {
            Ok(self)
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

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_CHILD: QName = QN_DIM;

    ele_parse! {
        enum Sut;

        type AttrValueError = FooError;
        type Object = Foo;

        Root := QN_ROOT {
            @ {
                // This one will always fail:
                QN_NAME => FooError,

                // This one will always succeed:
                QN_YIELDS => Foo::Attr,
            } => Foo::Open,

            // Important to check that this is not emitted.
            / => Foo::Close,
        };

        Child := QN_CHILD {
            @ {} => Foo::Child,
        };
    }

    let name_val = "name_val".into();
    let yields_val = "yields_val".into();

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // This will fail to parse and will not yield a token
        XirfToken::Attr(Attr(QN_NAME, name_val, AttrSpan(S2, S3))),
        // But this will parse successfully,
        //   since recovery was restricted to that one token of input.
        XirfToken::Attr(Attr(QN_YIELDS, yields_val, AttrSpan(S4, S5))),
        XirfToken::Close(Some(QN_ROOT), CloseSpan::empty(S6), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;

    // Root will open normally.
    assert_eq!(sut.next(), Some(Ok(Object(Foo::Open)))); // [Root] Root Open

    // The next token is the attribute that will result in an error.
    // Recovery concludes immediately after ignoring it.
    let err = sut.next().unwrap().unwrap_err();
    assert_matches!(
        err,
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::Attrs(..)
        )),
    ); // [Root@] QN_NAME

    assert_eq!(
        Ok(vec![
            Object(Foo::Attr(SPair(yields_val, S5))), // [Root@] QN_YIELDS
            Object(Foo::Close),                       // [Root]  Root Close
        ]),
        sut.collect(),
    );
}

// `[test]` allows for dynamic streaming attribute parsing without any sort
//   of schema.
// This is necessary for elements like short-hand template applications.
#[test]
fn element_with_streaming_attrs() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open,
        Attr(Attr),
        Child,
        Close,
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_CHILD: QName = QN_DIM;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_ROOT {
            // symbol soup
            @ {} => Foo::Open,
            / => Foo::Close,

            // This binds all attributes in place of `@ {}` above.
            [attr](attr) => Foo::Attr(attr),

            Child,
        };

        Child := QN_CHILD {
            @ {} => Foo::Child,
        };
    }

    let attr1 = Attr(QN_NAME, "one".into(), AttrSpan(S2, S3));
    let attr2 = Attr(QN_TYPE, "two".into(), AttrSpan(S3, S4));

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // These attributes should stream,
        //   but only _after_ having emitted the opening object from `@ {}`.
        XirfToken::Attr(attr1.clone()),
        XirfToken::Attr(attr2.clone()),
        // A child should halt attribute parsing just the same as `@ {}`
        //   would without the `[text]` special form.
        XirfToken::Open(QN_CHILD, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S2, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Open),        // [Root]   Root Open
            Object(Foo::Attr(attr1)), // [Root]   attr1
            Object(Foo::Attr(attr2)), // [Root]   attr2
            Object(Foo::Child),       // [Child]  Child Open
            Incomplete,               // [Child]  Child Close
            Object(Foo::Close),       // [Root]   Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// An unexpected element produces an error for the offending token and
//   then employs a recovery strategy so that parsing may continue.
#[test]
fn unexpected_element() {
    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = ();

        Root := QN_PACKAGE {
            // symbol soup
            @ {} => (),
        };
    }

    let unexpected = "unexpected".unwrap_into();
    let span = OpenSpan(S1, 3);

    // Note that the depth is >0 just to ensure that we don't
    //   hard-code some assumption that `0` means "root".
    const DEPTH_ROOT: Depth = Depth(5);
    const DEPTH_CHILD: Depth = Depth(6);

    // Implied here is that we have valid XIRF.
    // This means that,
    //   in the context of the larger real-world system
    //     (not these test cases),
    //   even as we discard tokens,
    //   XIRF is still doing its job before feeding them to us,
    //     meaning that we get XIRF's parsing even though we've chosen
    //     to ignore further input for this element.
    // In other words---our
    //   decision to skip tokens does not skip the validations that XIRF
    //   performs,
    //     such as ensuring proper nesting.
    let toks = vec![
        // Any name besides `QN_PACKAGE`
        XirfToken::Open(unexpected, span, DEPTH_ROOT),
        // From this point on we are in a recovery state,
        //   and will not emit tokens
        //     (or further errors)
        //   for these inputs.
        XirfToken::Attr(Attr(QN_VALUE, "ignored".into(), AttrSpan(S2, S3))),
        XirfToken::Open(QN_NAME, OpenSpan(S4, N), DEPTH_CHILD),
        // This ensures that closing at a different depth will not count
        //   as the closing node for recovery.
        XirfToken::Close(None, CloseSpan::empty(S5), DEPTH_CHILD),
        // This final token closes the element that caused the error,
        //   and so brings us into an accepting state.
        XirfToken::Close(Some(unexpected), CloseSpan(S6, 3), DEPTH_ROOT),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The first token of input is the unexpected element,
    //   and so should result an error.
    // The referenced span should be the _name_ of the element,
    //   not the tag,
    //   since the error is referring not to the fact that an element
    //     was encountered
    //       (which was expected),
    //       but to the fact that the name was not the one expected.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::UnexpectedEle(
                unexpected,
                span.name_span()
            )
        )),
        err,
    );

    // The diagnostic should describe the name of the element as being
    //   invalid.
    assert_eq!(err.describe()[0].span(), span.name_span());

    // We should have now entered a recovery mode whereby we discard
    //   input until we close the element that introduced the error.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Attr
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Open  (C)
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Close (C)

    // The recovery state must not be in an accepting state,
    //   because we didn't close at the root depth yet.
    let (mut sut, _) =
        sut.finalize().expect_err("recovery must not be accepting");

    // The next token should close the element that is in error,
    //   and bring us into an accepting state.
    // But since we are not emitting tokens,
    //   we'll still be marked as incomplete.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Close (R)
    sut.finalize()
        .expect("recovery must complete in an accepting state");
}

#[test]
fn single_child_element() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        RootAttr,
        ChildAttr,
    }

    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::RootAttr,

            Child,
        };

        Child := QN_CLASSIFY {
            @ {} => Foo::ChildAttr,
        };
    }

    let toks = vec![
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Open(QN_CLASSIFY, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S4, N), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo::RootAttr),  // [Root]  Root Open
            Parsed::Object(Foo::ChildAttr), // [Child] Child Open
            Parsed::Incomplete,             // [Child] Child Close
            Parsed::Incomplete,             // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Since all NTs are zero-or-more,
//   we should accept when an expecting child is missing
//     (when we receive `Close` instead of an `Open` for the child).
#[test]
fn single_child_element_missing() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        Child,
    }

    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::Root,

            // Expected,
            //   but will not be provided.
            Child,
        };

        // We never yield this.
        Child := QN_CLASSIFY {
            @ {} => Foo::Child,
        };
    }

    let toks = vec![
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        // Missing child,
        //   which should be okay.
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S4, N), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Object(Foo::Root), // [Root]  Root Open
            Parsed::Incomplete,        // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

/// Expands off of [`single_child_element`],
///   but the former provides a clear indication of whether a single state
///   is properly recognized without having to worry about how nonterminals'
///   states transition to one-another in sequence.
#[test]
fn multiple_child_elements_sequential() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        RootOpen(Span),
        ChildAOpen(Span),
        ChildAClose(Span),
        ChildBOpen,
        ChildBClose,
        RootClose(Span),
    }

    impl crate::parse::Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE(_, ospan) {
            @ {} => Foo::RootOpen(ospan.span()),
            /(cspan) => Foo::RootClose(cspan.span()),

            // Order matters here.
            ChildA,
            ChildB,
        };

        // Demonstrates that span identifier bindings are scoped to the
        //   nonterminal block
        //     (so please keep the identifiers the same as above).
        ChildA := QN_CLASSIFY(_, ospan) {
            @ {} => Foo::ChildAOpen(ospan.span()),
            /(cspan) => Foo::ChildAClose(cspan.span()),
        };

        ChildB := QN_EXPORT {
            @ {} => Foo::ChildBOpen,
            / => Foo::ChildBClose,
        };
    }

    let toks = vec![
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        // ChildA
        XirfToken::Open(QN_CLASSIFY, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Child B
        XirfToken::Open(QN_EXPORT, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S5, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::RootOpen(S1)),    // [Root]   Root Open
            Object(Foo::ChildAOpen(S2)),  // [ChildA] ChildA Open
            Object(Foo::ChildAClose(S3)), // [ChildA] ChildA Close
            Object(Foo::ChildBOpen),      // [ChildB] ChildB Open
            Object(Foo::ChildBClose),     // [ChildB] ChildB Close
            Object(Foo::RootClose(S5)),   // [Root]   Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Used by below tests.
fn x_ignored_between_elements(tok: XirfToken<RefinedText>) {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        A,
        B,
    }

    impl crate::parse::Object for Foo {}

    const QN_SUT: QName = QN_PACKAGE;
    const QN_A: QName = QN_CLASSIFY;
    const QN_B: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_SUT {
            @ {} => Foo::Root,

            A,
            B,
        };

        A := QN_A {
            @ {} => Foo::A,
        };

        B := QN_B {
            @ {} => Foo::B,
        };
    }

    let toks = vec![
        // Whitespace before start tag.
        tok.clone(),
        XirfToken::Open(QN_SUT, OpenSpan(S1, N), Depth(0)),
        // Whitespace between children.
        tok.clone(),
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        tok.clone(),
        XirfToken::Open(QN_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        tok.clone(),
        XirfToken::Close(Some(QN_SUT), CloseSpan(S5, N), Depth(0)),
        // Whitespace after end tag.
        tok.clone(),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Incomplete,        // [Root]  tok
            Object(Foo::Root), // [Root]  Root Open
            Incomplete,        // [Root@] tok
            Object(Foo::A),    // [A]     A Open
            Incomplete,        // [A]     A Close
            Incomplete,        // [A]     tok
            Object(Foo::B),    // [B]     B Open
            Incomplete,        // [B]     B Close
            Incomplete,        // [Root]  tok
            Incomplete,        // [Root]  Root Close
            Incomplete,        // [Root]  tok
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Even if we do not accept mixed data
//   (text and elements),
//   whitespace text ought to be accepted and entirely ignored.
#[test]
fn whitespace_ignored_between_elements() {
    x_ignored_between_elements(XirfToken::Text(
        RefinedText::Whitespace(Whitespace(Text("  ".unwrap_into(), S1))),
        Depth(0),
    ));
}

// Comments have no semantic meaning,
//   and ought not to,
//   because we control the language and can do better.
#[test]
fn comments_ignored_between_elements() {
    x_ignored_between_elements(XirfToken::Comment(
        "comment".into(),
        S1,
        Depth(0),
    ));
}

// TODO: This error recovery seems to be undesirable,
//     both consuming an element and skipping the requirement;
//       it is beneficial only in showing that recovery is possible and
//       accounted for.
//  Let's revisit once we're further along and have concrete examples to
//    determine if there is a proper umbrella recovery strategy,
//      or if it needs to be configurable depending on context.
#[test]
fn child_error_and_recovery() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        RootOpen,
        ChildABad, // Will not yield this one.
        ChildB,
        RootClose,
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_CLASSIFY;
    const QN_B: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_ROOT {
            @ {} => Foo::RootOpen,

            // Must be emitted if `RootOpen` is to maintain balance.
            / => Foo::RootClose,

            // This is what we're expecting,
            //   but not what we will provide.
            ChildA,

            // But we _will_ provide this expected value,
            //   after error recovery ignores the above.
            ChildB,
        };

        ChildA := QN_A {
            @ {} => Foo::ChildABad,
        };

        ChildB := QN_B {
            @ {} => Foo::ChildB,
        };
    }

    let unexpected = "unexpected".unwrap_into();
    let span = OpenSpan(S2, N);

    let toks = vec![
        // The first token is the expected root.
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // --> But this one is unexpected (name).
        XirfToken::Open(unexpected, span, Depth(1)),
        // And so we should ignore it up to this point.
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // At this point,
        //   having encountered the closing tag,
        //   the next token should result in a dead state,
        //     which should then result in a transition away from the state
        //     for `ChildA`,
        //       which means that we expect `ChildB`.
        // Parsing continues normally.
        XirfToken::Open(QN_B, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S4, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;

    // The first token is expected,
    //   and we enter attribute parsing for `Root`.
    assert_eq!(Some(Ok(Object(Foo::RootOpen))), sut.next()); // [Root] Open 0

    // The token of lookahead (`Open`) is unexpected for `ChildA`,
    //   when then skips to `ChildB`,
    //   which is _also_ not expecting it and must throw an error and enter
    //   a recovery state.
    // The token should be consumed and returned in the error,
    //   _not_ produced as a token of lookahead,
    //   since we do not want to reprocess bad input.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        err,
        ParseError::StateError(<Sut as ParseState>::Error::ChildB(
            <ChildB as ParseState>::Error::UnexpectedEle(
                unexpected,
                span.name_span(),
            )
        )),
    );

    // TODO: Can't deal with this until we know exactly what error we'll
    //   have above;
    //     see above TODO.
    // Diagnostic message should be delegated to the child.
    assert_eq!(err.describe()[0].span(), span.name_span());

    // The next token is the self-closing `Close` for the unexpected opening
    //   tag.
    // Since we are in recovery,
    //   it should be ignored.
    assert_eq!(Some(Ok(Incomplete)), sut.next()); // [ChildA!] Close 1

    // Having recovered from the error,
    //   we should happily accept the remaining tokens starting with
    //   `ChildB`.
    // An intelligent system ought to accept `ChildA` if it didn't produce
    //   any output for the erroneous input,
    //     but that's not what we're doing yet.
    assert_eq!(
        Ok(vec![
            Object(Foo::ChildB),    // [ChildB]  Open 1
            Incomplete,             // [ChildB]  Close 1
            Object(Foo::RootClose), // [Root]    Close 0
        ]),
        sut.collect()
    );
}

// This differs from the above test in that we encounter unexpected elements
//   when we expected to find the end tag.
// This means that the element _name_ is not in error,
//   but the fact that an element exists _at all_ is.
#[test]
fn child_error_and_recovery_at_close() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open,
        Close,
    }

    impl Object for Foo {}

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::Open,
            / => Foo::Close,
        };
    }

    let unexpected_a = "unexpected a".unwrap_into();
    let unexpected_b = "unexpected b".unwrap_into();
    let span_a = OpenSpan(S2, N);
    let span_b = OpenSpan(S4, N);

    let toks = vec![
        // The first token is the expected root.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        // Root is now expecting either attributes
        //   (of which there are none),
        //   or a closing element.
        // In either case,
        //   an opening element is entirely unexpected.
        XirfToken::Open(unexpected_a, span_a, Depth(1)),
        // And so we should ignore it up to this point.
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Let's do the same thing again.
        // It may be ideal to have another error exposed for each individual
        //   element that is unexpected,
        //     but for now the parser is kept simple and we simply continue
        //     to ignore elements until we reach the close.
        XirfToken::Open(unexpected_b, span_b, Depth(1)),
        // And so we should ignore it up to this point.
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        // Let's mix it up a bit with some text and make sure that is
        //   ignored too.
        XirfToken::Text(
            RefinedText::Unrefined(Text("unexpected text".unwrap_into(), S5)),
            Depth(1),
        ),
        // Having recovered from the above tokens,
        //   this will end parsing for `Root` as expected.
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S6, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The first token is expected,
    //   and we enter attribute parsing for `Root`.
    assert_eq!(Some(Ok(Parsed::Object(Foo::Open))), sut.next()); // [Root] Open 0

    // The token of lookahead (`Open`) is unexpected for `Root`,
    //   which is expecting `Close`.
    // The token should be consumed and returned in the error,
    //   _not_ produced as a token of lookahead,
    //   since we do not want to reprocess bad input.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::CloseExpected(
                QN_PACKAGE,
                OpenSpan(S1, N),
                XirfToken::Open(unexpected_a, span_a, Depth(1)),
            )
        )),
        err,
    );

    // The diagnostic information should include a reference to where the
    //   element was opened
    //     (so that the user understands what needs closing),
    //     followed by the span of the token in error
    //       (which naturally comes after the opening tag).
    let desc = err.describe();
    assert_eq!(desc[0].span(), S1); // Span of opening tag we want closed
    assert_eq!(desc[1].span(), span_a.span()); // Span of error

    // The recovery state must not be in an accepting state,
    //   because we didn't close at the root depth yet.
    let (mut sut, _) =
        sut.finalize().expect_err("recovery must not be accepting");

    // The next token is the self-closing `Close` for the unexpected opening
    //   tag.
    // Since we are in recovery,
    //   it should be ignored.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Root!] Close 1

    // We are still in recovery,
    //   and so we should still be ignoring tokens.
    // It may be more ideal to throw individual errors per unexpected
    //   element
    //     (though doing so may be noisy if there is a lot),
    //       but for now the parser is kept simple.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Root!] Open 1
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Root!] Close 1
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Root!] Text

    // Having recovered from the error,
    //   we should now be able to close successfully.
    assert_eq!(Some(Ok(Parsed::Object(Foo::Close))), sut.next());
    sut.finalize()
        .expect("recovery must complete in an accepting state");
}

// A nonterminal of the form `(A | ... | Z)` should accept the element of
//   any of the inner nonterminals.
#[test]
fn sum_nonterminal_accepts_any_valid_element() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        A,
        B,
        C,
    }

    impl crate::parse::Object for Foo {}

    // QNames don't matter as long as they are unique.
    const QN_A: QName = QN_PACKAGE;
    const QN_B: QName = QN_CLASSIFY;
    const QN_C: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := (A | B | C);

        A := QN_A {
            @ {} => Foo::A,
        };

        B := QN_B {
            @ {} => Foo::B,
        };

        C := QN_C {
            @ {} => Foo::C,
        };
    }

    use Parsed::*;
    use XirfToken::{Close, Open};

    // Try each in turn with a fresh instance of `Root`.
    [(QN_A, Foo::A), (QN_B, Foo::B), (QN_C, Foo::C)]
        .into_iter()
        .for_each(|(qname, obj)| {
            let toks = vec![
                Open(qname, OpenSpan(S1, N), Depth(0)),
                Close(None, CloseSpan::empty(S2), Depth(0)),
            ];

            assert_eq!(
                Ok(vec![
                    Object(obj), // [X] Open
                    Incomplete,  // [X] Close
                ]),
                Sut::parse(toks.into_iter()).collect(),
            );
        });
}

// Whitespace should be accepted around elements.
fn sum_nonterminal_accepts_x(tok: XirfToken<RefinedText>) {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        A,
        B,
    }

    impl crate::parse::Object for Foo {}

    // QNames don't matter as long as they are unique.
    const QN_A: QName = QN_PACKAGE;
    const QN_B: QName = QN_CLASSIFY;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        // Sum type requires two NTs but we only use A.
        Root := (A | B);

        A := QN_A {
            @ {} => Foo::A,
        };

        B := QN_B {
            @ {} => Foo::B,
        };
    }

    use Parsed::*;
    use XirfToken::{Close, Open};

    // Try each in turn with a fresh instance of `Root`.
    let toks = vec![
        // Leading.
        tok.clone(),
        Open(QN_A, OpenSpan(S1, N), Depth(0)),
        Close(None, CloseSpan::empty(S2), Depth(0)),
        // Trailing.
        tok.clone(),
    ];

    assert_eq!(
        Ok(vec![
            Incomplete,     // [A] tok
            Object(Foo::A), // [A] Open
            Incomplete,     // [A] Close
            Incomplete,     // [A] tok
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn sum_nonterminal_accepts_whitespace() {
    sum_nonterminal_accepts_x(XirfToken::Text(
        RefinedText::Whitespace(Whitespace(Text("   ".unwrap_into(), S1))),
        Depth(0),
    ));
}

#[test]
fn sum_nonterminal_accepts_comments() {
    sum_nonterminal_accepts_x(XirfToken::Comment(
        "comment".into(),
        S1,
        Depth(0),
    ));
}

// Compose sum NTs with a parent element.
#[test]
fn sum_nonterminal_as_child_element() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open(QName),
        Close(QName),
    }

    impl crate::parse::Object for Foo {}

    // QNames don't matter as long as they are unique.
    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_DIM;
    const QN_B: QName = QN_CLASSIFY;
    const QN_C: QName = QN_VALUE;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            // A|B followed by a C.
            AB,
            C,
        };

        AB := (A | B);

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),
        };

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        };

        C := QN_C {
            @ {} => Foo::Open(QN_C),
            / => Foo::Close(QN_C),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // A
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // B
        XirfToken::Open(QN_C, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S5, N), Depth(0)),
    ];

    use Parsed::*;

    assert_eq!(
        Ok(vec![
            Object(Foo::Open(QN_ROOT)),  // [Root] Root Open
            Object(Foo::Open(QN_A)),     // [A]    A Open
            Object(Foo::Close(QN_A)),    // [A]    A Close
            Object(Foo::Open(QN_C)),     // [C]    B Open
            Object(Foo::Close(QN_C)),    // [C@]   C Close (>LA)
            Object(Foo::Close(QN_ROOT)), // [Root] Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn sum_nonterminal_error_recovery() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        A,
        B,
    }

    impl crate::parse::Object for Foo {}

    // QNames don't matter as long as they are unique.
    const QN_A: QName = QN_PACKAGE;
    const QN_B: QName = QN_CLASSIFY;
    let unexpected: QName = "unexpected".unwrap_into();

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := (A | B);

        A := QN_A {
            @ {} => Foo::A,
        };

        B := QN_B {
            @ {} => Foo::B,
        };
    }

    // Something >0 just to assert that we're actually paying attention to
    //   it when consuming tokens during recovery.
    let depth = Depth(5);
    let depth_child = Depth(6);

    // An extra token to yield after we're done parsing to ensure that we
    //   properly yield a dead state transition.
    let dead_tok = XirfToken::Open(QN_NAME, OpenSpan(S5, N), depth);

    let toks = vec![
        // Neither A nor B,
        //   which will produce an error and enter recovery.
        XirfToken::Open(unexpected, OpenSpan(S1, N), depth),
        // A child element to be ignored,
        //   to ensure that its closing tag will not halt recovery
        //   prematurely.
        // This further tests that it's too late to provide a valid opening
        //   token
        //     (which is good because we're not at the right depth).
        XirfToken::Open(QN_A, OpenSpan(S2, N), depth_child),
        XirfToken::Close(None, CloseSpan::empty(S3), depth_child),
        // Closing token for the bad element at the corresponding depth,
        //   which will end recovery.
        XirfToken::Close(Some(unexpected), CloseSpan(S4, N), depth),
        // Should result in a dead state post-recovery,
        //   just as we would expect if we _didn't_ recover.
        dead_tok.clone(),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The first token of input is the unexpected element,
    //   and so should result an error.
    // The referenced span should be the _name_ of the element,
    //   not the tag,
    //   since the error is referring not to the fact that an element
    //     was encountered
    //       (which was expected),
    //       but to the fact that the name was not the one expected.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        err,
        ParseError::StateError(<Sut as ParseState>::Error::Root(
            <Root as ParseState>::Error::UnexpectedEle(
                unexpected,
                OpenSpan(S1, N).name_span(),
                Default::default(),
            )
        )),
    );

    // Diagnostic message should describe the name of the element.
    assert_eq!(err.describe()[0].span(), OpenSpan(S1, N).name_span());

    // We should have now entered a recovery mode whereby we discard
    //   input until we close the element that introduced the error.
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Open child
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Close child

    // The recovery state must not be in an accepting state,
    //   because we didn't close at the root depth yet.
    let (mut sut, _) =
        sut.finalize().expect_err("recovery must not be accepting");

    // The next token should close the element that is in error,
    //   and bring us into an accepting state.
    // But since we are not emitting tokens,
    //   we'll still be marked as incomplete.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Close root

    // Encountering any tokens post-recovery should result in a dead state
    //   just the same as if we had closed normally.
    let err = sut.next().unwrap().unwrap_err();
    assert_matches!(
        err,
        ParseError::UnexpectedToken(given_tok, _) if given_tok == dead_tok,
    );

    // Having otherwise completed successfully,
    //   and now yielding dead states,
    //   we must indicate that parsing has completed successfully so that
    //     the caller knows that it can safely move on.
    sut.finalize()
        .expect("recovery must complete in an accepting state");
}

#[test]
fn child_repetition() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        RootOpen,
        ChildOpen(QName),
        ChildClose(QName),
        RootClose,
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_DIM;
    const QN_B: QName = QN_CLASSIFY;
    const QN_C: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::RootOpen,
            / => Foo::RootClose,

            ChildA,
            ChildB,
            ChildC,
        };

        ChildA := QN_A {
            @ {} => Foo::ChildOpen(QN_A),
            / => Foo::ChildClose(QN_A),
        };

        ChildB := QN_B {
            @ {} => Foo::ChildOpen(QN_B),
            / => Foo::ChildClose(QN_B),
        };

        ChildC := QN_C {
            @ {} => Foo::ChildOpen(QN_C),
            / => Foo::ChildClose(QN_C),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // ChildA (1)
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // ChildA (2)
        XirfToken::Open(QN_A, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // ChildB (1)
        XirfToken::Open(QN_B, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        // ChildB (2)
        XirfToken::Open(QN_B, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        // ChildC (only)
        XirfToken::Open(QN_C, OpenSpan(S6, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S7), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S8, N), Depth(0)),
    ];

    use Parsed::*;

    // Note that we cannot observe the handoff after the repeating parsers
    //   below because Parser immediately recur.
    // For example,
    //   when ChildA has been closed,
    //   it awaits the next token to see if it should reset or if it should
    //   emit a dead state.
    // If it receives `QN_A`,
    //   then it'll reset.
    // However,
    //   `QN_B` will cause it to emit `dead` with the `Open` token as
    //   lookahead,
    //     which then gets turned into `Incomplete` with lookahead by
    //       `ParseState::delegate`,
    //     which then causes `Parser` to immediate recur,
    //       masking the `Incomplete` entirely.
    // And so what we see below is a cleaner,
    //   albeit not entirely honest,
    //   script.
    //
    // (Also please note that the above description is true as of the time
    //   of writing,
    //     but it's possible that this comment has not been updated since
    //     then.)
    assert_eq!(
        Ok(vec![
            Object(Foo::RootOpen),         // [Root]    Root Open
            Object(Foo::ChildOpen(QN_A)),  // [ChildA]  ChildA Open
            Object(Foo::ChildClose(QN_A)), // [ChildA]  ChildA Close
            Object(Foo::ChildOpen(QN_A)),  // [ChildA]  ChildA Open
            Object(Foo::ChildClose(QN_A)), // [ChildA]  ChildA Close
            Object(Foo::ChildOpen(QN_B)),  // [ChildB]  ChildB Open
            Object(Foo::ChildClose(QN_B)), // [ChildB]  ChildB Close
            Object(Foo::ChildOpen(QN_B)),  // [ChildB]  ChildB Open
            Object(Foo::ChildClose(QN_B)), // [ChildB]  ChildB Close
            Object(Foo::ChildOpen(QN_C)),  // [ChildC]  ChildC Open
            Object(Foo::ChildClose(QN_C)), // [ChildC]  ChildC Close
            Object(Foo::RootClose),        // [Root]    Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Once we transition `(S) -> (S')`,
//   we should not be able to transition back under any circumstance.
#[test]
fn child_nt_sequence_no_prev_after_next() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open(QName),
        Close(QName),
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_DIM;
    const QN_B: QName = QN_CLASSIFY;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_ROOT {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            A,
            B,
        };

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),
        };

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // A
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(1)),
        // A -> A OK
        XirfToken::Open(QN_A, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // A -> B
        XirfToken::Open(QN_B, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // B -> B OK
        XirfToken::Open(QN_B, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // B -> A _not_ OK.
        XirfToken::Open(QN_A, OpenSpan(S6, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S8, N), Depth(0)),
    ];

    use Parsed::*;

    assert_eq!(
        vec![
            Ok(Object(Foo::Open(QN_ROOT))), // [Root]  Root Open
            Ok(Object(Foo::Open(QN_A))),    // [A]     A Open
            Ok(Object(Foo::Close(QN_A))),   // [A]     A Close
            Ok(Object(Foo::Open(QN_A))),    // [A]     A Open
            Ok(Object(Foo::Close(QN_A))),   // [A]     A Close
            Ok(Object(Foo::Open(QN_B))),    // [B]     B Open
            Ok(Object(Foo::Close(QN_B))),   // [B]     B Close
            Ok(Object(Foo::Open(QN_B))),    // [B]     B Open
            Ok(Object(Foo::Close(QN_B))),   // [B]     B Close
            Err(ParseError::StateError(<Sut as ParseState>::Error::B(
                <B as ParseState>::Error::UnexpectedEle(
                    QN_A,
                    OpenSpan(S6, N).name_span()
                )
            ))), // [B!] A Open
            Ok(Incomplete),                 // [B!] A Close
            Ok(Object(Foo::Close(QN_ROOT))), // [Root] Root Close
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<ParsedResult<Sut>>>(),
    );
}

#[test]
fn child_repetition_invalid_tok_dead() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        RootOpen,
        ChildOpen,
        ChildClose,
        RootClose,
    }

    impl crate::parse::Object for Foo {}

    // QNames don't matter as long as they are unique.
    const QN_ROOT: QName = QN_PACKAGE;
    const QN_CHILD: QName = QN_DIM;
    let unexpected: QName = "unexpected".unwrap_into();

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::RootOpen,
            / => Foo::RootClose,

            Child,
        };

        Child := QN_CHILD {
            @ {} => Foo::ChildOpen,
            / => Foo::ChildClose,
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // Child (success)
        XirfToken::Open(QN_CHILD, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // unexpected
        XirfToken::Open(unexpected, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S8, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;

    let mut next = || sut.next();

    assert_eq!(next(), Some(Ok(Object(Foo::RootOpen)))); // [Root] Open
    assert_eq!(next(), Some(Ok(Object(Foo::ChildOpen)))); // [Child] Open
    assert_eq!(next(), Some(Ok(Object(Foo::ChildClose)))); // [Child] Close

    // Intuitively,
    //   we may want to enter recovery and ignore the element.
    // But the problem is that we need to emit a dead state so that other
    //   parsers can handle the input,
    //     because it may simply be the case that our repetition is over.
    //
    // Given that dead state and token of lookahead,
    //   `Parser` will immediately recurse to re-process the erroneous
    //   `Open`.
    // The next state after the `Child` NT is expecting a `Close`,
    //   but upon encountering a `Open` it forces the last NT to perform the
    //   processing,
    //     and so the error will occur on `Child`.
    assert_eq!(
        next(),
        Some(Err(ParseError::StateError(
            <Sut as ParseState>::Error::Child(
                <Child as ParseState>::Error::UnexpectedEle(
                    unexpected,
                    OpenSpan(S2, N).name_span()
                )
            )
        ))),
    );

    // This next token is also ignored as part of recovery.
    assert_eq!(next(), Some(Ok(Incomplete))); // [Root] Child Close

    // Finally,
    //   `Root` encounters its expected `Close` and ends recovery.
    assert_eq!(next(), Some(Ok(Object(Foo::RootClose)))); // [Root] Close
    sut.finalize()
        .expect("recovery must complete in an accepting state");
}

// Repetition on a nonterminal of the form `(A | ... | Z)` will allow any
//   number of `A` through `Z` in any order.
// This is similar to the above test.
#[test]
fn sum_repetition() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open(QName),
        Close(QName),
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_DIM;
    const QN_B: QName = QN_CLASSIFY;
    const QN_C: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_PACKAGE {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            // A|B|C in any order,
            //   any number of times.
            ABC,
        };

        ABC := (A | B | C );

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),
        };

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        };

        C := QN_C {
            @ {} => Foo::Open(QN_C),
            / => Foo::Close(QN_C),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // A (1)
        XirfToken::Open(QN_A, OpenSpan(S1, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(1)),
        // A (2)
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // B (1)
        XirfToken::Open(QN_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // C (1)
        XirfToken::Open(QN_C, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        // B (2)
        XirfToken::Open(QN_B, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S7, N), Depth(0)),
    ];

    use Parsed::*;

    // See notes on preceding repetition test `child_repetition` regarding
    //   the suppression of `Incomplete` for dead states.
    assert_eq!(
        Ok(vec![
            Object(Foo::Open(QN_ROOT)),  // [Root]  Root Open
            Object(Foo::Open(QN_A)),     // [A]     A Open
            Object(Foo::Close(QN_A)),    // [A]     A Close
            Object(Foo::Open(QN_A)),     // [A]     A Open
            Object(Foo::Close(QN_A)),    // [A]     A Close
            Object(Foo::Open(QN_B)),     // [B]     B Open
            Object(Foo::Close(QN_B)),    // [B]     B Close
            Object(Foo::Open(QN_C)),     // [C]     C Open
            Object(Foo::Close(QN_C)),    // [C]     C Close
            Object(Foo::Open(QN_B)),     // [B]     B Open
            Object(Foo::Close(QN_B)),    // [B]     B Close
            Object(Foo::Close(QN_ROOT)), // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Text nodes may appear between elements if a `[text]` special form
//   specifies a mapping on the superstate.
// This is "mixed content" in XML.
#[test]
fn mixed_content_text_nodes() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Open(QName),
        Close(QName),
        Text(SymbolId, Span),
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_CLASSIFY;
    const QN_B: QName = QN_EXPORT;
    const QN_C: QName = QN_DIM;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        [super] {
            // The `[text]` special form here introduces a `Text` mapping
            //   for all non-whitespace text nodes.
            [text](sym, span) => Foo::Text(sym, span),
        };

        Root := QN_ROOT {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            // Text allowed at any point between these elements because of
            //   the `[super]` definition.
            A,
            AB,

            // Used to verify that Text doesn't force a dead state
            //   transition away from AB at the close of a `A|B`.
            C,
        };

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),

            // Text should be permitted even though we permit no children,
            //   because of the `[super]` definition.
        };

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        };

        AB := (A | B);

        C := QN_C {
            @ {} => Foo::Open(QN_C),
            / => Foo::Close(QN_C),
        };
    }

    let tok_ws = XirfToken::Text(
        RefinedText::Whitespace(Whitespace(Text("  ".unwrap_into(), S1))),
        Depth(0),
    );

    let text_root = "text root".into();
    let text_a = "text a".into();
    let text_a2 = "text a2".into();
    let text_b = "text b".into();
    let text_b2 = "text b2".into();

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // Whitespace will not match the `[text]` special form.
        tok_ws.clone(),
        // Text before root open.
        // This must be emitted as a _child_ of Root,
        //   meaning that Root must be given the opportunity to report that
        //   attribute parsing is finished before we emit the object.
        XirfToken::Text(RefinedText::Unrefined(Text(text_root, S1)), Depth(1)),
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        // Text within a child.
        XirfToken::Text(RefinedText::Unrefined(Text(text_a, S2)), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Text _after_ a child node,
        //   which does not require ending attribute parsing before emitting.
        XirfToken::Text(RefinedText::Unrefined(Text(text_a2, S3)), Depth(1)),
        // Try to yield B with text.
        XirfToken::Open(QN_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Text(RefinedText::Unrefined(Text(text_b, S4)), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // Finally, some more text permitted at the close of b.
        XirfToken::Text(RefinedText::Unrefined(Text(text_b2, S5)), Depth(1)),
        // Encountering the text at the close should not have transitioned
        //   us away from the parser,
        //     so let's verify that we can still parse `AB`.
        XirfToken::Open(QN_B, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        // Provide C,
        //   just so this test doesn't depend on being able to accept zero
        //   of an NT.
        // This otherwise has no impact on this test beyond ensuring it
        //   doesn't fail for reasons unrelated to whitespace.
        XirfToken::Open(QN_C, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S6, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Open(QN_ROOT)),       // [Root]  Root Open
            Incomplete,                       // [Root@] WS
            Object(Foo::Text(text_root, S1)), // [Sut]   Text
            Object(Foo::Open(QN_A)),          // [A]     A Open (<LA)
            Object(Foo::Text(text_a, S2)),    // [Sut]   Text
            Object(Foo::Close(QN_A)),         // [A]     A Close
            Object(Foo::Text(text_a2, S3)),   // [Sut]   Text
            Object(Foo::Open(QN_B)),          // [B]     B Open
            Object(Foo::Text(text_b, S4)),    // [Sut]   Text
            Object(Foo::Close(QN_B)),         // [B]     B Close
            Object(Foo::Text(text_b2, S5)),   // [Sut]   Text
            Object(Foo::Open(QN_B)),          // [B]     B Open
            Object(Foo::Close(QN_B)),         // [B]     B Close
            Object(Foo::Open(QN_C)),          // [C]     C Open
            Object(Foo::Close(QN_C)),         // [C]     C Close
            Object(Foo::Close(QN_ROOT)),      // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

/// Contrast this test with [`mixed_content_text_nodes`] above.
#[test]
fn no_mixed_content_super() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        A,
    }

    impl crate::parse::Object for Foo {}

    const QN_SUT: QName = QN_PACKAGE;
    const QN_A: QName = QN_CLASSIFY;

    // No text permitted.
    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        Root := QN_SUT {
            @ {} => Foo::Root,

            A,
        };

        A := QN_A {
            @ {} => Foo::A,
        };
    }

    let text_a = "text a".into();

    let toks = vec![
        XirfToken::Open(QN_SUT, OpenSpan(S1, N), Depth(0)),
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        // Text should not be permitted.
        XirfToken::Text(RefinedText::Unrefined(Text(text_a, S2)), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_SUT), CloseSpan(S6, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;

    // The first two tokens should parse successfully
    //   (four calls because of LA).
    assert_eq!(sut.next(), Some(Ok(Object(Foo::Root)))); // [Root] Root Open
    assert_eq!(sut.next(), Some(Ok(Object(Foo::A)))); // [A] A Open

    // The next token is text,
    //   which is not permitted because of a lack of `[super]` with
    //   `[text`].
    assert_matches!(sut.next(), Some(Err(_))); // [A] Text

    // A then enters recovery,
    //   completes recovery,
    //   and parsing finishes.
    assert_eq!(
        Ok(vec![
            Incomplete, // [A]    A Close
            Incomplete, // [Root]  Root Close
        ]),
        sut.collect()
    );
}

// Using the same superstate node preemption mechanism as `[text]` above,
//   the superstate can also preempt opening element nodes.
// This is useful for things that can appear in _any_ context,
//   such as template applications.
#[test]
fn superstate_preempt_element_open_sum() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        RootClose,
        ChildA,
        ChildAClose,
        ChildB,
        ChildBClose,
        PreA(Span),
        PreAClose,
        PreB(Span),
        PreBClose,
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_CHILDA: QName = QN_NAME;
    const QN_CHILDB: QName = QN_DIM;
    const QN_PRE_A: QName = QN_CLASSIFY;
    const QN_PRE_B: QName = QN_EXPORT;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        [super] {
            // We can provide a _single_ NT to preempt.
            // Using a sum type allows us to preempt multiple nodes.
            PreAB
        };

        Root := QN_ROOT {
            @ {} => Foo::Root,
            / => Foo::RootClose,

            // Note how `AB` is _not_ a child here.
            ChildA,
            ChildB,
        };

        ChildA := QN_CHILDA {
            @ {} => Foo::ChildA,
            / => Foo::ChildAClose,
        };

        ChildB := QN_CHILDB {
            @ {} => Foo::ChildB,
            / => Foo::ChildBClose,
        };

        PreA := QN_PRE_A(_, ospan) {
            @ {} => Foo::PreA(ospan.span()),
            / => Foo::PreAClose,
        };

        PreB := QN_PRE_B(_, ospan) {
            @ {} => Foo::PreB(ospan.span()),
            / => Foo::PreBClose,
        };

        PreAB := (PreA | PreB);
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S2, N), Depth(0)),
        // At this point we are performing attribute parsing.
        // Let's try to preempt;
        //   we'll want to ensure that attributes will be omitted before the
        //   preempted node,
        //     otherwise we'd be a sibling rather than a child.
        XirfToken::Open(QN_PRE_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Now let's return to normal parsing with the expected child.
        XirfToken::Open(QN_CHILDA, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // We're now expecting `ChildB`.
        // Preempt again.
        XirfToken::Open(QN_PRE_A, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        // Preemption should not have changed the state of `Root`,
        //   and so _we should still be expecting `ChildB`_.
        XirfToken::Open(QN_CHILDB, OpenSpan(S6, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        // We ought to be able to preempt before the closing tag too.
        XirfToken::Open(QN_PRE_B, OpenSpan(S7, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S7), Depth(1)),
        // Adjacent,
        //   just to be sure that we allow the previous to close before we
        //   preempt again.
        XirfToken::Open(QN_PRE_A, OpenSpan(S8, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S7), Depth(1)),
        // This poor document has had enough.
        // Let it close.
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S2, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Root),        // [Root]    Root Open
            Object(Foo::PreB(S3)),    // [PreB]    B Open
            Object(Foo::PreBClose),   // [PreB]    B Close
            Object(Foo::ChildA),      // [ChildA]  ChildA Open
            Object(Foo::ChildAClose), // [ChildA]  ChildA Close
            Object(Foo::PreA(S5)),    // [PreA]    A Open
            Object(Foo::PreAClose),   // [PreA]    A Close
            Object(Foo::ChildB),      // [ChildB]  ChildB Open
            Object(Foo::ChildBClose), // [ChildB]  ChildB Close
            Object(Foo::PreB(S7)),    // [PreB]    B Open
            Object(Foo::PreBClose),   // [PreB]    B Close
            Object(Foo::PreA(S8)),    // [PreA]    A Open
            Object(Foo::PreAClose),   // [PreA]    A Close
            Object(Foo::RootClose),   // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Superstate preemption as above,
//   but using a normal NT instead of Sum NT.
#[test]
fn superstate_preempt_element_open_non_sum() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        RootClose,
        ChildA,
        ChildAClose,
        ChildB,
        ChildBClose,
        PreA(Span),
        PreAClose,
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_CHILDA: QName = QN_NAME;
    const QN_CHILDB: QName = QN_DIM;
    const QN_PRE_A: QName = QN_CLASSIFY;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        [super] {
            // We can provide a _single_ NT to preempt.
            PreA
        };

        Root := QN_ROOT {
            @ {} => Foo::Root,
            / => Foo::RootClose,

            // Note how `AB` is _not_ a child here.
            ChildA,
            ChildB,
        };

        ChildA := QN_CHILDA {
            @ {} => Foo::ChildA,
            / => Foo::ChildAClose,
        };

        ChildB := QN_CHILDB {
            @ {} => Foo::ChildB,
            / => Foo::ChildBClose,
        };

        PreA := QN_PRE_A(_, ospan) {
            @ {} => Foo::PreA(ospan.span()),
            / => Foo::PreAClose,
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S2, N), Depth(0)),
        // At this point we are performing attribute parsing.
        // Let's try to preempt;
        //   we'll want to ensure that attributes will be omitted before the
        //   preempted node,
        //     otherwise we'd be a sibling rather than a child.
        XirfToken::Open(QN_PRE_A, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Now let's return to normal parsing with the expected child.
        XirfToken::Open(QN_CHILDA, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        // We're now expecting `ChildB`.
        // Preempt again.
        XirfToken::Open(QN_PRE_A, OpenSpan(S5, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        // Preemption should not have changed the state of `Root`,
        //   and so _we should still be expecting `ChildB`_.
        XirfToken::Open(QN_CHILDB, OpenSpan(S6, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S6), Depth(1)),
        // Finally,
        //   we ought to be able to preempt before the closing tag too.
        XirfToken::Open(QN_PRE_A, OpenSpan(S7, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S7), Depth(1)),
        // This poor document has had enough.
        // Let it close.
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S2, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Root),        // [Root]    Root Open
            Object(Foo::PreA(S3)),    // [PreA]    A Open
            Object(Foo::PreAClose),   // [PreA]    A Close
            Object(Foo::ChildA),      // [ChildA] ChildA Open
            Object(Foo::ChildAClose), // [ChildA]  ChildA Close
            Object(Foo::PreA(S5)),    // [PreA]    A Open
            Object(Foo::PreAClose),   // [PreA]    A Close
            Object(Foo::ChildB),      // [ChildB]  ChildB Open
            Object(Foo::ChildBClose), // [ChildB]  ChildB Close
            Object(Foo::PreA(S7)),    // [PreA]    A Open (<LA)
            Object(Foo::PreAClose),   // [PreA]    A Close
            Object(Foo::RootClose),   // [Root]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Layers of preemption
//   (e.g. nested template applications).
#[test]
fn superstate_preempt_element_open_nested() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Root,
        RootClose,
        PreA(Span),
        PreAClose(Span),
    }

    impl crate::parse::Object for Foo {}

    const QN_ROOT: QName = QN_PACKAGE;
    const QN_PRE_A: QName = QN_CLASSIFY;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = Foo;

        [super] {
            // We can provide a _single_ NT to preempt.
            PreA
        };

        Root := QN_ROOT {
            @ {} => Foo::Root,
            / => Foo::RootClose,
        };

        PreA := QN_PRE_A(_, ospan) {
            @ {} => Foo::PreA(ospan.span()),
            /(cspan) => Foo::PreAClose(cspan.span()),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S2, N), Depth(0)),
        // First preemption
        XirfToken::Open(QN_PRE_A, OpenSpan(S3, N), Depth(1)),
        // And now a second preemption as a child of the first.
        XirfToken::Open(QN_PRE_A, OpenSpan(S4, N), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(2)),
        // Adjacent to ensure previous one closed.
        XirfToken::Open(QN_PRE_A, OpenSpan(S5, N), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(2)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S2, N), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Object(Foo::Root),          // [Root] Root Open
            Object(Foo::PreA(S3)),      // [PreA] PreA Open
            Object(Foo::PreA(S4)),      // [PreA] PreA Open
            Object(Foo::PreAClose(S4)), // [PreA] PreA Close
            Object(Foo::PreA(S5)),      // [PreA] PreA Open
            Object(Foo::PreAClose(S5)), // [PreA] PreA Close
            Object(Foo::PreAClose(S3)), // [PreA] PreA Close
            Object(Foo::RootClose),     // [Root] Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// If there are any parsers that still have work to do
//   (any on the stack),
//   we cannot consider ourselves to be done parsing.
#[test]
fn superstate_not_accepting_until_root_close() {
    const QN_ROOT: QName = QN_PACKAGE;
    const QN_A: QName = QN_CLASSIFY;

    ele_parse! {
        enum Sut;

        type AttrValueError = Infallible;
        type Object = ();

        Root := QN_ROOT {
            @ {} => (),

            A,
        };

        A := QN_A {
            @ {} => (),
        };
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // A is in an accepting state here,
        //   but we haven't yet closed Root and so Sut should not allow us
        //   to finish parsing at this point.
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;
    assert_eq!(sut.next(), Some(Ok(Object(())))); // [Root]  Open Root
    assert_eq!(sut.next(), Some(Ok(Object(())))); // [A]     Open A
    assert_eq!(sut.next(), Some(Ok(Incomplete))); // [A]     Close A

    // Since we haven't yet finished parsing the root,
    //   this should not be an accepting state even though the active child
    //   is in an accepting state.
    let (mut sut, _) = sut
        .finalize()
        .expect_err("child accepting must not be accepting for superstate");

    let err = sut.next().unwrap().unwrap_err();
    assert_matches!(
        err,
        ParseError::FinalizeError(FinalizeError::UnexpectedEof(..))
    );
}

// Ensure that we can actually export the generated identifiers
//   (add visibility to them).
// We don't want to always make them public by default because then Rust
//   forces us to make any other objects they use public,
//     which is annoying and confusing for things like test cases.
// Otherwise it wouldn't pose much of a practical issue,
//   since we could still encapsulate default-pub identifiers within private
//   modules.
//
// This will fail at compile time if there's a problem.
pub use test_exportable_generated_idents::ExportMe;

mod test_exportable_generated_idents {
    use super::*;

    ele_parse! {
        // This is the line that determines visibility of all identifiers
        //   generated within this macro invocation.
        pub enum Sut;

        type AttrValueError = Infallible;
        type Object = ();

        ExportMe := QN_PACKAGE {
            @ {} => (),
        };
    }
}
