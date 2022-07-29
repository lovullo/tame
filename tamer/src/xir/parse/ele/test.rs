// XIR element parser generator tests
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

//! Element parser generator tests.
//!
//! It is expected to be understood for these tests that `ele_parse`
//!   directly invokes `attr_parse` to perform all attribute parsing,
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

use std::{error::Error, fmt::Display};

use crate::{
    convert::ExpectInto,
    diagnose::Diagnostic,
    parse::{Object, ParseError, ParseState, Parsed},
    span::{dummy::*, Span},
    sym::SymbolId,
    xir::{
        attr::{Attr, AttrSpan},
        flat::{Depth, RefinedText, Text, Whitespace, XirfToken},
        st::qname::*,
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo,
        }
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,  // [Sut]  Open
            Parsed::Object(Foo), // [Sut@] Close (>LA)
            Parsed::Incomplete,  // [Sut]  Close (<LA)
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::Attr,
            / => Foo::Close,
        }
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,         // [Sut]  Open
            Parsed::Object(Foo::Attr),  // [Sut@] Close (>LA)
            Parsed::Object(Foo::Close), // [Sut]  Close (<LA)
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
        type Object = Foo;

        Sut := QN_PACKAGE(ospan) {
            @ {} => Foo::Attr(ospan),
            /(cspan) => Foo::Close(cspan),
        }
    }

    let toks = vec![
        // Length (second argument) here is arbitrary.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Close(None, CloseSpan::empty(S2), Depth(0)),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Incomplete,                               // [Sut]  Open
            Object(Foo::Attr(OpenSpan(S1, N))),       // [Sut@] Close (>LA)
            Object(Foo::Close(CloseSpan::empty(S2))), // [Sut]  Close (<LA)
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn empty_element_with_attr_bindings() {
    #[derive(Debug, PartialEq, Eq)]
    struct Foo(SymbolId, SymbolId, (Span, Span));
    impl Object for Foo {}

    #[derive(Debug, PartialEq, Eq)]
    struct AttrVal(Attr);

    impl TryFrom<Attr> for AttrVal {
        // Type must match AttrValueError on `ele_parse!`
        type Error = AttrValueError;

        fn try_from(attr: Attr) -> Result<Self, Self::Error> {
            Ok(AttrVal(attr))
        }
    }

    #[derive(Debug, PartialEq)]
    enum AttrValueError {}

    impl Error for AttrValueError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }

    impl Display for AttrValueError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "test AttrValueError")
        }
    }

    impl Diagnostic for AttrValueError {
        fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
            vec![]
        }
    }

    ele_parse! {
        // AttrValueError should be passed to `attr_parse!`
        //   (which is invoked by `ele_parse!`)
        //   as ValueError.
        type AttrValueError = AttrValueError;

        type Object = Foo;

        // In practice we wouldn't actually use Attr
        //   (we'd use an appropriate newtype),
        //     but for the sake of this test we'll keep things simple.
        Sut := QN_PACKAGE {
            @ {
                name: (QN_NAME) => AttrVal,
                value: (QN_VALUE) => AttrVal,
            } => Foo(
                name.0.value(),
                value.0.value(),
                (name.0.attr_span().value_span(), value.0.attr_span().value_span())
            ),
        }
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
            Parsed::Incomplete,                                 // Open
            Parsed::Incomplete,                                 // Attr
            Parsed::Incomplete,                                 // Attr
            Parsed::Object(Foo(name_val, value_val, (S5, S3))), // Close
            Parsed::Incomplete,                                 // Close (LA)
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// An unexpected element produces an error for the offending token and
//   then employs a recovery strategy so that parsing may continue.
#[test]
fn unexpected_element() {
    ele_parse! {
        type Object = ();

        Sut := QN_PACKAGE {
            // symbol soup
            @ {} => (),
        }
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
        // TODO: This references generated identifiers.
        ParseError::StateError(SutError_::UnexpectedEle_(
            unexpected,
            span.name_span()
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::RootAttr,

            Child,
        }

        Child := QN_CLASSIFY {
            @ {} => Foo::ChildAttr,
        }
    }

    let toks = vec![
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        XirfToken::Open(QN_CLASSIFY, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S4, N), Depth(0)),
    ];

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,             // [Sut]    Root Open
            Parsed::Object(Foo::RootAttr),  // [Sut@]   Child Open (>LA)
            Parsed::Incomplete,             // [Child]  Child Open (<LA)
            Parsed::Object(Foo::ChildAttr), // [Child@] Child Close (>LA)
            Parsed::Incomplete,             // [Child]  Child Close (<LA)
            Parsed::Incomplete,             // [Sut]    Root Close
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
        type Object = Foo;

        Sut := QN_PACKAGE(ospan) {
            @ {} => Foo::RootOpen(ospan.span()),
            /(cspan) => Foo::RootClose(cspan.span()),

            // Order matters here.
            ChildA,
            ChildB,
        }

        // Demonstrates that span identifier bindings are scoped to the
        //   nonterminal block
        //     (so please keep the identifiers the same as above).
        ChildA := QN_CLASSIFY(ospan) {
            @ {} => Foo::ChildAOpen(ospan.span()),
            /(cspan) => Foo::ChildAClose(cspan.span()),
        }

        ChildB := QN_EXPORT {
            @ {} => Foo::ChildBOpen,
            / => Foo::ChildBClose,
        }
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
            Incomplete,                   // [Sut]     Root Open
            Object(Foo::RootOpen(S1)),    // [Sut@]    ChildA Open (>LA)
            Incomplete,                   // [ChildA]  ChildA Open (<LA)
            Object(Foo::ChildAOpen(S2)),  // [ChildA@] ChildA Close (>LA)
            Object(Foo::ChildAClose(S3)), // [ChildA]  ChildA Close (<LA)
            Incomplete,                   // [ChildB]  ChildB Open
            Object(Foo::ChildBOpen),      // [ChildB@] ChildB Close (>LA)
            Object(Foo::ChildBClose),     // [ChildB]  ChildB Close (<LA)
            Object(Foo::RootClose(S5)),   // [Sut]     Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

// Even if we do not accept mixed data
//   (text and elements),
//   whitespace text ought to be accepted and entirely ignored.
#[test]
fn whitespace_ignored_between_elements() {
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
        type Object = Foo;

        Sut := QN_SUT {
            @ {} => Foo::Root,

            A,
            B,
        }

        A := QN_A {
            @ {} => Foo::A,
        }

        B := QN_B {
            @ {} => Foo::B,
        }
    }

    let tok_ws = XirfToken::Text(
        RefinedText::Whitespace(Whitespace(Text("  ".unwrap_into(), S1))),
        Depth(0),
    );

    let toks = vec![
        // Whitespace before start tag.
        tok_ws.clone(),
        XirfToken::Open(QN_SUT, OpenSpan(S1, N), Depth(0)),
        // Whitespace between children.
        tok_ws.clone(),
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        tok_ws.clone(),
        XirfToken::Open(QN_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        tok_ws.clone(),
        XirfToken::Close(Some(QN_SUT), CloseSpan(S5, N), Depth(0)),
        // Whitespace after end tag.
        tok_ws.clone(),
    ];

    use Parsed::*;
    assert_eq!(
        Ok(vec![
            Incomplete,        // [Sut]  WS
            Incomplete,        // [Sut]  Root Open
            Incomplete,        // [Sut@] WS
            Object(Foo::Root), // [Sut@] A Open (>LA)
            Incomplete,        // [A]    A Open (<LA)
            Object(Foo::A),    // [A@]   A Close (>LA)
            Incomplete,        // [A]    A Close (<LA)
            Incomplete,        // [A]    WS
            Incomplete,        // [B]    B Open
            Object(Foo::B),    // [B@]   B Close (>LA)
            Incomplete,        // [B]    B Close (<LA)
            Incomplete,        // [Sut]  WS
            Incomplete,        // [Sut]  Root Close
            Incomplete,        // [Sut]  WS
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
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
        Root,
        ChildABad, // Will not yield this one.
        ChildB,
    }

    impl Object for Foo {}

    ele_parse! {
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::Root,

            // This is what we're expecting,
            //   but not what we will provide.
            ChildA,

            // But we _will_ provide this expected value,
            //   after error recovery ignores the above.
            ChildB,
        }

        ChildA := QN_CLASSIFY {
            @ {} => Foo::ChildABad,
        }

        ChildB := QN_EXPORT {
            @ {} => Foo::ChildB,
        }
    }

    let unexpected = "unexpected".unwrap_into();
    let span = OpenSpan(S2, N);

    let toks = vec![
        // The first token is the expected root.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
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
        XirfToken::Open(QN_EXPORT, OpenSpan(S4, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S5), Depth(1)),
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S4, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The first token is expected,
    //   and we enter attribute parsing for `Sut`.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut] Open 0

    // The second token _will_ be unexpected,
    //   but we're parsing attributes for `Sut`,
    //   so we don't know that yet.
    // Instead,
    //   the `Open` ends attribute parsing and yields a token of lookahead.
    assert_eq!(
        Some(Ok(Parsed::Object(Foo::Root))), // [Sut@] Open 1 (>LA)
        sut.next()
    );

    // The token of lookahead (`Open`) is unexpected for `ChildA`,
    //   which must throw an error and enter a recovery state.
    // The token should be consumed and returned in the error,
    //   _not_ produced as a token of lookahead,
    //   since we do not want to reprocess bad input.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        // TODO: This references generated identifiers.
        ParseError::StateError(SutError_::ChildA(
            ChildAError_::UnexpectedEle_(unexpected, span.name_span())
        )),
        err,
    );

    // Diagnostic message should be delegated to the child.
    assert_eq!(err.describe()[0].span(), span.name_span());

    // The next token is the self-closing `Close` for the unexpected opening
    //   tag.
    // Since we are in recovery,
    //   it should be ignored.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [ChildA!] Close 1

    // Having recovered from the error,
    //   we should happily accept the remaining tokens starting with
    //   `ChildB`.
    // An intelligent system ought to accept `ChildA` if it didn't produce
    //   any output for the erroneous input,
    //     but that's not what we're doing yet.
    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,          // [ChildB]  Open 1
            Parsed::Object(Foo::ChildB), // [ChildB@] Close 1 (>LA)
            Parsed::Incomplete,          // [ChildB]  Close 1 (<LA)
            Parsed::Incomplete,          // [Sut]     Close 0
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::Open,
            / => Foo::Close,
        }
    }

    let unexpected_a = "unexpected a".unwrap_into();
    let unexpected_b = "unexpected b".unwrap_into();
    let span_a = OpenSpan(S2, N);
    let span_b = OpenSpan(S4, N);

    let toks = vec![
        // The first token is the expected root.
        XirfToken::Open(QN_PACKAGE, OpenSpan(S1, N), Depth(0)),
        // Sut is now expecting either attributes
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
        //   this will end parsing for `Sut` as expected.
        XirfToken::Close(Some(QN_PACKAGE), CloseSpan(S6, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // The first token is expected,
    //   and we enter attribute parsing for `Sut`.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut] Open 0

    // The second token _will_ be unexpected,
    //   but we're parsing attributes for `Sut`,
    //   so we don't know that yet.
    // Instead,
    //   the `Open` ends attribute parsing and yields a token of lookahead.
    assert_eq!(
        Some(Ok(Parsed::Object(Foo::Open))), // [Sut@] Open 1 (>LA)
        sut.next()
    );

    // The token of lookahead (`Open`) is unexpected for `Sut`,
    //   which is expecting `Close`.
    // The token should be consumed and returned in the error,
    //   _not_ produced as a token of lookahead,
    //   since we do not want to reprocess bad input.
    let err = sut.next().unwrap().unwrap_err();
    assert_eq!(
        // TODO: This references generated identifiers.
        ParseError::StateError(SutError_::CloseExpected_(
            OpenSpan(S1, N).tag_span(),
            XirfToken::Open(unexpected_a, span_a, Depth(1)),
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
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut!] Close 1

    // We are still in recovery,
    //   and so we should still be ignoring tokens.
    // It may be more ideal to throw individual errors per unexpected
    //   element
    //     (though doing so may be noisy if there is a lot),
    //       but for now the parser is kept simple.
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut!] Open 1
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut!] Close 1
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // [Sut!] Text

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
        type Object = Foo;

        Sut := (A | B | C);

        A := QN_A {
            @ {} => Foo::A,
        }

        B := QN_B {
            @ {} => Foo::B,
        }

        C := QN_C {
            @ {} => Foo::C,
        }
    }

    use Parsed::*;
    use XirfToken::{Close, Open};

    // Try each in turn with a fresh instance of `Sut`.
    [(QN_A, Foo::A), (QN_B, Foo::B), (QN_C, Foo::C)]
        .into_iter()
        .for_each(|(qname, obj)| {
            let toks = vec![
                Open(qname, OpenSpan(S1, N), Depth(0)),
                Close(None, CloseSpan::empty(S2), Depth(0)),
            ];

            assert_eq!(
                Ok(vec![
                    Incomplete,  // [X] Open
                    Object(obj), // [X@] Close (>LA)
                    Incomplete,  // [X] Close
                ]),
                Sut::parse(toks.into_iter()).collect(),
            );
        });
}

// Whitespace should be accepted around elements.
#[test]
fn sum_nonterminal_accepts_whitespace() {
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
        type Object = Foo;

        // Sum type requires two NTs but we only use A.
        Sut := (A | B);

        A := QN_A {
            @ {} => Foo::A,
        }

        B := QN_B {
            @ {} => Foo::B,
        }
    }

    use Parsed::*;
    use XirfToken::{Close, Open};

    let tok_ws = XirfToken::Text(
        RefinedText::Whitespace(Whitespace(Text("   ".unwrap_into(), S1))),
        Depth(0),
    );

    // Try each in turn with a fresh instance of `Sut`.
    let toks = vec![
        // Leading whitespace.
        tok_ws.clone(),
        Open(QN_A, OpenSpan(S1, N), Depth(0)),
        Close(None, CloseSpan::empty(S2), Depth(0)),
        // Trailing whitespace.
        tok_ws.clone(),
    ];

    assert_eq!(
        Ok(vec![
            Incomplete,     // [A] WS
            Incomplete,     // [A] Open
            Object(Foo::A), // [A@] Close (>LA)
            Incomplete,     // [A] Close
            Incomplete,     // [A] WS
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
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
    const QN_A: QName = QN_PACKAGE;
    const QN_B: QName = QN_CLASSIFY;

    ele_parse! {
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            // A|B followed by a B.
            AB,
            B,
        }

        AB := (A | B);

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),
        }

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        }
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // A
        XirfToken::Open(QN_A, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // B
        XirfToken::Open(QN_B, OpenSpan(S3, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S4), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S5, N), Depth(0)),
    ];

    use Parsed::*;

    assert_eq!(
        Ok(vec![
            Incomplete,                  // [Sut]  Root Open
            Object(Foo::Open(QN_ROOT)),  // [Sut@] A Open (>LA)
            Incomplete,                  // [A]  A Open (<LA)
            Object(Foo::Open(QN_A)),     // [A@] A Close (>LA)
            Object(Foo::Close(QN_A)),    // [A]  A Close (<LA)
            Incomplete,                  // [B]  B Open
            Object(Foo::Open(QN_B)),     // [B@] B Close (>LA)
            Object(Foo::Close(QN_B)),    // [B]  B Close (<LA)
            Object(Foo::Close(QN_ROOT)), // [Sut]  Root Close
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
        type Object = Foo;

        Sut := (A | B);

        A := QN_A {
            @ {} => Foo::A,
        }

        B := QN_B {
            @ {} => Foo::B,
        }
    }

    // Something >0 just to assert that we're actually paying attention to
    //   it when consuming tokens during recovery.
    let depth = Depth(5);
    let depth_child = Depth(6);

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
        // TODO: This references generated identifiers.
        ParseError::StateError(SutError_::UnexpectedEle_(
            unexpected,
            OpenSpan(S1, N).name_span(),
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::RootOpen,
            / => Foo::RootClose,

            // Two adjacent repeating followed by a non-repeating.
            // While there's nothing inherently concerning here,
            //   this is just meant to test both types of following states.
            ChildA[*],
            ChildB[*],
            ChildC,
        }

        ChildA := QN_A {
            @ {} => Foo::ChildOpen(QN_A),
            / => Foo::ChildClose(QN_A),
        }

        ChildB := QN_B {
            @ {} => Foo::ChildOpen(QN_B),
            / => Foo::ChildClose(QN_B),
        }

        ChildC := QN_C {
            @ {} => Foo::ChildOpen(QN_C),
            / => Foo::ChildClose(QN_C),
        }
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
            Incomplete,                    // [Sut]     Root Open
            Object(Foo::RootOpen),         // [Sut@]    ChildA Open (>LA)
            Incomplete,                    // [ChildA]  ChildA Open (<LA)
            Object(Foo::ChildOpen(QN_A)),  // [ChildA@] ChildA Close (>LA)
            Object(Foo::ChildClose(QN_A)), // [ChildA]  ChildA Close (<LA)
            Incomplete,                    // [ChildA]  ChildA Open (<LA)
            Object(Foo::ChildOpen(QN_A)),  // [ChildA@] ChildA Close (>LA)
            Object(Foo::ChildClose(QN_A)), // [ChildA]  ChildA Close (<LA)
            Incomplete,                    // [ChildB]  ChildB Open (<LA)
            Object(Foo::ChildOpen(QN_B)),  // [ChildB@] ChildB Close (>LA)
            Object(Foo::ChildClose(QN_B)), // [ChildB]  ChildB Close (<LA)
            Incomplete,                    // [ChildB]  ChildB Open (<LA)
            Object(Foo::ChildOpen(QN_B)),  // [ChildB@] ChildB Close (>LA)
            Object(Foo::ChildClose(QN_B)), // [ChildB]  ChildB Close (<LA)
            Incomplete,                    // [ChildC]  ChildC Open (<LA)
            Object(Foo::ChildOpen(QN_C)),  // [ChildC@] ChildC Close (>LA)
            Object(Foo::ChildClose(QN_C)), // [ChildC]  ChildC Close (<LA)
            Object(Foo::RootClose),        // [Sut]     Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::RootOpen,
            / => Foo::RootClose,

            Child[*],
        }

        Child := QN_CHILD {
            @ {} => Foo::ChildOpen,
            / => Foo::ChildClose,
        }
    }

    let toks = vec![
        XirfToken::Open(QN_ROOT, OpenSpan(S1, N), Depth(0)),
        // Child (success)
        XirfToken::Open(QN_CHILD, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        // Repeat (unexpected)
        XirfToken::Open(unexpected, OpenSpan(S2, N), Depth(1)),
        XirfToken::Close(None, CloseSpan::empty(S3), Depth(1)),
        XirfToken::Close(Some(QN_ROOT), CloseSpan(S8, N), Depth(0)),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    use Parsed::*;

    let mut next = || sut.next();

    assert_eq!(next(), Some(Ok(Incomplete))); // [Sut] Open
    assert_eq!(next(), Some(Ok(Object(Foo::RootOpen)))); // [Sut@] Open >
    assert_eq!(next(), Some(Ok(Incomplete))); // [Child] Open <
    assert_eq!(next(), Some(Ok(Object(Foo::ChildOpen)))); // [Child@] Close >
    assert_eq!(next(), Some(Ok(Object(Foo::ChildClose)))); // [Child] Close <

    // Intuitively,
    //   we may want to enter recovery and ignore the element.
    // But the problem is that we need to emit a dead state so that other
    //   parsers can handle the input,
    //     because it may simply be the case that our repetition is over.
    //
    // Given that dead state and token of lookahead,
    //   `Parser` will immediately recurse to re-process the erroneous
    //   `Open`.
    // Since the next token expected after the `Child` NT is `Close`,
    //   this will result in an error and trigger recovery _on `Sut`_,
    //     which will ignore the erroneous `Open`.
    assert_eq!(
        next(),
        // TODO: This references generated identifiers.
        Some(Err(ParseError::StateError(SutError_::CloseExpected_(
            OpenSpan(S1, N).tag_span(),
            XirfToken::Open(unexpected, OpenSpan(S2, N), Depth(1)),
        )))),
    );

    // This next token is also ignored as part of recovery.
    assert_eq!(next(), Some(Ok(Incomplete))); // [Sut] Child Close

    // Finally,
    //   `Sut` encounters its expected `Close` and ends recovery.
    assert_eq!(next(), Some(Ok(Object(Foo::RootClose)))); // [Sut] Close
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
        type Object = Foo;

        Sut := QN_PACKAGE {
            @ {} => Foo::Open(QN_ROOT),
            / => Foo::Close(QN_ROOT),

            // A|B|C in any order,
            //   any number of times.
            ABC[*],
        }

        ABC := (A | B | C );

        A := QN_A {
            @ {} => Foo::Open(QN_A),
            / => Foo::Close(QN_A),
        }

        B := QN_B {
            @ {} => Foo::Open(QN_B),
            / => Foo::Close(QN_B),
        }

        C := QN_C {
            @ {} => Foo::Open(QN_C),
            / => Foo::Close(QN_C),
        }
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
            Incomplete,                  // [Sut]  Root Open
            Object(Foo::Open(QN_ROOT)),  // [Sut@] A Open (>LA)
            Incomplete,                  // [A]  A Open (<LA)
            Object(Foo::Open(QN_A)),     // [A@] A Close (>LA)
            Object(Foo::Close(QN_A)),    // [A]  A Close (<LA)
            Incomplete,                  // [A]  A Open
            Object(Foo::Open(QN_A)),     // [A@] A Close (>LA)
            Object(Foo::Close(QN_A)),    // [A]  A Close (<LA)
            Incomplete,                  // [B]  B Open
            Object(Foo::Open(QN_B)),     // [B@] B Close (>LA)
            Object(Foo::Close(QN_B)),    // [B]  B Close (<LA)
            Incomplete,                  // [C]  C Open
            Object(Foo::Open(QN_C)),     // [C@] C Close (>LA)
            Object(Foo::Close(QN_C)),    // [C]  C Close (<LA)
            Incomplete,                  // [B]  B Open
            Object(Foo::Open(QN_B)),     // [B@] B Close (>LA)
            Object(Foo::Close(QN_B)),    // [B]  B Close (<LA)
            Object(Foo::Close(QN_ROOT)), // [Sut]  Root Close
        ]),
        Sut::parse(toks.into_iter()).collect(),
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
        vis(pub);

        type Object = ();

        ExportMe := QN_PACKAGE {
            @ {} => (),
        }
    }
}
