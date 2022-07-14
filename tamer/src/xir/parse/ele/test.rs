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

use crate::{
    convert::ExpectInto,
    parse::{Object, ParseError, ParseState, Parsed},
    span::{Span, DUMMY_SPAN},
    sym::SymbolId,
    xir::{
        attr::{Attr, AttrSpan},
        flat::{Depth, XirfToken},
        st::qname::*,
        CloseSpan, EleNameLen, EleSpan, OpenSpan, QName,
    },
};

const S1: Span = DUMMY_SPAN;
const S2: Span = S1.offset_add(1).unwrap();
const S3: Span = S2.offset_add(1).unwrap();
const S4: Span = S3.offset_add(1).unwrap();
const S5: Span = S4.offset_add(1).unwrap();
const S6: Span = S5.offset_add(1).unwrap();

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

    ele_parse! {
        type Object = Foo;

        // In practice we wouldn't actually use Attr
        //   (we'd use an appropriate newtype),
        //     but for the sake of this test we'll keep things simple.
        Sut := QN_PACKAGE {
            @ {
                name: (QN_NAME) => Attr,
                value: (QN_VALUE) => Attr,
            } => Foo(
                name.value(),
                value.value(),
                (name.attr_span().value_span(), value.attr_span().value_span())
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
    assert_eq!(
        // TODO: This references generated identifiers.
        Some(Err(ParseError::StateError(SutError_::UnexpectedEle_(
            unexpected,
            span.name_span()
        )))),
        sut.next(),
    );

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
    assert_eq!(
        // TODO: This references generated identifiers.
        Some(Err(ParseError::StateError(SutError_::ChildA(
            ChildAError_::UnexpectedEle_(unexpected, span.name_span())
        )))),
        sut.next(),
    );

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
    assert_eq!(
        sut.next(),
        // TODO: This references generated identifiers.
        Some(Err(ParseError::StateError(SutError_::UnexpectedEle_(
            unexpected,
            OpenSpan(S1, N).name_span(),
        )))),
    );

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
