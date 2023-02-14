// Test automatically close elements of XIRF streams
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

use super::*;
use crate::{
    convert::ExpectInto,
    span::dummy::*,
    xir::{attr, CloseSpan, OpenSpan},
};

use XirfToken::*;

type Sut = XirfAutoClose;

// For the sake of brevity:
fn ospan(span: Span) -> OpenSpan {
    OpenSpan::without_name_span(span)
}
fn cspan(span: Span) -> CloseSpan {
    CloseSpan::without_name_span(span)
}

#[test]
fn auto_close_all_opens_at_end_of_stream_increasing_depth() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();

    #[rustfmt::skip]
    let toks = vec![
        Open(qn_a, ospan(S1), Depth(1)),
          Open(qn_b, ospan(S2), Depth(2)),
    ];

    use Parsed::Object as O;

    // Note that the physical depth below is not the same as the virtual
    //   depth above.
    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(qn_a, ospan(S1), Depth(0))),
              O(Open(qn_b, ospan(S2), Depth(1))),
              // Self-closing, because there are no children.
              O(Close(None, cspan(S2), Depth(1))),
            // But this does have children.
            O(Close(
                Some(qn_a),
                cspan(S1),
                Depth(0)
            )),
            Parsed::Incomplete, // implementation detail
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

#[test]
fn auto_close_siblings() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();

    let toks = vec![
        // Both are at the same depth.
        Open(qn_a, ospan(S1), Depth(1)),
        Open(qn_b, ospan(S2), Depth(1)),
    ];

    use Parsed::Object as O;

    assert_eq!(
        Ok(vec![
            // Both are self-closing with no children.
            O(Open(qn_a, ospan(S1), Depth(0))),
            O(Close(None, cspan(S1), Depth(0))),
            O(Open(qn_b, ospan(S2), Depth(0))),
            O(Close(None, cspan(S2), Depth(0))),
            Parsed::Incomplete,
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

#[test]
fn auto_close_ancestors() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();
    let qn_c = "c".unwrap_into();

    #[rustfmt::skip]
    let toks = vec![
        Open(qn_a, ospan(S1), Depth(1)),
          Open(qn_b, ospan(S2), Depth(2)),
        // Should close a, b
        Open(qn_c, ospan(S3), Depth(1)),
    ];

    use Parsed::Object as O;

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(qn_a, ospan(S1), Depth(0))),
              O(Open(qn_b, ospan(S2), Depth(1))),
              O(Close(None, cspan(S2), Depth(1))),
            O(Close(Some(qn_a), cspan(S1), Depth(0))),
            O(Open(qn_c, ospan(S3), Depth(0))),
            O(Close(None, cspan(S3), Depth(0))),
            Parsed::Incomplete,
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

// While this system does auto-close,
//   accepting close allows us to avoid an error condition and just do what
//   is intuitively correct,
//     which is not only a simpler implementation,
//     but results in an Infallible lowering operation,
//       which also simplifies the lowering pipeline.
// Because of the behavior of the system,
//   you aren't supposed to be able to _observe_ explicit closes,
//     though technically an implementation detail
//       (yielding `Incomplete`)
//       leaks some information about the behavior.
// So this test amounts to sprinkling them in and making sure the system
//   still operates as intended _despite_ them.
#[test]
fn close_closes_up_to_and_including_given_vdepth() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();
    let qn_c = "c".unwrap_into();

    #[rustfmt::skip]
    let toks = vec![
        // Nest deeply enough that we can close multiple while also ensuring
        //   that we're not relying on the behavior of auto-closing root.
        Open(qn_a, ospan(S1), Depth(1)),
          Open(qn_b, ospan(S2), Depth(2)),
            Open(qn_c, ospan(S3), Depth(3)),
          // Should close `b` and `c` properly,
          //   despite having a `None`.
          // The span will also be ignored,
          //   but let's provide a unique one so we can actually observe
          //   that.
          Close(None, cspan(S4), Depth(2)),
          Open(qn_b, ospan(S5), Depth(2)),
            Open(qn_c, ospan(S6), Depth(3)),
            Close(None, cspan(S7), Depth(3)),
                // This is some nonsense,
                //   closing at a depth we haven't encountered,
                //   and so it'll be ignored.
                Close(None, cspan(S4), Depth(5)),
        Open(qn_a, ospan(S8), Depth(1)),
        // And `a` will auto-close
    ];

    use Parsed::Object as O;

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(qn_a, ospan(S1), Depth(0))),
              O(Open(qn_b, ospan(S2), Depth(1))),
                O(Open(qn_c, ospan(S3), Depth(2))),
                // S4 not used
                O(Close(None, cspan(S3), Depth(2))),
              // S4 not used
              O(Close(Some(qn_b), cspan(S2), Depth(1))),
              Parsed::Incomplete,
              O(Open(qn_b, ospan(S5), Depth(1))),
                O(Open(qn_c, ospan(S6), Depth(2))),
                // S7 not used
                O(Close(None, cspan(S6), Depth(2))),
                Parsed::Incomplete,
                    // Nonsense should be ignored.
                    // This leaks information about the previous close,
                    //   since we can observe that auto-close hasn't
                    //   triggered yet for `b` or `a`.
                    Parsed::Incomplete,
              O(Close(Some(qn_b), cspan(S5), Depth(1))),
            O(Close(Some(qn_a), cspan(S1), Depth(0))),
            O(Open(qn_a, ospan(S8), Depth(0))),
            O(Close(None, cspan(S8), Depth(0))),
            Parsed::Incomplete,
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

// Attributes are proxied along without any changes,
//   since they do not carry depth information and do not affect whether an
//   element is self-closing.
// Since they carry no depth and must follow an `Open`,
//   we also do not need to concern ourselves with element association.
#[test]
fn attributes_proxied() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();
    let qn_attr = "attr".unwrap_into();
    let attr_val = "attr value".into();

    let attr_1 = Attr(attr::Attr::new(qn_attr, attr_val, (S2, S3)));
    let attr_2 = Attr(attr::Attr::new(qn_attr, attr_val, (S5, S6)));

    #[rustfmt::skip]
    let toks = vec![
        Open(qn_a, ospan(S1), Depth(1)),
          attr_1.clone(),
          Open(qn_b, ospan(S4), Depth(2)),
            attr_2.clone(),
    ];

    use Parsed::Object as O;

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(qn_a, ospan(S1), Depth(0))),
              O(attr_1),
              O(Open(qn_b, ospan(S4), Depth(1))),
                O(attr_2),
              O(Close(None, cspan(S4), Depth(1))),
            O(Close(Some(qn_a), cspan(S1), Depth(0))),
            Parsed::Incomplete,
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}

/////
// All tokens that are not `Open`, `Close`, or `Attr` have their depth
//   adjusted to match the current physical depth and are otherwise proxied
//   along.
// Furthermore,
//   commments; text; and CData count as children and so must not have a
//   self-closing parent node.
//
// If the virtual depth requires tag closing,
//   then auto-closing happens just the same as with `Open`.
// This is necessary for mixed content and to be able to interleave
//   comments with elements.

#[test]
fn comment_depth_adjusted_non_self_closing() {
    let qn_a = "a".unwrap_into();
    let qn_b = "b".unwrap_into();

    let comment_a = "child of a".into();
    let comment_b = "child of b".into();

    #[rustfmt::skip]
    let toks = vec![
        Open(qn_a, ospan(S1), Depth(1)),
          // `b` must no longer be self-closing
          Open(qn_b, ospan(S2), Depth(2)),
            Comment(comment_b, S3, Depth(3)),
          // Must close `b` so that it's a sibling
          Comment(comment_a, S4, Depth(2)),
    ];

    use Parsed::Object as O;

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(Open(qn_a, ospan(S1), Depth(0))),
              O(Open(qn_b, ospan(S2), Depth(1))),
                O(Comment(comment_b, S3, Depth(2))),
              O(Close(Some(qn_b), cspan(S2), Depth(1))),
              O(Comment(comment_a, S4, Depth(1))),
            O(Close(Some(qn_a), cspan(S1), Depth(0))),
            Parsed::Incomplete,
        ]),
        Sut::parse(toks.into_iter()).collect()
    );
}
