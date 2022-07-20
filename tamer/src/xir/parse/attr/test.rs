// XIR attribute parser generator tests
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
use crate::{
    parse::{ParseError, ParseState, Parsed, Parser, TokenStream},
    span::{Span, DUMMY_SPAN},
    xir::{
        attr::{Attr, AttrSpan},
        flat::{test::close_empty, Depth, XirfToken},
        st::qname::*,
    },
};
use std::assert_matches::assert_matches;

const S1: Span = DUMMY_SPAN.offset_add(1).unwrap();
const S2: Span = S1.offset_add(1).unwrap();
const S3: Span = S2.offset_add(1).unwrap();
const SE: OpenSpan = OpenSpan(S1.offset_add(100).unwrap(), 0);

// Random choice of QName for tests.
const QN_ELE: QName = QN_YIELDS;

fn parse_aggregate<S: AttrParseState>(
    toks: impl TokenStream<S::Token>,
) -> Result<(S::Object, S::Token), ParseError<S::Token, S::Error>>
where
    S: AttrParseState,
    S::Context: Default,
{
    parse_aggregate_with(&mut Parser::with_state(
        S::with_element(QN_ELE, SE),
        toks,
    ))
}

fn parse_aggregate_with<S: AttrParseState, I>(
    sut: &mut Parser<S, I>,
) -> Result<(S::Object, S::Token), ParseError<S::Token, S::Error>>
where
    S: ParseState,
    S::Context: Default,
    I: TokenStream<S::Token>,
{
    let mut obj = None;

    for item in sut {
        match item {
            Ok(Parsed::Object(result)) => {
                obj.replace(result);
            }
            Ok(Parsed::Incomplete) => continue,
            // This represents the dead state,
            //   since this is the top-level parser.
            Err(ParseError::UnexpectedToken(tok, _)) => {
                return Ok((
                    obj.expect(
                        "parser did not produce aggregate attribute object",
                    ),
                    tok,
                ))
            }
            Err(other) => return Err(other),
        }
    }

    panic!("expected AttrParseState dead state (obj: {obj:?})");
}

#[test]
fn required_with_values() {
    attr_parse! {
        struct ReqValuesState -> ReqValues {
            name: (QN_NAME) => Attr,
            yields: (QN_YIELDS) => Attr,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, "val_value".into(), AttrSpan(S2, S3));
    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        XirfToken::Attr(attr_name.clone()),
        XirfToken::Attr(attr_yields.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    assert_eq!(
        Ok((
            ReqValues {
                name: attr_name,
                yields: attr_yields,
            },
            tok_dead
        )),
        parse_aggregate::<ReqValuesState>(toks),
    );
}

// Same as above test,
//   but the order of the tokens is swapped.
#[test]
fn required_with_values_out_of_order() {
    attr_parse! {
        struct ReqValuesState -> ReqValues {
            name: (QN_NAME) => Attr,
            yields: (QN_YIELDS) => Attr,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, "val_value".into(), AttrSpan(S2, S3));
    let tok_dead = close_empty(S3, Depth(0));

    // @yields then @name just to emphasize that order does not matter.
    let toks = vec![
        XirfToken::Attr(attr_yields.clone()),
        XirfToken::Attr(attr_name.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    assert_eq!(
        Ok((
            ReqValues {
                name: attr_name,
                yields: attr_yields,
            },
            tok_dead
        )),
        parse_aggregate::<ReqValuesState>(toks),
    );
}

#[test]
fn optional_with_values() {
    attr_parse! {
        struct OptValuesState -> OptValues {
            name: (QN_NAME?) => Option<Attr>,
            yields: (QN_YIELDS?) => Option<Attr>,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_yields = Attr(QN_YIELDS, "val_value".into(), AttrSpan(S2, S3));
    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        XirfToken::Attr(attr_name.clone()),
        XirfToken::Attr(attr_yields.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    assert_eq!(
        Ok((
            OptValues {
                name: Some(attr_name),
                yields: Some(attr_yields),
            },
            tok_dead
        )),
        parse_aggregate::<OptValuesState>(toks),
    );
}

#[test]
fn optional_with_all_missing() {
    attr_parse! {
        struct OptMissingState -> OptMissing {
            name: (QN_NAME?) => Option<Attr>,
            yields: (QN_YIELDS?) => Option<Attr>,
        }
    }

    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    assert_eq!(
        Ok((
            OptMissing {
                name: None,
                yields: None,
            },
            tok_dead
        )),
        parse_aggregate::<OptMissingState>(toks),
    );
}

#[test]
fn mixed_some_optional_missing() {
    attr_parse! {
        struct MixedState -> Mixed {
            name: (QN_NAME) => Attr,
            src: (QN_SRC?) => Option<Attr>,
            yields: (QN_YIELDS?) => Option<Attr>,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_src = Attr(QN_SRC, "val_src".into(), AttrSpan(S2, S3));
    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        // `name` and `src` but no optional `yields`.
        XirfToken::Attr(attr_name.clone()),
        XirfToken::Attr(attr_src.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    assert_eq!(
        Ok((
            Mixed {
                name: attr_name,
                src: Some(attr_src),
                yields: None,
            },
            tok_dead
        )),
        parse_aggregate::<MixedState>(toks),
    );
}

mod required {
    use super::*;
    use crate::sym::st;

    attr_parse! {
        struct ReqMissingState -> ReqMissing {
            name: (QN_NAME) => Attr,
            src: (QN_SRC) => Attr,
            ty: (QN_TYPE) => Attr,
            yields: (QN_YIELDS) => Attr,
        }
    }

    const ATTR_NAME: Attr = Attr(QN_NAME, st::raw::L_NAME, AttrSpan(S1, S2));
    const ATTR_YIELDS: Attr =
        Attr(QN_YIELDS, st::raw::L_VALUE, AttrSpan(S2, S3));

    #[test]
    fn required_missing_values() {
        let tok_dead = close_empty(S3, Depth(0));

        let toks = vec![
            XirfToken::Attr(ATTR_NAME),
            // <Missing @src, but no error yet.>
            // <Missing @type, but no error yet.>
            XirfToken::Attr(ATTR_YIELDS),
            // Will cause dead state,
            //   which will then trigger the error:
            tok_dead.clone(),
        ]
        .into_iter();

        let err = parse_aggregate::<ReqMissingState>(toks)
            .expect_err("expected failure from missing attributes");

        // The error should provide the state of the parser during the
        //   finalization step.
        // Since this happens in a dead state,
        //   we must also receive the token that triggered it,
        //   just as we would normally receive on successful parsing.
        assert_matches!(
            err,
            ParseError::StateError(AttrParseError::MissingRequired(
                ReqMissingState {
                    name: Some((ref given_name, _)),
                    src: None, // cause of the error
                    ty: None, // another cause of the error
                    yields: Some((ref given_yields, _)),
                    ..
                },
            )) if given_name == &ATTR_NAME
                && given_yields == &ATTR_YIELDS
        );
    }

    /// Relies on [`required_missing_values`] above to verify state of the
    ///   parser used in the error.
    #[test]
    fn error_contains_all_required_missing_attr_names() {
        // Manually construct the partial state rather than parsing tokens.
        // `required_missing_values` above verifies that this state is what
        //   is in fact constructed from a failed parsing attempt.
        let mut partial = ReqMissingState::with_element(QN_ELE, SE);
        partial.name.replace((ATTR_NAME, S1));
        partial.yields.replace((ATTR_YIELDS, S2));

        let err = AttrParseError::MissingRequired(partial);

        // When represented as a string,
        //   the error should produce _all_ required attributes that do not
        //   have values,
        //     rather than requiring the user to fix one and re-compile only
        //     to encounter another,
        //       and potentially repeat multiple times.
        let err_str = err.to_string();
        assert!(
            err_str.contains(&format!("@{QN_SRC}")),
            "\"{err_str}\" must contain \"@{QN_SRC}\""
        );
        assert!(
            err_str.contains(&format!("@{QN_TYPE}")),
            "\"{err_str}\" must contain \"@{QN_TYPE}\""
        );

        // The error should also reference the element name
        //   (which is provided in `parse_aggregate`).
        assert!(
            err_str.contains(&QN_ELE.to_string()),
            "\"{err_str}\" must contain name of element being parsed"
        );
    }

    /// See also [`error_contains_all_required_missing_attr_names`].
    #[test]
    fn diagnostic_message_contains_all_required_missing_attr_name() {
        let mut partial = ReqMissingState::with_element(QN_ELE, SE);
        partial.name.replace((ATTR_NAME, S1));
        partial.yields.replace((ATTR_YIELDS, S2));

        let err = AttrParseError::MissingRequired(partial);
        let desc = err.describe();

        // The diagnostic message should reference the element.
        assert_eq!(desc[0].span(), SE.span());

        // It should re-state the required attributes,
        //   since this is where the user will most likely be looking.
        let label_str = desc[0]
            .label()
            .expect("missing diagnostic label")
            .to_string();

        assert!(
            label_str.contains(&format!("@{QN_SRC}")),
            "diagnostic label \"{label_str}\" must contain \"@{QN_SRC}\""
        );
        assert!(
            label_str.contains(&format!("@{QN_TYPE}")),
            "diagnostic label \"{label_str}\" must contain \"@{QN_TYPE}\""
        );
    }
}

#[test]
fn unexpected_attr_with_recovery() {
    attr_parse! {
        struct UnexpectedState -> Unexpected {
            name: (QN_NAME) => Attr,
            src: (QN_SRC) => Attr,
        }
    }

    let attr_name = Attr(QN_NAME, "val_name".into(), AttrSpan(S1, S2));
    let attr_unexpected = Attr(QN_TYPE, "unexpected".into(), AttrSpan(S1, S2));
    let attr_src = Attr(QN_SRC, "val_src".into(), AttrSpan(S2, S3));
    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        // This is expected:
        XirfToken::Attr(attr_name.clone()),
        // NOT expected (produce an error):
        XirfToken::Attr(attr_unexpected.clone()),
        // <Recovery must take place here.>
        // This is expected after recovery:
        XirfToken::Attr(attr_src.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    let mut sut =
        Parser::with_state(UnexpectedState::with_element(QN_ELE, SE), toks);

    // This will fail at the unknown attribute,
    //   and must then remain in a state where parsing can be resumed.
    // This simply means ignoring the provided attribute,
    //   which in XIRF is discarding a single token of input,
    //   rather than having to continue parsing the attribute to then
    //     discard.
    assert_eq!(
        Err(ParseError::StateError(AttrParseError::UnexpectedAttr(
            attr_unexpected,
            QN_ELE,
        ))),
        parse_aggregate_with(&mut sut),
    );

    // The final result,
    //   after having failed and recovered.
    assert_eq!(
        Ok((
            Unexpected {
                name: attr_name,
                src: attr_src,
            },
            tok_dead
        )),
        parse_aggregate_with(&mut sut),
    );
}

// A duplicate attribute will result in an error,
//   and recovery will cause the duplicate to be ignored.
#[test]
fn duplicate_attr_with_recovery() {
    attr_parse! {
        struct DupState -> Dup {
            name: (QN_NAME) => Attr,
            src: (QN_SRC) => Attr,
        }
    }

    let attr_keep = Attr(QN_NAME, "keep me".into(), AttrSpan(S1, S2));
    let attr_dup = Attr(QN_NAME, "duplicate".into(), AttrSpan(S2, S3));
    let attr_src = Attr(QN_SRC, "val_src".into(), AttrSpan(S3, S1));
    let tok_dead = close_empty(S3, Depth(0));

    let toks = vec![
        // Both of these have the same name (@name).
        XirfToken::Attr(attr_keep.clone()),
        XirfToken::Attr(attr_dup.clone()),
        // Another attribute just to show that error recovery permits
        //   further attribute parsing.
        XirfToken::Attr(attr_src.clone()),
        // Will cause dead state:
        tok_dead.clone(),
    ]
    .into_iter();

    let mut sut = Parser::with_state(DupState::with_element(QN_ELE, SE), toks);

    // The first token is good,
    //   since we haven't encountered the attribute yet.
    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));

    // The second one results in an error,
    //   since the name is the same.
    let err = sut
        .next()
        .unwrap()
        .expect_err("DuplicateAttr error expected");

    assert_eq!(
        ParseError::StateError(AttrParseError::DuplicateAttr(
            attr_dup,
            attr_keep.attr_span().key_span(),
            QN_ELE,
        )),
        err,
    );

    // The diagnostic description of this error should contain first a
    //   reference to the original attribute,
    //     and then a reference to the duplicate.
    let desc = err.describe();
    assert_eq!(desc[0].span(), S1);
    assert_eq!(desc[1].span(), S2);

    // Once parsing is completed,
    //   we must have kept the first occurrence of the attribute and
    //   discarded the second.
    assert_eq!(
        Ok((
            Dup {
                name: attr_keep,
                src: attr_src,
            },
            tok_dead
        )),
        parse_aggregate_with(&mut sut),
    );
}
