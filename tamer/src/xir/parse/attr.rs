// XIR attribute parser generator
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

//! Attribute parser generator for parsing of [XIRF](super::super::flat).
//!
//! The parser generator is invoked via the macro
//!   [`attr_parse!`](crate::attr_parse),
//!   which expects a struct-like definition describing the mapping between
//!   attribute names and the final value.
//! It produces both an [`AttrParseState`] parser and a concrete struct to
//!   be yielded by the parser.
//! The parser completes upon reaching a dead state.
//!
//! Required attributes are checked after reaching a dead state,
//!   so attributes may appear in any order.
//! Further,
//!   the struct produced by the parser will contain an [`Option`] type
//!   _only_ for the optional fields,
//!     freeing the caller from having to deal with wrapped values.
//!
//! The parser automatically produces detailed error and diagnostic
//!   messages.

use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    fmt::ListDisplayWrapper,
    parse::ParseState,
    span::Span,
    xir::{attr::Attr, fmt::XmlAttrList, QName},
};
use std::{error::Error, fmt::Display};

pub type ElementQName = QName;

/// Error while parsing element attributes.
#[derive(Debug, PartialEq)]
pub enum AttrParseError<S: AttrParseState> {
    /// One or more required attributes are missing.
    ///
    /// Since required attributes are not checked until parsing is complete,
    ///   and that determination requires a token of lookahead,
    ///   this error produces a lookahead token that must be handled by the
    ///   caller.
    ///
    /// This also provices the actual [`AttrParseState`],
    ///   which can be used to retrieve the missing required attributes
    ///     (using [`AttrParseState::required_missing`]),
    ///       can be used to retrieve information about the attributes that
    ///         _have_ been successfully parsed,
    ///       and can be used to resume parsing if desired.
    ///
    /// The caller must determine whether to proceed with parsing of the
    ///   element despite these problems;
    ///     such recovery is beyond the scope of this parser.
    MissingRequired(S),

    /// An attribute was encountered that was not expected by this parser.
    ///
    /// Parsing may recover by simply ignoring this attribute.
    UnexpectedAttr(Attr, ElementQName),
}

impl<S: AttrParseState> Display for AttrParseError<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::MissingRequired(st) => {
                let ele_name = st.element_name();
                write!(f, "element `{ele_name}` missing required ")?;

                XmlAttrList::fmt(&st.required_missing(), f)
            }

            Self::UnexpectedAttr(attr, ele_name) => {
                write!(
                    f,
                    "element `{ele_name}` contains unexpected attribute `{attr}`"
                )
            }
        }
    }
}

impl<S: AttrParseState> Error for AttrParseError<S> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl<S: AttrParseState> Diagnostic for AttrParseError<S> {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::MissingRequired(st) => st
                .element_span()
                .error(format!(
                    "missing required {}",
                    XmlAttrList::wrap(&st.required_missing()),
                ))
                .into(),

            // TODO: help stating attributes that can appear instead
            Self::UnexpectedAttr(attr @ Attr(.., aspan), ele_name) => aspan
                .key_span()
                .error(format!("element `{ele_name}` cannot contain `{attr}`"))
                .into(),
        }
    }
}

/// Attribute parsing automaton.
///
/// These parsers are generated by [`attr_parse!`](crate::attr_parse).
pub trait AttrParseState: ParseState {
    /// Begin attribute parsing within the context of the provided element.
    ///
    /// This is used to provide diagnostic information.
    fn with_element(ele: QName, span: Span) -> Self;

    /// Name of the element being parsed.
    fn element_name(&self) -> QName;

    /// Span associated with the element being parsed.
    fn element_span(&self) -> Span;

    /// Attempt to narrow into the final type by checking the availability
    ///   of required attribute values.
    ///
    /// If a single missing required attribute does not have a value,
    ///   this will fail with [`AttrParseError::MissingRequired`],
    ///     which contains the parsing state that wraps every field type in
    ///     [`Option`].
    /// The caller may use this to recover as much data as it can,
    ///   or choose to allow it to fail with an error stating which fields
    ///   are missing.
    /// The list of missing fields is generated dynamically during
    ///   diagnostic reporting.
    fn finalize_attr(self) -> Result<Self::Object, AttrParseError<Self>>;

    /// Names of attributes that are required but do not yet have a value.
    fn required_missing(&self) -> Vec<QName>;
}

/// Parse attributes for the given element.
///
/// This function is useful when the type of [`AttrParseState`] `S` can be
///   inferred,
///     so that the expression reads more like natural language.
#[cfg(test)] // currently only used by tests; remove when ready
pub fn parse_attrs<S: AttrParseState>(ele: QName, span: Span) -> S {
    S::with_element(ele, span)
}

#[macro_export]
macro_rules! attr_parse {
    ($(#[$sattr:meta])*
        $vis:vis struct $state_name:ident -> $struct_name:ident {
            $(
                $(#[$fattr:meta])*
                $field:ident: ($qname:ident $($fmod:tt)?) => $ty:ty,
            )*
        }
    ) => {
        $(
            // This provides a nice error on $ty itself at the call site,
            //   rather than relying on `Into::into` to cause the error
            //   later on,
            //     which places the error inside the macro definition.
            assert_impl_all!($ty: From<crate::xir::attr::Attr>);
        )*

        #[doc=concat!("Parser producing [`", stringify!($struct_name), "`].")]
        ///
        /// Unlike the final type,
        ///   this is an intermediate representation that holds every
        ///   pending value within an [`Option`] as it awaits further input,
        ///     before finally being narrowed into
        #[doc=concat!("[`", stringify!($struct_name), "`].")]
        ///
        /// This object is exposed for recovery and error reporting on
        ///   [`AttrParseError::MissingRequired`].
        #[derive(Debug, PartialEq, Eq)]
        $vis struct $state_name {
            #[doc(hidden)]
            ___ctx: (crate::xir::QName, Span),
            #[doc(hidden)]
            ___done: bool,
            $(
                pub $field: Option<$ty>,
            )*
        }

        impl crate::xir::parse::AttrParseState for $state_name {
            fn with_element(ele: crate::xir::QName, span: Span) -> Self {
                Self {
                    ___ctx: (ele, span),
                    ___done: false,
                    $(
                        $field: None,
                    )*
                }
            }

            fn element_name(&self) -> crate::xir::QName {
                match self.___ctx {
                    (name, _) => name,
                }
            }

            fn element_span(&self) -> Span {
                match self.___ctx {
                    (_, span) => span,
                }
            }

            fn finalize_attr(
                self,
            ) -> Result<
                Self::Object,
                crate::xir::parse::AttrParseError<Self>,
            > {
                // Validate required fields before we start moving data.
                $(
                    $crate::attr_parse!(@if_missing_req $($fmod)? self.$field {
                        return Err(
                            crate::xir::parse::AttrParseError::MissingRequired(
                                self,
                            )
                        )
                    });
                )*

                let obj = $struct_name {
                    $(
                        $field: $crate::attr_parse!(
                            @maybe_value $($fmod)? self.$field
                        ),
                    )*
                };

                Ok(obj)
            }

            fn required_missing(&self) -> Vec<crate::xir::QName> {
                #[allow(unused_mut)]
                let mut missing = vec![];

                $(
                    $crate::attr_parse!(@if_missing_req $($fmod)? self.$field {
                        missing.push($qname);
                    });
                )*

                missing
            }
        }

        impl $state_name {
            fn done_with_element(ele: crate::xir::QName, span: Span) -> Self {
                use crate::xir::parse::attr::AttrParseState;

                let mut new = Self::with_element(ele, span);
                new.___done = true;
                new
            }
        }

        $(#[$sattr])*
        #[doc=""]
        #[doc=concat!(
            "This is produced by the parser [`",
            stringify!($state_name),
            "`]."
        )]
        #[derive(Debug, PartialEq)]
        $vis struct $struct_name {
            $(
                $(#[$fattr])*
                pub $field: $ty,
            )*
        }

        impl crate::parse::Object for $struct_name {}

        impl std::fmt::Display for $state_name {
            /// Additional error context shown in diagnostic messages for
            ///   certain variants of [`ParseError`].
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                // TODO
                write!(f, "parsing attributes")
            }
        }

        impl crate::parse::ParseState for $state_name {
            type Token = crate::xir::flat::XirfToken;
            type Object = $struct_name;
            type Error = crate::xir::parse::AttrParseError<Self>;

            fn parse_token(
                #[allow(unused_mut)]
                mut self,
                tok: Self::Token,
                _: crate::parse::NoContext,
            ) -> crate::parse::TransitionResult<Self> {
                use crate::parse::{Transition, Transitionable, ParseStatus};
                use crate::xir::{
                    flat,
                    parse::{AttrParseError, AttrParseState}
                };
                #[allow(unused_imports)]
                use crate::xir::attr::Attr; // unused if no attrs

                match tok {
                    $(
                        // Use guard so we don't bind as a variable if we
                        //   forget to import a const for `$qname`.
                        // We don't use `$qname:pat` because we reuse
                        //   `$qname` for error messages.
                        flat::XirfToken::Attr(attr @ Attr(qn, ..)) if qn == $qname => {
                            // TODO: Error on prev value
                            self.$field.replace(attr.into());
                            Transition(self).incomplete()
                        }
                    )*

                    flat::XirfToken::Attr(attr) => {
                        let ele_name = self.element_name();

                        Transition(self).err(AttrParseError::UnexpectedAttr(
                            attr,
                            ele_name,
                        ))
                    },

                    // Any tokens received after aggregation is completed
                    //   must not be processed,
                    //     otherwise we'll recurse indefinitely.
                    tok_dead if self.___done => {
                        Transition(self).dead(tok_dead)
                    },

                    // Aggregation complete (dead state).
                    tok_dead => {
                        let (ele, span) = self.___ctx;

                        self.finalize_attr()
                            .map(ParseStatus::Object)
                            .transition(Self::done_with_element(ele, span))
                            .with_lookahead(tok_dead)
                    }
                }
            }

            fn is_accepting(&self) -> bool {
                // We must always be consumed via the dead state.
                false
            }
        }
    };

    // Optional attribute if input above is of the form `(QN_FOO?) => ...`.
    (@if_missing_req ? $from:ident.$field:ident $body:block) => {
        // This is an optional field;
        //   do nothing.
    };

    // Otherwise required.
    (@if_missing_req $from:ident.$field:ident $body:block) => {
        if $from.$field.is_none() {
            $body
        }
    };

    // Optional attribute if input above is of the form `(QN_FOO?) => ...`.
    (@maybe_value ? $from:ident.$field:ident) => {
        // This does not produce a great error if the user forgets to use an
        //   `Option` type for optional attributes,
        //     but the comment is better than nothing.
        $from.$field.unwrap_or(None) // field type must be Option<T>
    };

    // Otherwise,
    //   if the above `@maybe_value` does not match,
    //   the attribute is required.
    (@maybe_value $from:ident.$field:ident) => {
        // This assumes that we've already validated via
        //   `@validate_req` above,
        //     and so should never actually panic.
        $from.$field.unwrap()
    };
}

#[cfg(test)]
mod test {
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

    const S1: Span = DUMMY_SPAN;
    const S2: Span = S1.offset_add(1).unwrap();
    const S3: Span = S2.offset_add(1).unwrap();
    const SE: Span = S1.offset_add(100).unwrap();

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

        const ATTR_NAME: Attr =
            Attr(QN_NAME, st::raw::L_NAME, AttrSpan(S1, S2));
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
                        name: Some(ref given_name),
                        src: None, // cause of the error
                        ty: None, // another cause of the error
                        yields: Some(ref given_yields),
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
            let mut partial = ReqMissingState::with_element(QN_ELE, S1);
            partial.name.replace(ATTR_NAME);
            partial.yields.replace(ATTR_YIELDS);

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
            let mut partial = ReqMissingState::with_element(QN_ELE, S1);
            partial.name.replace(ATTR_NAME);
            partial.yields.replace(ATTR_YIELDS);

            let err = AttrParseError::MissingRequired(partial);
            let desc = err.describe();

            // The diagnostic message should reference the element.
            assert_eq!(desc[0].span(), S1);

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
        let attr_unexpected =
            Attr(QN_TYPE, "unexpected".into(), AttrSpan(S1, S2));
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
}
