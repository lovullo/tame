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
    xir::{attr::Attr, fmt::XmlAttrList, EleSpan, OpenSpan, QName},
};
use std::{convert::Infallible, error::Error, fmt::Display};

pub type ElementQName = QName;
pub type FirstSpan = Span;

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

    /// An attribute with the same name as a previous attribute has been
    ///   encountered within the context of this element.
    ///
    /// The duplicate attribute is provided in its entirety.
    /// The key span of the first-encountered attribute of the same name is
    ///   included to provide more robust diagnostic information.
    /// The value of the previous attribute is not included because it is
    ///   expected that the diagnostic system will render the code
    ///   associated with the span;
    ///     displaying an attribute value in an error message is asking for
    ///     too much trouble given that it is arbitrary text.
    DuplicateAttr(Attr, FirstSpan, ElementQName),

    /// An error occurred while parsing an attribute value into the
    ///   declared type.
    InvalidValue(S::ValueError, ElementQName),
}

impl<S: AttrParseState> Display for AttrParseError<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::fmt::{DisplayWrapper, TtQuote};

        match self {
            Self::MissingRequired(st) => {
                let ele_name = st.element_name();
                write!(f, "element `{ele_name}` missing required ")?;

                XmlAttrList::fmt(&st.required_missing(), f)
            }

            Self::UnexpectedAttr(attr, ele_name) => {
                write!(
                    f,
                    "unexpected attribute `{attr}` for \
                       element element `{ele_name}`"
                )
            }

            Self::DuplicateAttr(attr, _, ele_name) => {
                write!(
                    f,
                    "duplicate attribute `{attr}` for \
                       element element `{ele_name}`"
                )
            }

            Self::InvalidValue(ev, ele_name) => {
                Display::fmt(ev, f)?;
                write!(f, " for element {}", TtQuote::wrap(ele_name))
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
        use crate::fmt::{DisplayWrapper, TtQuote};

        match self {
            Self::MissingRequired(st) => st
                .element_span()
                .tag_span()
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

            Self::DuplicateAttr(Attr(name, _, aspan), first_span, _) => {
                vec![
                    first_span.note(format!(
                        "{} previously encountered here",
                        TtQuote::wrap(name)
                    )),
                    aspan.key_span().error(format!(
                        "{} here is a duplicate",
                        TtQuote::wrap(name)
                    )),
                ]
            }

            Self::InvalidValue(ev, _) => ev.describe(),
        }
    }
}

/// Attribute parsing automaton.
///
/// These parsers are generated by [`attr_parse!`](crate::attr_parse).
pub trait AttrParseState: ParseState {
    /// Type of error for failed parsing of attribute values.
    ///
    /// These originate from [`TryFrom`] conversions on the attribute
    ///   value.
    /// The default is [`Infallible`],
    ///   meaning such conversion cannot fail and [`From`] may be used in
    ///   place of [`TryFrom`].
    type ValueError: Diagnostic = Infallible;

    /// Begin attribute parsing within the context of the provided element.
    ///
    /// This is used to provide diagnostic information.
    fn with_element(ele: QName, span: OpenSpan) -> Self;

    /// Name of the element being parsed.
    fn element_name(&self) -> QName;

    /// Span associated with the element being parsed.
    fn element_span(&self) -> OpenSpan;

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
pub fn parse_attrs<S: AttrParseState>(ele: QName, span: OpenSpan) -> S {
    S::with_element(ele, span)
}

#[macro_export]
macro_rules! attr_parse {
    ($(#[$sattr:meta])*
        $(type ValueError = $evty:ty;)?

        struct $state_name:ident -> $struct_name:ident {
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
            $crate::attr_parse!(@ty_assert $($fmod)? $ty);
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
        struct $state_name {
            #[doc(hidden)]
            ___ctx: (crate::xir::QName, crate::xir::OpenSpan),
            #[doc(hidden)]
            ___done: bool,
            $(
                // Value + key span
                pub $field: Option<($ty, crate::span::Span)>,
            )*
        }

        impl crate::xir::parse::AttrParseState for $state_name {
            type ValueError = $crate::attr_parse!(@evty $($evty)?);

            fn with_element(
                ele: crate::xir::QName,
                span: crate::xir::OpenSpan
            ) -> Self {
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

            fn element_span(&self) -> crate::xir::OpenSpan {
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
            fn done_with_element(
                ele: crate::xir::QName,
                span: crate::xir::OpenSpan,
            ) -> Self {
                use crate::xir::parse::AttrParseState;

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
        struct $struct_name {
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
                use crate::fmt::{DisplayWrapper, TtQuote};

                match self {
                    Self { ___ctx: (ele, _), .. } => {
                        write!(
                            f,
                            "expecting attributes for element {}",
                            TtQuote::wrap(ele)
                        )
                    }
                }
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
                use crate::xir::attr::{Attr, AttrSpan}; // unused if no attrs

                let ele_name = self.element_name();

                match tok {
                    $(
                        // Use guard so we don't bind as a variable if we
                        //   forget to import a const for `$qname`.
                        // We don't use `$qname:pat` because we reuse
                        //   `$qname` for error messages.
                        flat::XirfToken::Attr(
                            attr @ Attr(qn, _, AttrSpan(kspan, _))
                        ) if qn == $qname => {
                            match self.$field {
                                // Duplicate attribute name
                                Some((_, first_kspan)) => {
                                    Transition(self).err(
                                        AttrParseError::DuplicateAttr(
                                            attr,
                                            first_kspan,
                                            ele_name,
                                        )
                                    )
                                }

                                // First time seeing attribute name
                                None => {
                                    let result = $crate::attr_parse!(
                                        @into_value $($fmod)? attr
                                    );

                                    match result {
                                        Ok(value) => {
                                            self.$field.replace((
                                                value,
                                                kspan,
                                            ));

                                            Transition(self).incomplete()
                                        },

                                        Err(e) => Transition(self).err(
                                            // Will complain about
                                            //   `Into::into` if Infallible.
                                            #[allow(unreachable_code)]
                                            AttrParseError::InvalidValue(
                                                e.into(),
                                                ele_name,
                                            )
                                        ),
                                    }
                                }
                            }
                        }
                    )*

                    flat::XirfToken::Attr(attr) => {
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
    (@ty_assert ? $ty:ty) => {
        // This type assertion isn't supported by `assert_impl_all!`.
        // The error isn't the most clear,
        //   but it's better than nothing and we can improve upon it later
        //   on.
        const _: fn() = || {
            trait OptionFromAttr {}
            impl<T: TryFrom<Attr>> OptionFromAttr for Option<T> {}

            // Fail when `$ty` is not Option<impl TryFrom<Attr>>.
            fn assert_attr_option<T: OptionFromAttr>() {}
            assert_attr_option::<$ty>();
        };
    };

    (@ty_assert $ty:ty) => {
        assert_impl_all!($ty: TryFrom<crate::xir::attr::Attr>);
    };

    (@evty $evty:ty) => {
        $evty
    };

    // If no ValueError type is provided,
    //   then it's not possible for values to fail parsing
    //     (their SymbolId is their final value).
    (@evty) => {
        std::convert::Infallible
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
    (@into_value ? $from:ident) => {
        $from.try_into().map(Some)
    };

    (@into_value $from:ident) => {
        $from.try_into()
    };

    // Optional attribute if input above is of the form `(QN_FOO?) => ...`.
    (@maybe_value ? $from:ident.$field:ident) => {
        // This does not produce a great error if the user forgets to use an
        //   `Option` type for optional attributes,
        //     but the comment is better than nothing.
        match $from.$field { // field type must be Option<T>
            Some((value, _kspan)) => value,
            None => None,
        }
    };

    // Otherwise,
    //   if the above `@maybe_value` does not match,
    //   the attribute is required.
    (@maybe_value $from:ident.$field:ident) => {
        // This assumes that we've already validated via
        //   `@validate_req` above,
        //     and so should never actually panic.
        match $from.$field.unwrap() {
            (value, _kspan) => value
        }
    };
}

#[cfg(test)]
mod test;
