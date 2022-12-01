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
//!   which expects a `match`-like definition describing the mapping between
//!   attribute [QNames](crate::xir::QName) and a value derived from the
//!   attribute value.
//! It produces a streaming attribute parser.
//!
//! All fields recognized by this parser are implicitly optional,
//!   as this is intended only to extract a grammar from an XML document.
//! This destructuring describes the _permissable attributes_ of an element,
//!   but nothing more.
//! Whether or not an attribute is required should be determined by whether
//!   the produced IR is missing necessary information,
//!     which is a later lowering operation.
//! Further,
//!   duplicate attributes should be inhibited earlier in the process by XIR,
//!   if desired.
//!
//! The parser automatically produces detailed error and diagnostic
//!   messages for unexpected attributes,
//!     or attributes that cannot be parsed into the final type.

#[macro_export]
macro_rules! attr_parse_stream {
    ($(#[$sattr:meta])*
        type Object = $objty:ty;
        type ValueError = $evty:ty;

        $(#[$st_attr:meta])?
        $vis:vis $state_name:ident {
            $(
                $(#[$fattr:meta])*
                $qname:ident => $attrf:expr,
            )*
        }
    ) => { paste::paste! {
        $(#[$st_attr])?
        ///
        #[doc=concat!("Parser producing [`", stringify!($struct_name), "`].")]
        // TODO: This can be extracted out of the macro.
        #[derive(Debug, PartialEq, Eq)]
        $vis enum $state_name {
            Parsing(crate::xir::QName, crate::xir::OpenSpan),
            Done(crate::xir::QName, crate::xir::OpenSpan),
        }

        /// Intermediate state of parser as fields are aggregated.
        ///
        /// TODO: Remove once integrated with `ele_parse!`.
        #[allow(non_camel_case_types)]
        #[derive(Debug, PartialEq, Eq, Default)]
        $vis struct [<$state_name Fields>];

        impl crate::xir::parse::AttrParseState for $state_name {
            type ValueError = $evty;
            type Fields = [<$state_name Fields>];

            fn with_element(
                ele: crate::xir::QName,
                span: crate::xir::OpenSpan
            ) -> Self {
                Self::Parsing(ele, span)
            }

            fn element_name(&self) -> crate::xir::QName {
                match self {
                    Self::Parsing(qname, _) | Self::Done(qname, _) => *qname,
                }
            }

            fn element_span(&self) -> crate::xir::OpenSpan {
                match self {
                    Self::Parsing(_, span) | Self::Done(_, span) => *span,
                }
            }

            fn finalize_attr(
                self,
                _ctx: &mut <Self as crate::parse::ParseState>::Context,
            ) -> Result<
                Self::Object,
                crate::xir::parse::AttrParseError<Self>,
            > {
                unimplemented!("attrstream finalize_attr")
            }

            fn required_missing(
                &self,
                #[allow(unused_variables)] // unused if no fields
                _ctx: &Self::Fields
            ) -> Vec<crate::xir::QName> {
                unimplemented!("attrstream required_missing")
            }
        }

        impl std::fmt::Display for $state_name {
            /// Additional error context shown in diagnostic messages for
            ///   certain variants of [`ParseError`].
            ///
            /// [`ParseError`]: crate::parse::ParseError
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use crate::fmt::{DisplayWrapper, TtQuote};
                use crate::xir::parse::AttrParseState;

                write!(
                    f,
                    "expecting attributes for element {}",
                    TtQuote::wrap(self.element_name())
                )
            }
        }

        impl crate::parse::ParseState for $state_name {
            type Token = crate::xir::flat::XirfToken<
                crate::xir::flat::RefinedText
            >;
            type Object = $objty;
            type Error = crate::xir::parse::AttrParseError<Self>;

            fn parse_token(
                #[allow(unused_mut)]
                mut self,
                tok: Self::Token,
                _ctx: &mut Self::Context,
            ) -> crate::parse::TransitionResult<Self> {
                use crate::parse::Transition;
                use crate::xir::{
                    flat,
                    parse::{AttrParseError, AttrParseState}
                };
                #[allow(unused_imports)] // unused if no attrs
                use crate::{
                    parse::{Transitionable, ParseStatus, util::SPair},
                    xir::attr::{Attr, AttrSpan}
                };

                let ele_name = self.element_name();

                match (self, tok) {
                    $(
                        // Use guard so we don't bind as a variable if we
                        //   forget to import a const for `$qname`.
                        // We don't use `$qname:pat` because we reuse
                        //   `$qname` for error messages.
                        (st @ Self::Parsing(_, _), flat::XirfToken::Attr(
                            Attr(qn, v, AttrSpan(_, vspan))
                        )) if qn == $qname => {
                            match Into::<Result<$objty, $evty>>::into($attrf(SPair(v, vspan))) {
                                Ok(value) => {
                                    Transition(st).ok::<$objty>(value)
                                },

                                Err(e) => Transition(st).err(
                                    // Unreachable `Into::into` if
                                    //   Infallible.
                                    #[allow(unreachable_code)]
                                    AttrParseError::InvalidValue(
                                        Into::<$evty>::into(e),
                                        ele_name,
                                    )
                                ),
                            }
                        }
                    )*

                    (st @ Self::Parsing(_, _), flat::XirfToken::Attr(attr)) => {
                        Transition(st).err(AttrParseError::UnexpectedAttr(
                            attr,
                            ele_name,
                        ))
                    },

                    // Aggregation complete (dead state).
                    (Self::Parsing(ele, span), tok_dead) => {
                        Transition(Self::Done(ele, span)).dead(tok_dead)
                    }

                    // Any tokens received after aggregation is completed
                    //   must not be processed,
                    //     otherwise we'll recurse indefinitely.
                    (st @ Self::Done(_, _), tok_dead) => {
                        Transition(st).dead(tok_dead)
                    }
                }
            }

            fn is_accepting(&self, _: &Self::Context) -> bool {
                // All attributes are optional for this parser,
                //   and each token is a complete attribute.
                true
            }
        }
    } };
}

#[cfg(test)]
mod test;
