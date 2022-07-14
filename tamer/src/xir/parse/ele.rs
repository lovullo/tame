// XIR element parser generator
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

//! Element parser generator for parsing of [XIRF](super::super::flat).

use crate::parse::ParseState;

/// A parser accepting a single element.
pub trait EleParseState: ParseState {}

#[macro_export]
macro_rules! ele_parse {
    (type Object = $objty:ty; $($rest:tt)*) => {
        ele_parse!(@!nonterm_decl <$objty> $($rest)*)
    };

    (@!nonterm_decl <$objty:ty> $nt:ident := $($rest:tt)*) => {
        ele_parse!(@!nonterm_def <$objty> $nt $($rest)*);
    };

    (@!nonterm_def <$objty:ty>
        $nt:ident $qname:ident $(($($ntp:tt)*))?
        { $($matches:tt)* } $($rest:tt)*
    ) => {
        ele_parse!(@!ele_expand_body <$objty> $nt $qname ($($($ntp)*)?) $($matches)*);

        ele_parse! {
            type Object = $objty;
            $($rest)*
        }
    };

    (@!nonterm_def <$objty:ty> $nt:ident
        ($ntref_first:ident $(| $ntref:ident)+); $($rest:tt)*
    ) => {
        ele_parse!(@!ele_dfn_sum <$objty> $nt [$ntref_first $($ntref)*]);

        ele_parse! {
            type Object = $objty;
            $($rest)*
        }
    };

    (@!nonterm_decl <$objty:ty>) => {};

    // Expand the provided data to a more verbose form that provides the
    //   context necessary for state transitions.
    (@!ele_expand_body <$objty:ty> $nt:ident $qname:ident ($($ntp:tt)*)
        @ { $($attrbody:tt)* } => $attrmap:expr,
        $(/$(($close_span:ident))? => $closemap:expr,)?

        // Nonterminal references are provided as a list.
        $(
            $ntref:ident,
        )*
    ) => {
        ele_parse! {
            @!ele_dfn_body <$objty> $nt $qname ($($ntp)*)
            @ { $($attrbody)* } => $attrmap,
            /$($($close_span)?)? => ele_parse!(@!ele_close $($closemap)?),

            <> {
                $(
                    $ntref,
                )*
            }

            // Generate state transitions of the form `(S) -> (S')`.
            -> {
                @ ->
                $(
                    ($nt::$ntref),
                    ($nt::$ntref) ->
                )* ($nt::ExpectClose_),
            }
        }
    };

    // No explicit Close mapping defaults to doing nothing at all
    //   (so yield Incomplete).
    (@!ele_close) => {
        crate::parse::ParseStatus::Incomplete
    };

    (@!ele_close $close:expr) => {
        crate::parse::ParseStatus::Object($close)
    };

    (@!ele_dfn_body <$objty:ty> $nt:ident $qname:ident ($($open_span:ident)?)
        // Attribute definition special form.
        @ {
            // We must lightly parse attributes here so that we can retrieve
            //   the field identifiers that may be later used as bindings in
            //   `$attrmap`.
            $(
                $(#[$fattr:meta])*
                $field:ident: ($fmatch:tt) => $fty:ty,
            )*
        } => $attrmap:expr,

        // Close expression
        //   (defaulting to Incomplete via @!ele_expand_body).
        /$($close_span:ident)? => $closemap:expr,

        // Nonterminal references.
        <> {
            $(
                $ntref:ident,
            )*
        }

        -> {
            @ -> ($ntfirst:path),
            $(
                ($ntprev:path) -> ($ntnext:path),
            )*
        }
    ) => {
        paste::paste! {
            crate::attr_parse! {
                struct [<$nt AttrsState_>] -> [<$nt Attrs_>] {
                    $(
                        $(#[$fattr])*
                        $field: ($fmatch) => $fty,
                    )*
                }
            }

            #[doc=concat!("Parser for element [`", stringify!($qname), "`].")]
            #[derive(Debug, PartialEq, Eq, Default)]
            enum $nt {
                #[doc=concat!(
                    "Expecting opening tag for element [`",
                    stringify!($qname),
                    "`]."
                )]
                #[default]
                Expecting_,
                /// Recovery state ignoring all remaining tokens for this
                ///   element.
                RecoverEleIgnore_(crate::xir::QName, crate::xir::OpenSpan, Depth),
                // Recovery completed because end tag corresponding to the
                //   invalid element has been found.
                RecoverEleIgnoreClosed_(crate::xir::QName, crate::xir::CloseSpan),
                /// Parsing element attributes.
                Attrs_([<$nt AttrsState_>]),
                $(
                    $ntref($ntref),
                )*
                ExpectClose_(()),
                /// Closing tag found and parsing of the element is
                ///   complete.
                Closed_(crate::span::Span),
            }

            impl crate::xir::parse::ele::EleParseState for $nt {}

            impl $nt {
                /// [`QName`](crate::xir::QName) of the element recognized
                ///   by this parser.
                #[allow(dead_code)] // used by sum parser
                const fn qname() -> crate::xir::QName {
                    $qname
                }
            }

            impl std::fmt::Display for $nt {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use crate::{
                        fmt::{DisplayWrapper, TtQuote},
                        xir::fmt::TtOpenXmlEle,
                    };

                    match self {
                        Self::Expecting_ => write!(
                            f,
                            "expecting opening tag {}",
                            TtOpenXmlEle::wrap($qname),
                        ),
                        Self::RecoverEleIgnore_(name, ..) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                            expected = TtQuote::wrap($qname),
                        ),

                        Self::Attrs_(sa) => todo!("Attrs_ Display: {sa:?}"),
                        Self::Closed_(_) => write!(
                            f,
                            "element {} closed",
                            TtQuote::wrap($qname)
                        ),
                        $(
                            Self::$ntref(st) => std::fmt::Display::fmt(st, f),
                        )*
                        todo => todo!("other Display: {todo:?}"),
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            enum [<$nt Error_>] {
                UnexpectedEle_(crate::xir::QName, crate::span::Span),
                Attrs_(crate::xir::parse::AttrParseError<[<$nt AttrsState_>]>),
                $(
                    $ntref([<$ntref Error_>]),
                )*
            }

            impl From<crate::xir::parse::AttrParseError<[<$nt AttrsState_>]>>
                for [<$nt Error_>]
            {
                fn from(
                    e: crate::xir::parse::AttrParseError<[<$nt AttrsState_>]>
                ) -> Self {
                    [<$nt Error_>]::Attrs_(e)
                }
            }

            $(
                impl From<[<$ntref Error_>]> for [<$nt Error_>] {
                    fn from(e: [<$ntref Error_>]) -> Self {
                        [<$nt Error_>]::$ntref(e)
                    }
                }
            )*

            impl std::error::Error for [<$nt Error_>] {
                fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                    // TODO
                    None
                }
            }

            impl std::fmt::Display for [<$nt Error_>] {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use crate::{
                        fmt::DisplayWrapper,
                        xir::fmt::TtOpenXmlEle,
                    };

                    match self {
                        Self::UnexpectedEle_(name, _) => {
                            write!(f, "unexpected {}", TtOpenXmlEle::wrap(name))
                        }
                        Self::Attrs_(e) => std::fmt::Display::fmt(e, f),
                        $(
                            Self::$ntref(e) => std::fmt::Display::fmt(e, f),
                        )*
                    }
                }
            }

            impl crate::diagnose::Diagnostic for [<$nt Error_>] {
                fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
                    todo!()
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken;
                type Object = $objty;
                type Error = [<$nt Error_>];

                fn parse_token(
                    self,
                    tok: Self::Token,
                    _: crate::parse::NoContext,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::{
                        parse::{EmptyContext, Transition, Transitionable},
                        xir::{
                            flat::XirfToken,
                            parse::attr::parse_attrs,
                        },
                    };

                    use $nt::{
                        Attrs_, Expecting_, RecoverEleIgnore_,
                        RecoverEleIgnoreClosed_, ExpectClose_, Closed_
                    };

                    match (self, tok) {
                        (Expecting_, XirfToken::Open(qname, span, ..)) if qname == $qname => {
                            Transition(Attrs_(parse_attrs(qname, span)))
                                .incomplete()
                        },

                        (Expecting_, XirfToken::Open(qname, span, depth)) => {
                            Transition(RecoverEleIgnore_(qname, span, depth)).err(
                                [<$nt Error_>]::UnexpectedEle_(qname, span.name_span())
                            )
                        },

                        (
                            RecoverEleIgnore_(qname, _, depth_open),
                            XirfToken::Close(_, span, depth_close)
                        ) if depth_open == depth_close => {
                            Transition(RecoverEleIgnoreClosed_(qname, span)).incomplete()
                        },

                        (st @ RecoverEleIgnore_(..), _) => {
                            Transition(st).incomplete()
                        },

                        (Attrs_(sa), tok) => {
                            sa.delegate_until_obj(
                                tok,
                                EmptyContext,
                                |sa| Transition(Attrs_(sa)),
                                || unreachable!("see ParseState::delegate_until_obj dead"),
                                |#[allow(unused_variables)] sa, attrs| {
                                    let obj = match attrs {
                                        // Attribute field bindings for `$attrmap`
                                        [<$nt Attrs_>] {
                                            $(
                                                $field,
                                            )*
                                        } => {
                                            // Optional `OpenSpan` binding
                                            $(
                                                use crate::xir::parse::attr::AttrParseState;
                                                let $open_span = sa.element_span();
                                            )?

                                            $attrmap
                                        },
                                    };

                                    Transition($ntfirst(Default::default())).ok(obj)
                                }
                            )
                        },

                        $(
                            ($ntprev(st_inner), tok) => {
                                st_inner.delegate(
                                    tok,
                                    EmptyContext,
                                    |si| Transition($ntprev(si)),
                                    || Transition($ntnext(Default::default()))
                                )
                            },
                        )*

                        // XIRF ensures proper nesting,
                        //   so this must be our own closing tag.
                        (ExpectClose_(_), XirfToken::Close(_, span, _)) => {
                            $(
                                let $close_span = span;
                            )?
                            $closemap.transition(Closed_(span.tag_span()))
                        },

                        // TODO: Use `is_accepting` guard if we do not utilize
                        //   exhaustiveness check.
                        (st @ (Closed_(..) | RecoverEleIgnoreClosed_(..)), tok) =>
                            Transition(st).dead(tok),

                        todo => todo!("{todo:?}"),
                    }
                }

                fn is_accepting(&self) -> bool {
                    matches!(*self, Self::Closed_(..) | Self::RecoverEleIgnoreClosed_(..))
                }
            }
        }
    };

    (@!ele_dfn_sum <$objty:ty> $nt:ident [$($ntref:ident)*]) => {
        $(
            // Provide a (hopefully) helpful error that can be corrected
            //   rather than any obscure errors that may follow from trying
            //   to compose parsers that were not generated with this macro.
            assert_impl_all!($ntref: crate::xir::parse::ele::EleParseState);
        )*

        paste::paste! {
            #[doc=concat!(
                "Parser expecting one of ",
                $("[`", stringify!($ntref), "`], ",)*
                "."
            )]
            #[derive(Debug, PartialEq, Eq, Default)]
            enum $nt {
                #[default]
                Expecting_,
                /// Recovery state ignoring all remaining tokens for this
                ///   element.
                RecoverEleIgnore_(crate::xir::QName, crate::xir::OpenSpan, Depth),
                RecoverEleIgnoreClosed_(crate::xir::QName, crate::xir::CloseSpan),
                $(
                    $ntref($ntref),
                )*
            }

            impl std::fmt::Display for $nt {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use crate::{
                        fmt::{DisplayWrapper, ListDisplayWrapper, TtQuote},
                        xir::fmt::OpenEleSumList,
                    };

                    let ntrefs = [
                        $(
                            $ntref::qname(),
                        )*
                    ];
                    let expected = OpenEleSumList::wrap(&ntrefs);

                    match self {
                        Self::Expecting_ => {
                            write!(f, "expecting {expected}")
                        },

                        Self::RecoverEleIgnore_(name, ..)
                        | Self::RecoverEleIgnoreClosed_(name, ..) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                        ),

                        $(
                            Self::$ntref(st) => std::fmt::Display::fmt(st, f),
                        )*
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            enum [<$nt Error_>] {
                UnexpectedEle_(crate::xir::QName, crate::span::Span),
                $(
                    $ntref([<$ntref Error_>]),
                )*
            }

            $(
                impl From<[<$ntref Error_>]> for [<$nt Error_>] {
                    fn from(e: [<$ntref Error_>]) -> Self {
                        [<$nt Error_>]::$ntref(e)
                    }
                }
            )*

            impl std::error::Error for [<$nt Error_>] {
                fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                    // TODO
                    None
                }
            }

            impl std::fmt::Display for [<$nt Error_>] {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use crate::{
                        fmt::DisplayWrapper,
                        xir::fmt::TtOpenXmlEle,
                    };

                    match self {
                        Self::UnexpectedEle_(qname, _) => {
                            write!(f, "unexpected {}", TtOpenXmlEle::wrap(qname))
                        },
                        $(
                            Self::$ntref(e) => std::fmt::Display::fmt(e, f),
                        )*
                    }
                }
            }

            impl crate::diagnose::Diagnostic for [<$nt Error_>] {
                fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
                    todo!()
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken;
                type Object = $objty;
                type Error = [<$nt Error_>];

                fn parse_token(
                    self,
                    tok: Self::Token,
                    _: crate::parse::NoContext,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::{
                        parse::{EmptyContext, Transition},
                        xir::flat::XirfToken,
                    };

                    use $nt::{Expecting_, RecoverEleIgnore_, RecoverEleIgnoreClosed_};

                    match (self, tok) {
                        $(
                            (
                                Expecting_,
                                XirfToken::Open(qname, span, depth)
                            ) if qname == $ntref::qname() => {
                                $ntref::default().delegate(
                                    XirfToken::Open(qname, span, depth),
                                    EmptyContext,
                                    |si| Transition(Self::$ntref(si)),
                                    || todo!("inner dead (should not happen here)"),
                                )
                            },
                        )*

                        (Expecting_, XirfToken::Open(qname, span, depth)) => {
                            Transition(RecoverEleIgnore_(qname, span, depth)).err(
                                // Use name span rather than full `OpenSpan`
                                //   since it's specifically the name that
                                //   was unexpected,
                                //     not the fact that it's an element.
                                [<$nt Error_>]::UnexpectedEle_(qname, span.name_span())
                            )
                        },

                        // XIRF ensures that the closing tag matches the opening,
                        //   so we need only check depth.
                        (
                            RecoverEleIgnore_(qname, _, depth_open),
                            XirfToken::Close(_, span, depth_close)
                        ) if depth_open == depth_close => {
                            Transition(RecoverEleIgnoreClosed_(qname, span)).incomplete()
                        },

                        (st @ RecoverEleIgnore_(..), _) => {
                            Transition(st).incomplete()
                        },

                        $(
                            (Self::$ntref(si), tok) => si.delegate(
                                tok,
                                EmptyContext,
                                |si| Transition(Self::$ntref(si)),
                                || todo!("inner dead"),
                            ),
                        )*

                        todo => todo!("sum {todo:?}"),
                    }
                }

                fn is_accepting(&self) -> bool {
                    match self {
                        Self::RecoverEleIgnoreClosed_(..) => true,

                        // Delegate entirely to the inner ParseState.
                        // It is desirable to maintain this state even after
                        //   the inner parser is completed so that the inner
                        //   state can accurately describe what took place.
                        $(
                            Self::$ntref(si) => si.is_accepting(),
                        )*

                        _ => false,
                    }
                }
            }
        }
    };
}

#[cfg(test)]
mod test;