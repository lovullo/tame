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

/// Element parser configuration.
///
/// This configuration is set on a nonterminal reference using square
///   brackets
///     (e.g. `Foo[*]`).
#[derive(Debug, PartialEq, Default)]
pub struct EleParseCfg {
    /// Whether to allow zero-or-more repetition for this element.
    ///
    /// This is the Kleene star modifier (`*`).
    pub repeat: bool,
}

#[macro_export]
macro_rules! ele_parse {
    (type Object = $objty:ty; $($rest:tt)*) => {
        ele_parse!(@!nonterm_decl <$objty> $($rest)*);
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
        // A configuration specifier can be provided,
        //   currently intended to support the Kleene star.
        $(
            $ntref:ident $([$ntref_cfg:tt])?,
        )*
    ) => {
        ele_parse! {
            @!ele_dfn_body <$objty> $nt $qname ($($ntp)*)
            @ { $($attrbody)* } => $attrmap,
            /$($($close_span)?)? => ele_parse!(@!ele_close $($closemap)?),

            <> {
                $(
                    $ntref [$($ntref_cfg)?],
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

    // NT[*] modifier.
    (@!ntref_cfg *) => {
        crate::parse::Context::from(crate::xir::parse::ele::EleParseCfg {
            repeat: true,
            ..Default::default()
        })
    };

    // No bracketed modifier following NT.
    (@!ntref_cfg) => {
        Self::Context::default()
    };

    (@!ele_dfn_body <$objty:ty> $nt:ident $qname:ident ($($open_span:ident)?)
        // Attribute definition special form.
        @ {
            // We must lightly parse attributes here so that we can retrieve
            //   the field identifiers that may be later used as bindings in
            //   `$attrmap`.
            $(
                $(#[$fattr:meta])*
                $field:ident: ($($fmatch:tt)+) => $fty:ty,
            )*
        } => $attrmap:expr,

        // Close expression
        //   (defaulting to Incomplete via @!ele_expand_body).
        /$($close_span:ident)? => $closemap:expr,

        // Nonterminal references.
        <> {
            $(
                $ntref:ident [$($ntref_cfg:tt)?],
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
                        $field: ($($fmatch)+) => $fty,
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
                RecoverEleIgnore_(
                    crate::xir::QName,
                    crate::xir::OpenSpan,
                    crate::xir::flat::Depth
                ),
                // Recovery completed because end tag corresponding to the
                //   invalid element has been found.
                RecoverEleIgnoreClosed_(crate::xir::QName, crate::xir::CloseSpan),
                /// Recovery state ignoring all tokens when a `Close` is
                ///   expected.
                ///
                /// This is token-agnostic---it
                ///   may be a child element,
                ///     but it may be text,
                ///     for example.
                CloseRecoverIgnore_(
                    (crate::span::Span, crate::xir::flat::Depth),
                    crate::span::Span
                ),
                /// Parsing element attributes.
                Attrs_(
                    (crate::span::Span, crate::xir::flat::Depth),
                    [<$nt AttrsState_>]
                ),
                $(
                    $ntref(
                        (crate::span::Span, crate::xir::flat::Depth),
                        $ntref
                    ),
                )*
                ExpectClose_((crate::span::Span, crate::xir::flat::Depth), ()),
                /// Closing tag found and parsing of the element is
                ///   complete.
                Closed_(crate::span::Span),
            }

            impl crate::xir::parse::EleParseState for $nt {}

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
                        xir::fmt::{TtOpenXmlEle, TtCloseXmlEle},
                    };

                    match self {
                        Self::Expecting_ => write!(
                            f,
                            "expecting opening tag {}",
                            TtOpenXmlEle::wrap($qname),
                        ),
                        Self::RecoverEleIgnore_(name, ..)
                        | Self::RecoverEleIgnoreClosed_(name, ..) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                            expected = TtQuote::wrap($qname),
                        ),
                        Self::CloseRecoverIgnore_((_, depth), _) => write!(
                            f,
                            "attempting to recover by ignoring input \
                               until the expected end tag {expected} \
                               at depth {depth}",
                            expected = TtCloseXmlEle::wrap($qname),
                        ),

                        Self::Attrs_(_, sa) => std::fmt::Display::fmt(sa, f),
                        Self::ExpectClose_((_, depth), _) => write!(
                            f,
                            "expecting closing element {} at depth {depth}",
                            TtCloseXmlEle::wrap($qname)
                        ),
                        Self::Closed_(_) => write!(
                            f,
                            "done parsing element {}",
                            TtQuote::wrap($qname)
                        ),
                        $(
                            Self::$ntref(_, st) => {
                                std::fmt::Display::fmt(st, f)
                            },
                        )*
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            enum [<$nt Error_>] {
                /// An element was expected,
                ///   but the name of the element was unexpected.
                UnexpectedEle_(crate::xir::QName, crate::span::Span),

                /// Unexpected input while expecting an end tag for this
                ///   element.
                ///
                /// The span corresponds to the opening tag.
                CloseExpected_(crate::span::Span, crate::xir::flat::XirfToken),

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
                        fmt::{DisplayWrapper, TtQuote},
                        xir::fmt::{TtOpenXmlEle, TtCloseXmlEle},
                    };

                    match self {
                        Self::UnexpectedEle_(name, _) => write!(
                            f,
                            "unexpected {unexpected} (expecting {expected})",
                            unexpected = TtOpenXmlEle::wrap(name),
                            expected = TtOpenXmlEle::wrap($qname),
                        ),
                        Self::CloseExpected_(_, tok) => write!(
                            f,
                            "expected {}, but found {}",
                            TtCloseXmlEle::wrap($qname),
                            TtQuote::wrap(tok)
                        ),
                        Self::Attrs_(e) => std::fmt::Display::fmt(e, f),
                        $(
                            Self::$ntref(e) => std::fmt::Display::fmt(e, f),
                        )*
                    }
                }
            }

            impl crate::diagnose::Diagnostic for [<$nt Error_>] {
                fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
                    use crate::{
                        diagnose::Annotate,
                        fmt::{DisplayWrapper, TtQuote},
                        parse::Token,
                        xir::fmt::{TtCloseXmlEle},
                    };

                    match self {
                        Self::UnexpectedEle_(_, ospan) => ospan.error(
                            format!(
                                "expected {ele_name} here",
                                ele_name = TtQuote::wrap($qname)
                            )
                        ).into(),

                        Self::CloseExpected_(span, tok) => vec![
                            span.note("element starts here"),
                            tok.span().error(format!(
                                "expected {}",
                                TtCloseXmlEle::wrap($qname),
                            )),
                        ],

                        Self::Attrs_(e) => e.describe(),

                        $(
                            Self::$ntref(e) => e.describe(),
                        )*
                    }
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Context = crate::parse::Context<crate::xir::parse::EleParseCfg>;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    cfg: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::{
                        parse::{EmptyContext, Transition, Transitionable},
                        xir::{
                            EleSpan,
                            flat::XirfToken,
                            parse::parse_attrs,
                        },
                    };

                    use $nt::{
                        Attrs_, Expecting_, RecoverEleIgnore_,
                        CloseRecoverIgnore_, RecoverEleIgnoreClosed_,
                        ExpectClose_, Closed_
                    };

                    match (self, tok) {
                        (
                            Expecting_,
                            XirfToken::Open(qname, span, depth)
                        ) if qname == $qname => {
                            Transition(Attrs_(
                                (span.tag_span(), depth),
                                parse_attrs(qname, span)
                            )).incomplete()
                        },

                        (
                            Closed_(..),
                            XirfToken::Open(qname, span, depth)
                        ) if cfg.repeat && qname == $qname => {
                            Transition(Attrs_(
                                (span.tag_span(), depth),
                                parse_attrs(qname, span)
                            )).incomplete()
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

                        (Attrs_(meta, sa), tok) => {
                            sa.delegate_until_obj(
                                tok,
                                EmptyContext,
                                |sa| Transition(Attrs_(meta, sa)),
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

                                    Transition($ntfirst(meta, Default::default()))
                                        .ok(obj)
                                }
                            )
                        },

                        $(
                            ($ntprev(depth, st_inner), tok) => {
                                st_inner.delegate(
                                    tok,
                                    &mut ele_parse!(@!ntref_cfg $($ntref_cfg)?),
                                    |si| Transition($ntprev(depth, si)),
                                    || Transition($ntnext(depth, Default::default()))
                                )
                            },
                        )*

                        // XIRF ensures proper nesting,
                        //   so we do not need to check the element name.
                        (
                            ExpectClose_((_, depth), ())
                            | CloseRecoverIgnore_((_, depth), _),
                            XirfToken::Close(_, span, tok_depth)
                        ) if tok_depth == depth => {
                            $(
                                let $close_span = span;
                            )?
                            $closemap.transition(Closed_(span.tag_span()))
                        },

                        (ExpectClose_(meta @ (otspan, _), ()), unexpected_tok) => {
                            use crate::parse::Token;
                            Transition(
                                CloseRecoverIgnore_(meta, unexpected_tok.span())
                            ).err([<$nt Error_>]::CloseExpected_(otspan, unexpected_tok))
                        }

                        // We're still in recovery,
                        //   so this token gets thrown out.
                        (st @ (RecoverEleIgnore_(..) | CloseRecoverIgnore_(..)), _) => {
                            Transition(st).incomplete()
                        },

                        // TODO: Use `is_accepting` guard if we do not utilize
                        //   exhaustiveness check.
                        (st @ (Closed_(..) | RecoverEleIgnoreClosed_(..)), tok) => {
                            Transition(st).dead(tok)
                        }

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
                /// Inner element has been parsed and is dead;
                ///   this indicates that this parser is also dead.
                Done_,
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

                        Self::Done_ => write!(f, "done parsing {expected}"),
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
                    use crate::{
                        diagnose::Annotate,
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
                        Self::UnexpectedEle_(qname, span) => {
                            span.error(format!(
                                "element {name} cannot appear here \
                                   (expecting {expected})",
                                name = TtQuote::wrap(qname),
                            )).into()
                        },

                        $(
                            Self::$ntref(e) => e.describe(),
                        )*
                    }
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Context = crate::parse::Context<crate::xir::parse::ele::EleParseCfg>;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    cfg: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::{
                        parse::Transition,
                        xir::flat::XirfToken,
                    };

                    use $nt::{
                        Expecting_, RecoverEleIgnore_,
                        RecoverEleIgnoreClosed_, Done_
                    };

                    match (self, tok) {
                        $(
                            (
                                Expecting_,
                                XirfToken::Open(qname, span, depth)
                            ) if qname == $ntref::qname() => {
                                $ntref::default().delegate(
                                    XirfToken::Open(qname, span, depth),
                                    &mut Self::Context::default(),
                                    |si| Transition(Self::$ntref(si)),
                                    || todo!("inner dead (should not happen here)"),
                                )
                            },
                        )*

                        // An unexpected token when repeating ends
                        //   repetition and should not result in an error.
                        (Expecting_, tok) if cfg.repeat => {
                            Transition(Done_).dead(tok)
                        }

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
                                &mut Self::Context::default(),
                                |si| Transition(Self::$ntref(si)),
                                || match cfg.repeat {
                                    true => Transition(Expecting_),
                                    false => Transition(Done_),
                                }
                            ),
                        )*

                        (st @ Self::Done_, tok) => Transition(st).dead(tok),

                        todo => todo!("sum {todo:?}"),
                    }
                }

                fn is_accepting(&self) -> bool {
                    match self {
                        Self::RecoverEleIgnoreClosed_(..) | Self::Done_ => true,

                        // Delegate entirely to the inner ParseState.
                        // It is desirable to maintain this state even after
                        //   the inner parser is completed so that the inner
                        //   state can accurately describe what took place.
                        // With that said,
                        //   we will transition to `Done_` on an inner dead
                        //   state,
                        //     because of current `delegate` limitations.
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
