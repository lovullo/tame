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
    (
        $vis:vis enum $super:ident;

        // Attr has to be first to avoid ambiguity with `$rest`.
        $(type AttrValueError = $evty:ty;)?
        type Object = $objty:ty;

        $($rest:tt)*
    ) => {
        ele_parse! {@!next $vis $super
            $(type AttrValueError = $evty;)?
            type Object = $objty;
            $($rest)*
        }

        ele_parse!(@!super_sum <$objty> $vis $super $($rest)*);
    };

    (@!next $vis:vis $super:ident
        // Attr has to be first to avoid ambiguity with `$rest`.
        $(type AttrValueError = $evty:ty;)?
        type Object = $objty:ty;

        $($rest:tt)*
    ) => {
        ele_parse!(@!nonterm_decl <$objty, $($evty)?> $vis $super $($rest)*);
    };

    (@!nonterm_decl <$objty:ty, $($evty:ty)?>
        $vis:vis $super:ident $nt:ident := $($rest:tt)*
    ) => {
        ele_parse!(@!nonterm_def <$objty, $($evty)?> $vis $super $nt $($rest)*);
    };

    (@!nonterm_def <$objty:ty, $($evty:ty)?>
        $vis:vis $super:ident $nt:ident $qname:ident $(($($ntp:tt)*))?
        { $($matches:tt)* }; $($rest:tt)*
    ) => {
        ele_parse!(@!ele_expand_body <$objty, $($evty)?>
            $vis $super $nt $qname ($($($ntp)*)?) $($matches)*
        );

        ele_parse! {@!next $vis $super
            $(type AttrValueError = $evty;)?
            type Object = $objty;
            $($rest)*
        }
    };

    (@!nonterm_def <$objty:ty, $($evty:ty)?>
        $vis:vis $super:ident $nt:ident
        ($ntref_first:ident $(| $ntref:ident)+); $($rest:tt)*
    ) => {
        ele_parse!(@!ele_dfn_sum <$objty>
            $vis $super $nt [$ntref_first $($ntref)*]
        );

        ele_parse! {@!next $vis $super
            $(type AttrValueError = $evty;)?
            type Object = $objty;
            $($rest)*
        }
    };

    (@!nonterm_decl <$objty:ty, $($evty:ty)?> $vis:vis $super:ident) => {};

    // Expand the provided data to a more verbose form that provides the
    //   context necessary for state transitions.
    (@!ele_expand_body <$objty:ty, $($evty:ty)?>
        $vis:vis $super:ident $nt:ident $qname:ident ($($ntp:tt)*)

        @ { $($attrbody:tt)* } => $attrmap:expr,
        $(/$(($close_span:ident))? => $closemap:expr,)?

        // Special forms (`[sp](args) => expr`).
        $(
            [$special:ident]$(($($special_arg:ident),*))?
                => $special_map:expr,
        )?

        // Nonterminal references are provided as a list.
        // A configuration specifier can be provided,
        //   currently intended to support the Kleene star.
        $(
            $ntref:ident $([$ntref_cfg:tt])?,
        )*
    ) => {
        ele_parse! {
            @!ele_dfn_body <$objty, $($evty)?> $vis $super $nt $qname ($($ntp)*)
            @ { $($attrbody)* } => $attrmap,
            /$($($close_span)?)? => ele_parse!(@!ele_close $($closemap)?),

            $([$special]$(($($special_arg),*))? => $special_map,)?

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
        crate::parse::Context::from(crate::xir::parse::EleParseCfg {
            repeat: true,
            ..Default::default()
        })
    };

    // No bracketed modifier following NT.
    (@!ntref_cfg) => {
        Self::Context::default()
    };

    (@!ele_dfn_body <$objty:ty, $($evty:ty)?>
        $vis:vis $super:ident $nt:ident $qname:ident ($($open_span:ident)?)

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

        // Non-whitespace text nodes can be mapped into elements with the
        //   given QName as a preprocessing step,
        //     allowing them to reuse the existing element NT system.
        $([text]($text:ident, $text_span:ident) => $text_map:expr,)?

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
                vis($vis);
                $(type ValueError = $evty;)?

                struct [<$nt AttrsState_>] -> [<$nt Attrs_>] {
                    $(
                        $(#[$fattr])*
                        $field: ($($fmatch)+) => $fty,
                    )*
                }
            }

            #[doc=concat!("Parser for element [`", stringify!($qname), "`].")]
            #[derive(Debug, PartialEq, Eq, Default)]
            $vis enum $nt {
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

                /// Yield the expected depth of child elements,
                ///   if known.
                #[allow(dead_code)] // used by text special form
                fn child_depth(&self) -> Option<Depth> {
                    match self {
                        $ntfirst((_, depth), _) => Some(depth.child_depth()),
                        $(
                            $ntnext((_, depth), _) => Some(depth.child_depth()),
                        )*
                        _ => None,
                    }
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
            $vis enum [<$nt Error_>] {
                /// An element was expected,
                ///   but the name of the element was unexpected.
                UnexpectedEle_(crate::xir::QName, crate::span::Span),

                /// Unexpected input while expecting an end tag for this
                ///   element.
                ///
                /// The span corresponds to the opening tag.
                CloseExpected_(
                    crate::span::Span,
                    crate::xir::flat::XirfToken<crate::xir::flat::RefinedText>,
                ),

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
                type Token = crate::xir::flat::XirfToken<
                    crate::xir::flat::RefinedText
                >;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Super = $super;
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
                            flat::{XirfToken, RefinedText},
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

                        // Depth check is unnecessary since _all_ xir::parse
                        //   parsers
                        //     (at least at the time of writing)
                        //     ignore whitespace,
                        //       so may as well return early.
                        (st, XirfToken::Text(RefinedText::Whitespace(..), _)) => {
                            Transition(st).incomplete()
                        }

                        (Attrs_(meta, sa), tok) => {
                            sa.delegate_until_obj(
                                tok,
                                EmptyContext,
                                |sa| Transition(Attrs_(meta, sa)).into_super(),
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

                        // Must come _after_ `Attrs_` above so that
                        //   attributes are yielded before text that
                        //   terminates attribute parsing.
                        $(
                            // Text nodes are handled a differently because
                            //   it implies mixed content;
                            //     the text is "owned" by us,
                            //       not by the parser we have chosen to
                            //       delegate _elements_ to.
                            // But we must be sure to only preempt parsing
                            //   of text nodes _at our child depth_,
                            //     so as not to interfere with the text
                            //     parsing of child elements.
                            // This also allows us to avoid implementing
                            //   Text handling in sum parsers.
                            (
                                st,
                                XirfToken::Text(
                                    RefinedText::Unrefined(
                                        Text($text, $text_span)
                                    ),
                                    depth
                                )
                            ) if Some(depth) == st.child_depth() => {
                                Transition(st).ok($text_map)
                            }
                        )?

                        $(
                            ($ntprev(depth, st_inner), tok) => {
                                st_inner.delegate(
                                    tok,
                                    &mut ele_parse!(@!ntref_cfg $($ntref_cfg)?),
                                    // TODO: proper trampoline delegation;
                                    //   this is maintaining BC for now
                                    |si| Transition($ntprev(depth, si.into())).into_super(),
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

    (@!ele_dfn_sum <$objty:ty> $vis:vis $super:ident $nt:ident [$($ntref:ident)*]) => {
        $(
            // Provide a (hopefully) helpful error that can be corrected
            //   rather than any obscure errors that may follow from trying
            //   to compose parsers that were not generated with this macro.
            assert_impl_all!($ntref: crate::xir::parse::EleParseState);
        )*

        paste::paste! {
            #[doc=concat!(
                "Parser expecting one of ",
                $("[`", stringify!($ntref), "`], ",)*
                "."
            )]
            #[derive(Debug, PartialEq, Eq, Default)]
            $vis enum $nt {
                #[default]
                Expecting_,
                /// Recovery state ignoring all remaining tokens for this
                ///   element.
                RecoverEleIgnore_(
                    crate::xir::QName,
                    crate::xir::OpenSpan,
                    crate::xir::flat::Depth,
                ),
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
            $vis enum [<$nt Error_>] {
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
                type Token = crate::xir::flat::XirfToken<
                    crate::xir::flat::RefinedText
                >;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Super = $super;
                type Context = crate::parse::Context<crate::xir::parse::EleParseCfg>;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    cfg: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::{
                        parse::Transition,
                        xir::flat::{XirfToken, RefinedText},
                    };

                    use $nt::{
                        Expecting_, RecoverEleIgnore_,
                        RecoverEleIgnoreClosed_, Done_
                    };

                    match (self, tok) {
                        // Depth check is unnecessary since _all_ xir::parse
                        //   parsers
                        //     (at least at the time of writing)
                        //     ignore whitespace,
                        //       so may as well return early.
                        (st, XirfToken::Text(RefinedText::Whitespace(..), _)) => {
                            Transition(st).incomplete()
                        }

                        $(
                            (
                                Expecting_,
                                XirfToken::Open(qname, span, depth)
                            ) if qname == $ntref::qname() => {
                                $ntref::default().delegate(
                                    XirfToken::Open(qname, span, depth),
                                    &mut Self::Context::default(),
                                    // TODO: proper trampoline delegation
                                    |si| Transition(Self::$ntref(si.into())).into_super(),
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
                            use crate::xir::EleSpan;
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
                                // TODO: proper trampoline delegation
                                |si| Transition(Self::$ntref(si.into())).into_super(),
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

    // Generate superstate sum type.
    //
    // This is really annoying because we cannot read the output of another
    //   macro,
    //     and so we have to do our best to re-parse the body of the
    //     original `ele_parse!` invocation without duplicating too much
    //     logic,
    //       and we have to do so in a way that we can aggregate all of
    //       those data.
    (@!super_sum <$objty:ty> $vis:vis $super:ident
        $(
            // NT definition is always followed by `:=`.
            $nt:ident :=
                // Identifier if an element NT.
                $($_i:ident)?
                // Parenthesis for a sum NT,
                //   or possibly the span match for an element NT.
                // So: `:= QN_IDENT(span)` or `:= (A | B | C)`.
                $( ($($_p:tt)*) )?
                // Braces for an element NT body.
                $( {$($_b:tt)*} )?
            // Element and sum NT both conclude with a semicolon,
            //   which we need to disambiguate the next `$nt`.
            ;
        )*
    ) => {
        paste::paste! {
            /// Superstate representing the union of all related parsers.
            ///
            /// This [`ParseState`] allows sub-parsers to independently
            ///   the states associated with their own subgraph,
            ///     and then yield a state transition directly to a state of
            ///     another parser.
            /// This is conceptually like CPS (continuation passing style),
            ///   where this [`ParseState`] acts as a trampoline.
            ///
            /// This [`ParseState`] is required for use with [`Parser`];
            ///   see [`ClosedParseState`] for more information.
            #[derive(Debug, PartialEq, Eq)]
            $vis enum $super {
                $(
                    $nt($nt),
                )*
            }

            // Default parser is the first NT.
            impl Default for $super {
                fn default() -> Self {
                    use $super::*;
                    ele_parse!(@!ntfirst $($nt)*)(Default::default())
                }
            }

            $(
                impl From<$nt> for $super {
                    fn from(st: $nt) -> Self {
                        $super::$nt(st)
                    }
                }
            )*

            // TODO: This is used only until we remove composition-based
            //   delegation in favor of trampolines---the
            //     composed parsers yield their superstate,
            //       which we have to convert back.
            $(
                impl From<$super> for $nt {
                    fn from(sup: $super) -> Self {
                        match sup {
                            $super::$nt(st) => st,
                            #[allow(unreachable_patterns)]
                            _ => unreachable!("From<Super> for NT mismatch"),
                        }
                    }
                }
            )*

            impl std::fmt::Display for $super {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$nt(e) => std::fmt::Display::fmt(e, f),
                        )*
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            $vis enum [<$super Error_>] {
                $(
                    $nt([<$nt Error_>]),
                )*
            }

            $(
                impl From<[<$nt Error_>]> for [<$super Error_>] {
                    fn from(e: [<$nt Error_>]) -> Self {
                        [<$super Error_>]::$nt(e)
                    }
                }
            )*

            impl std::error::Error for [<$super Error_>] {
                fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                    // TODO
                    None
                }
            }

            impl std::fmt::Display for [<$super Error_>] {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$nt(e) => std::fmt::Display::fmt(e, f),
                        )*
                    }
                }
            }

            impl crate::diagnose::Diagnostic for [<$super Error_>] {
                fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
                    match self {
                        $(
                            Self::$nt(e) => e.describe(),
                        )*
                    }
                }
            }

            impl crate::parse::ParseState for $super {
                type Token = crate::xir::flat::XirfToken<
                    crate::xir::flat::RefinedText
                >;
                type Object = $objty;
                type Error = [<$super Error_>];
                type Context = crate::parse::Context<crate::xir::parse::EleParseCfg>;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    _cfg: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self> {
                    use crate::parse::Transition;

                    match self {
                        $(
                            Self::$nt(st) => st.delegate(
                                tok,
                                &mut Self::Context::default(),
                                Transition,
                                || todo!("DEAD super sum")
                            ),
                        )*
                    }
                }

                fn is_accepting(&self) -> bool {
                    match self {
                        $(
                            Self::$nt(si) => si.is_accepting(),
                        )*
                    }
                }
            }
        }
    };

    (@!ntfirst $ntfirst:ident $($nt:ident)*) => {
        $ntfirst
    }
}

#[cfg(test)]
mod test;
