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

use arrayvec::ArrayVec;

use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate},
    diagnostic_panic,
    fmt::{DisplayWrapper, TtQuote},
    parse::{
        ClosedParseState, Context, ParseState, Token, Transition,
        TransitionResult,
    },
};

#[cfg(doc)]
use crate::{ele_parse, parse::Parser};

/// A parser accepting a single element.
pub trait EleParseState: ParseState {}

/// Element parser configuration.
///
/// This configuration is set on a nonterminal reference using square
///   brackets
///     (e.g. `Foo[*]`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct EleParseCfg {
    /// Whether to allow zero-or-more repetition for this element.
    ///
    /// This is the Kleene star modifier (`*`).
    pub repeat: bool,
}

// This is an implementation detail for the internal state of EleParseState.
impl From<EleParseCfg> for () {
    fn from(_: EleParseCfg) -> Self {
        ()
    }
}

/// Maximum level of nesting for source XML trees.
///
/// Technically this is the maximum level of nesting for _parsing_ those
///   trees,
///     which may end up being less than this value.
///
/// This should be set to something reasonable,
///   but is not an alternative to coming up with code conventions that
///   disallow ridiculous levels of nesting.
/// TAME does have a lot of nesting with primitives,
///   but that nesting is easily abstracted with templates.
/// Templates may expand into ridiculous levels of nesting---this
///   has no impact on the template expansion phase.
///
/// Note that this is assuming that this parser is used only for TAME
///   sources.
/// If that's not the case,
///   this can be made to be configurable like XIRF.
pub const MAX_DEPTH: usize = 16;

/// Parser stack for trampoline.
///
/// This can be used as a call stack for parsers while avoiding creating
///   otherwise-recursive data structures with composition-based delegation.
/// However,
///   it is more similar to CPS,
///   in that the parser popped off the stack need not be the parser that
///     initiated the request and merely represents the next step in
///     a delayed computation.
/// If such a return context is unneeded,
///   a [`ParseState`] may implement tail calls by simply not pushing itself
///   onto the stack before requesting transfer to another [`ParseState`].
#[derive(Debug, Default)]
pub struct StateStack<S: ClosedParseState>(ArrayVec<S, MAX_DEPTH>);

pub type StateStackContext<S> = Context<StateStack<S>>;

// Note that public visibility is needed because `ele_parse` expands outside
//   of this module.
impl<S: ClosedParseState> StateStack<S> {
    /// Request a transfer to another [`ParseState`],
    ///   expecting that control be returned to `ret` after it has
    ///   completed.
    ///
    /// This can be reasoned about like calling a thunk:
    ///   the return [`ParseState`] is put onto the stack,
    ///   the target [`ParseState`] is used for the state transition to
    ///     cause [`Parser`] to perform the call to it,
    ///   and when it is done
    ///     (e.g. a dead state),
    ///     `ret` will be pop'd from the stack and we'll transition back to
    ///     it.
    /// Note that this method is not responsible for returning;
    ///   see [`Self::ret`] to perform a return.
    ///
    /// However,
    ///   the calling [`ParseState`] is not responsible for its return,
    ///   unlike a typical function call.
    /// Instead,
    ///   this _actually_ more closely resembles CPS
    ///     (continuation passing style),
    ///     and so [`ele_parse!`] must be careful to ensure that stack
    ///     operations are properly paired.
    /// On the upside,
    ///   if something is erroneously `ret`'d,
    ///   the parser is guaranteed to be in a consistent state since the
    ///   entire state has been reified
    ///     (but the input would then be parsed incorrectly).
    ///
    /// Note that tail calls can be implemented by transferring control
    ///   without pushing an entry on the stack to return to,
    ///     but that hasn't been formalized \[yet\] and requires extra care.
    pub fn transfer_with_ret<SA, ST>(
        &mut self,
        Transition(ret): Transition<SA>,
        target: TransitionResult<ST>,
    ) -> TransitionResult<ST>
    where
        SA: ParseState<Super = S::Super>,
        ST: ParseState,
    {
        let Self(stack) = self;

        // TODO: Global configuration to (hopefully) ensure that XIRF will
        //   actually catch this.
        if stack.is_full() {
            // TODO: We need some spans here and ideally convert the
            //   parenthetical error message into a diagnostic footnote.
            // TODO: Or should we have a special error type that tells the
            //   parent `Parser` to panic with context?
            diagnostic_panic!(
                vec![],
                "maximum parsing depth of {} exceeded while attempting \
                   to push return state {} \
                   (expected XIRF configuration to prevent this error)",
                MAX_DEPTH,
                TtQuote::wrap(ret),
            );
        }

        stack.push(ret.into());
        target
    }

    /// Return to a previous [`ParseState`] that transferred control away
    ///   from itself.
    ///
    /// Conceptually,
    ///   this is like returning from a function call,
    ///   where the function was invoked using [`Self::transfer_with_ret`].
    /// However,
    ///   this system is more akin to CPS
    ///     (continuation passing style);
    ///       see [`Self::transfer_with_ret`] for important information.
    pub fn ret(&mut self, lookahead: S::Token) -> TransitionResult<S> {
        let Self(stack) = self;

        // This should certainly never happen unless there is a bug in the
        //   `ele_parse!` parser-generator,
        //     since it means that we're trying to return to a caller that
        //     does not exist.
        let st = stack.pop().diagnostic_expect(
            lookahead
                .span()
                .internal_error("while processing this token")
                .with_help(
                    "this implies a bug in TAMER's `ele_parse` \
                       parser-generator",
                )
                .into(),
            "missing expected return ParseState",
        );

        Transition(st).incomplete().with_lookahead(lookahead)
    }
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
                    $ntref,
                )*
            }

            // Generate state transitions of the form `(S) -> (S')`.
            -> {
                @ ->
                $(
                    ($nt::$ntref, $ntref) [$($ntref_cfg)?],
                    ($nt::$ntref) ->
                )* ($nt::ExpectClose_, ()) [],
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
        crate::xir::parse::EleParseCfg {
            repeat: true,
        }
    };

    // No bracketed modifier following NT.
    (@!ntref_cfg) => {
        crate::xir::parse::EleParseCfg {
            repeat: false,
        }
    };

    // Delegation when the destination type is `()`,
    //   indicating that the next state is not a child NT
    //   (it is likely the state expecting a closing tag).
    (@!ntref_delegate
        $stack:ident, $ret:expr, (), $_target:expr, $done:expr
    ) => {
        $done
    };

    // Delegate to a child parser by pushing self onto the stack and
    //   yielding to one of the child's states.
    // This uses a trampoline,
    //   which avoids recursive data structures
    //     (due to `ParseState` composition/stitching)
    //   and does not grow the call stack.
    (@!ntref_delegate
        $stack:ident, $ret:expr, $ntnext_st:ty, $target:expr, $_done:expr
    ) => {
        $stack.transfer_with_ret(
            Transition($ret),
            $target,
        )
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
                $ntref:ident,
            )*
        }

        -> {
            @ -> ($ntfirst:path, $ntfirst_st:ty) [$($ntfirst_cfg:tt)?],
            $(
                ($ntprev:path) -> ($ntnext:path, $ntnext_st:ty) [$($ntnext_cfg:tt)?],
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
            #[derive(Debug, PartialEq, Eq)]
            $vis enum $nt {
                #[doc=concat!(
                    "Expecting opening tag for element [`",
                    stringify!($qname),
                    "`]."
                )]
                Expecting_(crate::xir::parse::EleParseCfg),
                /// Recovery state ignoring all remaining tokens for this
                ///   element.
                RecoverEleIgnore_(
                    crate::xir::parse::EleParseCfg,
                    crate::xir::QName,
                    crate::xir::OpenSpan,
                    crate::xir::flat::Depth
                ),
                // Recovery completed because end tag corresponding to the
                //   invalid element has been found.
                RecoverEleIgnoreClosed_(
                    crate::xir::parse::EleParseCfg,
                    crate::xir::QName,
                    crate::xir::CloseSpan
                ),
                /// Recovery state ignoring all tokens when a `Close` is
                ///   expected.
                ///
                /// This is token-agnostic---it
                ///   may be a child element,
                ///     but it may be text,
                ///     for example.
                CloseRecoverIgnore_(
                    (
                        crate::xir::parse::EleParseCfg,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                    crate::span::Span
                ),
                /// Parsing element attributes.
                Attrs_(
                    (
                        crate::xir::parse::EleParseCfg,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                    [<$nt AttrsState_>]
                ),
                $(
                    $ntref(
                        (
                            crate::xir::parse::EleParseCfg,
                            crate::span::Span,
                            crate::xir::flat::Depth
                        ),
                    ),
                )*
                ExpectClose_(
                    (
                        crate::xir::parse::EleParseCfg,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                ),
                /// Closing tag found and parsing of the element is
                ///   complete.
                Closed_(crate::xir::parse::EleParseCfg, crate::span::Span),
            }

            impl From<crate::xir::parse::EleParseCfg> for $nt {
                fn from(repeat: crate::xir::parse::EleParseCfg) -> Self {
                    Self::Expecting_(repeat)
                }
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
                fn child_depth(&self) -> Option<crate::xir::flat::Depth> {
                    match self {
                        $ntfirst((_, _, depth)) => Some(depth.child_depth()),
                        $(
                            $ntnext((_, _, depth)) => Some(depth.child_depth()),
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
                        Self::Expecting_(_) => write!(
                            f,
                            "expecting opening tag {}",
                            TtOpenXmlEle::wrap($qname),
                        ),
                        Self::RecoverEleIgnore_(_, name, _, _)
                        | Self::RecoverEleIgnoreClosed_(_, name, _) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                            expected = TtQuote::wrap($qname),
                        ),
                        Self::CloseRecoverIgnore_((_, _, depth), _) => write!(
                            f,
                            "attempting to recover by ignoring input \
                               until the expected end tag {expected} \
                               at depth {depth}",
                            expected = TtCloseXmlEle::wrap($qname),
                        ),

                        Self::Attrs_(_, sa) => std::fmt::Display::fmt(sa, f),
                        Self::ExpectClose_((_, _, depth)) => write!(
                            f,
                            "expecting closing element {} at depth {depth}",
                            TtCloseXmlEle::wrap($qname)
                        ),
                        Self::Closed_(_, _cfg) => write!(
                            f,
                            "done parsing element {}",
                            TtQuote::wrap($qname)
                        ),
                        $(
                            // TODO: A better description.
                            Self::$ntref(_) => {
                                write!(
                                    f,
                                    "preparing to transition to \
                                       parser for next child element(s)"
                                )
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
                    }
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken<
                    crate::xir::flat::RefinedText
                >;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Context = crate::xir::parse::StateStackContext<Self::Super>;
                type Super = $super;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    #[allow(unused_variables)] // used only if child NTs
                    stack: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self::Super> {
                    use crate::{
                        parse::{EmptyContext, Transition, Transitionable},
                        xir::{
                            EleSpan,
                            flat::{XirfToken, RefinedText},
                            parse::parse_attrs,
                        },
                    };

                    // Used only by _some_ expansions.
                    #[allow(unused_imports)]
                    use crate::xir::flat::Text;

                    use $nt::{
                        Attrs_, Expecting_, RecoverEleIgnore_,
                        CloseRecoverIgnore_, RecoverEleIgnoreClosed_,
                        ExpectClose_, Closed_
                    };

                    match (self, tok) {
                        (
                            Expecting_(cfg),
                            XirfToken::Open(qname, span, depth)
                        ) if qname == $qname => {
                            Transition(Attrs_(
                                (cfg, span.tag_span(), depth),
                                parse_attrs(qname, span)
                            )).incomplete()
                        },

                        (
                            Closed_(cfg, ..),
                            XirfToken::Open(qname, span, depth)
                        ) if cfg.repeat && qname == $qname => {
                            Transition(Attrs_(
                                (cfg, span.tag_span(), depth),
                                parse_attrs(qname, span)
                            )).incomplete()
                        },

                        (
                            Expecting_(cfg),
                            XirfToken::Open(qname, span, depth)
                        ) => {
                            Transition(RecoverEleIgnore_(cfg, qname, span, depth)).err(
                                [<$nt Error_>]::UnexpectedEle_(qname, span.name_span())
                            )
                        },

                        (
                            RecoverEleIgnore_(cfg, qname, _, depth_open),
                            XirfToken::Close(_, span, depth_close)
                        ) if depth_open == depth_close => {
                            Transition(
                                RecoverEleIgnoreClosed_(cfg, qname, span)
                            ).incomplete()
                        },

                        // Depth check is unnecessary since _all_ xir::parse
                        //   parsers
                        //     (at least at the time of writing)
                        //     ignore whitespace and comments,
                        //       so may as well return early.
                        (
                            st,
                            XirfToken::Text(RefinedText::Whitespace(..), _)
                            | XirfToken::Comment(..)
                        ) => {
                            Transition(st).incomplete()
                        }

                        (Attrs_(meta, sa), tok) => {
                            sa.delegate_until_obj::<Self, _>(
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

                                    // Lookahead is added by `delegate_until_obj`.
                                    ele_parse!(@!ntref_delegate
                                        stack,
                                        $ntfirst(meta),
                                        $ntfirst_st,
                                        Transition(
                                            Into::<$ntfirst_st>::into(
                                                ele_parse!(@!ntref_cfg $($ntfirst_cfg)?)
                                            )
                                        ).ok(obj),
                                        Transition($ntfirst(meta)).ok(obj)
                                    )
                                }
                            )
                        },

                        // TODO: This is partly broken by the trampoline
                        //   implementation.
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
                            ($ntprev(meta), tok) => {
                                ele_parse!(@!ntref_delegate
                                    stack,
                                    $ntnext(meta),
                                    $ntnext_st,
                                    Transition(
                                        Into::<$ntnext_st>::into(
                                            ele_parse!(@!ntref_cfg $($ntnext_cfg)?)
                                        )
                                    ).incomplete().with_lookahead(tok),
                                    Transition($ntnext(meta)).incomplete().with_lookahead(tok)
                                )
                            },
                        )*

                        // XIRF ensures proper nesting,
                        //   so we do not need to check the element name.
                        (
                            ExpectClose_((cfg, _, depth))
                            | CloseRecoverIgnore_((cfg, _, depth), _),
                            XirfToken::Close(_, span, tok_depth)
                        ) if tok_depth == depth => {
                            $(
                                let $close_span = span;
                            )?
                            $closemap.transition(Closed_(cfg, span.tag_span()))
                        },

                        (ExpectClose_(meta @ (_, otspan, _)), unexpected_tok) => {
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
            #[derive(Debug, PartialEq, Eq)]
            $vis enum $nt {
                Expecting_(crate::xir::parse::EleParseCfg),
                /// Recovery state ignoring all remaining tokens for this
                ///   element.
                RecoverEleIgnore_(
                    crate::xir::parse::EleParseCfg,
                    crate::xir::QName,
                    crate::xir::OpenSpan,
                    crate::xir::flat::Depth,
                ),
                RecoverEleIgnoreClosed_(
                    crate::xir::parse::EleParseCfg,
                    crate::xir::QName,
                    crate::xir::CloseSpan
                ),
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
                        Self::Expecting_(_) => {
                            write!(f, "expecting {expected}")
                        },

                        Self::RecoverEleIgnore_(_, name, _, _)
                        | Self::RecoverEleIgnoreClosed_(_, name, _) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                        ),

                        Self::Done_ => write!(f, "done parsing {expected}"),
                    }
                }
            }

            impl From<crate::xir::parse::EleParseCfg> for $nt {
                fn from(repeat: crate::xir::parse::EleParseCfg) -> Self {
                    Self::Expecting_(repeat)
                }
            }

            #[derive(Debug, PartialEq)]
            $vis enum [<$nt Error_>] {
                UnexpectedEle_(crate::xir::QName, crate::span::Span),
            }

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
                    }
                }
            }

            impl crate::parse::ParseState for $nt {
                type Token = crate::xir::flat::XirfToken<
                    crate::xir::flat::RefinedText
                >;
                type Object = $objty;
                type Error = [<$nt Error_>];
                type Context = crate::xir::parse::StateStackContext<Self::Super>;
                type Super = $super;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    stack: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self::Super> {
                    use crate::{
                        parse::Transition,
                        xir::{
                            flat::{XirfToken, RefinedText},
                            parse::EleParseCfg,
                        },
                    };

                    use $nt::{
                        Expecting_, RecoverEleIgnore_,
                        RecoverEleIgnoreClosed_, Done_
                    };

                    match (self, tok) {
                        // Depth check is unnecessary since _all_ xir::parse
                        //   parsers
                        //     (at least at the time of writing)
                        //     ignore whitespace and comments,
                        //       so may as well return early.
                        (
                            st,
                            XirfToken::Text(RefinedText::Whitespace(..), _)
                            | XirfToken::Comment(..)
                        ) => {
                            Transition(st).incomplete()
                        }

                        $(
                            (
                                Expecting_(cfg),
                                XirfToken::Open(qname, span, depth)
                            ) if qname == $ntref::qname() => {
                                ele_parse!(@!ntref_delegate
                                    stack,
                                    match cfg.repeat {
                                        true => Expecting_(cfg),
                                        false => Done_,
                                    },
                                    $ntref,
                                    Transition(
                                        $ntref::from(
                                            EleParseCfg::default()
                                        )
                                    ).incomplete().with_lookahead(
                                        XirfToken::Open(qname, span, depth)
                                    ),
                                    unreachable!("TODO: remove me (ntref_delegate done)")
                                )
                            },
                        )*

                        // An unexpected token when repeating ends
                        //   repetition and should not result in an error.
                        (Expecting_(cfg), tok) if cfg.repeat => {
                            Transition(Done_).dead(tok)
                        }

                        (Expecting_(cfg), XirfToken::Open(qname, span, depth)) => {
                            use crate::xir::EleSpan;
                            Transition(RecoverEleIgnore_(cfg, qname, span, depth)).err(
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
                            RecoverEleIgnore_(cfg, qname, _, depth_open),
                            XirfToken::Close(_, span, depth_close)
                        ) if depth_open == depth_close => {
                            Transition(RecoverEleIgnoreClosed_(cfg, qname, span)).incomplete()
                        },

                        (st @ RecoverEleIgnore_(..), _) => {
                            Transition(st).incomplete()
                        },

                        (st @ Self::Done_, tok) => Transition(st).dead(tok),

                        todo => todo!("sum {todo:?}"),
                    }
                }

                fn is_accepting(&self) -> bool {
                    match self {
                        Self::RecoverEleIgnoreClosed_(..) | Self::Done_ => true,
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
                    ele_parse!(@!ntfirst $($nt)*)(
                        crate::xir::parse::EleParseCfg::default().into()
                    )
                }
            }

            $(
                impl From<$nt> for $super {
                    fn from(st: $nt) -> Self {
                        $super::$nt(st)
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

            /// Superstate error object representing the union of all
            ///   related parsers' errors.
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
                type Context = crate::xir::parse::StateStackContext<Self>;

                fn parse_token(
                    self,
                    tok: Self::Token,
                    stack: &mut Self::Context,
                ) -> crate::parse::TransitionResult<Self> {
                    match self {
                        $(
                            // Pass token directly to child until it reports
                            //   a dead state,
                            //     after which we return to the `ParseState`
                            //     atop of the stack.
                            Self::$nt(st) => st.delegate_child(
                                tok,
                                stack,
                                |tok, stack| stack.ret(tok),
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
