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
use std::fmt::Display;

use crate::{
    diagnostic_panic,
    fmt::{DisplayWrapper, TtQuote},
    parse::{
        ClosedParseState, Context, ParseState, Transition, TransitionResult,
    },
    xir::{Prefix, QName},
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
    ///   see [`Self::ret_or_dead`] to perform a return.
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

    /// Attempt to return to a previous [`ParseState`] that transferred
    ///   control away from itself,
    ///     otherwise yield a dead state transition to `deadst`.
    ///
    /// Conceptually,
    ///   this is like returning from a function call,
    ///   where the function was invoked using [`Self::transfer_with_ret`].
    /// However,
    ///   this system is more akin to CPS
    ///     (continuation passing style);
    ///       see [`Self::transfer_with_ret`] for important information.
    ///
    /// If there is no state to return to on the stack,
    ///   then it is assumed that we have received more input than expected
    ///   after having completed a full parse.
    pub fn ret_or_dead(
        &mut self,
        lookahead: S::Token,
        deadst: S,
    ) -> TransitionResult<S> {
        let Self(stack) = self;

        // This should certainly never happen unless there is a bug in the
        //   `ele_parse!` parser-generator,
        //     since it means that we're trying to return to a caller that
        //     does not exist.
        match stack.pop() {
            Some(st) => Transition(st).incomplete().with_lookahead(lookahead),
            None => Transition(deadst).dead(lookahead),
        }
    }

    /// Test every [`ParseState`] on the stack against the predicate `f`.
    pub fn all(&self, f: impl Fn(&S) -> bool) -> bool {
        let Self(stack) = self;
        stack[..].iter().all(f)
    }
}

/// Match some type of node.
#[derive(Debug, PartialEq, Eq)]
pub enum NodeMatcher {
    /// Static [`QName`] with a simple equality check.
    QName(QName),
    /// Any element with a matching [`Prefix`].
    Prefix(Prefix),
}

impl NodeMatcher {
    /// Match against the provided [`QName`].
    pub fn matches(&self, qname: QName) -> bool {
        match self {
            Self::QName(qn_match) if qn_match == &qname => true,
            Self::Prefix(prefix) if Some(*prefix) == qname.prefix() => true,
            _ => false,
        }
    }
}

impl From<QName> for NodeMatcher {
    fn from(qname: QName) -> Self {
        Self::QName(qname)
    }
}

impl From<Prefix> for NodeMatcher {
    fn from(prefix: Prefix) -> Self {
        Self::Prefix(prefix)
    }
}

impl Display for NodeMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::xir::fmt::XmlPrefixAnyLocal;

        match self {
            Self::QName(qname) => Display::fmt(qname, f),
            Self::Prefix(prefix) => XmlPrefixAnyLocal::fmt(prefix, f),
        }
    }
}

#[macro_export]
macro_rules! ele_parse {
    (
        $vis:vis enum $super:ident;

        // Attr has to be first to avoid ambiguity with `$rest`.
        $(type AttrValueError = $evty:ty;)?
        type Object = $objty:ty;

        $(
            [super] {
                $($super_body:tt)*
            };
        )?

        // Combination of square brackets above and the prefix here are
        //   needed for disambiguation.
        $nt_first:ident := $($nt_defs:tt)*
    ) => {
        ele_parse! {@!next $vis $super
            $(type AttrValueError = $evty;)?
            type Object = $objty;
            $nt_first := $($nt_defs)*
        }

        ele_parse!(@!super_sum <$objty> $vis $super
            $([super] { $($super_body)* })?
            $nt_first := $($nt_defs)*
        );
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
        $vis:vis $super:ident $nt:ident $qname:ident
        ($($qname_matched:pat, $open_span:pat)?)

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

        // Streaming (as opposed to aggregate) attribute parsing.
        $([attr]($attr_stream_binding:ident) => $attr_stream_map:expr,)?

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
                /// Non-preemptable [`Self::Expecting_`].
                #[allow(dead_code)] // used by sum parser
                NonPreemptableExpecting_(crate::xir::parse::EleParseCfg),
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
                        crate::xir::QName,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                    crate::span::Span
                ),
                /// Parsing element attributes.
                Attrs_(
                    (
                        crate::xir::parse::EleParseCfg,
                        crate::xir::QName,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                    [<$nt AttrsState_>]
                ),
                $(
                    $ntref(
                        (
                            crate::xir::parse::EleParseCfg,
                            crate::xir::QName,
                            crate::span::Span,
                            crate::xir::flat::Depth
                        ),
                    ),
                )*
                ExpectClose_(
                    (
                        crate::xir::parse::EleParseCfg,
                        crate::xir::QName,
                        crate::span::Span,
                        crate::xir::flat::Depth
                    ),
                ),
                /// Closing tag found and parsing of the element is
                ///   complete.
                Closed_(
                    crate::xir::parse::EleParseCfg,
                    crate::xir::QName,
                    crate::span::Span
                ),
            }

            impl From<crate::xir::parse::EleParseCfg> for $nt {
                fn from(repeat: crate::xir::parse::EleParseCfg) -> Self {
                    Self::Expecting_(repeat)
                }
            }

            impl crate::xir::parse::EleParseState for $nt {}

            impl $nt {
                /// Matcher describing the node recognized by this parser.
                #[allow(dead_code)] // used by sum parser
                #[inline]
                fn matcher() -> crate::xir::parse::NodeMatcher {
                    crate::xir::parse::NodeMatcher::from($qname)
                }

                // Whether the given QName would be matched by any of the
                //   parsers associated with this type.
                //
                #[inline]
                fn matches(qname: crate::xir::QName) -> bool {
                    Self::matcher().matches(qname)
                }

                #[allow(dead_code)] // used by sum parser
                fn cfg(&self) -> crate::xir::parse::EleParseCfg {
                    use $nt::*;

                    match self {
                        Expecting_(cfg)
                        | NonPreemptableExpecting_(cfg)
                        | RecoverEleIgnore_(cfg, ..)
                        | RecoverEleIgnoreClosed_(cfg, ..)
                        | CloseRecoverIgnore_((cfg, ..), ..)
                        | Attrs_((cfg, ..), ..)
                        | ExpectClose_((cfg, ..), ..)
                        | Closed_(cfg, ..) => *cfg,

                        $(
                            $ntref((cfg, ..)) => *cfg,
                        )*
                    }
                }

                /// Whether the parser is in a state that can tolerate
                ///   superstate node preemption.
                ///
                /// For more information,
                ///   see the superstate
                #[doc=concat!(
                    " [`", stringify!($super), "::can_preempt_node`]."
                )]
                fn can_preempt_node(&self) -> bool {
                    use $nt::*;

                    match self {
                        // Preemption before the opening tag is safe,
                        //   since we haven't started processing yet.
                        Expecting_(..) => true,

                        // The name says it all.
                        // Instantiated by the superstate.
                        NonPreemptableExpecting_(..) => false,

                        // Preemption during recovery would cause tokens to
                        //   be parsed when they ought to be ignored,
                        //     so we must process all tokens during recovery.
                        RecoverEleIgnore_(..)
                        | CloseRecoverIgnore_(..) => false,

                        // It is _not_ safe to preempt attribute parsing
                        //   since attribute parsers aggregate until a
                        //   non-attribute token is encountered;
                        //     we must allow attribute parsing to finish its
                        //     job _before_ any preempted nodes are emitted
                        //     since the attributes came _before_ that node.
                        Attrs_(..) => false,

                        // These states represent jump states where we're
                        //   about to transition to the next child parser.
                        // It's safe to preempt here,
                        //   since we're not in the middle of parsing.
                        //
                        // Note that this includes `ExpectClose_` because of
                        //   the macro preprocessing,
                        //     and Rust's exhaustiveness check will ensure
                        //     that it is accounted for if that changes.
                        // If we're expecting that the next token is a
                        //   `Close`,
                        //     then it must be safe to preempt other nodes
                        //     that may appear in this context as children.
                        $ntfirst(..) => true,
                        $(
                            $ntnext(..) => true,
                        )*

                        // If we're done,
                        //   we want to be able to yield a dead state so
                        //   that we can transition away from this parser.
                        RecoverEleIgnoreClosed_(..)
                        | Closed_(..) => false,
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
                        Self::Expecting_(_)
                        | Self::NonPreemptableExpecting_(_) => write!(
                            f,
                            "expecting opening tag {}",
                            TtOpenXmlEle::wrap(Self::matcher()),
                        ),
                        Self::RecoverEleIgnore_(_, name, _, _)
                        | Self::RecoverEleIgnoreClosed_(_, name, _) => write!(
                            f,
                            "attempting to recover by ignoring element \
                               with unexpected name {given} \
                               (expected {expected})",
                            given = TtQuote::wrap(name),
                            expected = TtQuote::wrap(Self::matcher()),
                        ),
                        Self::CloseRecoverIgnore_((_, qname, _, depth), _) => write!(
                            f,
                            "attempting to recover by ignoring input \
                               until the expected end tag {expected} \
                               at depth {depth}",
                            expected = TtCloseXmlEle::wrap(qname),
                        ),

                        Self::Attrs_(_, sa) => std::fmt::Display::fmt(sa, f),
                        Self::ExpectClose_((_, qname, _, depth)) => write!(
                            f,
                            "expecting closing element {} at depth {depth}",
                            TtCloseXmlEle::wrap(qname)
                        ),
                        Self::Closed_(_, qname, _) => write!(
                            f,
                            "done parsing element {}",
                            TtQuote::wrap(qname),
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
                    crate::xir::QName,
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
                            expected = TtOpenXmlEle::wrap($nt::matcher()),
                        ),
                        Self::CloseExpected_(qname, _, tok) => write!(
                            f,
                            "expected {}, but found {}",
                            TtCloseXmlEle::wrap(qname),
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
                                ele_name = TtQuote::wrap($nt::matcher())
                            )
                        ).into(),

                        Self::CloseExpected_(qname, span, tok) => vec![
                            span.note("element starts here"),
                            tok.span().error(format!(
                                "expected {}",
                                TtCloseXmlEle::wrap(qname),
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
                            flat::XirfToken,
                            parse::{parse_attrs, EleParseCfg},
                        },
                    };

                    use $nt::{
                        Attrs_, Expecting_, NonPreemptableExpecting_,
                        RecoverEleIgnore_, CloseRecoverIgnore_,
                        RecoverEleIgnoreClosed_, ExpectClose_, Closed_
                    };

                    // Needed since `$ntfirst_cfg` cannot be nested within
                    //   the conditional `[attr]` block.
                    #[allow(dead_code)]
                    const NTFIRST_CFG: EleParseCfg =
                        ele_parse!(@!ntref_cfg $($ntfirst_cfg)?);

                    match (self, tok) {
                        (
                            Expecting_(cfg) | NonPreemptableExpecting_(cfg),
                            XirfToken::Open(qname, span, depth)
                        ) if $nt::matches(qname) => {
                            let transition = Transition(Attrs_(
                                (cfg, qname, span.tag_span(), depth),
                                parse_attrs(qname, span)
                            ));

                            // Streaming attribute parsing will cause the
                            //   attribute map to be yielded immediately as
                            //   the opening object,
                            //     since we will not be aggregating attrs.
                            $(
                                // Used only to match on `[attr]`.
                                let [<_ $attr_stream_binding>] = ();
                                return transition.ok($attrmap);
                            )?

                            // If the `[attr]` special form was _not_
                            //   provided,
                            //     we'll be aggregating attributes.
                            #[allow(unreachable_code)]
                            transition.incomplete()
                        },

                        (
                            Closed_(cfg, ..),
                            XirfToken::Open(qname, span, depth)
                        ) if cfg.repeat && Self::matches(qname) => {
                            Transition(Attrs_(
                                (cfg, qname, span.tag_span(), depth),
                                parse_attrs(qname, span)
                            )).incomplete()
                        },

                        (
                            Expecting_(cfg) | NonPreemptableExpecting_(cfg),
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

                        // Streaming attribute matching takes precedence
                        //   over aggregate.
                        // This is primarily me being lazy,
                        //   because it's not worth a robust syntax for
                        //   something that's rarely used
                        //     (macro-wise, I mean;
                        //       it's heavily utilized as a percentage of
                        //         source file parsed since short-hand
                        //         template applications are heavily used).
                        $(
                            (
                                st @ Attrs_(..),
                                XirfToken::Attr($attr_stream_binding),
                            ) => Transition(st).ok($attr_stream_map),

                            // Override the aggregate attribute parser
                            //   delegation by forcing the below match to
                            //   become unreachable
                            //     (xref anchor <<SATTR>>).
                            // Since we have already emitted the `$attrmap`
                            //   object on `Open`,
                            //     this yields an incomplete parse.
                            (Attrs_(meta, _), tok) => {
                                ele_parse!(@!ntref_delegate
                                    stack,
                                    $ntfirst(meta),
                                    $ntfirst_st,
                                    Transition(
                                        Into::<$ntfirst_st>::into(
                                            NTFIRST_CFG
                                        )
                                    ).incomplete().with_lookahead(tok),
                                    Transition($ntfirst(meta))
                                        .incomplete()
                                        .with_lookahead(tok)
                                )
                            }
                        )?

                        // This becomes unreachable when the `[attr]` special
                        //   form is provided,
                        //     which overrides this match directly above
                        //       (xref <<SATTR>>).
                        #[allow(unreachable_patterns)]
                        (Attrs_(meta @ (_, qname, _, _), sa), tok) => {
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
                                            let _ = qname; // avoid unused warning
                                            $(
                                                use crate::xir::parse::attr::AttrParseState;
                                                let $qname_matched = qname;
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
                                                NTFIRST_CFG
                                            )
                                        ).ok(obj),
                                        Transition($ntfirst(meta)).ok(obj)
                                    )
                                }
                            )
                        },

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
                            ExpectClose_((cfg, qname, _, depth))
                            | CloseRecoverIgnore_((cfg, qname, _, depth), _),
                            XirfToken::Close(_, span, tok_depth)
                        ) if tok_depth == depth => {
                            $(
                                let $close_span = span;
                            )?
                            $closemap.transition(Closed_(cfg, qname, span.tag_span()))
                        },

                        (ExpectClose_(meta @ (_, qname, otspan, _)), unexpected_tok) => {
                            use crate::parse::Token;
                            Transition(
                                CloseRecoverIgnore_(meta, unexpected_tok.span())
                            ).err([<$nt Error_>]::CloseExpected_(qname, otspan, unexpected_tok))
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

                fn is_accepting(&self, _: &Self::Context) -> bool {
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
                #[allow(dead_code)] // used by superstate node preemption
                /// Non-preemptable [`Self::Expecting_`].
                NonPreemptableExpecting_(crate::xir::parse::EleParseCfg),
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

            impl $nt {
                // Whether the given QName would be matched by any of the
                //   parsers associated with this type.
                //
                // This is short-circuiting and will return as soon as one
                //   parser is found,
                //     so it may be a good idea to order the sum type
                //     according to the most likely value to be encountered.
                // At its worst,
                //   this may be equivalent to a linear search of the
                //   parsers.
                // With that said,
                //   Rust/LLVM may optimize this in any number of ways,
                //   especially if each inner parser matches on a QName
                //   constant.
                // Let a profiler and disassembly guide you.
                #[allow(dead_code)] // used by superstate
                fn matches(qname: crate::xir::QName) -> bool {
                    // If we used an array or a trait,
                    //   then we'd need everything to be a similar type;
                    //     this allows for _any_ type provided that it
                    //     expands into something that contains a `matches`
                    //     associated function of a compatible type.
                    false $(|| $ntref::matches(qname))*
                }

                fn cfg(&self) -> crate::xir::parse::EleParseCfg {
                    use $nt::*;

                    match self {
                        Expecting_(cfg)
                        | NonPreemptableExpecting_(cfg)
                        | RecoverEleIgnore_(cfg, ..)
                        | RecoverEleIgnoreClosed_(cfg, ..) => *cfg,

                        Done_ => crate::xir::parse::EleParseCfg::default()
                    }
                }

                /// Whether the parser is in a state that can tolerate
                ///   superstate node preemption.
                ///
                /// For more information,
                ///   see the superstate
                #[doc=concat!(
                    " [`", stringify!($super), "::can_preempt_node`]."
                )]
                fn can_preempt_node(&self) -> bool {
                    use $nt::*;

                    match self {
                        // Preemption before the opening tag is safe,
                        //   since we haven't started processing yet.
                        Expecting_(..) => true,

                        // The name says it all.
                        // Instantiated by the superstate.
                        NonPreemptableExpecting_(..) => false,

                        // Preemption during recovery would cause tokens to
                        //   be parsed when they ought to be ignored,
                        //     so we must process all tokens during recovery.
                        RecoverEleIgnore_(..) => false,

                        // If we're done,
                        //   we want to be able to yield a dead state so
                        //   that we can transition away from this parser.
                        RecoverEleIgnoreClosed_(..)
                        | Done_ => false,
                    }
                }
            }

            impl std::fmt::Display for $nt {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use crate::{
                        fmt::{DisplayWrapper, ListDisplayWrapper, TtQuote},
                        xir::fmt::EleSumList,
                    };

                    let ntrefs = [
                        $(
                            $ntref::matcher(),
                        )*
                    ];
                    let expected = EleSumList::wrap(&ntrefs);

                    match self {
                        Self::Expecting_(_)
                        | Self::NonPreemptableExpecting_(_) => {
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
                        xir::fmt::EleSumList,
                    };

                    let ntrefs = [
                        $(
                            $ntref::matcher(),
                        )*
                    ];
                    let expected = EleSumList::wrap(&ntrefs);

                    match self {
                        Self::UnexpectedEle_(qname, span) => {
                            span
                                .error(format!(
                                    "element {name} cannot appear here",
                                    name = TtQuote::wrap(qname),
                                ))
                                .with_help(format!("expecting {expected}"))
                                .into()
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
                            flat::XirfToken,
                            parse::EleParseCfg,
                        },
                    };

                    use $nt::{
                        Expecting_, NonPreemptableExpecting_, RecoverEleIgnore_,
                        RecoverEleIgnoreClosed_, Done_
                    };

                    match (self, tok) {
                        $(
                            (
                                st @ (Expecting_(_) | NonPreemptableExpecting_(_)),
                                XirfToken::Open(qname, span, depth)
                            ) if $ntref::matches(qname) => {
                                ele_parse!(@!ntref_delegate
                                    stack,
                                    match st.cfg() {
                                        cfg @ EleParseCfg { repeat: true, .. } => {
                                            Expecting_(cfg)
                                        },
                                        _ => Done_,
                                    },
                                    $ntref,
                                    Transition(
                                        // Propagate non-preemption status,
                                        //   otherwise we'll provide a
                                        //   lookback of the original token
                                        //   and end up recursing until we
                                        //   hit the `stack` limit.
                                        match st {
                                            NonPreemptableExpecting_(_) => {
                                                $ntref::NonPreemptableExpecting_(
                                                    EleParseCfg::default()
                                                )
                                            }
                                            _ => {
                                                $ntref::from(
                                                    EleParseCfg::default()
                                                )
                                            }
                                        }
                                    ).incomplete().with_lookahead(
                                        XirfToken::Open(qname, span, depth)
                                    ),
                                    unreachable!("TODO: remove me (ntref_delegate done)")
                                )
                            },

                            (
                                NonPreemptableExpecting_(cfg),
                                XirfToken::Open(qname, span, depth)
                            ) if $ntref::matches(qname) => {
                                ele_parse!(@!ntref_delegate
                                    stack,
                                    match cfg.repeat {
                                        true => Expecting_(cfg),
                                        false => Done_,
                                    },
                                    $ntref,
                                    Transition(
                                        $ntref::NonPreemptableExpecting_(
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
                        (
                            Expecting_(cfg) | NonPreemptableExpecting_(cfg),
                            tok
                        ) if cfg.repeat => Transition(Done_).dead(tok),

                        (
                            Expecting_(cfg) | NonPreemptableExpecting_(cfg),
                            XirfToken::Open(qname, span, depth)
                        ) => {
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

                        (
                            st @ (Done_ | RecoverEleIgnoreClosed_(..)),
                            tok
                        ) => Transition(st).dead(tok),

                        todo => todo!("sum {todo:?}"),
                    }
                }

                fn is_accepting(&self, _: &Self::Context) -> bool {
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
            [super] {
                // Non-whitespace text nodes can be mapped into elements
                //   with the given QName as a preprocessing step,
                //     allowing them to reuse the existing element NT system.
                $([text]($text:ident, $text_span:ident) => $text_map:expr,)?

                // Optional _single_ NT to preempt arbitrary elements.
                // Sum NTs can be used to preempt multiple elements.
                $($pre_nt:ident)?
            }
        )?
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
                    use crate::{
                        parse::Transition,
                        xir::flat::{XirfToken, RefinedText},
                    };

                    // Used only by _some_ expansions.
                    #[allow(unused_imports)]
                    use crate::xir::flat::Text;

                    match (self, tok) {
                        // [super] {
                        $(
                            // [text] preemption;
                            //   see `Self::can_preempt_node`.
                            $(
                                (
                                    st,
                                    XirfToken::Text(
                                        RefinedText::Unrefined(
                                            Text($text, $text_span)
                                        ),
                                        _,
                                    )
                                ) if st.can_preempt_node() => {
                                    Transition(st).ok($text_map)
                                },
                            )?

                            // Preemption NT
                            $(
                                (
                                    st,
                                    XirfToken::Open(
                                        qname,
                                        ospan,
                                        depth,
                                    ),
                                ) if st.can_preempt_node() && $pre_nt::matches(qname) => {
                                    stack.transfer_with_ret(
                                        Transition(st),
                                        Transition(
                                            // Prevent recursing on this token.
                                            $pre_nt::NonPreemptableExpecting_(
                                                ele_parse!(@!ntref_cfg)
                                            )
                                        )
                                        .incomplete()
                                        .with_lookahead(XirfToken::Open(
                                            qname,
                                            ospan,
                                            depth,
                                        )),
                                    )
                                },
                            )?
                        )?
                        // }

                        // Depth check is unnecessary since _all_ xir::parse
                        //   parsers
                        //     (at least at the time of writing)
                        //     ignore whitespace and comments,
                        //       so may as well return early.
                        // TODO: I'm ignoring _all_ text for now to
                        //   proceed with development; fix.
                        (
                            st,
                            XirfToken::Text(RefinedText::Whitespace(..), _)
                            | XirfToken::Comment(..)
                        ) => {
                            Transition(st).incomplete()
                        }

                        $(
                            // Pass token directly to child until it reports
                            //   a dead state,
                            //     after which we return to the `ParseState`
                            //     atop of the stack.
                            (Self::$nt(st), tok) => st.delegate_child(
                                tok,
                                stack,
                                |deadst, tok, stack| {
                                    stack.ret_or_dead(tok, deadst)
                                },
                            ),
                        )*
                    }
                }

                fn is_accepting(&self, stack: &Self::Context) -> bool {
                    // This is short-circuiting,
                    //   starting at the _bottom_ of the stack and
                    //   moving upward.
                    // The idea is that,
                    //   is we're still in the middle of parsing,
                    //   then it's almost certain that the [`ParseState`] on
                    //     the bottom of the stack will not be in an
                    //     accepting state,
                    //       and so we can stop checking early.
                    // In most cases,
                    //   if we haven't hit EOF early,
                    //   the stack should be either empty or consist of only
                    //     the root state.
                    //
                    // After having considered the stack,
                    //   we can then consider the active `ParseState`.
                    stack.all(|st| st.is_inner_accepting(stack))
                        && self.is_inner_accepting(stack)
                }
            }

            impl $super {
                /// Whether the inner (active child) [`ParseState`] is in an
                ///   accepting state.
                fn is_inner_accepting(
                    &self,
                    ctx: &<Self as crate::parse::ParseState>::Context
                ) -> bool {
                    use crate::parse::ParseState;

                    match self {
                        $(
                            Self::$nt(st) => st.is_accepting(ctx),
                        )*
                    }
                }

                /// Whether the inner parser is in a state that can tolerate
                ///   superstate node preemption.
                ///
                /// Node preemption allows us (the superstate) to ask for
                ///   permission from the inner parser to parse some token
                ///   ourselves,
                ///     by asking whether the parser is in a state that
                ///     would cause semantic issues if we were to do so.
                ///
                /// For example,
                ///   if we were to preempt text nodes while an inner parser
                ///   was still parsing attributes,
                ///     then we would emit an object associated with that
                ///     text before the inner parser had a chance to
                ///     conclude that attribute parsing has completed and
                ///     emit the opening object for that node;
                ///       the result would otherwise be an incorrect
                ///       `Text, Open` instead of the correct `Open, Text`,
                ///         which would effectively unparent the text.
                /// Similarly,
                ///   if we were to parse our own tokens while an inner
                ///   parser was performing error recovery in such a way as
                ///   to ignore all child tokens,
                ///     then we would emit an object in an incorrect
                ///     context.
                #[allow(dead_code)] // TODO: Remove when using for tpl apply
                fn can_preempt_node(&self) -> bool {
                    match self {
                        $(
                            Self::$nt(st) => st.can_preempt_node(),
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
