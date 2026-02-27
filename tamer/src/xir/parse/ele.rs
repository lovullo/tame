// XIR element parser generator
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

//! Element parser generator for parsing of [XIRF](super::super::flat).
//!
//! _TODO:_ This needs significantly more documentation;
//!   this is one of the most confusing and complex components of TAMER.
//! Until then,
//!   this module has extensive test cases that illustrate its behavior and
//!   can serve as examples.

use super::{AttrParseState, SumNtError};
use crate::{
    diagnose::Diagnostic,
    fmt::{DisplayWrapper, TtQuote},
    parse::{
        ClosedParseState, Context, ParseState, StateStack, Transition,
        TransitionResult,
    },
    span::Span,
    xir::{
        CloseSpan, EleSpan, OpenSpan, Prefix, QName,
        flat::{Depth, RefinedText, XirfToken},
    },
};
use std::{
    fmt::{Debug, Display, Formatter},
    marker::PhantomData,
};

#[cfg(doc)]
use crate::{ele_parse, parse::Parser};

/// A parser accepting a single element.
pub trait EleParseState: ParseState {}

/// Maximum level of parser stack nesting.
///
/// The intent of this value is to ensure that TAMER will fail if something
///   goes terribly wrong,
///     e.g. if the system has a bug where lookahead causes the system to
///     recurse infinitely.
///
/// Unfortunately,
///   this limit _does not_ correspond to the level of XML nesting;
///     parsers composed of Sum NTs,
///       in particular,
///       push multiple parsers onto the stack for a single element.
pub const XIR_MAX_DEPTH: usize = 1024;

/// [`SuperState`] [`Context`] that gets propagated to each child parser.
pub type SuperStateContext<S> = Context<StateStack<S, XIR_MAX_DEPTH>>;

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
        $(#[$super_attr:meta])*
        $vis:vis enum $super:ident;

        // Will either be the user-provided followed by default,
        //   or only the default.
        // This is placed after `enum` due to `vis` ambiguity,
        //   but it also suggests that everything following it will be
        //   within a module,
        //     with `$super` exported.
        $(mod $mod:ident;)?

        // Attr has to be first to avoid ambiguity with `$rest`.
        type AttrValueError = $avety:ty;
        type Object = $objty:ty;

        $(
            [super] {
                $($super_body:tt)*
            };
        )?

        $(
          $(#[$nt_attr:meta])*
          $nt:ident :=
            // normal NT: QNAME(args...) { ... }
            $( $qname:ident {                           // \
              $($matches:tt)*                           //  }---.
            } )?                                        // /    |
                                                        //      |
            // sum NT: (N₀ | N₁ | ... | Nₙ)             //      |
            $( ($($sum:tt)*) )?                         // --.  |
          ;                                             //   |  |
        )*                                              //   |  |
    ) => {paste::paste!{                                //   |  |
        ele_parse!(@!mod $vis $super; $(mod $mod)? {    //   |  |
            use super::*;                               //   |  |
                                                        //   |  |
            use $crate::{                               //   |  |
                diagnose::{Diagnostic, AnnotatedSpan},  //   |  |
                parse::*,                               //   |  |
                xir::{                                  //   |  |
                    *,                                  //   |  |
                    flat::{                             //   |  |
                        Depth, RefinedText, XirfToken   //   |  |
                    },                                  //   |  |
                    parse::*,                           //   |  |
                },                                      //   |  |
            };                                          //   |  |
                                                        //   |  |
            mod meta {                                  //   |  |
                use super::*;                           //   |  |
                                                        //   |  |
                pub type Super = $super;                //   |  |
                pub type AttrValueError = $avety;       //   |  |
                pub type Object = $objty;               //   |  |
            }                                           //   |  |
                                                        //   |  |
            // Dispatch to a parser for either the      //   |  |
            // "normal" or the sum NT grammar.          //   |  |
            $(                                          //   |  |
                ele_parse!(@!define_nt                  //   |  |
                  $(#[$nt_attr])*                       //   |  |
                  $nt                                   //   |  |
                  $(( $($sum)*            ))?           // <-'  |
                  $({ $qname $($matches)* })?           // <----'
                );
            )*

            ele_parse!(@!super_sum $(#[$super_attr])* $super
                $([super] { $($super_body)* })?
                $($nt),*
            );
        });
    }};

    // `mod $ident;` form was provided by the user.
    (@!mod $vis:vis $super:ident; mod $mod:ident { $($body:tt)* }) => {paste::paste!{
        mod $mod {
            $($body)*
        }

        $vis use $mod::$super;
    }};

    // `mod $ident;` form was _not_ provided, and so we must generate one.
    (@!mod $vis:vis $super:ident; { $($body:tt)* }) => {paste::paste!{
        mod [<_ele_parse $super:snake>] {
            $($body)*
        }

        $vis use [<_ele_parse $super:snake>]::$super;
    }};

    // Expand the provided data to a more verbose form that provides the
    //   context necessary for state transitions.
    (@!define_nt
        $(#[$nt_attr:meta])*
        $nt:ident {
            $qname:ident

            // An opening token is always required to ensure that every
            //   parse has a mapping and things don't get lost.
            (Open $openpat:pat) => $openmap:expr,

            // Attribute definition special form.
            @ {
                $(
                    $(#[$fattr:meta])*
                    $fmatch:tt => $fexpr:expr,
                )*
            }

            // Alternative attribute handling
            $(
                (Attr $attrpat:pat) => $attrmap:expr,
            )?

            // Nonterminal references are provided as a list.
            // A configuration specifier can be provided,
            //   currently intended to support the Kleene star.
            $(
                $ntref:ident,
            )*

            $( (Close $closepat:pat) => $closemap:expr, )?
        }
    ) => { paste::paste! {
        $crate::attr_parse_stream! {
            /// Attribute parser for
            #[doc=concat!("[`", stringify!($nt), "`].")]
            type Object = meta::Object;
            type ValueError = meta::AttrValueError;

            #[doc(hidden)]
            pub [<$nt AttrState_>] {
                $(
                    $(#[$fattr])*
                    $fmatch => $fexpr,
                )*
            }
        }

        ele_parse! {
            @!ele_dfn_body
            $(#[$nt_attr])* $nt $qname

            ($openpat) => $openmap,
            ($($closepat)?) => ele_parse!(@!ele_close $($closemap)?),

            $(($attrpat) => $attrmap,)?

            <> {
                $(
                    $ntref,
                )*
            }

            // Generate state transitions of the form `(S) -> (S')`.
            -> {
                @ ->
                $(
                    ([<$nt ChildNt_>]::$ntref, $ntref),
                    ([<$nt ChildNt_>]::$ntref, $ntref) ->
                )* ([<$nt ChildNt_>]::ExpectClose_, ()),
            }
        }
    } };

    (@!define_nt
        $(#[$nt_attr:meta])*
        $nt:ident( $ntref_first:ident $(| $ntref:ident)+ )
    ) => {
        ele_parse!(@!ele_dfn_sum
            $(#[$nt_attr])* $nt [$ntref_first $($ntref)*]
        );
    };

    // No explicit Close mapping defaults to doing nothing at all
    //   (so yield Incomplete).
    (@!ele_close) => {
        ParseStatus::Incomplete
    };

    (@!ele_close $close:expr) => {
        ParseStatus::Object($close)
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

    (@!ele_dfn_body
        $(#[$nt_attr:meta])* $nt:ident $qname:ident

        ($openpat:pat) => $openmap:expr,
        ($($closepat:pat)?) => $closemap:expr,

        // Attribute delegation special form.
        $(($attr_stream_binding:pat) => $attr_stream_map:expr,)?

        // Nonterminal references.
        <> {
            $(
                $ntref:ident,
            )*
        }

        -> {
            @ -> ($ntfirst:path, $ntfirst_st:ty),
            $(
                ($ntprev:path, $ntprev_st:ty) -> ($ntnext:path, $ntnext_st:ty),
            )*
        }
    ) => { paste::paste! {
        #[derive(Debug, PartialEq, Eq)]
        pub enum [<$nt ChildNt_>] {
            $(
                $ntref((QName, OpenSpan, Depth)),
            )*
            ExpectClose_((QName, OpenSpan, Depth)),
        }

        $(#[$nt_attr])*
        ///
        #[doc=concat!("Parser for element [`", stringify!($qname), "`].")]
        #[derive(Debug, PartialEq, Eq)]
        pub struct $nt(NtState<$nt>);

        impl NtBase for $nt {
            type NtSuper = meta::Super;
            type ParseState = Self;
            type ParseError = NtError<$nt>;

            fn preemptable() -> Self::ParseState {
                Self(NtState::Expecting)
            }

            /// A default state that cannot be preempted by the superstate.
            #[allow(dead_code)] // not utilized for every NT
            fn non_preemptable() -> Self::ParseState {
                Self(NtState::NonPreemptableExpecting)
            }

            /// Whether the given QName would be matched by any of the
            ///   parsers associated with this type.
            #[inline]
            fn matches(qname: QName) -> Option<Self::NtSuper> {
                <Self as Nt>::matcher()
                    .matches(qname)
                    .then_some(Self::preemptable().into())
            }

            /// Number of [`NodeMatcher`]s considered by this parser.
            ///
            /// This is always `1` for this parser.
            #[allow(dead_code)] // used by Sum NTs
            fn matches_n() -> usize {
                1
            }

            /// Format matcher for display.
            ///
            /// This value may be rendered singularly or as part of a list of
            ///   values joined together by Sum NTs.
            /// This function receives the number of values to be formatted
            ///   as `n` and the current 0-indexed offset within that list
            ///   as `i`.
            /// This allows for zero-copy rendering of composable NTs.
            ///
            /// `i` must be incremented after the operation.
            #[allow(dead_code)] // used by Sum NTs
            fn fmt_matches(
                n: usize,
                i: &mut usize,
                f: &mut std::fmt::Formatter
            ) -> std::fmt::Result {
                use $crate::{
                    fmt::ListDisplayWrapper,
                    xir::fmt::EleSumList,
                };

                let matcher = &<Self as Nt>::matcher();
                EleSumList::fmt_nth(n, *i, matcher, f)?;
                *i += 1;

                Ok(())
            }

            /// Whether the parser is in a state that can tolerate superstate
            ///   node preemption.
            ///
            /// For more information,
            ///   see the superstate.
            fn can_preempt_node(&self) -> bool {
                match self {
                    Self(st) => st.can_preempt_node(),
                }
            }

            #[allow(dead_code)] // used only when there are child NTs
            /// Whether the current state represents the last child NT.
            fn is_last_nt(&self) -> bool {
                use NtState::*;

                let Self(st) = self;

                // This results in `Self::$ntref(..) => true,` for the
                //   _last_ NT,
                //     and `=> false` for all others.
                // If there are no NTs,
                //   it results in `Self::Attrs(..) => true,`,
                //     which is technically true but will never be called in
                //     that context.
                match st {
                    Attrs(..) => $(
                        false,
                        Jmp([<$nt ChildNt_>]::$ntref(..)) =>
                    )* true,

                    _ => false,
                }
            }
        }

        impl Nt for $nt {
            type AttrState = [<$nt AttrState_>];
            type ChildNt = [<$nt ChildNt_>];

            #[inline]
            fn matcher() -> NodeMatcher {
                NodeMatcher::from($qname)
            }
        }

        impl std::fmt::Display for $nt {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    Self(st) => std::fmt::Display::fmt(st, f),
                }
            }
        }

        impl ParseState for $nt {
            type Token = <Self::Super as ParseState>::Token;
            type Object = <Self::Super as ParseState>::Object;
            type Error = NtError<$nt>;
            type Context = <Self::Super as ParseState>::Context;
            type Super = <Self as NtBase>::NtSuper;

            fn parse_token(
                self,
                tok: Self::Token,
                #[allow(unused_variables)] // used only if child NTs
                stack: &mut Self::Context,
            ) -> TransitionResult<Self::Super> {
                use NtState::{
                    Attrs, Expecting, NonPreemptableExpecting,
                    RecoverEleIgnore, CloseRecoverIgnore,
                    RecoverEleIgnoreClosed, Closed, Jmp,
                };

                let Self(selfst) = self;

                match (selfst, tok) {
                    (
                        Expecting | NonPreemptableExpecting | Closed(..),
                        XirfToken::Open(qname, span, depth)
                    ) if Self::matches(qname).is_some() => {
                        let $openpat = (qname, span);

                        <Self::Object>::try_from($openmap)
                            .map(ParseStatus::Object)
                            .transition(Self(Attrs(
                                (qname, span, depth),
                                parse_attrs(qname, span)
                            )))
                    },

                    // We only attempt recovery when encountering an
                    //   unknown token if we're forced to accept that token.
                    (
                        NonPreemptableExpecting,
                        XirfToken::Open(qname, span, depth)
                    ) => {
                        Transition(Self(
                            RecoverEleIgnore(qname, span, depth)
                        )).err(
                            Self::Error::UnexpectedEle(
                                qname, span.name_span()
                            )
                        )
                    },

                    (
                        RecoverEleIgnore(qname, _, depth_open),
                        XirfToken::Close(_, span, depth_close)
                    ) if depth_open == depth_close => {
                        Transition(Self(
                            RecoverEleIgnoreClosed(qname, span)
                        )).incomplete()
                    },

                    (Attrs(meta, sa), tok) => {
                        // When the [attr] special form is used,
                        //   we entirely delegate attribute parsing without
                        //   performing our own explicit mapping.
                        $(
                            if let XirfToken::Attr(Attr(name, value, attrspan)) = tok {
                                let $attr_stream_binding = (name, value, attrspan);

                                return Transition(Self(Attrs(meta, sa)))
                                    .ok(<Self::Object>::from($attr_stream_map));
                            }
                        )?

                        // TODO: We want to detect pattern conflicts so that
                        //   they can be exposed in the macro DSL.
                        sa.delegate::<Self, _>(
                            tok,
                            EmptyContext,
                            |sa| Transition(Self(Attrs(meta, sa))),
                            || Transition(Self(Jmp($ntfirst(meta)))),
                        )
                    },

                    $(
                        // We're transitioning from `(ntprev) -> (ntnext)`.
                        // If we have a token that matches `ntprev`,
                        //   we can transition _back_ to that state rather
                        //   than transitioning forward.
                        // We can _only_ do this when we know we are
                        //   transitioning away from this state,
                        //     otherwise we could return to a previous state,
                        //     which violates the semantics of the implied
                        //     DFA.
                        (
                            Jmp($ntprev(meta)),
                            XirfToken::Open(qname, span, depth)
                        ) if $ntprev_st::matches(qname).is_some() => {
                            let tok = XirfToken::Open(qname, span, depth);

                            ele_parse!(@!ntref_delegate
                                stack,
                                Self(Jmp($ntprev(meta))),
                                $ntprev_st,
                                // This NT said it could process this token,
                                //   so force it to either do so or error,
                                //   to ensure that bugs don't cause infinite
                                //     processing of lookahead.
                                Transition(<$ntprev_st>::non_preemptable())
                                    .incomplete()
                                    .with_lookahead(tok),
                                Transition(Self(Jmp($ntprev(meta))))
                                    .incomplete()
                                    .with_lookahead(tok)
                            )
                        },

                        (Jmp($ntprev(meta)), tok) => {
                            ele_parse!(@!ntref_delegate
                                stack,
                                Self(Jmp($ntnext(meta))),
                                $ntnext_st,
                                Transition(<$ntnext_st>::preemptable())
                                    .incomplete()
                                    .with_lookahead(tok),
                                Transition(Self(Jmp($ntnext(meta))))
                                    .incomplete()
                                    .with_lookahead(tok)
                            )
                        },

                        // Since `ExpectClose_` does not have an `$ntprev`
                        //   match,
                        //     we have to handle transitioning back to the
                        //     previous state as a special case.
                        // Further,
                        //   we choose to transition back to this state
                        //   _no matter what the element_,
                        //     to force error recovery and diagnostics
                        //     in that context,
                        //       which will tell the user what elements were
                        //       expected in the last NT rather than just
                        //       telling them a closing tag was expected.
                        //
                        // To avoid a bunch of rework of this macro
                        //   (which can hopefully be done in the future),
                        //   this match is output for _every_ NT,
                        //     but takes effect only for the final NT because
                        //     of the `is_last_nt` predicate.
                        // _It is important that it only affect the
                        //   final NT_,
                        //     otherwise we'll transition back to _any_
                        //     previous state at the close,
                        //       which completely defeats the purpose of
                        //       having ordered states.
                        (
                            Jmp(<Self as Nt>::ChildNt::ExpectClose_(meta)),
                            XirfToken::Open(qname, span, depth)
                        ) if Self(Jmp($ntprev(meta))).is_last_nt() => {
                            let tok = XirfToken::Open(qname, span, depth);
                            stack.transfer_with_ret(
                                Transition(Self(Jmp($ntprev(meta)))),
                                // If this NT cannot handle this element,
                                //   it should error and enter recovery to
                                //   ignore it.
                                Transition(<$ntprev_st>::non_preemptable())
                                    .incomplete()
                                    .with_lookahead(tok),
                            )
                        },
                    )*

                    // XIRF ensures proper nesting,
                    //   so we do not need to check the element name.
                    (
                        Jmp(<Self as Nt>::ChildNt::ExpectClose_((qname, _, depth)))
                        | CloseRecoverIgnore((qname, _, depth), _),
                         XirfToken::Close(_, span, tok_depth)
                    ) if tok_depth == depth => {
                        $(let $closepat = (qname, span);)?
                        $closemap.transition(Self(Closed(Some(qname), span.tag_span())))
                    },

                    (
                        Jmp(<Self as Nt>::ChildNt::ExpectClose_(meta @ (qname, otspan, _))),
                        unexpected_tok
                    ) => {
                        Transition(Self(
                            CloseRecoverIgnore(meta, unexpected_tok.span())
                        )).err(
                            Self::Error::CloseExpected(qname, otspan, unexpected_tok)
                        )
                    }

                    // We're still in recovery,
                    //   so this token gets thrown out.
                    (st @ (RecoverEleIgnore(..) | CloseRecoverIgnore(..)), _) => {
                        Transition(Self(st)).incomplete()
                    },

                    // Note that this does not necessarily represent an
                    //   accepting state
                    //     (see `is_accepting`).
                    (
                        st @ (
                            Expecting
                            | NonPreemptableExpecting
                            | Closed(..)
                            | RecoverEleIgnoreClosed(..)
                        ),
                        tok
                    ) => {
                        Transition(Self(st)).dead(tok)
                    }
                }
            }

            fn is_accepting(&self, _: &Self::Context) -> bool {
                use NtState::*;
                matches!(*self, Self(Closed(..) | RecoverEleIgnoreClosed(..)))
            }
        }
    }};

    (@!ele_dfn_sum
        $(#[$nt_attr:meta])* $nt:ident [$($ntref:ident)*]
    ) => {paste::paste! {
        $(#[$nt_attr])*
        ///
        #[doc=concat!(
            "Parser expecting one of ",
            $("[`", stringify!($ntref), "`], ",)*
            "."
        )]
        #[derive(Debug, PartialEq, Eq, Default)]
        pub struct $nt(SumNtState<$nt>);

        impl NtBase for $nt {
            type NtSuper = meta::Super;
            type ParseState = SumNtState<$nt>;
            type ParseError = SumNtError<$nt>;

            fn preemptable() -> Self::ParseState {
               SumNtState::Expecting
            }

            fn non_preemptable() -> Self::ParseState {
                SumNtState::NonPreemptableExpecting
            }

            fn matches(qname: QName) -> Option<Self::NtSuper> {
                None::<Self::NtSuper> $( .or_else(|| $ntref::matches(qname)) )*
            }

            // Number of
            //   [`NodeMatcher`](crate::xir::parse::NodeMatcher)s
            //   considered by this parser.
            //
            // This is the sum of the number of matches of each
            //   constituent NT.
            fn matches_n() -> usize {
                // Count the number of NTs by adding the number of
                //   matches in each.
                0 $(+ $ntref::matches_n())*
            }

            /// Format constituent NTs for display.
            ///
            /// This function receives the number of values to be
            ///   formatted as `n` and the current 0-indexed offset within
            ///   that list as `i`.
            /// This allows for zero-copy rendering of composable NTs.
            ///
            /// See also [`SumNt::fmt_matches_top`] to initialize the
            ///   formatting process with the correct values.
            ///
            /// [`SumNt::fmt_matches_top`]: crate::xir::parse::SumNt
            fn fmt_matches(
                n: usize,
                i: &mut usize,
                f: &mut std::fmt::Formatter
            ) -> std::fmt::Result {
                $(
                    $ntref::fmt_matches(n, i, f)?;
                )*

                Ok(())
            }

            /// Whether the parser is in a state that can tolerate
            ///   superstate node preemption.
            ///
            /// For more information,
            ///   see the superstate.
            fn can_preempt_node(&self) -> bool {
                match self {
                    Self(st) => st.can_preempt_node(),
                }
            }

            fn is_last_nt(&self) -> bool {
                false
            }
        }

        impl SumNt for $nt {
            fn fmt_matches_top(f: &mut std::fmt::Formatter) -> std::fmt::Result {
                Self::fmt_matches(Self::matches_n().saturating_sub(1), &mut 0, f)
            }
        }

        impl std::fmt::Display for $nt {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    Self(st) => std::fmt::Display::fmt(st, f),
                }
            }
        }
    }};

    // Generate superstate sum type.
    //
    // This is really annoying because we cannot read the output of another
    //   macro,
    //     and so we have to do our best to re-parse the body of the
    //     original `ele_parse!` invocation without duplicating too much
    //     logic,
    //       and we have to do so in a way that we can aggregate all of
    //       those data.
    (@!super_sum $(#[$super_attr:meta])* $super:ident
        $(
            [super] {
                // Non-whitespace text nodes can be mapped into elements
                //   with the given QName as a preprocessing step,
                //     allowing them to reuse the existing element NT system.
                $( (Text $textpat:pat) => $textmap:expr, )?

                // Optional _single_ NT to preempt arbitrary elements.
                // Sum NTs can be used to preempt multiple elements.
                $($pre_nt:ident)?
            }
        )?
        $($nt:ident),*
    ) => { paste::paste! {
        $(#[$super_attr])*
        ///
        /// Superstate representing the union of all related parsers.
        ///
        /// This [`ParseState`] allows sub-parsers to independently the
        ///   states associated with their own subgraph,
        ///     and then yield a state transition directly to a state of
        ///     another parser.
        /// This is conceptually like CPS (continuation passing style),
        ///   where this [`ParseState`] acts as a trampoline.
        ///
        /// This [`ParseState`] is required for use with [`Parser`];
        ///   see [`ClosedParseState`] for more information.
        ///
        /// [`Parser`]: crate::parse::Parser
        /// [`ParseState`]: crate::parse::ParseState
        /// [`ClosedParseState`]: crate::parse::ClosedParseState
        #[derive(Debug, PartialEq, Eq)]
        pub enum $super {
            $(
                $nt(<$nt as NtBase>::ParseState),
            )*
        }

        // Default parser is the first NT,
        //   and is non-preemptable to force error handling if the root node
        //   is unexpected.
        // Note that this also prevents preemption at the root,
        //   which is necessary for now anyway since we need to be able
        //   to statically resolve imports without template expansion in
        //     NIR
        //     (otherwise we have a chicken-and-egg problem).
        impl Default for $super {
            fn default() -> Self {
                ele_parse!(@!ntfirst_init $super, $($nt)*)
            }
        }

        $(
            impl From<<$nt as NtBase>::ParseState> for $super {
                fn from(st: <$nt as NtBase>::ParseState) -> Self {
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

        /// Superstate error object representing the union of all related
        ///   parsers' errors.
        #[derive(Debug, PartialEq)]
        pub enum [<$super Error_>] {
            $(
                $nt(<<$nt as NtBase>::ParseState as ParseState>::Error),
            )*
        }

        $(
            impl From<<<$nt as NtBase>::ParseState as ParseState>::Error>
                for [<$super Error_>]
            {
                fn from(e: <<$nt as NtBase>::ParseState as ParseState>::Error) -> Self {
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

        impl Diagnostic for [<$super Error_>] {
            fn describe(&self) -> Vec<AnnotatedSpan<'_>> {
                match self {
                    $(
                        Self::$nt(e) => e.describe(),
                    )*
                }
            }
        }

        impl ParseState for $super {
            type Token = XirfToken<RefinedText>;
            type Object = meta::Object;
            type Error = [<$super Error_>];
            type Context = SuperStateContext<Self>;

            fn parse_token(
                self,
                tok: Self::Token,
                stack: &mut Self::Context,
            ) -> TransitionResult<Self> {
                // Used only by _some_ expansions.
                #[allow(unused_imports)]
                use $crate::xir::flat::Text;

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
                                        Text(sym, span)
                                    ),
                                    _,
                                )
                            ) if st.can_preempt_node() => {
                                let $textpat = (sym, span);
                                Transition(st).ok(<Self::Object>::from($textmap))
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
                            ) if st.can_preempt_node() && $pre_nt::matches(qname).is_some() => {
                                stack.transfer_with_ret(
                                    Transition(st),
                                    Transition(
                                        // Prevent recursing on this token.
                                        $pre_nt::non_preemptable()
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
                                stack.ret_or_dead(deadst, tok)
                            },
                        ),
                    )*
                }
            }

            fn is_accepting(&self, stack: &Self::Context) -> bool {
                // This is short-circuiting,
                //   starting at the _bottom_ of the stack and moving
                //   upward.
                // The idea is that,
                //   is we're still in the middle of parsing,
                //   then it's almost certain that the [`ParseState`] on the
                //     bottom of the stack will not be in an accepting
                //     state,
                //       and so we can stop checking early.
                // In most cases,
                //   if we haven't hit EOF early,
                //   the stack should be either empty or consist of only the
                //     root state.
                //
                // After having considered the stack,
                //   we can then consider the active `ParseState`.
                stack.iter().all(|st| st.is_inner_accepting(stack))
                    && self.is_inner_accepting(stack)
            }
        }

        impl SuperState for $super {
            fn is_inner_accepting(
                &self,
                ctx: &<Self as ParseState>::Context
            ) -> bool {
                match self {
                    $(
                        Self::$nt(st) => st.is_accepting(ctx),
                    )*
                }
            }

            #[allow(dead_code)] // TODO: Remove when using for tpl apply
            fn can_preempt_node(&self) -> bool {
                match self {
                    $(
                        Self::$nt(st) => st.can_preempt_node(),
                    )*
                }
            }

            /// Force current state into a non-preemptable expecting state.
            #[allow(dead_code)]  // Only used with sum NTs
            fn expect_non_preemptable(self) -> Self {
                match self {
                    $(
                        Self::$nt(_) => $nt::non_preemptable().into(),
                    )*
                }
            }
        }
    }};

    (@!ntfirst_init $super:ident, $ntfirst:ident $($nt:ident)*) => {
        $ntfirst::non_preemptable().into()
    }
}

/// Superstate.
///
/// A superstate is responsible for aggregating all nonterminals and serving
///   as a trampoline to delegate parsing operations.
///
/// Conceptually,
///   a superstate acts as a runtime for the state machine defined by NT
///   interdependencies.
/// It represents the reification of such a state machine and all of its
///   transitions.
pub trait SuperState:
    ClosedParseState<
        Token = XirfToken<RefinedText>,
        Context = SuperStateContext<Self>,
    > + Default
{
    /// Whether the inner (active child) [`ParseState`] is in an accepting
    ///   state.
    ///
    /// [`ParseState`]: crate::parse::ParseState
    fn is_inner_accepting(&self, ctx: &<Self as ParseState>::Context) -> bool;

    /// Whether the inner parser is in a state that can tolerate superstate
    ///   node preemption.
    ///
    /// Node preemption allows us (the superstate) to ask for
    ///   permission from the inner parser to parse some token ourselves,
    ///     by asking whether the parser is in a state that would cause
    ///     semantic issues if we were to do so.
    ///
    /// For example,
    ///   if we were to preempt text nodes while an inner parser was still
    ///   parsing attributes,
    ///     then we would emit an object associated with that text before
    ///     the inner parser had a chance to conclude that attribute parsing
    ///     has completed and emit the opening object for that node;
    ///       the result would otherwise be an incorrect `Text, Open`
    ///       instead of the correct `Open, Text`,
    ///         which would effectively unparent the text.
    /// Similarly,
    ///   if we were to parse our own tokens while an inner parser was
    ///   performing error recovery in such a way as to ignore all child
    ///   tokens,
    ///     then we would emit an object in an incorrect context.
    #[allow(dead_code)] // TODO: Remove when using for tpl apply
    fn can_preempt_node(&self) -> bool;

    /// Force current state into a non-preemptable expecting state.
    fn expect_non_preemptable(self) -> Self;
}

pub trait NtBase: PartialEq + Debug
where
    <Self::NtSuper as ParseState>::Error: From<Self::ParseError>,
{
    // Superstate of all NTs.
    type NtSuper: From<Self::ParseState> + SuperState;
    /// Parser for this NT.
    type ParseState: ParseState<Super = Self::NtSuper, Error = Self::ParseError>;
    /// Errors emitted by [`Self::ParseState`].
    type ParseError: Diagnostic + Debug + PartialEq;

    /// A default state that can be preempted by [`Self::NtSuper`].
    fn preemptable() -> Self::ParseState;

    /// A default state that cannot be preempted by [`Self::NtSuper`].
    #[allow(dead_code)] // not utilized for every NT
    fn non_preemptable() -> Self::ParseState;

    /// Whether the given QName would be matched by any of the
    ///   NT parsers associated with this type.
    ///
    /// If any such NT is found,
    ///   this returns it.
    /// This widens to the supertype to support returning any number of
    ///   possible NTs.
    /// By returning the matching NT,
    ///   we are able to generalize [`QName`] matching without having to
    ///   generate custom code per parser via [`ele_parse`].
    fn matches(qname: QName) -> Option<Self::NtSuper>;

    /// Number of
    ///   [`NodeMatcher`]s considered by this parser.
    ///
    /// This is always `1` for this parser.
    fn matches_n() -> usize;

    /// Format matcher for display.
    ///
    /// This value may be rendered singularly or as part of a list of
    ///   values joined together by Sum NTs.
    /// This function receives the number of values to be formatted
    ///   as `n` and the current 0-indexed offset within that list
    ///   as `i`.
    /// This allows for zero-copy rendering of composable NTs.
    ///
    /// `i` must be incremented after the operation.
    fn fmt_matches(
        n: usize,
        i: &mut usize,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result;

    /// Whether the parser is in a state that can tolerate superstate
    ///   node preemption.
    ///
    /// For more information,
    ///   see the superstate.
    fn can_preempt_node(&self) -> bool;

    /// Whether the current state represents the last child NT.
    fn is_last_nt(&self) -> bool;
}

/// Nonterminal.
///
/// This trait is used internally by the [`ele_parse!`] parser-generator.
pub trait Nt: Debug {
    /// Attribute parser for this element.
    type AttrState: AttrParseState;
    /// [`NtState::Jmp`] states for child NTs.
    type ChildNt: Debug + PartialEq + Eq;

    /// Matcher describing the node recognized by this parser.
    fn matcher() -> NodeMatcher;
}

/// States for nonterminals (NTs).
#[derive(Debug, PartialEq, Eq)]
pub enum NtState<NT: Nt> {
    /// Expecting opening tag for element.
    Expecting,

    /// Non-preemptable [`Self::Expecting`].
    NonPreemptableExpecting,

    /// Recovery state ignoring all remaining tokens for this
    ///   element.
    RecoverEleIgnore(QName, OpenSpan, Depth),

    // Recovery completed because end tag corresponding to the
    //   invalid element has been found.
    RecoverEleIgnoreClosed(QName, CloseSpan),

    /// Recovery state ignoring all tokens when a `Close` is
    ///   expected.
    ///
    /// This is token-agnostic---it
    ///   may be a child element,
    ///     but it may be text,
    ///     for example.
    CloseRecoverIgnore((QName, OpenSpan, Depth), Span),

    /// Parsing element attributes.
    Attrs((QName, OpenSpan, Depth), NT::AttrState),

    /// Preparing to pass control (jump) to a child NT's parser.
    Jmp(NT::ChildNt),

    /// Closing tag found and parsing of the element is
    ///   complete.
    Closed(Option<QName>, Span),
}

impl<NT: Nt> Default for NtState<NT> {
    fn default() -> Self {
        Self::Expecting
    }
}

impl<NT: Nt> NtState<NT> {
    pub fn can_preempt_node(&self) -> bool {
        use NtState::*;

        match self {
            // Preemption before the opening tag is safe,
            //   since we haven't started processing yet.
            Expecting => true,

            // The name says it all.
            // Instantiated by the superstate.
            NonPreemptableExpecting => false,

            // Preemption during recovery would cause tokens to be parsed
            //   when they ought to be ignored,
            //     so we must process all tokens during recovery.
            RecoverEleIgnore(..) | CloseRecoverIgnore(..) => false,

            // It is _not_ safe to preempt attribute parsing since attribute
            //   parsers aggregate until a non-attribute token is
            //   encountered;
            //     we must allow attribute parsing to finish its job
            //     _before_ any preempted nodes are emitted since the
            //     attributes came _before_ that node.
            Attrs(..) => false,

            // These states represent jump states where we're about to
            //   transition to the next child parser.
            // It's safe to preempt here,
            //   since we're not in the middle of parsing.
            //
            // Note that this includes `ExpectClose_` because of the macro
            //   preprocessing,
            //     and Rust's exhaustiveness check will ensure that it is
            //     accounted for if that changes.
            // If we're expecting that the next token is a `Close`,
            //     then it must be safe to preempt other nodes that may
            //     appear in this context as children.
            Jmp(..) => true,

            // If we're done,
            //   we want to be able to yield a dead state so that we can
            //   transition away from this parser.
            RecoverEleIgnoreClosed(..) | Closed(..) => false,
        }
    }
}

impl<NT: Nt> Display for NtState<NT> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use crate::xir::fmt::{TtCloseXmlEle, TtOpenXmlEle};
        use NtState::*;

        match self {
            Expecting | NonPreemptableExpecting => write!(
                f,
                "expecting opening tag {}",
                TtOpenXmlEle::wrap(NT::matcher()),
            ),
            RecoverEleIgnore(name, _, _) | RecoverEleIgnoreClosed(name, _) => {
                write!(
                    f,
                    "attempting to recover by ignoring element \
                    with unexpected name {given} \
                    (expected {expected})",
                    given = TtQuote::wrap(name),
                    expected = TtQuote::wrap(NT::matcher()),
                )
            }
            CloseRecoverIgnore((qname, _, depth), _) => write!(
                f,
                "attempting to recover by ignoring input \
                    until the expected end tag {expected} \
                    at depth {depth}",
                expected = TtCloseXmlEle::wrap(qname),
            ),

            Attrs(_, sa) => Display::fmt(sa, f),
            Closed(Some(qname), _) => {
                write!(f, "done parsing element {}", TtQuote::wrap(qname),)
            }
            // Should only happen on an unexpected `Close`.
            Closed(None, _) => write!(
                f,
                "skipped parsing element {}",
                TtQuote::wrap(NT::matcher()),
            ),
            // TODO: A better description.
            Jmp(_) => {
                write!(
                    f,
                    "preparing to transition to \
                        parser for next child element(s)"
                )
            }
        }
    }
}

/// Sum nonterminal.
///
/// This trait is used internally by the [`ele_parse!`] parser-generator.
pub trait SumNt: NtBase {
    fn fmt_matches_top(f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

/// States for sum nonterminals.
///
/// Sum NTs act like a sum type,
///   transitioning to the appropriate inner NT based on the next token of
///   input.
/// Sum NTs have order-based precedence when faced with ambiguity,
///   like a PEG.
///
/// This is expected to be wrapped by a newtype for each Sum NT,
///   and does not implement [`ParseState`] itself.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum SumNtState<NT: SumNt> {
    /// Expecting an opening tag for an element.
    #[default]
    Expecting,

    /// Non-preemptable [`Self::Expecting`].
    NonPreemptableExpecting,

    /// Recovery state ignoring all remaining tokens for this
    ///   element.
    RecoverEleIgnore(QName, OpenSpan, Depth, PhantomData<NT>),
}

impl<NT: SumNt> SumNtState<NT> {
    /// Whether the parser is in a state that can tolerate
    ///   superstate node preemption.
    pub fn can_preempt_node(&self) -> bool {
        use SumNtState::*;

        match self {
            // Preemption before the opening tag is safe,
            //   since we haven't started processing yet.
            Expecting => true,

            // The name says it all.
            // Instantiated by the superstate.
            NonPreemptableExpecting => false,

            // Preemption during recovery would cause tokens to
            //   be parsed when they ought to be ignored,
            //     so we must process all tokens during recovery.
            RecoverEleIgnore(..) => false,
        }
    }
}

impl<NT: SumNt> Display for SumNtState<NT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SumNtState::*;

        match self {
            Expecting | NonPreemptableExpecting => {
                write!(f, "expecting ")?;
                NT::fmt_matches_top(f)
            }

            RecoverEleIgnore(name, _, _, _) => {
                write!(
                    f,
                    "attempting to recover by ignoring element \
                    with unexpected name {given} \
                    (expected",
                    given = TtQuote::wrap(name),
                )?;

                NT::fmt_matches_top(f)?;
                f.write_str(")")
            }
        }
    }
}

impl<NT: SumNt> ParseState for SumNtState<NT>
where
    Self: Into<NT::NtSuper>,
    <NT as NtBase>::ParseError: From<SumNtError<NT>>,
{
    type Token = XirfToken<RefinedText>;
    type Object = <Self::Super as ParseState>::Object;
    type Error = <NT as NtBase>::ParseError;
    type Context = SuperStateContext<NT::NtSuper>;
    type Super = <NT as NtBase>::NtSuper;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use SumNtState::*;

        match (self, tok) {
            (
                NonPreemptableExpecting,
                tok @ XirfToken::Open(qname, span, depth),
            ) => {
                NT::matches(qname)
                    .map(|nt| {
                        stack.transfer_with_ret(
                            Transition(Expecting),
                            // Propagate non-preemption status,
                            //   otherwise we'll provide a lookback of
                            //   the original token and end up recursing
                            //   until we hit the `stack` limit.
                            Transition(nt.expect_non_preemptable())
                                .incomplete()
                                .with_lookahead(tok),
                        )
                    })
                    .unwrap_or_else(|| {
                        // Since we're non-preemptable,
                        //   we're expected to be able to process this token
                        //   or fail trying.
                        Transition(RecoverEleIgnore(
                            qname,
                            span,
                            depth,
                            Default::default(),
                        ))
                        .err(
                            // Use name span rather than full `OpenSpan`
                            //   since it's specifically the name that was
                            //   unexpected,
                            //     not the fact that it's an element.
                            SumNtError::UnexpectedEle(
                                qname,
                                span.name_span(),
                                Default::default(),
                            ),
                        )
                    })
            }

            (Expecting, tok @ XirfToken::Open(qname, ..)) => {
                NT::matches(qname)
                    .map(|nt| {
                        stack.transfer_with_ret(
                            Transition(Expecting),
                            // note: this clone is just because the
                            //   borrow checker can't prove a single use
                            //   between this closure and below; it should
                            //   optimize away
                            Transition(nt)
                                .incomplete()
                                .with_lookahead(tok.clone()),
                        )
                    })
                    .unwrap_or_else(
                        // An unexpected token ends repetition
                        //   and should not result in an error.
                        || Transition(Expecting).dead(tok),
                    )
            }

            // An unexpected token when repeating ends repetition
            //   and should not result in an error.
            (Expecting | NonPreemptableExpecting, tok) => {
                Transition(Expecting).dead(tok)
            }

            // XIRF ensures that the closing tag matches the opening,
            //   so we need only check depth.
            (
                RecoverEleIgnore(_, _, depth_open, _),
                XirfToken::Close(_, _, depth_close),
            ) if depth_open == depth_close => {
                Transition(Expecting).incomplete()
            }

            (st @ RecoverEleIgnore(..), _) => Transition(st).incomplete(),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, SumNtState::Expecting)
    }
}

#[cfg(test)]
mod test;
