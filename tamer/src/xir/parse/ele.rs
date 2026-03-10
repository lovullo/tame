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

#[cfg(doc)]
use crate::{ele_parse, parse::Parser};

mod nt;
mod superst;

// TODO: We can encapsulate further once more is extracted from the
//   `ele_parse` macro.
pub use nt::{
    ChildNt, ChildNtMeta, NodeMatcher, Nt, NtBase, NtError, NtExpectKind,
    NtParseResult, NtState, SumNt, SumNtError, SumNtState,
};
pub use superst::{SuperState, SuperStateContext};

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
            $( $qname:ident {
              $($matches:tt)*
            } )?

            // sum NT: (N₀ | N₁ | ... | Nₙ)
            $( ($($sum:tt)*) )?
          ;
        )*
    ) => {
        ele_parse!(@!mod $vis $super; $(mod $mod)? {
            use super::*;

            use $crate::{
                diagnose::{Diagnostic, AnnotatedSpan},
                parse::*,
                xir::{
                    *,
                    flat::{
                        RefinedText, XirfToken
                    },
                    parse::*,
                },
            };

            mod meta {
                use super::*;

                pub type Super = $super;
                pub type AttrValueError = $avety;
                pub type Object = $objty;
            }

            // Dispatch to a parser for either the
            // "normal" or the sum NT grammar.
            $(
                ele_parse!(@!define_nt
                  $(#[$nt_attr])*
                  $nt
                  $(( $($sum)*            ))?
                  $({ $qname $($matches)* })?
                );
            )*

            ele_parse!(@!super_sum $(#[$super_attr])* $super
                $([super] { $($super_body)* })?
                $($nt),*
            );
        });
    };

    // `mod $ident;` form was provided by the user.
    (@!mod $vis:vis $super:ident; mod $mod:ident { $($body:tt)* }) => {
        mod $mod {
            $($body)*
        }

        $vis use $mod::$super;
    };

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
            $(Close($closepat) => $closemap,)?

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
                    ([<$nt ChildNt_>]::$ntref, $ntref, false),
                    ([<$nt ChildNt_>]::$ntref, $ntref) ->
                )* (NtState::<$nt>::ExpectCloseOrLast, (), true),
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

    (@!ele_dfn_body
        $(#[$nt_attr:meta])* $nt:ident $qname:ident

        ($openpat:pat) => $openmap:expr,
        $(Close($closepat:pat) => $closemap:expr,)?

        // Attribute delegation special form.
        $(($attr_stream_binding:pat) => $attr_stream_map:expr,)?

        // Nonterminal references.
        <> {
            $(
                $ntref:ident,
            )*
        }

        -> {
            @ -> ($ntfirst:path, $ntfirst_st:ty, $_ntfirst_last:expr),
            $(
                ($ntprev:path, $ntprev_st:ty) -> ($ntnext:path, $ntnext_st:ty, $ntnext_last:expr),
            )*
        }
    ) => { paste::paste! {
        // Clippy complains when all child NTs have a common prefix
        #[allow(clippy::enum_variant_names)]
        #[derive(Debug, PartialEq, Eq)]
        pub enum [<$nt ChildNt_>] {
            $(
                $ntref(ChildNtMeta),
            )*
        }

        impl From<[<$nt ChildNt_>]> for meta::Super {
            fn from(child_nt: [<$nt ChildNt_>]) -> Self {
                NtState::<$nt>::from(child_nt).into()
            }
        }

        impl From<[<$nt ChildNt_>]> for NtState<$nt> {
            fn from(child_nt: [<$nt ChildNt_>]) -> Self {
                NtState::Jmp(child_nt)
            }
        }

        impl ChildNt for [<$nt ChildNt_>] {
            type Nt = $nt;

            fn jmp_next_child_nt(self) -> <Self::Nt as NtBase>::NtSuper {
                match self {
                    $(
                        $ntprev(meta) => $ntnext(meta).into(),
                    )*
                }
            }

            fn as_nt_preemptable(&self) -> <Self::Nt as NtBase>::NtSuper {
                match *self {
                    $(
                        Self::$ntref(..) => <$ntref>::preemptable().into(),
                    )*
                }
            }

            fn first_nt_or_close(meta: ChildNtMeta) -> NtState<Self::Nt> {
                $ntfirst(meta).into()
            }

            fn last_nt(meta: ChildNtMeta) -> Option<Self> {
                let _ = meta; // not used if there are no child NTs

                None::<Self>
                $(
                    .or($ntnext_last.then_some($ntprev(meta)))
                )*
            }
        }

        $(#[$nt_attr])*
        ///
        #[doc=concat!("Parser for element [`", stringify!($qname), "`].")]
        #[derive(Debug, PartialEq, Eq)]
        pub struct $nt(NtState<$nt>);

        impl From<NtState<$nt>> for $nt {
            fn from(st: NtState<$nt>) -> Self {
                $nt(st)
            }
        }

        impl NtBase for $nt {
            type NtSuper = meta::Super;
            type ParseState = NtState<$nt>;
            type ParseError = NtError<$nt>;

            fn preemptable() -> Self::ParseState {
                NtState::Expecting(NtExpectKind::Preemptable)
            }

            #[allow(dead_code)] // not utilized for every NT
            fn non_preemptable() -> Self::ParseState {
                NtState::Expecting(NtExpectKind::NonPreemptable)
            }

            #[inline]
            fn matches(qname: QName) -> Option<Self::NtSuper> {
                <Self as Nt>::matcher()
                    .matches(qname)
                    .then_some(Self::preemptable().into())
            }

            #[allow(dead_code)] // used by Sum NTs
            fn matches_n() -> usize {
                1
            }

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

            fn can_preempt_node(&self) -> bool {
                match self {
                    Self(st) => st.can_preempt_node(),
                }
            }
        }

        impl Nt for $nt {
            type AttrState = [<$nt AttrState_>];
            type ChildNt = [<$nt ChildNt_>];

            fn matcher() -> NodeMatcher {
                NodeMatcher::from($qname)
            }

            fn try_open_from(
                qname: QName,
                span: OpenSpan,
            ) -> NtParseResult<Self> {
                let $openpat = (qname, span);
                TryFrom::try_from($openmap).map_err(Into::into)
            }

            fn try_attr_stream_from(
                qname: QName,
                value: SymbolId,
                attrspan: AttrSpan,
            ) -> Option<NtParseResult<Self>> {
                let _ = (qname, value, &attrspan);

                None
                $(
                    // Attr special form
                    .or_else(|| {
                        let $attr_stream_binding = (qname, value, attrspan);
                        Some(
                            $attr_stream_map.try_into()
                                .map_err(Into::into)
                        )
                    })
                )?
            }

            fn try_close_from(
                qname: QName,
                span: CloseSpan,
            ) -> Option<NtParseResult<Self>> {
                let _ = (qname, span); // only used if Close form provided

                None
                $(
                    .or_else(|| {
                        let $closepat = (qname, span);
                        // TODO: We ought to support errors here
                        Some(Ok($closemap))
                    })
                )?
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

    (@!ele_dfn_sum
        $(#[$nt_attr:meta])* $nt:ident [$($ntref:ident)*]
    ) => {
        $(#[$nt_attr])*
        ///
        #[doc=concat!(
            "Parser expecting one of ",
            $("[`", stringify!($ntref), "`], ",)*
            "."
        )]
        #[derive(Debug, PartialEq, Eq)]
        pub struct $nt(SumNtState<$nt>);

        impl NtBase for $nt {
            type NtSuper = meta::Super;
            type ParseState = SumNtState<$nt>;
            type ParseError = SumNtError<$nt>;

            fn preemptable() -> Self::ParseState {
               SumNtState::Expecting(NtExpectKind::Preemptable)
            }

            fn non_preemptable() -> Self::ParseState {
                SumNtState::Expecting(NtExpectKind::NonPreemptable)
            }

            fn matches(qname: QName) -> Option<Self::NtSuper> {
                None::<Self::NtSuper> $( .or_else(|| $ntref::matches(qname)) )*
            }

            fn matches_n() -> usize {
                0 $(+ $ntref::matches_n())*
            }

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

            fn can_preempt_node(&self) -> bool {
                match self {
                    Self(st) => st.can_preempt_node(),
                }
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
            type AttrValueError = meta::AttrValueError;

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

#[cfg(test)]
mod test;
