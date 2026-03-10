// XIR element parser generator node nonterminal
//
//  Copyright (C) 2014-2026 Ryan Specialty, LLC.
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

use super::{NtBase, NtExpectKind, NtParseResult};

use crate::{
    fmt::TtQuote,
    parse::{EmptyContext, ParseState, StitchableParseState, prelude::*},
    span::Span,
    sym::SymbolId,
    xir::{
        CloseSpan, EleSpan, OpenSpan, Prefix, QName,
        attr::{Attr, AttrSpan},
        flat::{Depth, RefinedText, XirfToken},
        parse::{
            AttrParseError, AttrParseState, SuperState, SuperStateContext,
            parse_attrs,
        },
    },
};

use std::{
    convert::Infallible,
    fmt::{Debug, Display, Formatter},
    marker::PhantomData,
};

/// Nonterminal.
pub trait Nt: NtBase {
    /// Attribute parser for this element.
    ///
    /// The expected [`ParseState`] here is very rigid to simplify trait
    ///   bounds.
    type AttrState: AttrParseState
        + StitchableParseState<<Self as NtBase>::ParseState>
        + ParseState<Token = XirfToken<RefinedText>, Context = EmptyContext>;

    /// [`NtState::Jmp`] states for child NTs.
    type ChildNt: ChildNt<Nt = Self> + Debug + PartialEq + Eq;

    /// Matcher describing the node recognized by this parser.
    fn matcher() -> NodeMatcher;

    /// Attempt to produce an object from the NT's `Open` mapping.
    fn try_open_from(qname: QName, span: OpenSpan) -> NtParseResult<Self>;

    /// Attempt to produce an object from the NT's `Attr` mapping,
    ///   if any.
    fn try_attr_stream_from(
        qname: QName,
        value: SymbolId,
        attrspan: AttrSpan,
    ) -> Option<NtParseResult<Self>>;

    /// Attempt to produce an object from the NT's `Close` mapping,
    ///   if any.
    fn try_close_from(
        qname: QName,
        span: CloseSpan,
    ) -> Option<NtParseResult<Self>>;
}

/// States for nonterminals (NTs).
#[derive(Debug, PartialEq, Eq)]
pub enum NtState<NT: Nt> {
    /// Expecting opening tag for element.
    Expecting(NtExpectKind),

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

    /// Expecting a closing tag for this element,
    ///   or another child element matching the last [`Nt::ChildNt`].
    ExpectCloseOrLast((QName, OpenSpan, Depth)),

    /// Closing tag found and parsing of the element is
    ///   complete.
    Closed(Option<QName>, Span),
}

impl<NT: Nt> Default for NtState<NT> {
    fn default() -> Self {
        Self::Expecting(NtExpectKind::default())
    }
}

impl<NT: Nt> NtState<NT> {
    pub fn can_preempt_node(&self) -> bool {
        use NtState::*;

        match self {
            // Preemption before the opening tag is safe,
            //   since we haven't started processing yet.
            Expecting(preempt) => preempt.can_preempt_node(),

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
            Jmp(..) => true,

            // Same situation as Jmp,
            //  since we are expecting either another child NT or a close.
            // If we're expecting that the next token is a `Close`,
            //   then it must be safe to preempt other nodes that may
            //   appear in this context as children.
            ExpectCloseOrLast(..) => true,

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
            Expecting(preempt) => write!(
                f,
                "expecting {preempt} opening tag {}",
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
            ExpectCloseOrLast(_) => {
                write!(
                    f,
                    "expecting either close of element {} \
                        or an opening of the last child NT",
                    TtQuote::wrap(NT::matcher())
                )
            }
            // TODO: A better description.
            Jmp(_) => {
                write!(
                    f,
                    "preparing to transition to \
                        parser for next child element(s)"
                )
            }
            Closed(Some(qname), _) => {
                write!(f, "done parsing element {}", TtQuote::wrap(qname),)
            }
            // Should only happen on an unexpected `Close`.
            Closed(None, _) => write!(
                f,
                "skipped parsing element {}",
                TtQuote::wrap(NT::matcher()),
            ),
        }
    }
}

impl<NT: Nt> ParseState for NtState<NT>
where
    Self: Into<NT::NtSuper>,
    NT: NtBase<ParseError = NtError<NT>>, // TODO: could use an abstraction
{
    type Token = XirfToken<RefinedText>;
    type Object = <Self::Super as ParseState>::Object;
    type Error = <NT as NtBase>::ParseError;
    type Context = SuperStateContext<NT::NtSuper>;
    type Super = <NT as NtBase>::NtSuper;

    fn parse_token(
        self,
        tok: Self::Token,
        #[allow(unused_variables)] // used only if child NTs
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NtExpectKind::*;
        use NtState::{
            Attrs, CloseRecoverIgnore, Closed, ExpectCloseOrLast, Expecting,
            Jmp, RecoverEleIgnore, RecoverEleIgnoreClosed,
        };

        match (self, tok) {
            (
                st @ (Expecting(..) | Closed(..)),
                tok @ XirfToken::Open(qname, span, depth),
            ) => {
                if NT::matches(qname).is_some() {
                    NT::try_open_from(qname, span)
                        .map(ParseStatus::Object)
                        .transition(Attrs(
                            (qname, span, depth),
                            parse_attrs(qname, span),
                        ))
                } else if st.can_preempt_node() || matches!(st, Closed(..)) {
                    // Maybe someone else can handle this for us
                    Transition(Expecting(Preemptable)).dead(tok)
                } else {
                    // We must do something with this token,
                    //   so the only option is to enter recovery.
                    Transition(RecoverEleIgnore(qname, span, depth)).err(
                        Self::Error::UnexpectedEle(qname, span.name_span()),
                    )
                }
            }

            (
                RecoverEleIgnore(qname, _, depth_open),
                XirfToken::Close(_, span, depth_close),
            ) if depth_open == depth_close => {
                Transition(RecoverEleIgnoreClosed(qname, span)).incomplete()
            }

            (Attrs(meta, sa), tok) => {
                // When the Attr special form is used,
                //   we entirely delegate attribute parsing without
                //   performing our own explicit mapping.
                if let XirfToken::Attr(Attr(qname, value, attrspan)) =
                    tok.clone()
                    && let Some(attr_stream) =
                        NT::try_attr_stream_from(qname, value, attrspan)
                {
                    attr_stream
                        .map(ParseStatus::Object)
                        .transition(Attrs(meta, sa))
                } else {
                    // TODO: We want to detect pattern conflicts so that
                    //   they can be exposed in the macro DSL.
                    sa.delegate::<Self, _>(
                        tok,
                        EmptyContext,
                        |sa| Transition(Attrs(meta, sa)),
                        || {
                            Transition(<NT as Nt>::ChildNt::first_nt_or_close(
                                meta,
                            ))
                        },
                    )
                }
            }

            // We're transitioning from `(ntprev) -> (ntnext)`.
            //
            // If we have a token that matches `ntprev`,
            //   we can transition _back_ to ntprev rather
            //   than transitioning forward.
            // We can _only_ do this when we know we are
            //   transitioning away from this state,
            //     otherwise we could return to a previous state,
            //     which violates the semantics of the implied
            //     DFA.
            //
            // We therefore have:  (ntprev)--->(ntnext)
            //                        ^     \
            //                         `-----'
            (Jmp(ntprev), tok) => {
                let ntprev_st = ntprev.as_nt_preemptable();
                let jmp_ntnext = ntprev.jmp_next_child_nt();

                stack.transfer_with_ret(
                    Transition(jmp_ntnext),
                    Transition(ntprev_st).incomplete().with_lookahead(tok),
                )
            }

            // This is similar to the NT transitions above:
            //   we can either close,
            //     or we can open more elements belonging to the
            //     final child NT.
            // We choose to transition back to the final NT
            //   _no matter what the element_,
            //     to force error recovery and diagnostics
            //     in that context,
            //       which will tell the user what elements were
            //       expected in the last NT rather than just
            //       telling them a closing tag was expected.
            (
                st @ ExpectCloseOrLast(meta @ (mqname, mspan, _)),
                tok @ XirfToken::Open(..),
            ) => {
                if let Some(child_nt) = <NT as Nt>::ChildNt::last_nt(meta) {
                    stack.transfer_with_ret(
                        Transition(st),
                        // If this NT cannot handle this element,
                        //   it should error and enter recovery to
                        //   ignore it.
                        Transition(child_nt.as_nt_non_preemptable())
                            .incomplete()
                            .with_lookahead(tok),
                    )
                } else {
                    // If there are no child NTs,
                    //   then all we can do is expect a close.
                    Transition(CloseRecoverIgnore(meta, tok.span()))
                        .err(Self::Error::CloseExpected(mqname, mspan, tok))
                }
            }

            // XIRF ensures proper nesting,
            //   so we do not need to check the element name.
            (
                ExpectCloseOrLast((qname, _, depth))
                | CloseRecoverIgnore((qname, _, depth), _),
                XirfToken::Close(_, span, tok_depth),
            ) if tok_depth == depth => {
                if let Some(close) = NT::try_close_from(qname, span) {
                    close.map(ParseStatus::Object)
                } else {
                    Ok(ParseStatus::Incomplete)
                }
                .transition(Closed(Some(qname), span.tag_span()))
            }

            (ExpectCloseOrLast(meta @ (qname, otspan, _)), unexpected_tok) => {
                Transition(CloseRecoverIgnore(meta, unexpected_tok.span())).err(
                    Self::Error::CloseExpected(qname, otspan, unexpected_tok),
                )
            }

            // We're still in recovery,
            //   so this token gets thrown out.
            (st @ (RecoverEleIgnore(..) | CloseRecoverIgnore(..)), _) => {
                Transition(st).incomplete()
            }

            // Note that this does not necessarily represent an
            //   accepting state
            //     (see `is_accepting`).
            (
                st @ (Expecting(..) | Closed(..) | RecoverEleIgnoreClosed(..)),
                tok,
            ) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        use NtState::*;
        matches!(*self, Closed(..) | RecoverEleIgnoreClosed(..))
    }
}

/// Metadata used to track active element for reporting and debugging.
pub type ChildNtMeta = (QName, OpenSpan, Depth);

/// Set of possible child NTs for some parent [`Nt`].
pub trait ChildNt: Sized {
    type Nt: Nt;

    /// Request a jump to the parser of the next child.
    fn jmp_next_child_nt(self) -> <Self::Nt as NtBase>::NtSuper;

    /// Parser of the current child,
    ///   able to be preempted and may ignore input.
    fn as_nt_preemptable(&self) -> <Self::Nt as NtBase>::NtSuper;

    /// Parser of current child,
    ///   not able to be preempted and must handle next token of input.
    fn as_nt_non_preemptable(&self) -> <Self::Nt as NtBase>::NtSuper {
        self.as_nt_preemptable().expect_non_preemptable()
    }

    /// Parser of the first child NT in the sequence.
    /// Otherwise,
    ///   [`NtState::ExpectCloseOrLast`].
    fn first_nt_or_close(meta: ChildNtMeta) -> NtState<Self::Nt>;

    /// The final child NT in the parent's sequence,
    ///   if any.
    fn last_nt(meta: ChildNtMeta) -> Option<Self>;
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

type NtAttrValueError<NT> =
    <<NT as NtBase>::NtSuper as SuperState>::AttrValueError;

#[derive(Debug, PartialEq)]
pub enum NtError<NT: Nt> {
    /// An element was expected,
    ///   but the name of the element was unexpected.
    UnexpectedEle(QName, Span),

    /// Unexpected input while expecting an end tag for this
    ///   element.
    ///
    /// The span corresponds to the opening tag.
    CloseExpected(QName, OpenSpan, XirfToken<RefinedText>),

    Attrs(AttrParseError<NtAttrValueError<NT>>, PhantomData<NT>),
}

impl<NT: Nt, EV: Diagnostic> From<AttrParseError<EV>> for NtError<NT>
where
    <NT as NtBase>::NtSuper: SuperState<AttrValueError = EV>,
{
    fn from(e: AttrParseError<EV>) -> Self {
        Self::Attrs(e, PhantomData)
    }
}

impl<NT: Nt> Error for NtError<NT> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        // TODO
        None
    }
}

impl<NT: Nt> From<Infallible> for NtError<NT> {
    fn from(_value: Infallible) -> Self {
        unreachable!("From<Infallible>")
    }
}

impl<NT: Nt> Display for NtError<NT> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use crate::xir::fmt::{TtCloseXmlEle, TtOpenXmlEle};

        match self {
            Self::UnexpectedEle(name, _) => write!(
                f,
                "unexpected {unexpected} (expecting {expected})",
                unexpected = TtOpenXmlEle::wrap(name),
                expected = TtOpenXmlEle::wrap(NT::matcher()),
            ),

            Self::CloseExpected(qname, _, tok) => write!(
                f,
                "expected {}, but found {}",
                TtCloseXmlEle::wrap(qname),
                TtQuote::wrap(tok)
            ),

            Self::Attrs(e, _) => Display::fmt(e, f),
        }
    }
}

impl<NT: Nt> Diagnostic for NtError<NT> {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan<'_>> {
        use crate::{parse::Token, xir::fmt::TtCloseXmlEle};

        match self {
            Self::UnexpectedEle(_, ospan) => ospan
                .error(format!(
                    "expected {ele_name} here",
                    ele_name = TtQuote::wrap(NT::matcher())
                ))
                .into(),

            Self::CloseExpected(qname, ospan, tok) => vec![
                ospan.span().note("element starts here"),
                tok.span()
                    .error(format!("expected {}", TtCloseXmlEle::wrap(qname),)),
            ],

            Self::Attrs(e, _) => e.describe(),
        }
    }
}
