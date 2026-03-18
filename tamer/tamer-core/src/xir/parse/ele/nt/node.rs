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

use super::{Nt, NtExpectKind, NtMeta, NtParseResult, PreemptionStatus};

use crate::{
    fmt::TtQuote,
    parse::{EmptyContext, ParseState, StitchableParseState, prelude::*},
    span::Span,
    sym::SymbolId,
    xir::{
        CloseSpan, EleSpan, OpenSpan, Prefix, QName,
        attr::{Attr, AttrSpan},
        flat::{RefinedText, XirfToken},
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

/// Nonterminal representing an XML node.
///
/// This matches against a single [`QName`] one or more times.
pub trait NodeNt: Nt {
    /// Attribute parser for this element.
    ///
    /// The expected [`ParseState`] here is very rigid to simplify trait
    ///   bounds.
    type AttrState: AttrParseState
        + StitchableParseState<<Self as Nt>::ParseState>
        + ParseState<Token = XirfToken<RefinedText>, Context = EmptyContext>;

    /// [`NodeNtState::Jmp`] states for child NTs.
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
pub enum NodeNtState<NT: NodeNt> {
    /// Expecting opening tag for element.
    Expecting(NtExpectKind),

    /// Recovery state ignoring all remaining tokens for this
    ///   element.
    RecoverEleIgnore(NtMeta),

    // Recovery completed because end tag corresponding to the
    //   invalid element has been found.
    RecoverEleIgnoreClosed(NtMeta, CloseSpan),

    /// Recovery state ignoring all tokens when a `Close` is
    ///   expected.
    ///
    /// This is token-agnostic---it
    ///   may be a child element,
    ///     but it may be text,
    ///     for example.
    CloseRecoverIgnore(NtMeta, Span),

    /// Parsing element attributes.
    Attrs(NtMeta, NT::AttrState),

    /// Preparing to pass control (jump) to a child NT's parser.
    Jmp(NT::ChildNt),

    /// Expecting a closing tag for this element,
    ///   or another child element matching the last [`NodeNt::ChildNt`].
    ExpectCloseOrLast(NtMeta),

    /// Closing tag found and parsing of the element is
    ///   complete;
    ///     the parser may resume by parsing another element.
    ClosedResumeable(Option<QName>, Span),

    /// Closing tag found and parsing of the element is
    ///   complete;
    ///     the parser must not parse another element.
    ///
    /// This indicates that the parser was used in an ephemeral context for
    ///   parsing of a single element and must yield to the prior state.
    ClosedEphemeral(Option<QName>, Span),
}

impl<NT: NodeNt> Default for NodeNtState<NT> {
    fn default() -> Self {
        Self::Expecting(NtExpectKind::default())
    }
}

impl<NT: NodeNt> NodeNtState<NT> {
    // TODO: This is also used by NodeNt to mean "I can indicate a dead
    //         state"; need another abstraction for that.
    /// Whether preemption is permitted in the current context.
    ///
    /// If preemption is _not_ permitted,
    ///   then this NT will be provided with an opening element as if it
    ///   were a child node.
    pub fn can_preempt_node(&self) -> bool {
        use NodeNtState::*;

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

            // These closed states await new elements
            //   and so are preemptable.
            RecoverEleIgnoreClosed(..) | ClosedResumeable(..) => true,

            // Upon completion of an ephemeral parse,
            //   we want to force ourselves to handle the next token so that
            //   we can produce a dead state to trigger a stack return.
            //
            // To understand this,
            //   assume that we have just completed a preemption parse,
            //   and the superstate just encountered another preemption
            //   token.
            // The superstate consults this function to see if preemption is
            //   permitted within this context.
            // If we were to return `true`,
            //   then every sibling preemption would continue to grow the
            //   stack;
            //     if there are enough siblings,
            //       we would eventually hit the parsing stack limit and
            //       fail.
            ClosedEphemeral(..) => false,
        }
    }

    /// Whether the parse of the active element should be considered to have
    ///   caused preemption of another parse.
    ///
    /// Children are never considered preemptions even when their immediate
    ///   parent is responsible for preemption;
    ///     consequently,
    ///       a [`Self::Jmp`] state is not a preemption as it represents an
    ///       eminent transition to a child.
    fn caused_preemption(&self) -> PreemptionStatus {
        use NodeNtState::*;

        match self {
            Expecting(kind) => (*kind).into(),

            RecoverEleIgnore(meta)
            | RecoverEleIgnoreClosed(meta, _)
            | CloseRecoverIgnore(meta, _)
            | Attrs(meta, _)
            | ExpectCloseOrLast(meta) => meta.caused_preemption,

            // We're about to transition to a child NT,
            //   and children are never considered to be the source of
            //   preemptions.
            Jmp(..) => PreemptionStatus::NoPreemption,

            ClosedResumeable(..) => PreemptionStatus::NoPreemption,
            ClosedEphemeral(..) => PreemptionStatus::PreemptedParsing,
        }
    }
}

impl<NT: NodeNt> Display for NodeNtState<NT> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use crate::xir::fmt::{TtCloseXmlEle, TtOpenXmlEle};
        use NodeNtState::*;

        match self {
            Expecting(preempt) => write!(
                f,
                "expecting opening tag {}, which is {preempt}",
                TtOpenXmlEle::wrap(NT::matcher()),
            ),
            RecoverEleIgnore(meta) | RecoverEleIgnoreClosed(meta, _) => {
                write!(
                    f,
                    "attempting to recover by ignoring element \
                        with unexpected name {given} \
                        (expected {expected})",
                    given = TtQuote::wrap(meta.qname),
                    expected = TtQuote::wrap(NT::matcher()),
                )
            }
            CloseRecoverIgnore(NtMeta { qname, depth, .. }, _) => write!(
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
            ClosedResumeable(Some(qname), _) => {
                write!(
                    f,
                    "done parsing element {} (resumeable)",
                    TtQuote::wrap(qname),
                )
            }
            // Should only happen on an unexpected `Close`.
            ClosedResumeable(None, _) | ClosedEphemeral(None, _) => write!(
                f,
                "skipped parsing element {}",
                TtQuote::wrap(NT::matcher()),
            ),
            ClosedEphemeral(Some(qname), _) => {
                write!(
                    f,
                    "done parsing single element {} (ephemeral)",
                    TtQuote::wrap(qname),
                )
            }
        }
    }
}

impl<NT: NodeNt> ParseState for NodeNtState<NT>
where
    Self: Into<NT::NtSuper>,
    NT: Nt<ParseError = NtError<NT>>, // TODO: could use an abstraction
{
    type Token = XirfToken<RefinedText>;
    type Object = <Self::Super as ParseState>::Object;
    type Error = <NT as Nt>::ParseError;
    type Context = SuperStateContext<NT::NtSuper>;
    type Super = <NT as Nt>::NtSuper;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use NodeNtState::*;
        use NtExpectKind::*;

        match (self, tok) {
            (
                st @ (Expecting(..) | ClosedResumeable(..)),
                tok @ XirfToken::Open(qname, ospan, depth),
            ) => {
                if NT::matches(qname).is_some() {
                    NT::try_open_from(qname, ospan)
                        .map(ParseStatus::Object)
                        .transition(Attrs(
                            NtMeta {
                                qname,
                                ospan,
                                depth,
                                caused_preemption: st.caused_preemption(),
                            },
                            parse_attrs(qname, ospan),
                        ))
                } else if st.can_preempt_node() {
                    // TODO: Using the concept of preemption for this isn't
                    //         appropriate; see TODO on `can_preempt_node`
                    // Maybe someone else can handle this for us
                    Transition(Expecting(Preemptable)).dead(tok)
                } else {
                    // We must do something with this token,
                    //   so the only option is to enter recovery.
                    Transition(RecoverEleIgnore(NtMeta {
                        qname,
                        ospan,
                        depth,
                        caused_preemption: st.caused_preemption(),
                    }))
                    .err(Self::Error::UnexpectedEle(qname, ospan.name_span()))
                }
            }

            (
                RecoverEleIgnore(
                    meta @ NtMeta {
                        depth: depth_open, ..
                    },
                ),
                XirfToken::Close(_, span, depth_close),
            ) if depth_open == depth_close => {
                Transition(RecoverEleIgnoreClosed(meta, span)).incomplete()
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
                            Transition(
                                <NT as NodeNt>::ChildNt::first_nt_or_close(
                                    meta,
                                ),
                            )
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
            (st @ ExpectCloseOrLast(meta), tok @ XirfToken::Open(..)) => {
                if let Some(child_nt) = <NT as NodeNt>::ChildNt::last_nt(meta) {
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
                    Transition(CloseRecoverIgnore(meta, tok.span())).err(
                        Self::Error::CloseExpected(meta.qname, meta.ospan, tok),
                    )
                }
            }

            // XIRF ensures proper nesting,
            //   so we do not need to check the element name.
            (
                ExpectCloseOrLast(meta) | CloseRecoverIgnore(meta, _),
                XirfToken::Close(_, span, tok_depth),
            ) if tok_depth == meta.depth => {
                let NtMeta {
                    qname,
                    caused_preemption: preempted,
                    ..
                } = meta;

                let status =
                    if let Some(close) = NT::try_close_from(qname, span) {
                        close.map(ParseStatus::Object)
                    } else {
                        Ok(ParseStatus::Incomplete)
                    };

                match preempted {
                    PreemptionStatus::PreemptedParsing => status.transition(
                        ClosedEphemeral(Some(qname), span.tag_span()),
                    ),

                    PreemptionStatus::NoPreemption => status.transition(
                        ClosedResumeable(Some(qname), span.tag_span()),
                    ),
                }
            }

            (
                ExpectCloseOrLast(meta @ NtMeta { qname, ospan, .. }),
                unexpected_tok,
            ) => Transition(CloseRecoverIgnore(meta, unexpected_tok.span()))
                .err(Self::Error::CloseExpected(qname, ospan, unexpected_tok)),

            // We're still in recovery,
            //   so this token gets thrown out.
            (st @ (RecoverEleIgnore(..) | CloseRecoverIgnore(..)), _) => {
                Transition(st).incomplete()
            }

            // Ephemeral parsers can never resume;
            //   wait to be discarded.
            (st @ ClosedEphemeral(..), tok) => Transition(st).dead(tok),

            // Note that this does not necessarily represent an
            //   accepting state
            //     (see `is_accepting`).
            (
                st @ (Expecting(..)
                | ClosedResumeable(..)
                | RecoverEleIgnoreClosed(..)),
                tok,
            ) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        use NodeNtState::*;
        matches!(
            *self,
            ClosedResumeable(..)
                | ClosedEphemeral(..)
                | RecoverEleIgnoreClosed(..)
        )
    }
}

/// Set of possible child NTs for some parent [`NodeNt`].
pub trait ChildNt: Sized {
    type Nt: NodeNt;

    /// Request a jump to the parser of the next child.
    fn jmp_next_child_nt(self) -> <Self::Nt as Nt>::NtSuper;

    /// Parser of the current child,
    ///   able to be preempted and may ignore input.
    fn as_nt_preemptable(&self) -> <Self::Nt as Nt>::NtSuper;

    /// Parser of current child,
    ///   not able to be preempted and must handle next token of input.
    fn as_nt_non_preemptable(&self) -> <Self::Nt as Nt>::NtSuper {
        self.as_nt_preemptable()
            .expect_kind(NtExpectKind::NonPreemptable)
    }

    /// Parser of the first child NT in the sequence.
    /// Otherwise,
    ///   [`NodeNtState::ExpectCloseOrLast`].
    fn first_nt_or_close(meta: NtMeta) -> NodeNtState<Self::Nt>;

    /// The final child NT in the parent's sequence,
    ///   if any.
    fn last_nt(meta: NtMeta) -> Option<Self>;
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

pub type NtAttrValueError<NT: Nt> =
    <<NT as Nt>::NtSuper as SuperState>::AttrValueError;

#[derive(Debug, PartialEq)]
pub enum NtError<NT: NodeNt> {
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

impl<NT: NodeNt, EV: Diagnostic> From<AttrParseError<EV>> for NtError<NT>
where
    <NT as Nt>::NtSuper: SuperState<AttrValueError = EV>,
{
    fn from(e: AttrParseError<EV>) -> Self {
        Self::Attrs(e, PhantomData)
    }
}

impl<NT: NodeNt> Error for NtError<NT> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        // TODO
        None
    }
}

impl<NT: NodeNt> From<Infallible> for NtError<NT> {
    fn from(_value: Infallible) -> Self {
        unreachable!("From<Infallible>")
    }
}

impl<NT: NodeNt> Display for NtError<NT> {
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

impl<NT: NodeNt> Diagnostic for NtError<NT> {
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
