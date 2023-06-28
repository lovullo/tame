// XIR flat (XIRF)
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

//! Lightly-parsed XIR as a flat stream (XIRF).
//!
//! XIRF lightly parses a raw XIR [`TokenStream`] into a stream of
//!   [`XirfToken`]s that are,
//!     like a [`TokenStream`],
//!     flat in structure.
//! It provides the following features over raw XIR:
//!
//!   1. All closing tags must correspond to a matching opening tag at the
//!        same depth;
//!   2. [`XirfToken`] exposes the [`Depth`] of each node-related token;
//!   3. Attribute tokens are parsed into [`Attr`] objects;
//!   4. Documents must begin with an element and end with the closing of
//!        that element;
//!   5. Parsing will fail if input ends before all elements have been
//!        closed.
//!   6. Text nodes may optionally be parsed into [`RefinedText`] to
//!        distinguish whitespace.
//!
//! XIRF lowering does not perform any dynamic memory allocation;
//!   maximum element nesting depth is set statically depending on the needs
//!   of the caller.

use super::{
    attr::{Attr, AttrParseError, AttrParseState},
    reader::is_xml_whitespace_char,
    CloseSpan, OpenSpan, QName, Token as XirToken, TokenStream,
};
use crate::{
    f::Functor,
    parse::prelude::*,
    span::Span,
    sym::{st::is_common_whitespace, GlobalSymbolResolve, SymbolId},
    xir::EleSpan,
};
use arrayvec::ArrayVec;
use std::marker::PhantomData;

// Used for organization.
pub use accept::*;

/// Tag nesting depth
///   (`0` represents the root).
///
/// Note: the lack of a [`Default`] implementation is intentional so that
///   this does not see lax initialization;
///     you probably want [`Depth::root`] in that case.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct Depth(pub usize);

impl Depth {
    /// Depth representing a root.
    pub fn root() -> Depth {
        Depth(0)
    }

    /// Yield a new [`Depth`] representing the expected depth of children of
    ///   an element at the current depth.
    ///
    /// That description is probably more confusing than the method name.
    pub fn child_depth(&self) -> Depth {
        match self {
            Depth(depth) => Depth(depth + 1),
        }
    }
}

impl Display for Depth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

/// A lightly-parsed XIRF object.
///
/// Certain XIR [`Token`]s are formed into a single object,
///   such as an [`Attr`].
/// Other objects retain the same format as their underlying token,
///   but are still validated to ensure that they are well-formed and that
///   the XML is well-structured.
///
/// Each token representing a child node contains a numeric [`Depth`]
///   indicating the nesting depth;
///     this can be used by downstream parsers to avoid maintaining their
///     own stack in certain cases.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum XirfToken<T: TextType> {
    /// Opening tag of an element.
    Open(QName, OpenSpan, Depth),

    /// Closing tag of an element.
    ///
    /// If the name is [`None`],
    ///   then the tag is self-closing.
    /// If the name is [`Some`],
    ///   then the tag is guaranteed to be balanced
    ///     (matching the depth of its opening tag).
    Close(Option<QName>, CloseSpan, Depth),

    /// An attribute and its value.
    ///
    /// The associated [`Span`]s can be found on the enclosed [`Attr`]
    ///   object.
    Attr(Attr),

    /// Comment node.
    Comment(SymbolId, Span, Depth),

    /// Character data as part of an element.
    ///
    /// See also [`CData`](XirfToken::CData) variant.
    Text(T, Depth),

    /// CData node (`<![CDATA[...]]>`).
    ///
    /// _Warning: It is up to the caller to ensure that the string `]]>` is
    ///   not present in the text!_
    /// This is intended for reading existing XML data where CData is
    ///   already present,
    ///     not for producing new CData safely!
    CData(SymbolId, Span, Depth),
}

impl<T: TextType> XirfToken<T> {
    pub fn open(
        qname: impl Into<QName>,
        span: impl Into<OpenSpan>,
        depth: Depth,
    ) -> Self {
        Self::Open(qname.into(), span.into(), depth)
    }

    pub fn close(
        qname: Option<impl Into<QName>>,
        span: impl Into<CloseSpan>,
        depth: Depth,
    ) -> Self {
        Self::Close(qname.map(Into::into), span.into(), depth)
    }

    pub fn attr(
        qname: impl Into<QName>,
        value: impl Into<SymbolId>,
        span: (impl Into<Span>, impl Into<Span>),
    ) -> Self {
        Self::Attr(Attr::new(
            qname.into(),
            value.into(),
            (span.0.into(), span.1.into()),
        ))
    }

    pub fn comment(
        comment: impl Into<SymbolId>,
        span: impl Into<Span>,
        depth: Depth,
    ) -> Self {
        Self::Comment(comment.into(), span.into(), depth)
    }

    pub fn text(text: impl Into<T>, depth: Depth) -> Self {
        Self::Text(text.into(), depth)
    }
}

impl<T: TextType> Token for XirfToken<T> {
    fn ir_name() -> &'static str {
        "XIRF"
    }

    fn span(&self) -> Span {
        use XirfToken::*;

        match self {
            Open(_, OpenSpan(span, _), _)
            | Close(_, CloseSpan(span, _), _)
            | Comment(_, span, _)
            | CData(_, span, _) => *span,

            Text(text, _) => text.span(),
            Attr(attr) => attr.span(),
        }
    }
}

impl<T: TextType> Object for XirfToken<T> {}

impl<T: TextType> Display for XirfToken<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use XirfToken::*;

        match self {
            Open(qname, span, _) => {
                Display::fmt(&XirToken::Open(*qname, *span), f)
            }
            Close(oqname, span, _) => {
                Display::fmt(&XirToken::Close(*oqname, *span), f)
            }
            Attr(attr) => Display::fmt(&attr, f),
            Comment(sym, span, _) => {
                Display::fmt(&XirToken::Comment(*sym, *span), f)
            }
            Text(text, _) => Display::fmt(text, f),
            CData(sym, span, _) => {
                Display::fmt(&XirToken::CData(*sym, *span), f)
            }
        }
    }
}

impl<T: TextType> XirfToken<T> {
    pub fn depth(&self) -> Option<Depth> {
        use XirfToken::*;

        match self {
            Open(_, _, depth)
            | Close(_, _, depth)
            | Comment(_, _, depth)
            | Text(_, depth)
            | CData(_, _, depth) => Some(*depth),
            Attr(_) => None,
        }
    }
}

impl<T: TextType> From<Attr> for XirfToken<T> {
    fn from(attr: Attr) -> Self {
        Self::Attr(attr)
    }
}

impl<T: TextType> Functor<Depth> for XirfToken<T> {
    fn map(self, f: impl FnOnce(Depth) -> Depth) -> Self::Target {
        use XirfToken::*;

        match self {
            Open(qn, span, depth) => Open(qn, span, f(depth)),
            Close(qn, span, depth) => Close(qn, span, f(depth)),
            Attr(_) => self,
            Comment(sym, span, depth) => Comment(sym, span, f(depth)),
            Text(text, depth) => Text(text, f(depth)),
            CData(cdata, span, depth) => CData(cdata, span, f(depth)),
        }
    }
}

/// Token of an optionally refined [`Text`].
///
/// XIRF is configurable on the type of processing it performs on [`Text`],
///   including the detection of [`Whitespace`].
///
/// See also [`RefinedText`].
pub trait TextType = From<Text> + Into<Text> + Token + Eq;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Text(pub SymbolId, pub Span);

impl Token for Text {
    fn ir_name() -> &'static str {
        "XIRF Text"
    }

    fn span(&self) -> Span {
        match self {
            Self(_, span) => *span,
        }
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: We'll need care to output text so that it does not mess up
        //   formatted output.
        // Further,
        //   text can be any arbitrary length,
        //   and so should probably be elided after a certain length.
        write!(f, "text")
    }
}

/// A sequence of one or more whitespace characters.
///
/// Whitespace here is expected to consist of `[ \n\t\r]`
///   (where the first character in that class is a space).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Whitespace(pub Text);

impl Display for Whitespace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: Escape output as necessary so that we can render the symbol
        //   string.
        // See also `<Text as Display>::fmt` TODO.
        write!(f, "whitespace")
    }
}

/// Text that has been refined to a more descriptive form.
///
/// This type may be used as a [`TextType`] to instruct XIRF to detect
///   [`Whitespace`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RefinedText {
    /// Provided [`Text`] has been determined to be [`Whitespace`].
    Whitespace(Whitespace),
    /// Provided [`Text`] was not able to be refined into a more specific
    ///   type.
    Unrefined(Text),
}

impl Token for RefinedText {
    fn ir_name() -> &'static str {
        "XIRF RefinedText"
    }

    fn span(&self) -> Span {
        match self {
            Self::Whitespace(Whitespace(text)) | Self::Unrefined(text) => {
                text.span()
            }
        }
    }
}

impl Display for RefinedText {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Whitespace(ws) => Display::fmt(ws, f),
            Self::Unrefined(text) => Display::fmt(text, f),
        }
    }
}

impl From<Text> for RefinedText {
    fn from(text: Text) -> Self {
        match text {
            Text(sym, _) if is_whitespace(sym) => {
                Self::Whitespace(Whitespace(text))
            }
            _ => Self::Unrefined(text),
        }
    }
}

impl From<RefinedText> for Text {
    fn from(value: RefinedText) -> Self {
        match value {
            RefinedText::Whitespace(Whitespace(text))
            | RefinedText::Unrefined(text) => text,
        }
    }
}

/// XIRF-compatible attribute parser.
pub trait FlatAttrParseState<const MAX_DEPTH: usize> =
    ClosedParseState<Token = XirToken, Object = Attr>
    where
        Self: Default,
        <Self as ParseState>::Error: Into<XirToXirfError>,
        StateContext<MAX_DEPTH>: AsMut<<Self as ParseState>::Context>;

/// Stack of element [`QName`] and [`Span`] pairs,
///   representing the current level of nesting.
///
/// This storage is statically allocated,
///   allowing XIRF's parser to avoid memory allocation entirely.
type ElementStack<const MAX_DEPTH: usize> = ArrayVec<(QName, Span), MAX_DEPTH>;

/// Lower [XIR](XirToken) into [XIRF](XirfToken),
///   accepting only fully parsed XML documents.
///
/// If parsing is expected to stop before reaching the end of the document,
///   see [`PartialXirToXirf`].
/// For more information on accepting states,
///   see [`XirfAcceptor`].
pub type FullXirToXirf<const MAX_DEPTH: usize, T> =
    XirToXirf<MAX_DEPTH, T, AttrParseState, FullXirfAcceptor>;

/// Lower [XIR](XirToken) into [XIRF](XirfToken),
///   accepting partially parsed XML documents at node boundaries.
///
/// If the entire XML document ought to be parsed,
///   see [`FullXirToXirf`] to provide a guarantee of an error in case the
///   system stops parsing before completion.
/// For more information on accepting states,
///   see [`XirfAcceptor`].
pub type PartialXirToXirf<const MAX_DEPTH: usize, T> =
    XirToXirf<MAX_DEPTH, T, AttrParseState, PartialXirfAcceptor>;

/// Lower [XIR](XirToken) into [XIRF](XirfToken).
///
/// This parser is a pushdown automaton that parses a single XML document.
#[derive(Debug, PartialEq, Eq)]
pub enum XirToXirf<
    const MAX_DEPTH: usize,
    T,
    SA = AttrParseState,
    A: XirfAcceptor = FullXirfAcceptor,
> where
    SA: FlatAttrParseState<MAX_DEPTH>,
    T: TextType,
{
    /// Document parsing has not yet begun.
    PreRoot(PhantomData<(T, A)>),
    /// Parsing nodes.
    NodeExpected,
    /// Delegating to attribute parser.
    AttrExpected(SA),
    /// End of document has been reached.
    Done,
}

impl<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor> Default
    for XirToXirf<MAX_DEPTH, T, SA, A>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
    T: TextType,
{
    fn default() -> Self {
        Self::PreRoot(PhantomData::default())
    }
}

pub type StateContext<const MAX_DEPTH: usize> =
    Context<ElementStack<MAX_DEPTH>>;

/// Whether the given [`SymbolId`] is all whitespace according to
///   [`is_xml_whitespace_char`].
///
/// This will first consult the pre-interned whitespace symbol list using
///   [`is_common_whitespace`].
/// If that check fails,
///   it will resort to looking up the symbol and performing a linear scan
///   of the string,
///     terminating early if a non-whitespace character is found.
///
/// Note that the empty string is considered to be whitespace.
#[inline]
fn is_whitespace(sym: SymbolId) -> bool {
    // See `sym::prefill`;
    //   this may require maintenance to keep the prefill list up-to-date
    //   with common whitespace symbols to avoid symbol lookups.
    // This common check is purely a performance optimization.
    is_common_whitespace(sym) || {
        // If this is called often and is too expensive,
        //   it may be worth caching metadata about symbols,
        //     either for XIRF or globally.
        // This requires multiple dereferences
        //   (for looking up the intern for the `SymbolId`,
        //     which may result in multiple (CPU) cache misses,
        //       but that would have to be profiled since the symbol may
        //       have just been interned and may be cached still)
        //   and then a linear scan of the associated `str`,
        //     though it will terminate as soon as it finds a non-whitespace
        //     character.
        sym.lookup_str().chars().all(is_xml_whitespace_char)
    }
}

impl<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor> ParseState
    for XirToXirf<MAX_DEPTH, T, SA, A>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
    T: TextType,
{
    type Token = XirToken;
    type Object = XirfToken<T>;
    type Error = XirToXirfError;
    type Context = StateContext<MAX_DEPTH>;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self> {
        use XirToXirf::{AttrExpected, Done, NodeExpected, PreRoot};

        match (self, tok) {
            // Comments are permitted before and after the first root element.
            (st @ (PreRoot(_) | Done), XirToken::Comment(sym, span)) => {
                let depth = Depth(stack.len());
                Transition(st).ok(XirfToken::Comment(sym, span, depth))
            }

            // Ignore whitespace before or after root.
            (st @ (PreRoot(_) | Done), XirToken::Text(sym, _))
                if is_whitespace(sym) =>
            {
                Transition(st).incomplete()
            }

            (PreRoot(_), tok @ XirToken::Open(..)) => {
                Self::parse_node(tok, stack)
            }

            (st @ PreRoot(_), tok) => {
                Transition(st).err(XirToXirfError::RootOpenExpected(tok))
            }

            (NodeExpected, tok) => Self::parse_node(tok, stack),

            (AttrExpected(sa), tok) => sa.delegate(
                tok,
                stack,
                |sa| Transition(AttrExpected(sa)),
                || Transition(NodeExpected),
            ),

            (Done, tok) => Transition(Done).dead(tok),
        }
    }

    /// Whether all elements have been closed.
    ///
    /// Parsing will fail if there are any open elements.
    /// Intuitively,
    ///   this means that the parser must have encountered the closing tag
    ///   for the root element.
    fn is_accepting(&self, _: &Self::Context) -> bool {
        // TODO: It'd be nice if we could also return additional context to
        //   aid the user in diagnosing the problem,
        //     e.g. what element(s) still need closing.
        A::is_accepting(self)
    }
}

/// Configurable accepting states for [`XirToXirf`].
///
/// See this module's [`XirfAcceptor`] for more information.
mod accept {
    use super::*;

    /// Acceptor for [`XirToXirf`].
    ///
    /// This is responsible for determining whether [`XirToXirf`] is in an
    ///   accepting state.
    /// There are two acceptors:
    ///
    ///   1. [`FullXirfAcceptor`] expects that the _entire_ XML document be
    ///        completely parsed up to and including the closing root node;
    ///        and
    ///   2. [`PartialXirfAcceptor`] allows parsing to halt part-way through
    ///        an XML document,
    ///          provided that parsing ends at a node boundary.
    ///
    /// See each respective acceptor for more information.
    pub trait XirfAcceptor: Debug + PartialEq + Eq + Display + Default {
        fn is_accepting<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor>(
            st: &XirToXirf<MAX_DEPTH, T, SA, A>,
        ) -> bool
        where
            SA: FlatAttrParseState<MAX_DEPTH>,
            T: TextType;
    }

    /// Acceptor for fully parsed XML documents for [`XirToXirf`].
    ///
    /// This acceptor should be used when the intent of the lowering
    ///   pipeline is to fully parse the [XIR](XirToken) stream.
    /// In other words:
    ///   this should be used when the XML document being read ought to be
    ///   read _fully_,
    ///     where halting parsing before the root node would indicate a
    ///     defect in the system.
    ///
    /// For example,
    ///   when reading a file with TAME sources in `tamec`,
    ///   the compiler ought to ensure that the entire file is read to
    ///     completion.
    /// If the lowering pipeline stops requesting tokens before the XIR
    ///   stream has ended,
    ///     then that means that compilation has halted before the system
    ///     has had a chance to consider the rest of the file.
    /// Because the lowering pipeline is intended to parse and present
    ///   errors on the entire file each run,
    ///     this would represent a bug in the system,
    ///       and so we ought to fail.
    ///
    /// Downstream parsers ought to fail for their own reasons as well,
    ///   but this provides an extra layer of protection for _anything_ that
    ///   happens to read XML files.
    ///
    /// For an example of a situation where we may not wish to fail,
    ///   see [`PartialXirfAcceptor`].
    #[derive(Debug, PartialEq, Eq, Default)]
    pub struct FullXirfAcceptor;

    impl XirfAcceptor for FullXirfAcceptor {
        fn is_accepting<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor>(
            st: &XirToXirf<MAX_DEPTH, T, SA, A>,
        ) -> bool
        where
            SA: FlatAttrParseState<MAX_DEPTH>,
            T: TextType,
        {
            matches!(st, XirToXirf::Done)
        }
    }

    impl Display for FullXirfAcceptor {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "accepting only full documents")
        }
    }

    /// Acceptor for either full _or_ partially parsed XML documents for
    ///   [`XirToXirf`].
    ///
    /// This acceptor is intended to be used when parsing the entirety of a
    ///   [XIR](XirToken) stream is not desirable;
    ///     it allows parsing to be completed at a node boundary
    ///       (when a node is expected).
    /// This acceptor builds on the behavior of [`FullXirfAcceptor`],
    ///   and so will also accept all fully parsed documents.
    ///
    /// For example,
    ///   when reading object files in `tameld`,
    ///   the linker is concerned only with header information;
    ///     the remainder of the XML document does not contain useful
    ///     information and would be wasteful to parse.
    /// In that case,
    ///   we rely on downstream parsers to determine whether the document
    ///   has been sufficiently parsed.
    ///
    /// This acceptor provides one weaker guarantee:
    ///   that parsing has _at least_ completed parsing a node,
    ///     such as an element.
    /// Parsing must complete at a node boundary,
    ///   and so cannot halt in the middle of attribute parsing for an
    ///     element,
    ///       for example.
    #[derive(Debug, PartialEq, Eq, Default)]
    pub struct PartialXirfAcceptor;

    impl XirfAcceptor for PartialXirfAcceptor {
        fn is_accepting<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor>(
            st: &XirToXirf<MAX_DEPTH, T, SA, A>,
        ) -> bool
        where
            SA: FlatAttrParseState<MAX_DEPTH>,
            T: TextType,
        {
            FullXirfAcceptor::is_accepting(st)
                || matches!(st, XirToXirf::NodeExpected)
        }
    }

    impl Display for PartialXirfAcceptor {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "accepting partial documents at node boundaries")
        }
    }
}

impl<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor> Display
    for XirToXirf<MAX_DEPTH, T, SA, A>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
    T: TextType,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XirToXirf::*;

        match self {
            PreRoot(_) => write!(f, "expecting document root"),
            NodeExpected => write!(f, "expecting a node"),
            AttrExpected(sa) => Display::fmt(sa, f),
            Done => write!(f, "done parsing document root"),
        }?;

        // e.g. ", accepting ..."
        write!(f, ", ")?;
        Display::fmt(&A::default(), f)
    }
}

impl<const MAX_DEPTH: usize, T, SA, A: XirfAcceptor>
    XirToXirf<MAX_DEPTH, T, SA, A>
where
    SA: FlatAttrParseState<MAX_DEPTH>,
    T: TextType,
{
    /// Parse a token while in a state expecting a node.
    fn parse_node(
        tok: <Self as ParseState>::Token,
        stack: &mut ElementStack<MAX_DEPTH>,
    ) -> TransitionResult<Self> {
        use XirToXirf::{AttrExpected, Done, NodeExpected};

        let depth = Depth(stack.len());

        match tok {
            XirToken::Open(qname, span) if stack.len() == MAX_DEPTH => {
                Transition(NodeExpected).err(XirToXirfError::MaxDepthExceeded {
                    open: (qname, span.tag_span()),
                    max: Depth(MAX_DEPTH),
                })
            }

            XirToken::Open(qname, span) => {
                stack.push((qname, span.tag_span()));

                // Delegate to the attribute parser until it is complete.
                Transition(AttrExpected(SA::default()))
                    .ok(XirfToken::Open(qname, span, depth))
            }

            XirToken::Close(close_oqname, close_span) => {
                match (close_oqname, stack.pop()) {
                    (_, None) => unreachable!("parser should be in Done state"),

                    (Some(qname), Some((open_qname, open_span)))
                        if qname != open_qname =>
                    {
                        Transition(NodeExpected).err(
                            XirToXirfError::UnbalancedTag {
                                open: (open_qname, open_span),
                                close: (qname, close_span.tag_span()),
                            },
                        )
                    }

                    // Final closing tag (for root node) completes the document.
                    (..) if stack.is_empty() => Transition(Done).ok(
                        XirfToken::Close(close_oqname, close_span, Depth(0)),
                    ),

                    (..) => {
                        let depth = stack.len();

                        Transition(NodeExpected).ok(XirfToken::Close(
                            close_oqname,
                            close_span,
                            Depth(depth),
                        ))
                    }
                }
            }

            XirToken::Comment(sym, span) => Transition(NodeExpected)
                .ok(XirfToken::Comment(sym, span, depth)),

            XirToken::Text(sym, span) => Transition(NodeExpected)
                .ok(XirfToken::Text(T::from(Text(sym, span)), depth)),

            XirToken::CData(sym, span) => {
                Transition(NodeExpected).ok(XirfToken::CData(sym, span, depth))
            }

            // We should transition to `State::Attr` before encountering any
            //   of these tokens.
            XirToken::AttrName(..)
            | XirToken::AttrValue(..)
            | XirToken::AttrValueFragment(..) => {
                unreachable!("attribute token in NodeExpected state: {tok:?}")
            }
        }
    }
}

/// Produce a streaming parser lowering a XIR [`TokenStream`] into a XIRF
///   stream.
pub fn parse<const MAX_DEPTH: usize, T: TextType>(
    toks: impl TokenStream,
) -> impl Iterator<Item = ParsedResult<XirToXirf<MAX_DEPTH, T>>> {
    XirToXirf::<MAX_DEPTH, T>::parse(toks)
}

/// Parsing error from [`XirToXirf`].
#[derive(Debug, Eq, PartialEq)]
pub enum XirToXirfError {
    /// Opening root element tag was expected.
    RootOpenExpected(XirToken),

    /// Opening tag exceeds the maximum nesting depth for this parser.
    MaxDepthExceeded { open: (QName, Span), max: Depth },

    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName, Span),
        close: (QName, Span),
    },

    /// Error from the attribute parser.
    AttrError(AttrParseError),
}

impl Display for XirToXirfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use XirToXirfError::*;

        match self {
            RootOpenExpected(_tok) => {
                write!(f, "missing opening root element",)
            }

            MaxDepthExceeded {
                open: (_name, _),
                max,
            } => {
                write!(
                    f,
                    "maximum XML element nesting depth of `{max}` exceeded"
                )
            }

            UnbalancedTag {
                open: (open_name, _),
                close: (_close_name, _),
            } => {
                write!(f, "expected closing tag for `{open_name}`")
            }

            AttrError(e) => Display::fmt(e, f),
        }
    }
}

impl Error for XirToXirfError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::AttrError(e) => Some(e),
            _ => None,
        }
    }
}

impl Diagnostic for XirToXirfError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use XirToXirfError::*;

        match self {
            RootOpenExpected(tok) => {
                // TODO: Should the span be the first byte,
                //   or should we delegate that question to an e.g. `SpanLike`?
                tok.span()
                    .error("an opening root node was expected here")
                    .into()
            }

            MaxDepthExceeded {
                open: (_, span),
                max,
            } => span
                .error(format!(
                    "this opening tag increases the level of nesting \
                       past the limit of {max}"
                ))
                .into(),

            UnbalancedTag {
                open: (open_name, open_span),
                close: (_close_name, close_span),
            } => {
                // TODO: hint saying that the nesting could be wrong, etc;
                //   we can't just suggest a replacement,
                //     since that's not necessarily the problem
                vec![
                    open_span
                        .note(format!("element `{open_name}` is opened here")),
                    // No need to state the close name since the source line
                    //   will be highlighted by the diagnostic message.
                    close_span.error(format!("expected `</{open_name}>`")),
                ]
            }

            AttrError(e) => e.describe(),
        }
    }
}

impl From<AttrParseError> for XirToXirfError {
    fn from(e: AttrParseError) -> Self {
        Self::AttrError(e)
    }
}

/// Lower a [`XirfToken`] stream into a [`XirToken`] stream.
///
/// This is the dual of [`XirToXirf`],
///   and is intended to be used when the system _generates_ XML.
/// If you do not need any features of XIRF,
///   and aren't using any operation that produces it,
///   then you may also skip a step and just emit XIR to avoid having to
///   perform this lowering operation.
#[derive(Debug, PartialEq, Eq)]
pub enum XirfToXir<T: TextType> {
    Ready(PhantomData<T>),
    AttrVal(PhantomData<T>),
}

impl<T: TextType> Default for XirfToXir<T> {
    fn default() -> Self {
        Self::Ready(Default::default())
    }
}

impl<T: TextType> Display for XirfToXir<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "translating XIRF to XIR")
    }
}

diagnostic_infallible! {
    pub enum XirfToXirError {}
}

impl<T: TextType> ParseState for XirfToXir<T> {
    type Token = XirfToken<T>;
    type Object = XirToken;
    type Error = XirfToXirError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self::Super> {
        use XirToken as Xir;
        use XirfToXir::*;
        use XirfToken as Xirf;

        macro_rules! to {
            ($tok:expr) => {
                Transition(self).ok($tok)
            };
        }

        match tok {
            Xirf::Open(qname, ospan, _) => to!(Xir::Open(qname, ospan)),
            Xirf::Close(qname, cspan, _) => to!(Xir::Close(qname, cspan)),
            Xirf::Attr(attr) => match self {
                Self::Ready(p) => Transition(AttrVal(p))
                    .ok(Xir::AttrName(attr.name(), attr.attr_span().key_span()))
                    .with_lookahead(Xirf::Attr(attr)),
                Self::AttrVal(p) => Transition(Ready(p)).ok(Xir::AttrValue(
                    attr.value(),
                    attr.attr_span().value_span(),
                )),
            },
            Xirf::Comment(sym, span, _) => to!(Xir::Comment(sym, span)),
            Xirf::Text(x, _) => match x.into() {
                Text(sym, span) => to!(Xir::Text(sym, span)),
            },
            Xirf::CData(sym, span, _) => to!(Xir::CData(sym, span)),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready(_))
    }
}

#[cfg(test)]
pub mod test;
