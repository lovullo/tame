// XIR tree representation
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

//! XIR token stream parsed into a tree-based IR (XIRT).
//!
//! **This is a work-in-progress implementation.**
//! It will be augmented only as needed.
//!
//! Parsing is handled by [`Stack::parse_token`].
//! An [`Iterator::scan`]-based parser can be constructed using
//!   [`parser_from`] or [`parse`][parse()].
//!
//! ```
//! use tamer::xir::tree::{Stack, parse, parser_from};
//!# use tamer::xir::Token;
//!
//!# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
//! // Lazily parse a stream of XIR tokens as an iterator, yielding the next
//! // fully parsed object.  This may consume any number of tokens.
//! let parser = parser_from(token_stream);
//!
//!# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
//! // Consume a single token at a time, yielding either an incomplete state
//! // or the next parsed object.
//! let parser = parse(token_stream);
//! ```
//!
//! `parser_from` Or `parse`?
//! =========================
//! [`parser_from`] is implemented in terms of [`parse`][parse()].
//! They have slightly different use cases and tradeoffs:
//!
//! [`parse`][parse()] yields a [`Result`] containing [`Parsed`],
//!   which _may_ contain an [`Parsed::Object`],
//!   but it's more likely to contain [`Parsed::Incomplete`];
//!     this is because it typically takes multiple [`Token`]s to complete
//!     parsing within a given context.
//!
//! In return, though, you get some important guarantees:
//!
//!   1. [`parse`][parse()] consumes only a _single_ token; and
//!   2. It has a constant upper bound for execution time.
//!
//! This means that [`parse`][parse()] will never cause the system to
//!   hang---you
//!     are in complete control over how much progress parsing makes,
//!       and are free to stop and resume it at any time.
//!
//! However,
//!   if you do not care about those things,
//!   working with [`Parsed`] is verbose and inconvenient;
//!     sometimes you just want the next [`Tree`] object.
//! For this,
//!   we have [`parser_from`],
//!     which does two things:
//!
//!   1. It filters out all [`Parsed::Incomplete`]; and
//!   2. On [`Parsed::Object`],
//!        it yields the inner [`Tree`].
//!
//! This is a much more convenient API,
//!   but is not without its downsides:
//!     if the context is large
//!       (e.g. the root node of a large XML document),
//!       parsing can take a considerable amount of time,
//!         and the [`Iterator`] produced by [`parser_from`] will cause the
//!         system to process [`Iterator::next`] for that entire duration.
//!
//! See also [`attr_parser_from`] for parsing only attributes partway
//!   through a token stream.
//!
//! [`Parsed::Incomplete`]: super::parse::Parsed::Incomplete
//! [`Parsed::Object`]: super::parse::Parsed::Object
//!
//! Cost of Parsing
//! ===============
//! While [`Tree`] is often much easier to work with than a stream of
//!   [`Token`],
//!     there are notable downsides:
//!
//!   - The context in which parsing began
//!       (see _Parser Implementation_ below)
//!       must complete before _any_ token is emitted.
//!     If parsing begins at the root element,
//!       this means that the _entire XML document_ must be loaded into
//!       memory before it is available for use.
//!   - While the token stream is capable of operating using constant memory
//!       (since [`Token`] can be discarded after being consumed),
//!         a [`Tree`] holds a significant amount of data in memory.
//!
//! It is recommended to parse into [`Tree`] only for the portions of the
//!   XML document that will benefit from it.
//! For example,
//!   by avoiding parsing of the root element into a tree,
//!     you can emit [`Tree`] for child elements without having to wait for
//!     the entire document to be parsed.
//!
//!
//! Validity Of Token Stream
//! ========================
//! XIR verifies that each [`Token`] is syntactically valid and follows an
//!   XML grammar subset;
//!     as such,
//!       the tree parser does not concern itself with syntax analysis.
//! It does,
//!   however,
//!   perform _[semantic analysis]_ on the token stream.
//! Given that,
//!   [`Stack::parse_token`] returns a [`Result`],
//!     with parsing errors represented by this module's [`StackError`].
//!
//! As an example,
//!   a XIR token stream permits unbalanced tags.
//! However,
//!   we cannot represent an invalid tree,
//!   so that would result in a semantic error.
//!
//! [semantic analysis]: https://en.wikipedia.org/wiki/Semantic_analysis_(compilers)
//!
//!
//! Parser Implementation
//! =====================
//! The parser that lowers the XIR [`Token`] stream into a [`Tree`]
//!   is implemented on [`Stack`].
//!
//! This parser is a [stack machine],
//!   where the stack represents the [`Tree`] that is under construction.
//! Parsing operates on _context_.
//! At present, the only parsing context is an element---it
//!   begins parsing at an opening tag ([`Token::Open`]) and completes
//!   parsing at a _matching_ [`Token::Close`].
//! All attributes and child nodes encountered during parsing of an element
//!   will automatically be added to the appropriate element,
//!     recursively.
//!
//! [stack machine]: https://en.wikipedia.org/wiki/Stack_machine
//!
//! State Machine With A Typed Stack
//! --------------------------------
//! The parser is a [finate-state machine (FSM)] with a stack encoded in
//!   variants of [`Stack`],
//!     where each variant represents the current state of the parser.
//! The parser cannot be reasoned about as a pushdown automaton because the
//!   language of the [`Stack`] is completely arbitrary,
//!     but it otherwise operates in a similar manner.
//!
//! Each state transition consumes the entire stack and produces a new one,
//!   which may be identical.
//! Intuitively, though, based on the construction of [`Stack`],
//!   this is equivalent to popping the needed data off of the stack and
//!   optionally pushing additional information.
//!
//! By encoding the stack in [`Stack`] variants,
//!   we are able to verify statically that the stack is always in a valid
//!   state and contains expected data---that
//!     is, our stack is fully type-safe.
//!
//! [state machine]: https://en.wikipedia.org/wiki/Finite-state_machine

use super::{
    attr::{Attr, AttrList, AttrParseError, AttrParseState},
    QName, Token, Token as XirToken, TokenStream,
};

use crate::{
    parse::{
        self, ParseError, ParseResult, ParseState, ParseStatus, ParsedResult,
        Transition, TransitionResult,
    },
    span::Span,
    sym::SymbolId,
};
use std::{error::Error, fmt::Display, result};

type Parsed = crate::parse::Parsed<Tree>;

/// A XIR tree (XIRT).
///
/// This object represents a XIR token stream parsed into a tree
///   representation.
/// This representation is easier to process and manipulate in most contexts,
///   but also requires memory allocation for the entire tree and requires
///   that a potentially significant portion of a token stream be processed
///     (e.g. from start to end tag for a given element).
///
/// _Note that this implementation is incomplete!_
/// It will be augmented as needed.
///
/// For more information,
///  see the [module-level documentation](self).
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Tree {
    /// XML element.
    Element(Element),

    /// Text node.
    ///
    /// A text node cannot contain other [`Tree`] elements;
    ///   sibling text nodes must exist within an [`Element`].
    Text(SymbolId, Span),

    /// This variant exists purely because `#[non_exhaustive]` has no effect
    ///   within the crate.
    ///
    /// This ensures that matches must account for other variants that will
    ///   be introduced in the future,
    ///     easing the maintenance burden
    ///       (for both implementation and unit tests).
    _NonExhaustive,
}

impl Into<Option<Element>> for Tree {
    #[inline]
    fn into(self) -> Option<Element> {
        match self {
            Self::Element(ele) => Some(ele),
            _ => None,
        }
    }
}

impl Into<Option<SymbolId>> for Tree {
    #[inline]
    fn into(self) -> Option<SymbolId> {
        match self {
            Self::Text(text, _) => Some(text),
            _ => None,
        }
    }
}

impl Tree {
    /// Yield a reference to the inner value if it is an [`Element`],
    ///   otherwise [`None`].
    #[inline]
    pub fn as_element<'a>(&'a self) -> Option<&'a Element> {
        match self {
            Self::Element(ele) => Some(ele),
            _ => None,
        }
    }

    /// Yield the inner value if it is an [`Element`],
    ///   otherwise [`None`].
    #[inline]
    pub fn into_element(self) -> Option<Element> {
        self.into()
    }

    /// Whether the inner value is an [`Element`].
    #[inline]
    pub fn is_element(&self) -> bool {
        matches!(self, Self::Element(_))
    }

    /// Yield a string representation of the element,
    ///   if applicable.
    ///
    /// This is incomplete.
    #[inline]
    pub fn as_sym(&self) -> Option<SymbolId> {
        match self {
            Self::Text(sym, ..) => Some(*sym),
            _ => None,
        }
    }
}

impl parse::Object for Tree {}

/// Element node.
///
/// This represents an [XML element] beginning with an opening tag that is
///   either self-closing or ending with a balanced closing tag.
/// The two spans together represent the span of the entire element with all
///   its constituents.
///
/// [XML element]: https://www.w3.org/TR/REC-xml/#sec-starttags
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Element {
    name: QName,
    /// Zero or more attributes.
    attrs: AttrList,
    /// Zero or more child nodes.
    children: Vec<Tree>,
    /// Spans for opening and closing tags respectively.
    span: (Span, Span),
}

impl Element {
    /// Element name.
    #[inline]
    pub fn name(&self) -> QName {
        self.name
    }

    /// Child [`Tree`] objects of this element.
    #[inline]
    pub fn children(&self) -> &Vec<Tree> {
        &self.children
    }

    /// Attributes of this element.
    #[inline]
    pub fn attrs(&self) -> &AttrList {
        &self.attrs
    }

    /// Opens an element for incremental construction.
    ///
    /// This is intended for use by the parser to begin building an element.
    /// It does not represent a completed element and should not be yielded
    ///   to any outside caller until it is complete.
    /// This incomplete state is encoded in [`Stack::BuddingElement`].
    #[inline]
    fn open(name: QName, span: Span) -> Self {
        Self {
            name,
            attrs: AttrList::new(),
            children: vec![],
            span: (span, span), // We do not yet know where the span will end
        }
    }

    /// Complete an element's span by setting its ending span.
    ///
    /// When elements are still budding (see [`Stack::BuddingElement`]),
    ///   the ending span is set to the starting span,
    ///   since the end is not yet known.
    #[inline]
    fn close_span(self, close_span: Span) -> Self {
        Element {
            span: (self.span.0, close_span),
            ..self
        }
    }
}

/// A [`Stack`] representing an element and its (optional) parent's stack.
///
/// Storing the parent of an [`Element`] allows it to be manipulated on the
///   [`Stack`] using the usual operations,
///     while maintaining the context needed to later add it as a child to
///     its parent once the element is completed.
///
/// This is used to represent a [`Stack::BuddingElement`].
/// This type exists because enum variants are not their own types,
///   but we want to nest _only_ element stacks,
///     not any type of stack.
#[derive(Debug, Eq, PartialEq)]
pub struct ElementStack {
    element: Element,

    /// Parent element stack to be restored once element has finished
    ///   processing.
    pstack: Option<Box<ElementStack>>,
}

impl ElementStack {
    /// Attempt to close an element,
    ///   verifying that the closing tag is either self-closing or
    ///   balanced.
    ///
    /// This does not verify that a request to self-close only happens if
    ///   there are no child elements;
    ///     that is the responsibility of the parser producing the XIR
    ///     stream to ensure that self-closing can only happen during
    ///     attribute parsing.
    fn try_close(
        self,
        close_name: Option<QName>,
        close_span: Span,
    ) -> Result<Self> {
        let Element {
            name: ele_name,
            span: (open_span, _),
            ..
        } = self.element;

        // Note that self-closing with children is syntactically
        // invalid and is expected to never make it into a XIR
        // stream to begin with, so we don't check for it.
        if let Some(name) = close_name {
            if name != ele_name {
                return Err(StackError::UnbalancedTag {
                    open: (ele_name, open_span),
                    close: (name, close_span),
                });
            }
        }

        Ok(Self {
            element: self.element.close_span(close_span),
            pstack: self.pstack,
        })
    }

    /// Transfer stack element into the parent as a child and return the
    ///   previous [`Stack`] state,
    ///     or yield a [`Stack::ClosedElement`] if there is no parent.
    ///
    /// If there is a parent element,
    ///   then the returned [`Stack`] will represent the state of the stack
    ///   prior to the child element being opened,
    ///     as stored with [`ElementStack::store`].
    fn consume_child_or_complete<SA: StackAttrParseState>(self) -> Stack<SA> {
        match self.pstack {
            Some(parent_stack) => Stack::BuddingElement(
                parent_stack.consume_element(self.element),
            ),

            None => Stack::ClosedElement(self.element),
        }
    }

    /// Push the provided [`Element`] onto the child list of the inner
    ///   [`Element`].
    fn consume_element(mut self, child: Element) -> Self {
        self.element.children.push(Tree::Element(child));
        self
    }

    /// Push the provided [`Attr`] onto the attribute list of the inner
    ///   [`Element`].
    fn consume_attrs(mut self, attr_list: AttrList) -> Self {
        self.element.attrs = attr_list;
        self
    }

    /// Transfer self to the heap to be later restored.
    ///
    /// This method simply exists for self-documentation.
    fn store(self) -> Box<Self> {
        Box::new(self)
    }
}

/// The state and typed stack of the XIR parser stack machine.
///
/// Since all possible states of the stack are known statically,
///   we encode the stack into variants,
///     where each variant represents the state of the parser's state
///     machine.
/// This way,
///   we know that the stack is always well-formed,
///   and benefit from strong type checking.
/// This also allows Rust to optimize its use.
///
/// Rust will compile this into a value that exists on the stack,
///   so we wind up with an actual stack machine in the end anyway.
///
/// For more information,
///   see the [module-level documentation](self).
#[derive(Debug, Eq, PartialEq)]
pub enum Stack<SA = AttrParseState>
where
    SA: StackAttrParseState,
{
    /// Empty stack.
    Empty,

    /// An [`Element`] that is still under construction.
    ///
    /// (This is a tree IR,
    ///    so here's a plant pun).
    BuddingElement(ElementStack),

    /// A completed [`Element`].
    ///
    /// This should be consumed and emitted.
    ClosedElement(Element),

    /// An [`AttrList`] that is still under construction.
    BuddingAttrList(ElementStack, AttrList),

    /// Parsing has been ceded to `SA` for attribute parsing.
    AttrState(ElementStack, AttrList, SA),

    /// Parsing has completed relative to the initial context.
    ///
    /// This is the final accepting state of the state machine.
    /// The parser will not operate while in this state,
    ///   which must be explicitly acknowledged and cleared in order to
    ///   indicate that additional tokens are expected and are not in
    ///   error.
    Done,
}

pub trait StackAttrParseState = ParseState<Token = XirToken, Object = Attr>
where
    <Self as ParseState>::Error: Into<StackError>;

impl<SA: StackAttrParseState> Default for Stack<SA> {
    fn default() -> Self {
        Self::Empty
    }
}

impl<SA: StackAttrParseState> ParseState for Stack<SA> {
    type Token = XirToken;
    type Object = Tree;
    type Error = StackError;

    fn parse_token(self, tok: Self::Token) -> TransitionResult<Self> {
        use Stack::*;

        match (self, tok) {
            // Open a root element (or lack of context).
            (Empty, Token::Open(name, span)) => {
                Self::begin_attrs(name, span, None)
            }

            // Open a child element.
            (BuddingElement(pstack), Token::Open(name, span)) => {
                Self::begin_attrs(name, span, Some(pstack.store()))
            }

            // Open a child element in attribute parsing context.
            (BuddingAttrList(pstack, attr_list), Token::Open(name, span)) => {
                Self::begin_attrs(
                    name,
                    span,
                    Some(pstack.consume_attrs(attr_list).store()),
                )
            }

            // Attribute parsing.
            (AttrState(estack, attrs, sa), tok) => {
                use ParseStatus::*;
                match sa.parse_token(tok).into() {
                    (Transition(sa), Ok(Incomplete)) => {
                        Transition(AttrState(estack, attrs, sa)).incomplete()
                    }
                    (Transition(sa), Ok(Object(attr))) => {
                        Transition(AttrState(estack, attrs.push(attr), sa))
                            .incomplete()
                    }
                    (_, Ok(Dead(lookahead))) => {
                        BuddingElement(estack.consume_attrs(attrs))
                            .parse_token(lookahead)
                    }
                    (Transition(sa), Err(x)) => {
                        Transition(AttrState(estack, attrs, sa)).err(x.into())
                    }
                }
            }

            (BuddingElement(stack), Token::Close(name, span)) => stack
                .try_close(name, span)
                .map(ElementStack::consume_child_or_complete)
                .map(|new_stack| match new_stack {
                    Stack::ClosedElement(ele) => {
                        Transition(Empty).ok(Tree::Element(ele))
                    }
                    _ => Transition(new_stack).incomplete(),
                })
                .unwrap_or_else(|err| Transition(Empty).err(err)),

            (BuddingAttrList(stack, attr_list), Token::Close(name, span)) => {
                stack
                    .consume_attrs(attr_list)
                    .try_close(name, span)
                    .map(ElementStack::consume_child_or_complete)
                    .map(|new_stack| match new_stack {
                        Stack::ClosedElement(ele) => {
                            Transition(Empty).ok(Tree::Element(ele))
                        }
                        _ => Transition(new_stack).incomplete(),
                    })
                    .unwrap_or_else(|err| Transition(Empty).err(err))
            }

            (BuddingElement(mut ele), Token::Text(value, span)) => {
                ele.element.children.push(Tree::Text(value, span));

                Transition(BuddingElement(ele)).incomplete()
            }

            (st, tok) if st.is_accepting() => Transition(st).dead(tok),

            (stack, tok) => {
                todo!(
                    "TODO: `{:?}` unrecognized.  The parser is not yet \
                        complete, so this could represent either a missing \
                        feature or a semantic error.  Stack: `{:?}`.",
                    tok,
                    stack
                )
            }
        }
    }

    fn is_accepting(&self) -> bool {
        *self == Self::Empty || *self == Self::Done
    }
}

impl<SA: StackAttrParseState> Stack<SA> {
    fn begin_attrs(
        name: QName,
        span: Span,
        pstack: Option<Box<ElementStack>>,
    ) -> TransitionResult<Self> {
        Transition(Self::AttrState(
            ElementStack {
                element: Element::open(name, span),
                pstack,
            },
            Default::default(),
            SA::default(),
        ))
        .incomplete()
    }
}

/// Result of a XIR tree parsing operation.
pub type Result<T> = std::result::Result<T, StackError>;

/// Parsing error from [`Stack`].
#[derive(Debug, Eq, PartialEq)]
pub enum StackError {
    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName, Span),
        close: (QName, Span),
    },

    AttrError(AttrParseError),

    /// An attribute was expected as the next [`Token`].
    AttrNameExpected(Token),

    /// Token stream ended before attribute parsing was complete.
    UnexpectedAttrEof,
}

impl Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // TODO: not a useful error because of symbols and missing span information
            Self::UnbalancedTag {
                open: (open_name, open_span),
                close: (close_name, close_span),
            } => {
                write!(
                    f,
                    "expected closing tag `{}`, but found `{}` at {} \
                     (opening tag at {})",
                    open_name, close_name, close_span, open_span
                )
            }

            Self::AttrError(e) => Display::fmt(e, f),

            Self::AttrNameExpected(tok) => {
                write!(f, "attribute name expected, found {}", tok)
            }

            // TODO: Perhaps we should include the last-encountered Span.
            Self::UnexpectedAttrEof => {
                write!(
                    f,
                    "unexpected end of input during isolated attribute parsing",
                )
            }
        }
    }
}

impl Error for StackError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::AttrError(e) => Some(e),
            _ => None,
        }
    }
}

/// Produce a streaming parser for the given [`TokenStream`].
///
/// If you do not require a single-step [`Iterator::next`] and simply want
///   the next parsed object,
///     use [`parser_from`] instead.
///
/// Note that parsing errors are represented by the wrapped [`Result`],
///   _not_ by [`None`].
///
/// This will produce an iterator that can only return [`None`] if the
///   iterator it scans returns [`None`].
///
/// ```
/// use tamer::xir::tree::{Stack, parse};
///# use tamer::xir::Token;
///
///# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
/// // The above is equivalent to:
/// let parser = parse(token_stream);
/// ```
pub fn parse(
    toks: impl TokenStream,
) -> impl Iterator<Item = ParsedResult<Stack>> {
    Stack::<AttrParseState>::parse(toks)
}

/// Produce a lazy parser from a given [`TokenStream`],
///   yielding only when an object has been fully parsed.
///
/// Unlike [`parse`][parse()],
///   which is intended for use with [`Iterator::scan`],
///   this will yield /only/ when the underlying parser yields
///   [`Tree`].
/// This interface is far more convenient,
///   but comes at the cost of not knowing how many parsing steps a single
///   [`Iterator::next`] call will take.
///
/// For more information on contexts,
///   and the parser in general,
///   see the [module-level documentation](self).
///
/// ```
/// use tamer::xir::tree::parser_from;
///# use tamer::xir::Token;
///
///# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
/// // Lazily parse a stream of XIR tokens as an iterator.
/// let parser = parser_from(token_stream);
/// ```
pub fn parser_from(
    toks: impl TokenStream,
) -> impl Iterator<Item = ParseResult<Stack, Tree>> {
    Stack::<AttrParseState>::parse(toks).filter_map(|parsed| match parsed {
        Ok(Parsed::Object(tree)) => Some(Ok(tree)),
        Ok(Parsed::Incomplete) => None,
        Err(x) => Some(Err(x)),
    })
}

/// Produce a lazy attribute parser from a given [`TokenStream`],
///   yielding only when an attribute has been fully parsed.
///
/// This is a specialized parser that begins parsing partway through a XIR
///   token stream.
/// To parse an entire stream as a tree,
///   see [`parser_from`].
///
/// This parser does not take ownership over the iterator,
///   allowing parsing to continue on the underlying token stream after
///   attribute parsing has completed.
/// Once attribute parsing is finished,
///   parsing is able to continue on the underlying token stream as if the
///   attributes were never present in XIR at all;
///     this also allows this parser to be used as an attribute filter while
///     ensuring that the attributes are syntactically valid.
///
/// For more information on contexts,
///   and the parser in general,
///   see the [module-level documentation](self).
#[inline]
pub fn attr_parser_from<'a>(
    toks: impl TokenStream,
) -> impl Iterator<Item = result::Result<Attr, ParseError<XirToken, StackError>>>
{
    use crate::parse::Parsed;

    AttrParseState::parse(toks).filter_map(|parsed| match parsed {
        Ok(Parsed::Object(attr)) => Some(Ok(attr)),
        Ok(Parsed::Incomplete) => None,
        Err(ParseError::StateError(e)) => {
            Some(Err(ParseError::StateError(StackError::AttrError(e))))
        }
        Err(e) => Some(Err(e.inner_into())),
    })
}

impl From<AttrParseError> for StackError {
    fn from(e: AttrParseError) -> Self {
        StackError::AttrError(e)
    }
}

#[cfg(test)]
pub fn merge_attr_fragments<'a>(
    toks: &'a mut impl TokenStream,
) -> impl TokenStream + 'a {
    use std::iter;

    use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve};

    let mut stack = Vec::with_capacity(4);

    iter::from_fn(move || {
        loop {
            match toks.next() {
                // Collect fragments and continue iterating until we find
                //   the final `Token::AttrValue`.
                Some(Token::AttrValueFragment(frag, ..)) => {
                    stack.push(frag);
                }

                // An AttrValue without any stack is just a normal value.
                // We are not interested in it.
                val @ Some(Token::AttrValue(..)) if stack.len() == 0 => {
                    return val;
                }

                // But if we have a stack,
                //   allocate a new string that concatenates each of the
                //   symbols and return a newly allocated symbol.
                Some(Token::AttrValue(last, span)) if stack.len() > 0 => {
                    stack.push(last);

                    let merged = stack
                        .iter()
                        .map(|frag| frag.lookup_str())
                        .collect::<String>()
                        .intern();

                    stack.clear();

                    return Some(Token::AttrValue(merged, span));
                }
                other => return other,
            }
        }
    })
}

#[cfg(test)]
mod test;
