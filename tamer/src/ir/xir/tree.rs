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
//! Parsing is handled by [`ParserState::parse_token`].
//! An [`Iterator::scan`]-based parser can be constructed using
//!   [`parser_from`] or [`parse`].
//!
//! ```
//! use tamer::ir::xir::tree::{ParserState, parse, parser_from};
//!# use tamer::ir::xir::Token;
//!
//!# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
//! // Lazily parse a stream of XIR tokens as an iterator, yielding the next
//! // fully parsed object.  This may consume any number of tokens.
//! let parser = parser_from(token_stream);
//!
//!# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
//! // Consume a single token at a time, yielding either an incomplete state
//! // or the next parsed object.
//! let parser = token_stream.scan(ParserState::new(), parse);
//! ```
//!
//! `parser_from` Or `parse`?
//! =========================
//! [`parser_from`] is implemented in terms of [`parse`].
//! They have slightly different use cases and tradeoffs:
//!
//! [`parse`] yields a [`Result`] containing [`Parsed`],
//!   which _may_ contain a [`Parsed::Tree`],
//!   but it's more likely to contain [`Parsed::Incomplete`];
//!     this is because it typically takes multiple [`Token`]s to complete
//!     parsing within a given context.
//!
//! In return, though, you get some important guarantees:
//!
//!   1. [`parse`] consumes only a _single_ token; and
//!   2. It has a constant upper bound for execution time.
//!
//! This means that [`parse`] will never cause the system to hang---you
//!   are in complete control over how much progress parsing makes,
//!     and are free to stop and resume it at any time.
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
//!   2. On [`Parsed::Tree`],
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
//!   [`ParserState::parse_token`] returns a [`Result`],
//!     with parsing errors represented by this module's [`ParseError`].
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
//!   is implemented on [`ParserState`],
//!     which exists to encapsulate the [`Stack`].
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
//!
//! High-Resolution Attributes
//! --------------------------
//! XIRT supports [`Token::AttrValueFragment`],
//!   which can produce concatenated attribute values that retain the
//!   [`Span`] of each of their constituent parts.
//! This could allow,
//!   for example,
//!   creating an LSP server that would expose all of the TAME templates and
//!     source inputs used to generate an identifier.
//!
//! However,
//!   note that the XIR token stream introduced [`Token::AttrValueFragment`]
//!   primarily to eliminate the need for unnecessary [symbol
//!   lookups](crate::sym), copying, and heap allocations.
//! XIRT must perform extra heap allocations to process these fragments.
//! Once processed,
//!   an [`Attr::Extensible`] object is produced;
//!     the value is _not_ concatenated and interned,
//!       allowing it to be cheaply converted back into a [`Token`] stream
//!       for writing without unnecessary overhead.
//!
//! For more information,
//!   see [`AttrParts`].

use super::{AttrValue, QName, Text, Token, TokenResultStream, TokenStream};
use crate::span::Span;
use std::{fmt::Display, mem::take};

mod attr;
pub use attr::{Attr, AttrList, AttrParts, SimpleAttr};

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
    Text(Text, Span),

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

impl Into<Option<Text>> for Tree {
    #[inline]
    fn into(self) -> Option<Text> {
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

    /// Yield a reference to the inner value if it is a [`Text`],
    ///   otherwise [`None`].
    #[inline]
    pub fn as_text<'a>(&'a self) -> Option<&'a Text> {
        match self {
            Self::Text(text, _) => Some(text),
            _ => None,
        }
    }

    /// Yield the inner value if it is a [`Text`],
    ///   otherwise [`None`].
    #[inline]
    pub fn into_text(self) -> Option<Text> {
        self.into()
    }
}

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
    attrs: Option<AttrList>,
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
    pub fn attrs(&self) -> Option<&AttrList> {
        self.attrs.as_ref()
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
            attrs: None,
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
                return Err(ParseError::UnbalancedTag {
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
    fn consume_child_or_complete(self) -> Stack {
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
        self.element.attrs.replace(attr_list);
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
pub enum Stack {
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
    BuddingAttrList(Option<ElementStack>, AttrList),

    /// An attribute is awaiting its value,
    ///   after which it will be attached to an element.
    AttrName(Option<ElementStack>, AttrList, QName, Span),

    /// An attribute whose value is being constructed of value fragments,
    ///   after which it will be attached to an element.
    AttrFragments(Option<ElementStack>, AttrList, AttrParts),

    /// A completed [`AttrList`] without any [`Element`] context.
    IsolatedAttrList(AttrList),
}

impl Default for Stack {
    fn default() -> Self {
        Self::Empty
    }
}

impl Stack {
    /// Attempt to open a new element.
    ///
    /// If the stack is [`Self::Empty`],
    ///   then the element will be considered to be a root element,
    ///     meaning that it will be completed once it is closed.
    /// If the stack contains [`Self::BuddingElement`],
    ///   then a child element will be started,
    ///     which will be consumed by the parent one closed rather than
    ///     being considered a completed [`Element`].
    ///
    /// Attempting to open an element in any other context is an error.
    fn open_element(self, name: QName, span: Span) -> Result<Self> {
        let element = Element::open(name, span);

        Ok(Self::BuddingElement(ElementStack {
            element,
            pstack: match self {
                // Opening a root element (or lack of context).
                Self::Empty => Ok(None),

                // Open a child element.
                Self::BuddingElement(pstack) => Ok(Some(pstack.store())),

                // Opening a child element in attribute parsing context.
                // Automatically close the attributes despite a missing
                //   AttrEnd to accommodate non-reader XIR.
                Self::BuddingAttrList(Some(pstack), attr_list) => {
                    Ok(Some(pstack.consume_attrs(attr_list).store()))
                }

                // Attempting to open a child element in an isolated
                //   attribute parsing context means that `AttrEnd` was not
                //   provided.
                Self::BuddingAttrList(None, ..) => {
                    Err(ParseError::AttrNameExpected(Token::Open(name, span)))
                }

                _ => todo! {},
            }?,
        }))
    }

    /// Attempt to close an element.
    ///
    /// Elements can be either self-closing
    ///   (in which case `name` is [`None`]),
    ///   or have their own independent closing tags.
    /// If a name is provided,
    ///   then it _must_ match the name of the element currently being
    ///     processed---that is,
    ///       the tree must be _balanced_.
    /// An unbalanced tree results in a [`ParseError::UnbalancedTag`].
    fn close_element(self, name: Option<QName>, span: Span) -> Result<Self> {
        match self {
            Self::BuddingElement(stack) => stack
                .try_close(name, span)
                .map(ElementStack::consume_child_or_complete),

            // We can implicitly complete the attribute list if there's a
            //   missing `Token::AttrEnd`,
            //     which alleviates us from having to unnecessarily generate
            //     it outside of readers.
            Self::BuddingAttrList(Some(stack), attr_list) => stack
                .consume_attrs(attr_list)
                .try_close(name, span)
                .map(ElementStack::consume_child_or_complete),

            // See the error variant description for more information.
            Self::BuddingAttrList(None, ..) => {
                Err(ParseError::MissingIsolatedAttrEnd(span))
            }

            _ => todo! {},
        }
    }

    /// Begin an attribute on an element.
    ///
    /// An attribute begins with a [`QName`] representing its name.
    /// It will be attached to a parent element after being closed with a
    ///   value via [`Stack::close_attr`].
    fn open_attr(self, name: QName, span: Span) -> Result<Self> {
        Ok(match self {
            // Begin construction of an attribute list on a new element.
            Self::BuddingElement(ele_stack) => {
                Self::AttrName(Some(ele_stack), Default::default(), name, span)
            }

            // Continuation of attribute list.
            Self::BuddingAttrList(ele_stack, attr_list) => {
                Self::AttrName(ele_stack, attr_list, name, span)
            }

            _ => todo! {},
        })
    }

    /// Push a value fragment onto an attribute.
    ///
    /// This begins to build an attribute out of value fragments,
    ///   which is also completed by [`Stack::close_attr`].
    /// The attribute information that was previously held in
    ///   [`Stack::AttrName`] is moved into a [`AttrParts`] if that has not
    ///   already happend,
    ///     which is responsible for managing future fragments.
    ///
    /// This will cause heap allocation.
    fn push_attr_value(self, value: AttrValue, span: Span) -> Result<Self> {
        Ok(match self {
            Self::AttrName(ele_stack, attr_list, name, open_span) => {
                // This initial capacity can be adjusted after we observe
                // empirically what we most often parse, or we can make it
                // configurable.
                let mut parts = AttrParts::with_capacity(name, open_span, 2);

                parts.push_value(value, span);
                Self::AttrFragments(ele_stack, attr_list, parts)
            }

            Self::AttrFragments(ele_stack, attr_list, mut parts) => {
                parts.push_value(value, span);
                Self::AttrFragments(ele_stack, attr_list, parts)
            }

            _ => todo! {},
        })
    }

    /// Assigns a value to an opened attribute and attaches to the parent
    ///   element.
    ///
    /// If the attribute is composed of fragments ([`Stack::AttrFragments`]),
    ///   this serves as the final fragment and will yield an
    ///   [`Attr::Extensible`] with no further processing.
    fn close_attr(self, value: AttrValue, span: Span) -> Result<Self> {
        Ok(match self {
            Self::AttrName(ele_stack, attr_list, name, open_span) => {
                Self::BuddingAttrList(
                    ele_stack,
                    attr_list.push(Attr::new(name, value, (open_span, span))),
                )
            }
            Self::AttrFragments(ele_stack, attr_list, mut parts) => {
                parts.push_value(value, span);

                Stack::BuddingAttrList(
                    ele_stack,
                    attr_list.push(Attr::Extensible(parts)),
                )
            }
            _ => todo! {},
        })
    }

    /// End attribute parsing.
    ///
    /// If parsing occurs within an element context,
    ///   the accumulated [`AttrList`] will be attached to the budding
    ///   [`Element`].
    fn end_attrs(self) -> Result<Self> {
        Ok(match self {
            Self::BuddingAttrList(None, attr_list) => {
                Self::IsolatedAttrList(attr_list)
            }

            Self::BuddingAttrList(Some(ele_stack), attr_list) => {
                Self::BuddingElement(ele_stack.consume_attrs(attr_list))
            }

            _ => todo!("attr error"),
        })
    }

    /// Appends a text node as a child of an element.
    ///
    /// This is valid only for a [`Stack::BuddingElement`].
    fn text(self, value: Text, span: Span) -> Result<Self> {
        Ok(match self {
            Self::BuddingElement(mut ele) => {
                ele.element.children.push(Tree::Text(value, span));

                Self::BuddingElement(ele)
            }
            _ => todo! {},
        })
    }
}

/// State while parsing a XIR token stream into a tree.
///
/// [`ParserState`] is responsible only for dispatch and bookkeeping;
///   state transitions and stack manipulation are handled by the various
///   methods on [`Stack`].
///
/// This is a stack machine with the interface of a state machine.
/// The stack is encoded into the variants themselves
///   (which Rust will allocate on the stack),
///     which is sensible given that we always know exactly what and how
///     many arguments we need.
/// This gives us both the stack we want and type safety,
///   and has compile-time guarantees to ensure that we cannot produce a
///   stack that is not suitable for the computation at hand.
///
/// Note that this cannot be reasoned about in terms of a pushdown automaton
///   because there is no set language for the stack---it
///     contains arbitrary data holding the state of the current computation,
///       which is (theoretically, but not practically) unbounded by the
///       recursive nature of [`Element`].
///
/// This is very similar to the [XmlWriter](super::writer::XmlWriter),
///   except that a stack is needed to accumulate tokens until we can begin
///   emitting a tree.
#[derive(Debug, Default)]
pub struct ParserState {
    stack: Stack,
}

impl ParserState {
    /// Create state of a new parser that has not yet seen any input
    ///   tokens.
    ///
    /// _Consider using [`parser_from`] instead._
    ///
    /// Parsers using this state are suitable only for valid starting
    ///   contexts,
    ///     as defined in the [module-level documentation](self).
    pub fn new() -> Self {
        Self {
            stack: Default::default(),
        }
    }

    /// Initialize the state of the parser with the given [`Stack`].
    fn with(stack: Stack) -> Self {
        Self { stack }
    }

    /// Consume a single XIR [`Token`] and attempt to parse it within the
    ///   context of the current [`Stack`].
    ///
    /// Each call to this method represents a [state transition].
    /// Invalid state transitions represent either a semantic error
    ///   (e.g. unbalanced tags)
    ///   or unimplemented features that will be added as needed.
    ///
    /// This parser is not responsible for validating _syntax_,
    ///   since valid syntax is already implied by the existence of
    ///   [`Token`].
    /// But it does perform semantic analysis on that token stream.
    ///
    /// All heavy lifting is done by the various methods on [`Stack`].
    ///
    /// See the [module-level documentation](self) for more information on
    ///   the implementation of the parser.
    pub fn parse_token(&mut self, tok: Token) -> Result<Parsed> {
        let stack = take(&mut self.stack);

        match tok {
            Token::Open(name, span) => stack.open_element(name, span),
            Token::Close(name, span) => stack.close_element(name, span),
            Token::AttrName(name, span) => stack.open_attr(name, span),
            Token::AttrValueFragment(value, span) => {
                stack.push_attr_value(value, span)
            }
            Token::AttrValue(value, span) => stack.close_attr(value, span),
            Token::AttrEnd => stack.end_attrs(),
            Token::Text(value, span) => stack.text(value, span),

            Token::Comment(..) | Token::CData(..) | Token::Whitespace(..) => {
                Err(ParseError::Todo(tok, stack))
            }
        }
        .map(|new_stack| self.store_or_emit(new_stack))
    }

    /// Emit a completed object or store the current stack for further processing.
    fn store_or_emit(&mut self, new_stack: Stack) -> Parsed {
        match new_stack {
            Stack::ClosedElement(ele) => Parsed::Tree(Tree::Element(ele)),
            Stack::IsolatedAttrList(attr_list) => Parsed::AttrList(attr_list),

            _ => {
                self.stack = new_stack;
                Parsed::Incomplete
            }
        }
    }
}

/// Result of a XIR tree parsing operation.
pub type Result<T> = std::result::Result<T, ParseError>;

/// Parsing error from [`ParserState`].
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName, Span),
        close: (QName, Span),
    },

    /// [`Token::AttrEnd`] was expected in an isolated attribute context,
    ///   but [`Token::Close`] was encountered instead.
    ///
    /// This means that we encountered an element close while parsing
    ///   attributes in an isolated context,
    ///     which may happen if we're parsing only attributes as part
    ///     of a larger XIR stream.
    /// This should never happen if our XIR is well-formed _from a reader_,
    ///     but could happen if we generate XIR that we are not expecting to
    ///     subsequently parse.
    ///
    /// There is nothing the user can do to correct it;
    ///   this represents a bug in the compiler.
    MissingIsolatedAttrEnd(Span),

    /// An attribute was expected as the next [`Token`].
    AttrNameExpected(Token),

    /// Token stream ended before attribute parsing was complete.
    UnexpectedAttrEof,

    /// Not yet implemented.
    Todo(Token, Stack),
}

impl Display for ParseError {
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

            Self::MissingIsolatedAttrEnd(span) => {
                // Try to be helpful to developers and users alike.
                #[cfg(test)]
                let testmsg = "or a problem with your test case";
                #[cfg(not(test))]
                let testmsg = "and should be reported";

                write!(
                    f,
                    "internal error: expecting AttrEnd, found Close at {}; \
                       this represents a compiler bug {}",
                    span, testmsg
                )
            }

            Self::AttrNameExpected(tok) => {
                write!(f, "attribute name expected, found `{}`", tok)
            }

            // TODO: Perhaps we should include the last-encountered Span.
            Self::UnexpectedAttrEof => {
                write!(
                    f,
                    "unexpected end of input during isolated attribute parsing",
                )
            }

            Self::Todo(tok, stack) => {
                write!(
                    f,
                    "TODO: `{:?}` unrecognized.  The parser is not yet \
                        complete, so this could represent either a missing \
                        feature or a semantic error.  Stack: `{:?}`.",
                    tok, stack
                )
            }
        }
    }
}

/// Either a parsed [`Tree`] or an indication that more tokens are needed to
///   complete the active context.
///
/// This has the same structure as [`Option`],
///   but is its own type to avoid confusion as to what this type may mean
///   when deeply nested within other types
///     (e.g. `Option<Result<Parsed, ParserError>>` reads a bit better
///       than `Option<Result<Option<Tree>, ParserError>>`).
#[derive(Debug, Eq, PartialEq)]
pub enum Parsed {
    /// Parsing of an object is complete.
    Tree(Tree),

    /// Parsing of an isolated attribute list is complete.
    AttrList(AttrList),

    /// The parser needs more token data to emit an object
    ///   (the active context is not yet complete).
    Incomplete,
}

/// Wrap [`ParserState::parse_token`] result in [`Some`],
///   suitable for use with [`Iterator::scan`].
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
/// use tamer::ir::xir::tree::{ParserState, parse};
///# use tamer::ir::xir::Token;
///
///# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
/// // The above is equivalent to:
/// let parser = token_stream.scan(ParserState::new(), parse);
/// ```
pub fn parse(state: &mut ParserState, tok: Token) -> Option<Result<Parsed>> {
    Some(ParserState::parse_token(state, tok))
}

/// Produce a lazy parser from a given [`Token`] iterator,
///   yielding only when an object has been fully parsed.
///
/// Unlike [`parse`],
///   which is intended for use with [`Iterator::scan`],
///   this will yield /only/ when the underlying parser yields
///   [`Parsed::Tree`],
///     unwrapping the inner [`Tree`] value.
/// This interface is far more convenient,
///   but comes at the cost of not knowing how many parsing steps a single
///   [`Iterator::next`] call will take.
///
/// For more information on contexts,
///   and the parser in general,
///   see the [module-level documentation](self).
///
/// ```
/// use tamer::ir::xir::tree::parser_from;
///# use tamer::ir::xir::Token;
///
///# let token_stream: std::vec::IntoIter<Token> = vec![].into_iter();
/// // Lazily parse a stream of XIR tokens as an iterator.
/// let parser = parser_from(token_stream);
/// ```
pub fn parser_from(
    toks: impl TokenStream,
) -> impl Iterator<Item = Result<Tree>> {
    toks.scan(ParserState::new(), parse)
        .filter_map(|parsed| match parsed {
            Ok(Parsed::Tree(tree)) => Some(Ok(tree)),
            Ok(Parsed::Incomplete) => None,
            Err(x) => Some(Err(x)),

            // These make no sense in this context and should never occur.
            Ok(Parsed::AttrList(x)) => unreachable!(
                "unexpected yield by XIRT (Tree expected): {:?}",
                x
            ),
        })
}

/// Begin parsing in an isolated attribute context,
///   producing an [`AttrList`] that is detached from any [`Element`].
///
/// This is useful when you wish to consume a XIR stream and collect only
///   the attributes of an element.
/// If you wish to process an entire element,
///   use [`parser_from`] instead.
///
/// Parsing must begin at a [`Token::AttrName`] token.
///
/// This will consume tokens until reaching [`Token::AttrEnd`],
///   and so it is important that the XIR stream contain this delimiter;
///     this should be the case with all readers.
#[inline]
pub fn parse_attrs<'a>(
    toks: &mut impl TokenStream,
    dest: AttrList,
) -> Result<AttrList> {
    let mut state = ParserState::with(Stack::BuddingAttrList(None, dest));

    loop {
        match toks.next().and_then(|tok| parse(&mut state, tok)) {
            None => return Err(ParseError::UnexpectedAttrEof),
            Some(Err(err)) => return Err(err),
            Some(Ok(Parsed::Incomplete)) => continue,
            Some(Ok(Parsed::AttrList(attr_list))) => return Ok(attr_list),

            // These make no sense in this context and should never occur.
            Some(Ok(Parsed::Tree(x))) => unreachable!(
                "unexpected yield by XIRT (AttrList expected): {:?}",
                x
            ),
        }
    }
}

#[cfg(test)]
mod test;
