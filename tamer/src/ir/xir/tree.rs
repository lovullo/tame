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

//! XIR token stream parsed into a tree-based IR.
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
//!# type Ix = u16;
//!# let token_stream: std::vec::IntoIter<Token<Ix>> = vec![].into_iter();
//! // Lazily parse a stream of XIR tokens as an iterator, yielding the next
//! // fully parsed object.  This may consume any number of tokens.
//! let parser = parser_from(token_stream);
//!
//!# let token_stream: std::vec::IntoIter<Token<Ix>> = vec![].into_iter();
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
//!   which _may_ contain a [`Parsed::Object`],
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

use super::{AttrValue, QName, Token};
use crate::{span::Span, sym::SymbolIndexSize};
use std::{fmt::Display, mem::take};

/// A XIR tree.
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
pub enum Tree<Ix: SymbolIndexSize> {
    /// XML element.
    Element(Element<Ix>),
}

impl<Ix: SymbolIndexSize> Tree<Ix> {
    /// If the tree object is an [`Element`], retrieve it.
    pub fn element(self) -> Option<Element<Ix>> {
        match self {
            Self::Element(ele) => Some(ele),
        }
    }
}

/// List of attributes.
///
/// Attributes are ordered in XIR so that this IR will be suitable for code
///   formatters and linters.
///
/// This abstraction will allow us to manipulate the internal data so that
///   it is suitable for a particular task in the future
///     (e.g. O(1) lookups by attribute name).
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct AttrList<Ix: SymbolIndexSize> {
    attrs: Vec<Attr<Ix>>,
}

impl<Ix: SymbolIndexSize> AttrList<Ix> {
    /// Construct a new, empty attribute list.
    pub fn new() -> Self {
        Self { attrs: vec![] }
    }

    /// Add an attribute to the end of the attribute list.
    pub fn push(&mut self, attr: Attr<Ix>) {
        self.attrs.push(attr)
    }
}

impl<Ix: SymbolIndexSize> From<Vec<Attr<Ix>>> for AttrList<Ix> {
    fn from(attrs: Vec<Attr<Ix>>) -> Self {
        AttrList { attrs }
    }
}

impl<Ix: SymbolIndexSize, const N: usize> From<[Attr<Ix>; N]> for AttrList<Ix> {
    fn from(attrs: [Attr<Ix>; N]) -> Self {
        AttrList {
            attrs: attrs.into(),
        }
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
pub struct Element<Ix: SymbolIndexSize> {
    name: QName<Ix>,
    /// Zero or more attributes.
    attrs: AttrList<Ix>,
    /// Zero or more child nodes.
    children: Vec<Tree<Ix>>,
    /// Spans for opening and closing tags respectively.
    span: (Span, Span),
}

impl<Ix: SymbolIndexSize> Element<Ix> {
    /// Complete an element's span by setting its ending span.
    ///
    /// When elements are still budding (see [`Stack::BuddingElement`]),
    ///   the ending span is set to the starting span,
    ///   since the end is not yet known.
    fn close_span(self, close_span: Span) -> Self {
        Element {
            span: (self.span.0, close_span),
            ..self
        }
    }
}

/// Element attribute.
///
/// Attributes in [`Tree`] may stand alone without an element context to
///   permit selective parsing of XIR token streams.
///
/// TODO: This doesn't yet handle whitespace for alignment of attributes;
///         deferring this until it's actually needed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attr<Ix: SymbolIndexSize> {
    name: QName<Ix>,
    value: AttrValue<Ix>,
    /// Spans for the attribute name and value respectively.
    span: (Span, Span),
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
pub struct ElementStack<Ix: SymbolIndexSize> {
    element: Element<Ix>,

    /// Parent element stack to be restored once element has finished
    ///   processing.
    pstack: Option<Box<ElementStack<Ix>>>,
}

impl<Ix: SymbolIndexSize> ElementStack<Ix> {
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
        close_name: Option<QName<Ix>>,
        close_span: Span,
    ) -> Result<Self, Ix> {
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
pub enum Stack<Ix: SymbolIndexSize> {
    /// Empty stack.
    Empty,

    /// An [`Element`] that is still under construction.
    ///
    /// (This is a tree IR,
    ///    so here's a plant pun).
    BuddingElement(ElementStack<Ix>),

    /// A completed [`Element`].
    ///
    /// This should be consumed and emitted.
    ClosedElement(Element<Ix>),

    /// A standalone attribute is awaiting its value.
    ///
    /// Standalone attributes will be emitted as [`Attr`] _instead_ of being
    ///   bound to an [`Element`].
    /// This occurs when attributes are parsed in an independent context.
    AttrName(QName<Ix>, Span),

    /// An attribute is awaiting its value,
    ///   after which it will be attached to an element.
    EleAttrName(ElementStack<Ix>, QName<Ix>, Span),
}

impl<Ix: SymbolIndexSize> Default for Stack<Ix> {
    fn default() -> Self {
        Self::Empty
    }
}

impl<Ix: SymbolIndexSize> Stack<Ix> {
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
    fn open_element(self, name: QName<Ix>, span: Span) -> Result<Self, Ix> {
        let element = Element {
            name,
            attrs: AttrList::new(),
            children: vec![],
            span: (span, span), // We do not yet know where the span will end
        };

        Ok(Self::BuddingElement(ElementStack {
            element,
            pstack: match self {
                // Opening a root element (or lack of context)
                Self::Empty => Ok(None),
                // Open a child element
                Self::BuddingElement(pstack) => Ok(Some(pstack.store())),

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
    fn close_element(
        self,
        name: Option<QName<Ix>>,
        span: Span,
    ) -> Result<Self, Ix> {
        match self {
            Self::BuddingElement(stack) => stack
                .try_close(name, span)
                .and_then(|ElementStack { element, pstack }| match pstack {
                    Some(mut parent_stack) => {
                        parent_stack
                            .element
                            .children
                            .push(Tree::Element(element));
                        Ok(Stack::BuddingElement(*parent_stack))
                    }

                    None => Ok(Stack::ClosedElement(element)),
                }),

            _ => todo! {},
        }
    }

    /// Begin an attribute on an element.
    ///
    /// An attribute begins with a [`QName`] representing its name.
    /// It will be attached to a parent element after being closed with a
    ///   value via [`Stack::close_attr`].
    fn open_attr(self, name: QName<Ix>, span: Span) -> Result<Self, Ix> {
        Ok(match self {
            Self::BuddingElement(ele_stack) => {
                Self::EleAttrName(ele_stack, name, span)
            }

            _ => todo! {},
        })
    }

    /// Assigns a value to an opened attribute and attaches to the parent
    ///   element.
    fn close_attr(self, value: AttrValue<Ix>, span: Span) -> Result<Self, Ix> {
        Ok(match self {
            Self::EleAttrName(mut ele_stack, name, open_span) => {
                // TODO: Demeter
                ele_stack.element.attrs.push(Attr {
                    name,
                    value,
                    span: (open_span, span),
                });

                Stack::BuddingElement(ele_stack)
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
pub struct ParserState<Ix: SymbolIndexSize> {
    stack: Stack<Ix>,
}

impl<Ix: SymbolIndexSize> ParserState<Ix> {
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
    pub fn parse_token(&mut self, tok: Token<Ix>) -> Result<Parsed<Ix>, Ix> {
        let stack = take(&mut self.stack);

        match tok {
            Token::Open(name, span) => stack.open_element(name, span),
            Token::Close(name, span) => stack.close_element(name, span),
            Token::AttrName(name, span) => stack.open_attr(name, span),
            Token::AttrValue(value, span) => stack.close_attr(value, span),

            todo => Err(ParseError::Todo(todo, stack)),
        }
        .map(|new_stack| self.store_or_emit(new_stack))
    }

    /// Emit a completed object or store the current stack for further processing.
    fn store_or_emit(&mut self, new_stack: Stack<Ix>) -> Parsed<Ix> {
        match new_stack {
            Stack::ClosedElement(ele) => Parsed::Object(Tree::Element(ele)),

            _ => {
                self.stack = new_stack;
                Parsed::Incomplete
            }
        }
    }
}

/// Result of a XIR tree parsing operation.
pub type Result<T, Ix> = std::result::Result<T, ParseError<Ix>>;

/// Parsing error from [`ParserState`].
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError<Ix: SymbolIndexSize> {
    /// The closing tag does not match the opening tag at the same level of
    ///   nesting.
    UnbalancedTag {
        open: (QName<Ix>, Span),
        close: (QName<Ix>, Span),
    },

    /// Not yet implemented.
    Todo(Token<Ix>, Stack<Ix>),
}

impl<Ix: SymbolIndexSize> Display for ParseError<Ix> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // TODO: not a useful error because of symbols and missing span information
            Self::UnbalancedTag {
                open: (open_name, _),
                close: (close_name, _),
            } => {
                write!(
                    f,
                    "expected closing tag `{:?}`, found `{:?}`",
                    open_name, close_name,
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
///     (e.g. `Option<Result<Parsed<Ix>, ParserError>>` reads a bit better
///       than `Option<Result<Option<Tree<Ix>>, ParserError>>`).
#[derive(Debug, Eq, PartialEq)]
pub enum Parsed<Ix: SymbolIndexSize> {
    /// Parsing of an object is complete.
    Object(Tree<Ix>),

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
///# type Ix = u16;
///# let token_stream: std::vec::IntoIter<Token<Ix>> = vec![].into_iter();
/// // The above is equivalent to:
/// let parser = token_stream.scan(ParserState::new(), parse);
/// ```
pub fn parse<Ix: SymbolIndexSize>(
    state: &mut ParserState<Ix>,
    tok: Token<Ix>,
) -> Option<Result<Parsed<Ix>, Ix>> {
    Some(ParserState::parse_token(state, tok))
}

/// Produce a lazy parser from a given [`Token`] iterator,
///   yielding only when an object has been fully parsed.
///
/// Unlike [`parse`],
///   which is intended for use with [`Iterator::scan`],
///   this will yield /only/ when the underlying parser yields
///   [`Parsed::Object`],
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
///# type Ix = u16;
///# let token_stream: std::vec::IntoIter<Token<Ix>> = vec![].into_iter();
/// // Lazily parse a stream of XIR tokens as an iterator.
/// let parser = parser_from(token_stream);
/// ```
pub fn parser_from<Ix: SymbolIndexSize>(
    toks: impl Iterator<Item = Token<Ix>>,
) -> impl Iterator<Item = Result<Tree<Ix>, Ix>> {
    toks.scan(ParserState::new(), parse)
        .filter_map(|parsed| match parsed {
            Ok(Parsed::Object(tree)) => Some(Ok(tree)),
            Ok(Parsed::Incomplete) => None,
            Err(x) => Some(Err(x)),
        })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::convert::ExpectInto;
    use crate::sym::GlobalSymbolIntern;

    type Ix = u16;

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case, 1".intern());
        static ref S2: Span =
            Span::from_byte_interval((0, 0), "test case, 2".intern());
        static ref S3: Span =
            Span::from_byte_interval((0, 0), "test case, 3".intern());
    }

    mod tree {
        use super::*;

        #[test]
        fn element_from_tree() {
            let ele = Element::<Ix> {
                name: "foo".unwrap_into(),
                attrs: AttrList::new(),
                children: vec![],
                span: (*S, *S2),
            };

            let tree = Tree::Element(ele.clone());

            assert_eq!(Some(ele), tree.element());
        }
    }

    #[test]
    fn empty_element_self_close_from_toks() {
        let name = ("ns", "elem").unwrap_into();

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::<Ix>::Close(None, *S2),
        ]);

        let expected = Element {
            name,
            attrs: AttrList::new(),
            children: vec![],
            span: (*S, *S2),
        };

        let mut sut = toks.scan(ParserState::new(), parse);

        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
        assert_eq!(
            sut.next(),
            Some(Ok(Parsed::Object(Tree::Element(expected))))
        );
        assert_eq!(sut.next(), None);
    }

    // Same as above test, but with balanced closing instead of self
    // closing.
    #[test]
    fn empty_element_balanced_close_from_toks() {
        let name = ("ns", "openclose").unwrap_into();

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::<Ix>::Close(Some(name), *S2),
        ]);

        let expected = Element {
            name,
            attrs: AttrList::new(),
            children: vec![],
            span: (*S, *S2),
        };

        let mut sut = toks.scan(ParserState::new(), parse);

        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
        assert_eq!(
            sut.next(),
            Some(Ok(Parsed::Object(Tree::Element(expected))))
        );
        assert_eq!(sut.next(), None);
    }

    // Unbalanced should result in error.  This does not test what happens
    // _after_ the error.
    #[test]
    fn empty_element_unbalanced_close_from_toks() {
        let open_name = "open".unwrap_into();
        let close_name = "unbalanced_name".unwrap_into();

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(open_name, *S),
            Token::<Ix>::Close(Some(close_name), *S2),
        ]);

        let mut sut = toks.scan(ParserState::new(), parse);

        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
        assert_eq!(
            sut.next(),
            Some(Err(ParseError::UnbalancedTag {
                open: (open_name, *S),
                close: (close_name, *S2),
            }))
        );

        // TODO: We need to figure out how to best implement recovery before
        // continuing with this design.
    }

    #[test]
    fn empty_element_with_attrs_from_toks() {
        let name = ("ns", "elem").unwrap_into();
        let attr1 = "a".unwrap_into();
        let attr2 = "b".unwrap_into();
        let val1 = AttrValue::Escaped("val1".intern());
        let val2 = AttrValue::Escaped("val2".intern());

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::AttrName(attr1, *S),
            Token::AttrValue(val1, *S2),
            Token::AttrName(attr2, *S),
            Token::AttrValue(val2, *S2),
            Token::Close(None, *S2),
        ]);

        let expected = Element {
            name,
            attrs: AttrList::from(vec![
                Attr {
                    name: attr1,
                    value: val1,
                    span: (*S, *S2),
                },
                Attr {
                    name: attr2,
                    value: val2,
                    span: (*S, *S2),
                },
            ]),
            children: vec![],
            span: (*S, *S2),
        };

        let mut sut = toks.scan(ParserState::new(), parse);

        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // Open
        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrName
        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrValue
        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrName
        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete))); // AttrValue
        assert_eq!(
            sut.next(),
            Some(Ok(Parsed::Object(Tree::Element(expected))))
        );
        assert_eq!(sut.next(), None);
    }

    #[test]
    fn element_with_empty_sibling_children() {
        let parent = "parent".unwrap_into();
        let childa = "childa".unwrap_into();
        let childb = "childb".unwrap_into();

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(parent, *S),
            Token::<Ix>::Open(childa, *S),
            Token::<Ix>::Close(None, *S2),
            Token::<Ix>::Open(childb, *S),
            Token::<Ix>::Close(None, *S2),
            Token::<Ix>::Close(Some(parent), *S2),
        ]);

        let expected = Element {
            name: parent,
            attrs: AttrList::new(),
            children: vec![
                Tree::Element(Element {
                    name: childa,
                    attrs: AttrList::new(),
                    children: vec![],
                    span: (*S, *S2),
                }),
                Tree::Element(Element {
                    name: childb,
                    attrs: AttrList::new(),
                    children: vec![],
                    span: (*S, *S2),
                }),
            ],
            span: (*S, *S2),
        };

        let mut sut = parser_from(toks);

        assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
        assert_eq!(sut.next(), None);
    }

    // Ensures that attributes do not cause the parent context to be lost.
    #[test]
    fn element_with_child_with_attributes() {
        let parent = "parent".unwrap_into();
        let child = "child".unwrap_into();
        let attr = "attr".unwrap_into();
        let value = AttrValue::Escaped("attr value".into());

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(parent, *S),
            Token::<Ix>::Open(child, *S),
            Token::<Ix>::AttrName(attr, *S),
            Token::<Ix>::AttrValue(value, *S2),
            Token::<Ix>::Close(None, *S3),
            Token::<Ix>::Close(Some(parent), *S3),
        ]);

        let expected = Element {
            name: parent,
            attrs: AttrList::new(),
            children: vec![Tree::Element(Element {
                name: child,
                attrs: AttrList::from([Attr {
                    name: attr,
                    value,
                    span: (*S, *S2),
                }]),
                children: vec![],
                span: (*S, *S3),
            })],
            span: (*S, *S3),
        };

        let mut sut = parser_from(toks);

        assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
        assert_eq!(sut.next(), None);
    }

    #[test]
    fn parser_from_filters_incomplete() {
        let name = ("ns", "elem").unwrap_into();
        let attr = "a".unwrap_into();
        let val = AttrValue::Escaped("val1".intern());

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::AttrName(attr, *S),
            Token::AttrValue(val, *S2),
            Token::Close(None, *S2),
        ]);

        let expected = Element {
            name,
            attrs: AttrList::from([Attr {
                name: attr,
                value: val,
                span: (*S, *S2),
            }]),
            children: vec![],
            span: (*S, *S2),
        };

        let mut sut = parser_from(toks);

        // Unlike the previous tests, we should filter out all the
        // `Parsed::Incomplete` and yield only when we have a fully parsed
        // object.
        assert_eq!(sut.next(), Some(Ok(Tree::Element(expected))));
        assert_eq!(sut.next(), None);
    }
}
