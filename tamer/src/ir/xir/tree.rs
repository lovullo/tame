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
//! // Lazily parse a stream of XIR tokens as an iterator.
//! let parser = parser_from(token_stream);
//!#
//!# let token_stream: std::vec::IntoIter<Token<Ix>> = vec![].into_iter();
//! // The above is equivalent to:
//! let parser = token_stream.scan(ParserState::new(), parse);
//! ```
//!
//! Cost of Parsing
//! ===============
//! While [`Tree`] is often much easier to work with than a stream of
//!   [`Token`],
//!     there are notable downsides:
//!
//!   - The context in which parsing began
//!       (see /Parser Implementation/ below)
//!       must complete before /any/ token is emitted.
//!     If parsing begins at the root element,
//!       this means that the /entire XML document/ must be loaded into
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
//! Parsing operates on /context/.
//! At present,
//!   there are two principal /starting contexts/:
//!
//!   1. Element parsing; and
//!   2. Independent attribute parsing.
//!
//! The first mode is the most familiar---it
//!   begins parsing at an opening tag ([`Token::Open`]) and completes
//!   parsing at a /matching/ [`Token::Close`] or [`Token::SelfClose`].
//! All attributes and child nodes encountered during parsing of an element
//!   will automatically be added to the appropriate element,
//!     recursively.
//!
//!
//! If attribute parsing begins /outside/ of an [`Element`] context,
//!   it is assumed that the caller is interested only in parsing the
//!   attributes of an element but is not interested in processing the
//!   element itself,
//!     in which case [`Attr`] will be emitted as those attributes are
//!     encountered.
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
use std::mem::take;

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
#[derive(Debug, Eq, PartialEq)]
pub enum Tree<Ix: SymbolIndexSize> {
    /// XML element.
    Element(Element<Ix>),

    /// Standalone XML attribute.
    ///
    /// An attribute is standalone if it is parsed outside the context of an
    ///   element.
    /// This may happen,
    ///   for example,
    ///   if the attributes of a token stream need further processing,
    ///     but the surrounding context would not benefit from such parsing.
    Attr(Attr<Ix>),
}

/// Element node.
///
/// This represents an [XML element] beginning with an opening tag that is
///   either self-closing or ending with a balanced closing tag.
/// The two spans together represent the span of the entire element with all
///   its constituents.
///
/// [XML element]: https://www.w3.org/TR/REC-xml/#sec-starttags
#[derive(Debug, Eq, PartialEq)]
pub struct Element<Ix: SymbolIndexSize> {
    name: QName<Ix>,
    /// Zero or more attributes.
    attrs: Vec<Attr<Ix>>,
    /// Zero or more child nodes.
    children: Vec<Tree<Ix>>,
    /// Spans for opening and closing tags respectively.
    span: (Span, Span),
}

/// Element attribute.
///
/// Attributes in [`Tree`] may stand alone without an element context to
///   permit selective parsing of XIR token streams.
///
/// TODO: This doesn't yet handle whitespace for alignment of attributes;
///         deferring this until it's actually needed.
#[derive(Debug, Eq, PartialEq)]
pub struct Attr<Ix: SymbolIndexSize> {
    name: QName<Ix>,
    value: AttrValue<Ix>,
    /// Spans for the attribute name and value respectively.
    span: (Span, Span),
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
#[derive(Debug)]
pub enum Stack<Ix: SymbolIndexSize> {
    /// Empty stack.
    Empty,

    /// An [`Element`] that is still under construction.
    ///
    /// (This is a tree IR,
    ///    so here's a plant pun).
    BuddingElement(Element<Ix>),

    /// A standalone attribute is awaiting its value.
    ///
    /// Standalone attributes will be emitted as [`Attr`] _instead_ of being
    ///   bound to an [`Element`].
    /// This occurs when attributes are parsed in an independent context.
    AttrName(QName<Ix>, Span),

    /// An attribute is awaiting its value,
    ///   after which it will be attached to an element.
    EleAttrName(Element<Ix>, QName<Ix>, Span),
}

impl<Ix: SymbolIndexSize> Default for Stack<Ix> {
    fn default() -> Self {
        Self::Empty
    }
}

/// State while parsing a XIR token stream into a tree.
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
    ///   context of the current [`ParserState`].
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
    /// See the [module-level documentation](self) for more information on
    ///   the implementation of the parser.
    pub fn parse_token(
        &mut self,
        tok: Token<Ix>,
    ) -> Result<Parsed<Ix>, ParseError> {
        match (tok, take(&mut self.stack)) {
            (Token::Open(name, span), Stack::Empty) => {
                self.stack = Stack::BuddingElement(Element {
                    name,
                    attrs: vec![],
                    children: vec![],
                    span: (span, span),
                });
                Ok(Parsed::Incomplete)
            }

            (Token::SelfClose(span), Stack::BuddingElement(ele)) => {
                Ok(Parsed::Object(Tree::Element(Element {
                    span: (ele.span.0, span),
                    ..ele
                })))
            }

            (Token::AttrName(name, span), Stack::Empty) => {
                self.stack = Stack::AttrName(name, span);
                Ok(Parsed::Incomplete)
            }

            (Token::AttrName(name, span), Stack::BuddingElement(ele)) => {
                self.stack = Stack::EleAttrName(ele, name, span);
                Ok(Parsed::Incomplete)
            }

            (Token::AttrValue(value, span), Stack::AttrName(name, sname)) => {
                Ok(Parsed::Object(Tree::Attr(Attr {
                    name,
                    value,
                    span: (sname, span),
                })))
            }

            (
                Token::AttrValue(value, span),
                Stack::EleAttrName(mut ele, name, sname),
            ) => {
                ele.attrs.push(Attr {
                    name,
                    value,
                    span: (sname, span),
                });
                self.stack = Stack::BuddingElement(ele);

                Ok(Parsed::Incomplete)
            }

            _ => todo! {},
        }
    }
}

/// Parsing error from [`ParserState`].
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {}

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
/// _Consider using [`parser_from`] instead,_
///   as it encapsulates [`ParserState`].
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
) -> Option<Result<Parsed<Ix>, ParseError>> {
    Some(ParserState::parse_token(state, tok))
}

/// Produce a lazy parser from a given [`Token`] iterator.
///
/// This produces a parser using [`parse`],
///   which uses [`Iterator::scan`].
/// This will lazily parse a stream of [`Token`],
///   emitting [`Parsed::Object`] once enough data has been gathered for the
///   running context.
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
) -> impl Iterator<Item = Result<Parsed<Ix>, ParseError>> {
    toks.scan(ParserState::new(), parse)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::GlobalSymbolIntern;
    use std::convert::TryInto;

    type Ix = u16;

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case, 1".intern());
        static ref S2: Span =
            Span::from_byte_interval((0, 0), "test case, 2".intern());
    }

    #[test]
    fn attr_from_toks() {
        let name: QName<Ix> = "attr".try_into().unwrap();
        let value = AttrValue::Escaped("value".intern());

        let toks = std::array::IntoIter::new([
            Token::<Ix>::AttrName(name, *S),
            Token::AttrValue(value, *S2),
        ]);

        let expected = Attr {
            name,
            value,
            span: (*S, *S2),
        };

        let mut sut = toks.scan(ParserState::new(), parse);

        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));
        assert_eq!(sut.next(), Some(Ok(Parsed::Object(Tree::Attr(expected)))));
        assert_eq!(sut.next(), None);
    }

    #[test]
    fn empty_element_from_toks() {
        let name = ("ns", "elem").try_into().unwrap();

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::<Ix>::SelfClose(*S2),
        ]);

        let expected = Element {
            name,
            attrs: vec![],
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

    #[test]
    fn empty_element_with_attrs_from_toks() {
        let name = ("ns", "elem").try_into().unwrap();
        let attr1 = "a".try_into().unwrap();
        let attr2 = "b".try_into().unwrap();
        let val1 = AttrValue::Escaped("val1".intern());
        let val2 = AttrValue::Escaped("val2".intern());

        let toks = std::array::IntoIter::new([
            Token::<Ix>::Open(name, *S),
            Token::AttrName(attr1, *S),
            Token::AttrValue(val1, *S2),
            Token::AttrName(attr2, *S),
            Token::AttrValue(val2, *S2),
            Token::SelfClose(*S2),
        ]);

        let expected = Element {
            name,
            attrs: vec![
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
            ],
            children: vec![],
            span: (*S, *S2),
        };

        let mut sut = parser_from(toks);

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
}
