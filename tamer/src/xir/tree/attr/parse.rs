// XIRT attribute parsers
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

//! Parse XIR attribute [`TokenStream`][super::super::TokenStream]s.

use crate::{
    span::Span,
    xir::{
        tree::parse::{Parsed, TokenStreamState, TokenStreamStateResult},
        QName, Token,
    },
};
use std::{error::Error, fmt::Display, mem::take};

use super::Attr;

/// Attribute parser DFA.
///
/// While this parser does store the most recently encountered [`QName`]
///   and [`Span`],
///     these data are used only for emitting data about the accepted state;
///       they do not influence the automaton's state transitions.
/// The actual parsing operation is therefore a FSM,
///   not a PDA.
#[derive(Debug, Eq, PartialEq)]
pub enum AttrParserState {
    Empty,
    Name(QName, Span),
}

impl TokenStreamState for AttrParserState {
    type Object = Attr;
    type Error = AttrParseError;

    fn parse_token(&mut self, tok: Token) -> TokenStreamStateResult<Self> {
        use AttrParserState::*;

        Some(match (take(self), tok) {
            (Empty, Token::AttrEnd(_)) => return None,

            (Empty, Token::AttrName(name, span)) => {
                *self = Name(name, span);
                Ok(Parsed::Incomplete)
            }

            (Empty, invalid) => Err(AttrParseError::AttrNameExpected(invalid)),

            (Name(name, nspan), Token::AttrValue(value, vspan)) => {
                Ok(Parsed::Object(Attr::new(name, value, (nspan, vspan))))
            }

            (Name(name, nspan), invalid) => {
                // Restore state for error recovery.
                *self = Name(name, nspan);
                Err(AttrParseError::AttrValueExpected(name, nspan, invalid))
            }
        })
    }

    #[inline]
    fn is_accepting(&self) -> bool {
        *self == Self::Empty
    }
}

impl Default for AttrParserState {
    fn default() -> Self {
        Self::Empty
    }
}

/// Attribute parsing error.
#[derive(Debug, PartialEq)]
pub enum AttrParseError {
    /// [`Token::AttrName`] was expected.
    AttrNameExpected(Token),

    /// [`Token::AttrValue`] was expected.
    AttrValueExpected(QName, Span, Token),
}

impl Display for AttrParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AttrNameExpected(tok) => {
                write!(f, "attribute name expected, found {}", tok)
            }

            Self::AttrValueExpected(name, span, tok) => {
                write!(
                    f,
                    "expected value for `@{}` at {}, found {}",
                    name, span, tok
                )
            }
        }
    }
}

impl Error for AttrParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{convert::ExpectInto, sym::GlobalSymbolIntern};

    // TODO: Just make these const
    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case, 1".intern());
        static ref S2: Span =
            Span::from_byte_interval((0, 0), "test case, 2".intern());
        static ref S3: Span =
            Span::from_byte_interval((0, 0), "test case, 3".intern());
    }

    #[test]
    fn fails_if_first_token_is_non_attr() {
        let tok = Token::Open("foo".unwrap_into(), *S);

        let mut sut = AttrParserState::default();

        // Fail immediately.
        assert_eq!(
            Some(Err(AttrParseError::AttrNameExpected(tok.clone()))),
            sut.parse_token(tok)
        );

        // Let's just make sure we're in the same state we started in so
        //   that we know we can accommodate recovery token(s).
        assert_eq!(sut, AttrParserState::default());
    }

    #[test]
    fn parse_single_attr() {
        let attr = "attr".unwrap_into();
        let val = "val".intern();

        let mut sut = AttrParserState::default();
        let expected = Attr::new(attr, val, (*S, *S2));

        // First token represents the name,
        //   and so we are awaiting a value.
        assert_eq!(
            sut.parse_token(Token::AttrName(attr, *S)),
            Some(Ok(Parsed::Incomplete))
        );

        // Once we have a value,
        //   an Attr can be emitted.
        assert_eq!(
            sut.parse_token(Token::AttrValue(val, *S2)),
            Some(Ok(Parsed::Object(expected)))
        );
    }

    #[test]
    fn parse_fails_when_attribute_value_missing_but_can_recover() {
        let attr = "bad".unwrap_into();

        let mut sut = AttrParserState::default();

        // This token indicates that we're expecting a value to come next in
        //   the token stream.
        assert_eq!(
            sut.parse_token(Token::AttrName(attr, *S)),
            Some(Ok(Parsed::Incomplete))
        );

        // But we provide something else unexpected.
        assert_eq!(
            sut.parse_token(Token::AttrEnd(*S2)),
            Some(Err(AttrParseError::AttrValueExpected(
                attr,
                *S,
                Token::AttrEnd(*S2)
            )))
        );

        // We should not be in an accepting state,
        //   given that we haven't finished parsing the attribute.
        assert!(!sut.is_accepting());

        // Despite this error,
        //   we should remain in a state that permits recovery should a
        //   proper token be substituted.
        // Rather than checking for that state,
        //   let's actually attempt a recovery.
        let recover = "value".intern();
        let expected = Attr::new(attr, recover, (*S, *S2));
        assert_eq!(
            sut.parse_token(Token::AttrValue(recover, *S2)),
            Some(Ok(Parsed::Object(expected)))
        );

        // Finally, we should now be in an accepting state.
        assert!(sut.is_accepting());
    }

    #[test]
    fn yields_none_on_attr_end() {
        let mut sut = AttrParserState::default();

        assert_eq!(sut.parse_token(Token::AttrEnd(*S)), None);
        assert!(sut.is_accepting());
    }
}
