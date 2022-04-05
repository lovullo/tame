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
    parse::{NoContext, ParseState, Transition, TransitionResult},
    span::Span,
    xir::{QName, Token as XirToken},
};
use std::{error::Error, fmt::Display};

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
pub enum AttrParseState {
    Empty,
    Name(QName, Span),
}

impl ParseState for AttrParseState {
    type Token = XirToken;
    type Object = Attr;
    type Error = AttrParseError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        use AttrParseState::{Empty, Name};

        match (self, tok) {
            (Empty, XirToken::AttrName(name, span)) => {
                Transition(Name(name, span)).incomplete()
            }

            (Empty, invalid) => Transition(Empty).dead(invalid),

            (Name(name, nspan), XirToken::AttrValue(value, vspan)) => {
                Transition(Empty).ok(Attr::new(name, value, (nspan, vspan)))
            }

            (Name(name, nspan), invalid) => {
                // Restore state for error recovery.
                Transition(Name(name, nspan)).err(
                    AttrParseError::AttrValueExpected(name, nspan, invalid),
                )
            }
        }
    }

    #[inline]
    fn is_accepting(&self) -> bool {
        *self == Self::Empty
    }
}

impl Default for AttrParseState {
    fn default() -> Self {
        Self::Empty
    }
}

/// Attribute parsing error.
#[derive(Debug, PartialEq, Eq)]
pub enum AttrParseError {
    /// [`XirToken::AttrName`] was expected.
    AttrNameExpected(XirToken),

    /// [`XirToken::AttrValue`] was expected.
    AttrValueExpected(QName, Span, XirToken),
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
    use crate::{
        convert::ExpectInto,
        parse::{EmptyContext, ParseStatus, Parsed},
        sym::GlobalSymbolIntern,
    };

    const S: Span = crate::span::DUMMY_SPAN;
    const S2: Span = S.offset_add(1).unwrap();

    #[test]
    fn dead_if_first_token_is_non_attr() {
        let tok = XirToken::Open("foo".unwrap_into(), S);

        let sut = AttrParseState::default();

        // There is no state that we can transition to,
        //   and we're in an empty accepting state.
        assert_eq!(
            (
                // Make sure we're in the same state we started in so that
                //   we know we can accommodate recovery token(s).
                Transition(AttrParseState::default()),
                Ok(ParseStatus::Dead(tok.clone()))
            ),
            sut.parse_token(tok, &mut EmptyContext).into()
        );
    }

    #[test]
    fn parse_single_attr() {
        let attr = "attr".unwrap_into();
        let val = "val".intern();

        let toks = [XirToken::AttrName(attr, S), XirToken::AttrValue(val, S2)]
            .into_iter();

        let sut = AttrParseState::parse(toks);

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete,
                Parsed::Object(Attr::new(attr, val, (S, S2))),
            ]),
            sut.collect()
        );
    }

    #[test]
    fn parse_fails_when_attribute_value_missing_but_can_recover() {
        let attr = "bad".unwrap_into();

        let sut = AttrParseState::default();

        // This token indicates that we're expecting a value to come next in
        //   the token stream.
        let TransitionResult(Transition(sut), result) =
            sut.parse_token(XirToken::AttrName(attr, S), &mut EmptyContext);
        assert_eq!(result, Ok(ParseStatus::Incomplete));

        // But we provide something else unexpected.
        let TransitionResult(Transition(sut), result) =
            sut.parse_token(XirToken::Close(None, S2), &mut EmptyContext);
        assert_eq!(
            result,
            Err(AttrParseError::AttrValueExpected(
                attr,
                S,
                XirToken::Close(None, S2)
            ))
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
        let TransitionResult(Transition(sut), result) = sut
            .parse_token(XirToken::AttrValue(recover, S2), &mut EmptyContext);
        assert_eq!(
            result,
            Ok(ParseStatus::Object(Attr::new(attr, recover, (S, S2)))),
        );

        // Finally, we should now be in an accepting state.
        assert!(sut.is_accepting());
    }
}
