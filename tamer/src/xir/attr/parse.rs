// XIRT attribute parsers
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    parse::{NoContext, ParseState, Token, Transition, TransitionResult},
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

impl Display for AttrParseState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AttrParseState::*;

        match self {
            Empty => write!(f, "expecting an attribute"),
            Name(name, _) => {
                write!(f, "expecting an attribute value for {name}")
            }
        }
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
            Self::AttrNameExpected(_) => {
                write!(f, "attribute name expected")
            }

            Self::AttrValueExpected(name, _span, _tok) => {
                write!(f, "expected value for `@{name}`",)
            }
        }
    }
}

impl Error for AttrParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl Diagnostic for AttrParseError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::AttrNameExpected(tok) => tok.span().mark_error().into(),

            Self::AttrValueExpected(_name, span, _tok) => {
                span.mark_error().into()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::{
        convert::ExpectInto,
        parse::{ParseError, Parsed},
        sym::GlobalSymbolIntern,
        xir::test::{close_empty, open},
    };

    const S: Span = crate::span::DUMMY_SPAN;
    const S2: Span = S.offset_add(1).unwrap();

    #[test]
    fn dead_if_first_token_is_non_attr() {
        let tok = open("foo", S);

        let mut sut = AttrParseState::parse(vec![tok.clone()].into_iter());

        // There is no state that we can transition to,
        //   and we're in an empty accepting state.
        assert_matches!(
            sut.next(),
            Some(Err(ParseError::UnexpectedToken(given, _))) if given == tok,
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
        let recover = "value".intern();

        let toks = vec![
            XirToken::AttrName(attr, S),
            close_empty(S2),
            XirToken::AttrValue(recover, S2),
        ];

        let mut sut = AttrParseState::parse(toks.into_iter());

        // This token indicates that we're expecting a value to come next in
        //   the token stream.
        assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)));

        // But we provide something else unexpected.
        assert_eq!(
            sut.next(),
            Some(Err(ParseError::StateError(
                AttrParseError::AttrValueExpected(attr, S, close_empty(S2))
            )))
        );

        // We should not be in an accepting state,
        //   given that we haven't finished parsing the attribute.
        let (mut sut, _) =
            sut.finalize().expect_err("unexpected accepting state");

        // Despite this error,
        //   we should remain in a state that permits recovery should a
        //   proper token be substituted.
        // Rather than checking for that state,
        //   let's actually attempt a recovery.
        assert_eq!(
            sut.next(),
            Some(Ok(Parsed::Object(Attr::new(attr, recover, (S, S2))))),
        );

        // Finally, we should now be in an accepting state.
        sut.finalize().expect("expected accepting state");
    }
}
