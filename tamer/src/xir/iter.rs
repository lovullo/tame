// XIR stream iterators
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

//! XIR [`Token`] stream iterators.
//!
//! These iterators are provided for convenience in constructing token
//!   streams.
//!
//!   - [`elem_wrap`] wraps a token stream iterator as the body of an
//!       element of the given name.

use super::{CloseSpan, OpenSpan, QName, Token, TokenStream};
use crate::span::Span;
use std::iter::{once, Chain, Once};

/// Wrap an inner [`Token`] stream iterator in an element.
///
/// This produces a [`Token::Open`] before the `inner` iterator and a
///   [`Token::Close`] after `inner` completes.
/// The provided two-[`Span`] is associated,
///   respectively,
///   with the opening and closing tags.
///
/// The inner iterator will be in the proper context to produce attributes.
#[inline]
pub fn elem_wrap<S, I>(name: QName, span: S, inner: I) -> ElemWrapIter<I>
where
    S: Into<(Span, Span)>,
    I: TokenStream,
{
    let twospan: (Span, Span) = span.into();

    // TODO: These tokens won't be able to derive name spans,
    //   but this is only used by the linker at the time of writing for
    //   generated tokens,
    //     where the provided span is a dummy linker span anyway.
    ElemWrapIter::new(
        Token::Open(name, OpenSpan::without_name_span(twospan.0)),
        inner,
        Token::Close(Some(name), CloseSpan::without_name_span(twospan.1)),
    )
}

/// An iterator that wraps a [`Token`] iterator in an element.
///
/// This introduces an opening and closing token before and after the
///   iterator.
///
/// See [`elem_wrap`] to construct this iterator.
pub struct ElemWrapIter<I: TokenStream>(
    Chain<Chain<Once<Token>, I>, Once<Token>>,
);

impl<I: TokenStream> ElemWrapIter<I> {
    #[inline]
    fn new(open: Token, inner: I, close: Token) -> Self {
        Self(once(open).chain(inner).chain(once(close)))
    }
}

impl<I: TokenStream> Iterator for ElemWrapIter<I> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{convert::ExpectInto, span::dummy::DUMMY_SPAN, xir::Token};

    #[test]
    fn elem_wrap_iter() {
        let inner = vec![
            Token::Open("foo".unwrap_into(), DUMMY_SPAN.into()),
            Token::Close(None, DUMMY_SPAN.into()),
        ];

        let elem_name = "element".unwrap_into();
        let twospan = (
            DUMMY_SPAN.offset_add(1).unwrap(),
            DUMMY_SPAN.offset_add(2).unwrap(),
        );

        let result = elem_wrap(elem_name, twospan, inner.clone().into_iter());

        assert_eq!(
            result.collect::<Vec<_>>(),
            vec![
                Token::Open(elem_name, twospan.0.into()),
                inner[0].clone(),
                inner[1].clone(),
                Token::Close(Some(elem_name), twospan.1.into()),
            ]
        );
    }
}
