// XIR stream iterators
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

//! XIR [`Token`] stream iterators.

use super::Token;
use std::iter::{once, Chain, Once};

/// An iterator that wraps a [`Token`] iterator in an element.
///
/// This introduces an opening and closing token before and after the
///   iterator.
pub struct ElemWrapIter<I: Iterator<Item = Token>>(
    Chain<Chain<Once<Token>, I>, Once<Token>>,
);

impl<I: Iterator<Item = Token>> ElemWrapIter<I> {
    #[inline]
    pub fn new(open: Token, inner: I, close: Token) -> Self {
        Self(once(open).chain(inner).chain(once(close)))
    }
}

impl<I: Iterator<Item = Token>> Iterator for ElemWrapIter<I> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
