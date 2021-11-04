// XIR predicates
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

//! Functional predicates for XIR token streams.

use super::{QName, Token};

#[inline]
pub fn not<T>(mut f: impl FnMut(&T) -> bool) -> impl FnMut(&T) -> bool {
    move |x| !f(x)
}

#[inline]
pub fn open(name: QName) -> impl FnMut(&Token) -> bool {
    move |tok| matches!(tok, Token::Open(tokname, _) if *tokname == name)
}

#[inline]
pub fn close(name: QName) -> impl FnMut(&Token) -> bool {
    move |tok| matches!(tok, Token::Close(Some(tokname), _) if *tokname == name)
}
