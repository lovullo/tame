// Normalized (desugared) IR that is "near" the source code.
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

//! Desugaring of [`SugaredNir`] into the normalized [`PlainNir`] form.
//!
//! For more information on the flavors of NIR,
//!   see [the parent module](super).

use std::{error::Error, fmt::Display};

use crate::{
    diagnose::{AnnotatedSpan, Diagnostic},
    parse::{NoContext, ParseState, Transition, TransitionResult},
};

use super::{PlainNir, SugaredNir};

#[derive(Debug, PartialEq, Eq, Default)]
pub enum DesugarNir {
    #[default]
    Ready,
}

impl Display for DesugarNir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ready => write!(f, "ready for next token"),
        }
    }
}

impl ParseState for DesugarNir {
    type Token = SugaredNir;
    type Object = PlainNir;
    type Error = DesugarNirError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self::Super> {
        use SugaredNir::*;

        match tok {
            Primitive(nir) => Transition(self).ok(nir),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        self == &Self::Ready
    }
}

#[derive(Debug, PartialEq)]
pub enum DesugarNirError {}

impl Display for DesugarNirError {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // No errors yet.
        Ok(())
    }
}

impl Error for DesugarNirError {}

impl Diagnostic for DesugarNirError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        // No errors yet.
        vec![]
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::Parsed;

    type Sut = DesugarNir;

    // Given the simplicity,
    //   this just really ensures that the parser terminates.
    #[test]
    fn maps_plain_nir() {
        let toks = vec![SugaredNir::Primitive(PlainNir::Todo)];

        use Parsed::*;
        assert_eq!(
            Ok(vec![Object(PlainNir::Todo)]),
            Sut::parse(toks.into_iter()).collect(),
        );
    }
}
