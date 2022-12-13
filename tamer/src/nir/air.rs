// Lower NIR into AIR
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

//! Lower [NIR](super) into [AIR](crate::asg::air).

use std::{error::Error, fmt::Display};

use crate::{asg::air::Air, diagnose::Diagnostic, parse::prelude::*};

use super::Nir;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum NirToAir {
    #[default]
    Ready,
}

impl Display for NirToAir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAir::*;

        match self {
            Ready => write!(f, "ready to lower NIR to AIR"),
        }
    }
}

impl ParseState for NirToAir {
    type Token = Nir;
    type Object = Air;
    type Error = NirToAirError;

    fn parse_token(
        self,
        _tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self::Super> {
        Transition(self).ok(Air::Todo)
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        true
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NirToAirError {
    Todo,
}

impl Display for NirToAirError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NirToAirError::*;

        match self {
            Todo => write!(f, "TODO"),
        }
    }
}

impl Error for NirToAirError {}

impl Diagnostic for NirToAirError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        // TODO
        vec![]
    }
}
