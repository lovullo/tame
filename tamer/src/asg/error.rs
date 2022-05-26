// Abstract semantic graph (ASG) errors
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

//! Errors resulting from operations on the ASG.

use std::{
    error::Error,
    fmt::{self, Display},
};

use super::TransitionError;

/// An error from an ASG operation.
#[derive(Debug, PartialEq)]
pub enum AsgError {
    /// An object could not change state in the manner requested.
    ObjectTransition(TransitionError),
    /// The node was not expected in the current context
    UnexpectedNode(String),
}

impl Display for AsgError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ObjectTransition(err) => Display::fmt(&err, fmt),
            Self::UnexpectedNode(msg) => {
                write!(fmt, "unexpected node: {}", msg)
            }
        }
    }
}

impl Error for AsgError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ObjectTransition(err) => err.source(),
            _ => None,
        }
    }
}

impl From<TransitionError> for AsgError {
    fn from(err: TransitionError) -> Self {
        Self::ObjectTransition(err)
    }
}