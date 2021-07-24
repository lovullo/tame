// `quick-xml` wrappers for TAME.
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

use std::fmt::Display;

/// Wrapped error type.
pub type InnerXmlError = quick_xml::Error;

/// Thin wrapper around [`quick_xml::Error`] to implement [`PartialEq`].
///
/// This will always yield `false`,
///   but allows us to derive the trait on types using [`Error`];
///     otherwise, this madness propagates indefinitely.
#[derive(Debug)]
pub struct Error(pub InnerXmlError);

impl PartialEq for Error {
    /// [`quick_xml::Error`] does not implement [`PartialEq`] and so this
    ///   will always yield `false`.
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl From<InnerXmlError> for Error {
    fn from(e: InnerXmlError) -> Self {
        Self(e)
    }
}

impl Into<InnerXmlError> for Error {
    fn into(self) -> InnerXmlError {
        self.0
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        // NB: If we eventually use `source` to display a hierarchy of
        //   errors, then we likely do not want the duplication here.
        self.0.fmt(fmt)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.0)
    }
}
