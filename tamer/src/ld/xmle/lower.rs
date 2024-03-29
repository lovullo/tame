// ASG lowering into xmle sections
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

//! Lowering of the [ASG](crate::asg) into `xmle` [`XmleSections`].
//!
//! See the [parent module](super) for more information.

use super::section::{SectionsError, XmleSections};
use crate::{
    asg::{visit::topo_sort, Asg, AsgError, Ident, Object},
    diagnose::{Annotate, Diagnostic},
    diagnostic_unreachable,
    span::UNKNOWN_SPAN,
};

// Result of [`sort`].
pub type SortResult<T> = Result<T, SortError>;

/// Lower ASG into [`XmleSections`] by ordering relocatable text fragments.
///
/// The topological sort is performed by [`topo_sort`].
pub fn sort<'a, S: XmleSections<'a>>(asg: &'a Asg, mut dest: S) -> SortResult<S>
where
    S: XmleSections<'a>,
{
    // All rooted identifiers will be linked.
    // This is important,
    //   otherwise the sort will traverse into packages
    //     (which are also rooted)
    //     and we'll wind up linking everything rather than just the things
    //     we need.
    let roots = asg.root(UNKNOWN_SPAN).edges_filtered::<Ident>(asg);

    for result in topo_sort(asg, roots) {
        let oi = result.map_err(AsgError::UnsupportedCycle)?;

        match asg.get(oi).expect("missing object") {
            Object::Root(_) => (),
            Object::Ident(ident) => dest.push(ident)?,

            // Identifiers are parented to their packages,
            //   but they're nothing more than containers.
            Object::Pkg(_) => (),

            obj => {
                diagnostic_unreachable!(
                    obj.internal_error(
                        "this object should not be present on the graph"
                    )
                    .into(),
                    "linker graph should not contain {obj}",
                )
            }
        }
    }

    Ok(dest)
}

/// Error during graph sorting.
///
/// These errors reflect barriers to meaningfully understanding the
///   properties of the data in the graph with respect to sorting.
/// It does not represent bad underlying data that does not affect the
///   sorting process.
#[derive(Debug, PartialEq)]
pub enum SortError {
    /// Error while lowering into [`XmleSections`].
    SectionsError(SectionsError),

    /// The graph has a cyclic dependency.
    AsgError(AsgError),
}

impl From<SectionsError> for SortError {
    fn from(err: SectionsError) -> Self {
        Self::SectionsError(err)
    }
}

impl From<AsgError> for SortError {
    fn from(value: AsgError) -> Self {
        Self::AsgError(value)
    }
}

impl std::fmt::Display for SortError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SectionsError(e) => e.fmt(f),
            Self::AsgError(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for SortError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Diagnostic for SortError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use SortError::*;

        match self {
            SectionsError(e) => e.describe(),
            AsgError(e) => e.describe(),
        }
    }
}

#[cfg(test)]
mod test;
