// Lowering pipelines
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

//! Lowering pipelines.
//!
//! TAMER is composed of a series of IR lowering operations connected in
//!   series as a pipeline of [`Parser`](crate::parse::Parser)s,
//!     called the _lowering pipeline_.
//! Each parser in the pipeline produces a stream of tokens that is read and
//!   acted upon by the next.
//! The system can therefore be reasoned about as a series of mappings
//!   between IRs,
//!     where each parser in the pipeline produces a lower-level IR.
//!
//! Portions of the pipeline require operating on data in aggregate.
//! Most notably,
//!   the [ASG](crate::asg) aggregates data into a graph;
//!     the graph acts as a _sink_ for the pipeline.
//! At the other end,
//!   the ASG serves as a source for another lowering pipeline that emits
//!   the target object.
//!
//! The module is responsible for pipeline composition.
//! For information on the lowering pipeline as an abstraction,
//!   see [`Lower`].

use std::convert::Infallible;

use crate::{
    asg::{air::AirAggregate, visit::TreeWalkRel, AsgTreeToXirf},
    diagnose::Diagnostic,
    nir::{InterpolateNir, NirToAir, TplShortDesugar, XirfToNir},
    obj::xmlo::{XmloReader, XmloToAir, XmloToken},
    parse::{
        terminal, FinalizeError, Lower, LowerSource, ParseError, ParseState,
        Parsed, ParsedObject, UnknownToken,
    },
    xir::{
        autoclose::XirfAutoClose,
        flat::{PartialXirToXirf, RefinedText, Text, XirToXirf, XirfToXir},
        Error as XirError, Token as XirToken,
    },
};

// The `lower_pipeline` macro is tucked away here to allow the reader to
//   focus on the pipelines themselves rather than how they wired together.
#[macro_use]
mod r#macro;

lower_pipeline! {
    /// Load an `xmlo` file represented by `src` into the graph held
    ///   by `air_ctx`.
    ///
    /// Loading an object file will result in opaque objects being added to the
    ///   graph.
    ///
    /// TODO: To re-use this in `tamec` we want to be able to ignore fragments.
    ///
    /// TODO: More documentation once this has been further cleaned up.
    pub load_xmlo(XirToken, XirError)
        |> PartialXirToXirf<4, Text>
        |> XmloReader
        |> XmloToAir[xmlo_ctx], until (XmloToken::Eoh(..))
        |> AirAggregate[air_ctx];

    /// Parse a source package into the [ASG](crate::asg) using TAME's XML
    ///   source language.
    ///
    /// TODO: More documentation once this has been further cleaned up.
    pub parse_package_xml(XirToken, XirError)
        |> XirToXirf<64, RefinedText>
        |> XirfToNir
        |> TplShortDesugar
        |> InterpolateNir
        |> NirToAir
        |> AirAggregate[air_ctx];

    /// Lower an [`Asg`](crate::asg::Asg)-derived token stream into an
    ///   `xmli` file.
    ///
    /// TODO: More documentation once this has been further cleaned up.
    pub lower_xmli<'a>(TreeWalkRel, Infallible)
        |> AsgTreeToXirf<'a>[asg]
        |> XirfAutoClose
        |> XirfToXir<Text>;
}
