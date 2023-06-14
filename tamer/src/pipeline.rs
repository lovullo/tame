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
//!
//! Error Widening
//! ==============
//! Every lowering pipeline will have an associated error sum type generated
//!   for it;
//!     this is necessary to maintain an appropriate level of encapsulation
//!     and keep implementation details away from the caller.
//! All of the individual errors types is otherwise significant source of
//!   complexity.
//!
//! Since all [`ParseState`]s in the lowering pipeline are expected to
//!   support error recovery,
//!     this generated error sum type represents a _recoverable_ error.
//! It is up to the sink to deermine whether the error should be promoted
//!   into an unrecoverable error `EU`,
//!     which is the error type yielded by the lowering operation.
//! Error reporting and recovery should be utilized whenever it makes sense
//!   to present the user with as many errors as possible rather than
//!   aborting the process immediately,
//!     which would otherwise force the user to correct errors one at a
//!     time.
//!
//! [`ParseState`] Requirements
//! ---------------------------
//! Each [`ParseState`] in the pipeline is expected to have its own unique
//!   error type,
//!     utilizing newtypes if necessary;
//!       this ensures that errors are able to be uniquely paired with each
//!         [`ParseState`] that produced it without having to perform an
//!         explicit mapping at the call site.
//! This uniqueness property allows for generation of [`From`]
//!   implementations that will not overlap,
//!     and remains compatible with the API of [`Lower`].
//!
//! [`ParseState::Error`] Lifetime Requirements and Workarounds
//! -----------------------------------------------------------
//! Error types in TAMER _never_ have lifetime bounds;
//!   this is necessary to allow error types to be propapgated all the way
//!   up the stack regardless of dependencies.[^lifetime-alt]
//!
//! [^lifetime-alt]: Rather than utilizing references with lifetimes,
//!   TAMER error types may hold symbols representing interned values,
//!   or may instead [`Copy`] data that has no interner.
//!
//! However,
//!   [`ParseState::Error`] is an associated type on [`ParseState`],
//!     which _may_ have lifetimes.[^parse-state-lifetime-ex]
//! At the time of writing,
//!   even though the associated error type does not utilize the lifetime
//!   bounds of the [`ParseState`],
//!     Rust still requires some lifetime specification and will not elide
//!     it or allow for anonymous lifetimes.
//!
//! [^parse-state-lifetime-ex]: One example of a [`ParseState`] with
//!   an associated lifetime is [`AsgTreeToXirf`].
//!
//! We want to be able to derive error types from the provided
//!   [`ParseState`]s along so that the caller does not have to peel back
//!   layers of abstraction in order to determine how the error type ought
//!   to be specified.
//! To handle this,
//!   the `lower_pipeline!` macro will _rewrite all lifetimes to `'static`'_
//!   in the provided pipeline types.
//! Since no [`ParseState::Error`] type should have a lifetime,
//!   and therefore should not reference the lifetime of its parent
//!     [`ParseState`],
//!   this should have no practical effect on the error type itself.

use crate::{
    asg::{air::AirAggregate, AsgTreeToXirf},
    diagnose::Diagnostic,
    nir::{InterpolateNir, NirToAir, TplShortDesugar, XirfToNir},
    obj::xmlo::{XmloReader, XmloToAir, XmloToken},
    parse::{
        terminal, FinalizeError, Lower, LowerSource, ParseError, ParseState,
        ParseStateError, Parsed, ParsedObject, UnknownToken,
    },
    xir::{
        autoclose::XirfAutoClose,
        flat::{PartialXirToXirf, RefinedText, Text, XirToXirf, XirfToXir},
    },
};

// The `lower_pipeline` macro is tucked away here to allow the reader to
//   focus on the pipelines themselves rather than how they wired together.
#[macro_use]
mod r#macro;

lower_pipeline! {
    /// Parse a source package into the [ASG](crate::asg) using TAME's XML
    ///   source language.
    ///
    /// Source XML is represented by [XIR](crate::xir).
    /// This is parsed into [NIR`](crate::nir),
    ///   which extracts TAME's high-level source language from the document
    ///   format (XML).
    /// NIR is then desugared in various ways,
    ///   producing a more verbose NIR than what the user originally
    ///   entered.
    ///
    /// NIR is then lowered into [AIR](crate::asg::air),
    ///   which is then aggregated into the [ASG](crate::asg) to await
    ///   further processing.
    /// It is after this point that package imports should be processed and
    ///   also aggregated into the same ASG so that all needed dependencies
    ///   definitions are available.
    pub parse_package_xml -> ParsePackageXml
        |> XirToXirf<64, RefinedText>
        |> XirfToNir
        |> TplShortDesugar
        |> InterpolateNir
        |> NirToAir[nir_air_ty]
        |> AirAggregate[air_ctx];

    /// Load an `xmlo` file into the graph held by `air_ctx`.
    ///
    /// Loading an object file will result in opaque objects being added to the
    ///   graph;
    ///     no sources will be parsed.
    ///
    /// To parse sources instead,
    ///   see [`parse_package_xml`].
    pub load_xmlo -> LoadXmlo
        |> PartialXirToXirf<4, Text>
        |> XmloReader
        |> XmloToAir[xmlo_ctx], until (XmloToken::Eoh(..))
        |> AirAggregate[air_ctx];

    /// Lower an [`Asg`](crate::asg::Asg)-derived token stream into an
    ///   `xmli` file.
    ///
    /// After a package has been parsed with [`parse_package_xml`] and
    ///   further processing has taken place,
    ///     this pipeline will re-generate TAME sources from the ASG for the
    ///     purpose of serving as source input to the XSLT-based compiler.
    /// This allows us to incrementally replace that compiler's
    ///   functionality by having the XSLT-based system pick up where we
    ///   leave off,
    ///     skipping anything that we have already done.
    pub lower_xmli<'a> -> LowerXmli
        |> AsgTreeToXirf<'a>[asg]
        |> XirfAutoClose
        |> XirfToXir<Text>;
}
