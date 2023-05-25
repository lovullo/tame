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

use crate::{
    asg::air::{AirAggregate, AirAggregateCtx},
    diagnose::Diagnostic,
    obj::xmlo::{XmloAirContext, XmloReader, XmloToAir, XmloToken},
    parse::{
        FinalizeError, FromParseError, Lower, LowerSource, ParseError, Parsed,
        ParsedObject, UnknownToken,
    },
    xir::{
        flat::{PartialXirToXirf, Text},
        Error as XirError, Token as XirToken,
    },
};

/// Load an `xmlo` file represented by `src` into the graph held
///   by `air_ctx`.
///
/// Loading an object file will result in opaque objects being added to the
///   graph.
///
/// TODO: To re-use this in `tamec` we want to be able to ignore fragments.
///
/// TODO: More documentation once this has been further cleaned up.
pub fn load_xmlo<EO: Diagnostic + PartialEq>(
    src: impl LowerSource<UnknownToken, XirToken, XirError>,
    air_ctx: AirAggregateCtx,
    xmlo_ctx: XmloAirContext,
) -> Result<(AirAggregateCtx, XmloAirContext), EO>
where
    EO: From<ParseError<UnknownToken, XirError>>
        + FromParseError<PartialXirToXirf<4, Text>>
        + FromParseError<XmloReader>
        + FromParseError<XmloToAir>
        + FromParseError<AirAggregate>
        + From<FinalizeError>,
{
    // TODO: This entire block is a WIP and will be incrementally
    //   abstracted away.
    Lower::<
        ParsedObject<UnknownToken, XirToken, XirError>,
        PartialXirToXirf<4, Text>,
        EO,
    >::lower(&mut src.map(|result| result.map_err(EO::from)), |toks| {
        Lower::<PartialXirToXirf<4, Text>, XmloReader, _>::lower(toks, |xmlo| {
            let mut iter = xmlo.scan(false, |st, rtok| match st {
                true => None,
                false => {
                    *st =
                        matches!(rtok, Ok(Parsed::Object(XmloToken::Eoh(..))));
                    Some(rtok)
                }
            });

            Lower::<XmloReader, XmloToAir, _>::lower_with_context(
                &mut iter,
                xmlo_ctx,
                |air| {
                    let (_, air_ctx) =
                            Lower::<XmloToAir, AirAggregate, _>::lower_with_context(
                                air,
                                air_ctx,
                                |end| {
                                    for result in end {
                                        let _ = result?;
                                    }

                                    Ok::<_, EO>(())
                                },
                            )?;

                    Ok::<_, EO>(air_ctx)
                },
            )
        })
    })
}
