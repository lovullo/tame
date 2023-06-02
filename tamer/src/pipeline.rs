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
    asg::{air::AirAggregate, visit::TreeWalkRel, Asg, AsgTreeToXirf},
    diagnose::Diagnostic,
    iter::TrippableIterator,
    nir::{InterpolateNir, NirToAir, TplShortDesugar, XirfToNir},
    obj::xmlo::{XmloReader, XmloToAir, XmloToken},
    parse::{
        terminal, FinalizeError, FromParseError, Lower, LowerSource,
        ParseError, ParseState, Parsed, ParsedObject, UnknownToken,
    },
    xir::{
        autoclose::XirfAutoClose,
        flat::{PartialXirToXirf, RefinedText, Text, XirToXirf, XirfToXir},
        writer::WriterState,
        Error as XirError, Token as XirToken,
    },
};

/// Declaratively define a lowering pipeline.
///
/// A lowering pipeline stitches together parsers such that the objects of
///   one become the tokens of the next;
///     see the [module-level documentation](self) for more information.
///
/// Composing those types results in a significant amount of boilerplate.
/// This macro is responsible for generating a function that,
///   given a source and optional [`ParseState`] contexts,
///   will carry out the lowering operation and invoke the provided sink on
///     each object that comes out the end.
///
/// TODO: This is not yet finalized;
///   more documentation is needed once the approach is solid.
macro_rules! lower_pipeline {
    ($(
        $(#[$meta:meta])*
        $vis:vis $fn:ident($srcobj:ty, $srcerr:ty)
            $(|> $lower:ty $([$ctx:ident])? $(, until ($until:pat))?)*;
    )*) => {$(
        $(#[$meta])*
        $vis fn $fn<ER: Diagnostic, EU: Diagnostic>(
            src: impl LowerSource<UnknownToken, $srcobj, $srcerr>,
            $(
                // Each parser may optionally receive context from an
                //   earlier run.
                $($ctx: impl Into<<$lower as ParseState>::PubContext>,)?
            )*
            sink: impl FnMut(Result<(), ER>) -> Result<(), EU>,
        ) -> Result<
            (
                $(
                    // Any context that is passed in is also returned so
                    //   that individual pipelines can continue to build
                    //   upon state from previous pipelines.
                    $( lower_pipeline!(@ret_ty $lower, $ctx), )?
                )*
            ),
            EU
        >
        where
            // Recoverable errors (ER) are errors that could potentially be
            //   handled by the sink.
            // Parsers are always expected to perform error recovery to the
            //   best of their ability.
            // We need to support widening into this error type from every
            //   individual ParseState in this pipeline,
            //     plus the source.
            ER: From<ParseError<UnknownToken, $srcerr>>
            $(
                + From<ParseError<
                    <$lower as ParseState>::Token,
                    <$lower as ParseState>::Error,
                >>
            )*,

            // Unrecoverable errors (EU) are errors that the sink chooses
            //   not to handle.
            // It is constructed explicitly from the sink,
            //   so the only type conversion that we are concerned about
            //   ourselves is producing it from a finalization error,
            //     which is _not_ an error that parsers are expected to
            //     recover from.
            EU: From<FinalizeError>,
        {
            let lower_pipeline!(@ret_pat $($($ctx)?)*) = lower_pipeline!(
                @body_head(src, $srcobj, $srcerr, sink)
                $((|> $lower $([$ctx])? $(, until ($until))?))*
            )?;

            Ok(($(
                $($ctx,)?
            )*))
        }
    )*};

    (@ret_ty $lower:ty, $_ctx:ident) => {
        <$lower as ParseState>::PubContext
    };

    // Because of how the lowering pipeline composes,
    //   contexts are nested with a terminal unit at the left,
    //     as in `(((), B), A)` with `A` being a parser that appears earlier
    //     in the pipeline than `B`.
    (@ret_pat $ctx:ident $($rest:tt)*) => {
        (lower_pipeline!(@ret_pat $($rest)*), $ctx)
    };
    (@ret_pat) => {
        ()
    };

    // First lowering operation from the source.
    //
    // This doesn't support context or `until`;
    //   it can be added if ever it is needed.
    (
        @body_head($src:ident, $srcobj:ty, $srcerr:ty, $sink:ident)
        (|> $head:ty) $($rest:tt)*
    ) => {
        Lower::<
            ParsedObject<UnknownToken, $srcobj, $srcerr>,
            $head,
            ER,
        >::lower::<_, EU>(&mut $src.map(|result| result.map_err(ER::from)), |next| {
            lower_pipeline!(
                @body_inner(next, $head, $sink)
                $($rest)*
            )
        })
    };

    // Lower without context
    //   (with the default context for the parser).
    //
    // This doesn't support `until`;
    //   it can be added if needed.
    (
        @body_inner($next:ident, $lower_prev:ty, $sink:ident)
        (|> $lower:ty) $($rest:tt)*
    ) => {
        Lower::<$lower_prev, $lower, _>::lower($next, |next| {
            lower_pipeline!(
                @body_inner(next, $lower, $sink)
                $($rest)*
            )
        })
    };

    // Lower with a context provided by the caller,
    //   optionally with an `until` clause that stops at (and includes) a
    //   matching object from the previous parser.
    (
        @body_inner($next:ident, $lower_prev:ty, $sink:ident)
        (|> $lower:ty [$ctx:ident] $(, until ($until:pat))?) $($rest:tt)*
    ) => {{
        // Shadow $next with a new iterator if `until` clause was provided.
        // `take_while` unfortunately doesn't include the first object that
        //   does not match,
        //     thus the more verbose `scan` which includes the first
        //     non-match and then trips the iterator.
        $(
            let $next = &mut $next.scan(false, |st, rtok| match st {
                true => None,
                false => {
                    *st =
                        matches!(rtok, Ok(Parsed::Object($until)));
                    Some(rtok)
                }
            });
        )?

        Lower::<$lower_prev, $lower, _>::lower_with_context($next, $ctx, |next| {
            lower_pipeline!(
                @body_inner(next, $lower, $sink)
                $($rest)*
            )
        })
    }};

    // Terminal sink is applied after there are no more lowering operations
    //   remaining in the pipeline definition.
    (@body_inner($next:ident, $lower_prev:ty, $sink:ident)) => {
        terminal::<$lower_prev, _>($next).try_for_each($sink)
    };
}

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
}

/// Lower an [`Asg`]-derived token stream into an `xmli` file.
///
/// TODO: More documentation once this has been further cleaned up.
pub fn lower_xmli<EU: Diagnostic>(
    src: impl LowerSource<UnknownToken, TreeWalkRel, Infallible>,
    asg: &Asg,
    sink: impl FnMut(WriterState, XirToken) -> Result<WriterState, EU>,
) -> Result<(), EU>
where
    EU: From<ParseError<UnknownToken, Infallible>>
        + From<ParseError<TreeWalkRel, Infallible>> // see note above
        + FromParseError<XirfAutoClose>
        + FromParseError<XirfToXir<Text>>
        + From<FinalizeError>,
{
    #[rustfmt::skip] // better visualize the structure despite the line length
    Lower::<
        ParsedObject<UnknownToken, TreeWalkRel, Infallible>,
        AsgTreeToXirf,
        _,
    >::lower_with_context::<_, EU>(
        &mut src.map(|result| result.map_err(EU::from)),
        asg,
        |xirf_unclosed| {
            Lower::<AsgTreeToXirf, XirfAutoClose, _>::lower(xirf_unclosed, |xirf| {
                Lower::<XirfAutoClose, XirfToXir<Text>, _>::lower(xirf, |xir| {
                    terminal::<XirfToXir<Text>, _>(xir).while_ok(|toks| {
                        // Write failures should immediately bail out;
                        //   we can't skip writing portions of the file and
                        //   just keep going!
                        toks.try_fold(Default::default(), sink)
                    })
                })
            })
        }
    )?;

    Ok(())
}
