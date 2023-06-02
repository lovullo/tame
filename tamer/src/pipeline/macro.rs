// Lowering pipeline macro
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

//! Declarative lowering pipelines.
//!
//! This module defines the `lower_pipeline` macro,
//!   which allows for declarative lowering pipeline definitions.
//! For more information of what a lowering pipeline is,
//!   and to see TAMER's pipelines,
//!   see the [parent module](super).

/// Declaratively define a lowering pipeline.
///
/// A lowering pipeline stitches together parsers such that the objects of
///   one become the tokens of the next;
///     see the [module-level documentation](self) for more information.
///
/// Composing those types results in a significant amount of boilerplate.
/// This macro is responsible for generating a function that,
///   given a source and optional [`ParseState`](crate::parse::ParseState) contexts,
///   will carry out the lowering operation and invoke the provided sink on
///     each object that comes out the end.
///
/// TODO: This is not yet finalized;
///   more documentation is needed once the approach is solid.
macro_rules! lower_pipeline {
    ($(
        $(#[$meta:meta])*
        $vis:vis $fn:ident$(<$l:lifetime>)?
            $(|> $lower:ty $([$ctx:ident])? $(, until ($until:pat))?)*;
    )*) => {$(
        $(#[$meta])*
        ///
        /// Pipeline Definition
        /// ===================
        /// This pipeline consists of the following parsers,
        ///   where the first parser accepts the `src` token stream and the
        ///   final parser outputs to the provided `sink`.
        ///
        $(
            #[doc=concat!("  - [`", stringify!($lower), "`]")]
            $(
                #[doc=concat!(" with context `", stringify!($ctx), "`")]
            )?
            $(
                #[doc=concat!(
                    "up to and including the first token matching `",
                    stringify!($until),
                    "`",
                )]
            )?
            #[doc="\n"]
        )*
        ///
        /// The type signature of this function is complex due to the wiring
        ///   of the types of all parsers in the pipeline.
        /// It can be understood as:
        ///
        ///   1. A function accepting three classes of arguments:
        ///      1. The _source_ token stream,
        ///           which consists of tokens expected by the first parser
        ///           in the pipeline;
        ///      2. _Context_ for certain parsers that request it,
        ///           allowing for state to persist between separate
        ///           pipelines; and
        ///      3. A _sink_ that serves as the final destination for the
        ///           token stream.
        ///   2. A [`Result`] consisting of the updated context that was
        ///        originally passed into the function,
        ///          so that it may be utilized in future pipelines.
        ///   3. A _recoverable error_ type `ER` that may be utilized when
        ///        compilation should continue despite an error.
        ///      All parsers are expected to perform their own error
        ///        recovery in an attempt to continue parsing to discover
        ///        further errors;
        ///          as such,
        ///            this error type `ER` must be able to contain the
        ///            errors of any parser in the pipeline,
        ///              which is the reason for the large block of
        ///              [`From`]s in this function's `where` clause.
        ///   4. An _unrecoverable error_ type `EU` that may be yielded by
        ///        the sink to terminate compilation immediately.
        ///      This is a component of the [`Result`] type that is
        ///        ultimately yielded as the result of this function.
        $vis fn $fn<$($l,)? ES: Diagnostic, ER: Diagnostic, EU: Diagnostic>(
            src: impl LowerSource<
                UnknownToken,
                lower_pipeline!(@first_tok_ty $($lower),*),
                ES
                >,
            $(
                // Each parser may optionally receive context from an
                //   earlier run.
                $($ctx: impl Into<<$lower as ParseState>::PubContext>,)?
            )*
            sink: impl FnMut(
                Result<lower_pipeline!(@last_obj_ty $($lower),*), ER>
            ) -> Result<(), EU>,
        ) -> Result<
            (
                $(
                    // Any context that is passed in is also returned so
                    //   that individual pipelines can continue to build
                    //   upon state from previous pipelines.
                    $( lower_pipeline!(@ret_ctx_ty $lower, $ctx), )?
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
            ER: From<ParseError<UnknownToken, ES>>
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
                @body_head(src, sink)
                $((|> $lower $([$ctx])? $(, until ($until))?))*
            )?;

            Ok(($(
                $($ctx,)?
            )*))
        }
    )*};

    (@ret_ctx_ty $lower:ty, $_ctx:ident) => {
        <$lower as ParseState>::PubContext
    };

    // We are able to determine the necessary type of the source token
    //   stream by observing what token is expected by the first parser in the
    //   pipeline.
    (@first_tok_ty $lower:ty, $($rest:ty),+) => {
        <$lower as ParseState>::Token
    };

    // The last object type enters the sink.
    (@last_obj_ty $lower:ty, $($rest:ty),+) => {
        lower_pipeline!(@last_obj_ty $($rest),+)
    };
    (@last_obj_ty $last:ty) => {
        <$last as ParseState>::Object
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
        @body_head($src:ident, $sink:ident)
        (|> $head:ty) $($rest:tt)*
    ) => {
        Lower::<
            ParsedObject<UnknownToken, _, ES>,
            $head,
            ER,
        >::lower::<_, EU>(&mut $src.map(|result| result.map_err(ER::from)), |next| {
            lower_pipeline!(
                @body_inner(next, $head, $sink)
                $($rest)*
            )
        })
    };

    // TODO: Roll this into the above
    (
        @body_head($src:ident, $sink:ident)
        (|> $head:ty [$ctx:ident]) $($rest:tt)*
    ) => {
        Lower::<
            ParsedObject<UnknownToken, _, ES>,
            $head,
            ER,
        >::lower_with_context::<_, EU>(
            &mut $src.map(|result| result.map_err(ER::from)),
            $ctx,
            |next| {
                lower_pipeline!(
                    @body_inner(next, $head, $sink)
                    $($rest)*
                )
            }
        )
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
    //
    // TODO: Roll this into the above
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