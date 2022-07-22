// Parser tracing
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

//! Tracing for parsing operations.
//!
//! This provides human-readable traces on standard error any time a token
//!   is fed to the parser.
//! These traces are provided automatically when `cfg(test)`,
//!   which means that they are automatically included in the output of any
//!   test failure.
//!
//! Outside of tests,
//!   this can be enabled at configuration-time using the
//!   `parser-trace-stderr` feature flag
//!     (`./configure FEATURES=parser-trace-stderr`).
//!
//! _These traces are not meant to be machine-readable!_
//! There may be other useful tracing formats in the future,
//!   including OpenTelemetry and the DOT graph description language.
//! Do not try to use the human-readable traces in that way since the format
//!   is subject to change without notice.

use super::{state::TransitionData, ParseState, Token};

pub(super) trait ParserTrace: Default {
    /// Output the upper portion of a token trace.
    ///
    /// This begins the trace with information about the current
    ///   [`ParseState`] and the token that was received.
    /// Post-transition tracing is handled by [`Self::trace_tok_end`].
    ///
    /// There is no means to return an error and a failure to output the
    ///   trace should not interrupt processing.
    fn trace_tok_begin<S: ParseState>(&mut self, st_orig: &S, tok: &S::Token);

    /// Output the lower portion of a token trace.
    ///
    /// This ends the trace with information about the transition and the
    ///   resulting [`ParseState`].
    ///   
    /// There is no means to return an error and a failure to output the
    ///   trace should not interrupt processing.
    fn trace_tok_end<S: ParseState>(
        &mut self,
        st_new: &S,
        data: &TransitionData<S>,
    );
}

/// Perform no tracing.
///
/// This should be used by default for non-test builds,
///   since tracing can incur a significant performance cost.
#[derive(Debug, PartialEq, Default)]
pub struct VoidTrace;

impl ParserTrace for VoidTrace {
    fn trace_tok_begin<S: ParseState>(
        &mut self,
        _st_orig: &S,
        _tok: &S::Token,
    ) {
        // Do nothing at all.
    }

    fn trace_tok_end<S: ParseState>(
        &mut self,
        _st_new: &S,
        _data: &TransitionData<S>,
    ) {
        // Do nothing at all.
    }
}

/// Human-readable [`ParserTrace`].
///
///
/// Note: if one of these trace blocks does not fully output,
///   then you may have a `Display::fmt` or `Debug::fmt` panic---like
///     a `todo!` or `unimplemented!`---in
///     your `Token` or `ParseState`.
///
/// See [module-level](super) documentation for more information.
#[derive(Debug, PartialEq, Default)]
pub struct HumanReadableTrace;

impl ParserTrace for HumanReadableTrace {
    fn trace_tok_begin<S: ParseState>(&mut self, st_orig: &S, tok: &S::Token) {
        eprint!(
            "\
[Parser::feed_tok] (input IR: {ir})
| ==> Parser before tok is {st_orig}.
|  |  {st_orig:?}
|
| ==> {ir} tok: {tok}
|  |  {tok:?}
|\n",
            ir = S::Token::ir_name()
        );
    }

    fn trace_tok_end<S: ParseState>(
        &mut self,
        st_new: &S,
        data: &TransitionData<S>,
    ) {
        eprint!(
            "\
| ==> Parser after tok is {st_new}.
|  |  {st_new:?}
|  |  Lookahead: {la:?}\n",
            la = data.lookahead_ref(),
        );

        if let Some(obj) = data.object_ref() {
            // Note that `Object` does not implement `Display`,
            //   but you'll see a `Display` representation if the object
            //   is passed to another `Parser` as a `Token`.
            eprint!(
                "\
|
| ==> Yielded object:
|  |  {obj:?}\n",
            );
        }

        if let Some(err) = data.err_ref() {
            eprint!(
                "\
|
| ==> !!! error: {err}.
|  |  {err:?}\n",
            );
        }

        #[allow(unused_variables)]
        let cfg = ""; // so that this compiles without matching cfg
        #[cfg(feature = "parser-trace-stderr")]
        #[allow(unused_variables)]
        let cfg = "feature = \"parser-trace-stderr\"";
        #[cfg(test)] // takes precedence if both are set
        let cfg = "test";
        eprint!(
            "= note: this trace was output as a debugging aid \
                because `cfg({cfg})`.\n\n",
        );
    }
}
