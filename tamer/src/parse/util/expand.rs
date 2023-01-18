// TAMER parsing framework utilities for token expansion
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

//! Token expansion utilities.
//!
//! _Expansion_ refers to the production of many [`Object`]s that are
//!   derived from a single [`Token`].
//! An [`ExpandableParseState`] is a [`ClosedParseState`] that,
//!   provided a [`Token`],
//!   produces an [`Expansion`] of zero or more [`Expansion::Expanded`]
//!     [`Object`]s before terminating with a [`Expansion::DoneExpanding`]
//!     [`Token`] intended to replace the originally provided [`Token`].
//!
//! An [`ExpandableParseState`] can be stitched with a parent parser using
//!   [`StitchExpansion`],
//!     giving the perception of expanding into that parent's token stream.

use super::super::{prelude::*, state::Lookahead};
use crate::{
    diagnose::{panic::DiagnosticOptionPanic, Annotate},
    parse::state::PartiallyStitchableParseState,
};

/// Represents an expansion operation on some source token of type `T`.
///
/// See variants and [`ExpandableParseState`] for more information.
#[derive(Debug, PartialEq, Eq)]
pub enum Expansion<T, O: Object> {
    /// A token of type `O` has been derived from the source token and
    ///   should be merged into the target token stream.
    Expanded(O),

    /// Expansion is complete and the source token should be replaced with
    ///   the inner `T`.
    ///
    /// Since the expectation is that the parser has completed parsing and
    /// no longer requires the token provided to it,
    ///   the parser yielding this variant _must not_ yield a token of
    ///   lookahead,
    ///     otherwise the system assume that the parser has an
    ///     implementation defect (bug) and will be forced to panic rather
    ///     than discard it.
    DoneExpanding(T),
}

impl<T: Token, O: Object> Object for Expansion<T, O> {}

/// A [`ClosedParseState`] that is able to serve as an expansion parser.
///
/// An expansion parser is a parser yielding [`Expansion`],
///   intended to be integrated into another token stream.
pub trait ExpandableParseState<O: Object> = ClosedParseState
where
    O: Token + Eq,
    Self: ParseState<Object = Expansion<<Self as ParseState>::Token, O>>;

/// An [`ExpandableParseState`] capable of expanding into the token stream
///   of a parent [`ParseState`] `SP`.
pub trait ExpandableInto<SP: ParseState> =
    ExpandableParseState<<SP as ParseState>::Object>
    where
        Self: ExpandableParseState<<SP as ParseState>::Object>
            + PartiallyStitchableParseState<SP>;

/// [`ExpandableParseState`] state stitching.
///
/// See [`Self::stitch_expansion`] for more information.
pub trait StitchExpansion: ClosedParseState {
    /// Stitch a [`ExpandableParseState`] that is
    ///   [`ExpandableInto<SP>`](ExpandableInto).
    ///
    /// This combines the state machine of an [`ExpandableParseState`],
    ///   allowing that parser to expand into the token stream of [`Self`].
    ///
    /// Panics
    /// ======
    /// This will panic with diagnostic information if a token of lookahead
    ///   is provided with a [`Expansion::DoneExpanding`] variant.
    /// See that variant for more information.
    fn stitch_expansion<SP: ParseState, C>(
        self,
        tok: <Self as ParseState>::Token,
        mut ctx: C,
        into: impl Fn(Transition<Self>) -> Transition<SP>,
        done: impl FnOnce(
            Transition<Self>,
            <SP as ParseState>::Token,
        ) -> TransitionResult<SP>,
    ) -> TransitionResult<<SP as ParseState>::Super>
    where
        Self: ExpandableInto<SP>,
        C: AsMut<<Self as ParseState>::Context>,
    {
        use Expansion::{DoneExpanding, Expanded};

        self.parse_token(tok, ctx.as_mut()).branch_obj_la(
            |st, obj, la| match (obj, la) {
                (Expanded(obj), la) => into(st)
                    .ok(obj)
                    .maybe_with_lookahead(la.map(Lookahead::inner_into)),

                (DoneExpanding(tok), la) => {
                    // Uphold parser lookahead invariant.
                    la.diagnostic_expect_none(
                        |Lookahead(la_tok)| {
                            vec![
                                la_tok.span().note(
                                    "this token of lookahead would be lost",
                                ),
                                tok.span().internal_error(
                                    "unexpected token of lookahead while \
                                        completing expansion of this token",
                                ),
                            ]
                        },
                        "cannot provide lookahead token with \
                            Expansion::DoneExpanding",
                    );

                    done(st, tok.into()).into_super()
                }
            },
            &into,
        )
    }
}

#[cfg(test)]
pub mod test;
