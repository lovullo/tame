// XIR element parser generator nonterminal (NT)
//
//  Copyright (C) 2014-2026 Ryan Specialty, LLC.
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

use super::SuperState;

use crate::{parse::prelude::*, xir::QName};

mod node;
mod sum;

pub use node::{ChildNt, ChildNtMeta, NodeMatcher, Nt, NtState};
pub use sum::{SumNt, SumNtState};

pub trait NtBase: PartialEq + Debug
where
    <Self::NtSuper as ParseState>::Error: From<Self::ParseError>,
{
    // Superstate of all NTs.
    type NtSuper: From<Self::ParseState> + SuperState;

    /// Parser for this NT.
    type ParseState: ParseState<
            Super = Self::NtSuper,
            Error = Self::ParseError,
            Object = <Self::NtSuper as ParseState>::Object,
        >;

    /// Errors emitted by [`Self::ParseState`].
    type ParseError: Diagnostic + Debug + PartialEq;

    /// A default state that can be preempted by [`Self::NtSuper`].
    fn preemptable() -> Self::ParseState;

    /// A default state that cannot be preempted by [`Self::NtSuper`].
    #[allow(dead_code)] // not utilized for every NT
    fn non_preemptable() -> Self::ParseState;

    /// Whether the given QName would be matched by any of the
    ///   NT parsers associated with this type.
    ///
    /// If any such NT is found,
    ///   this returns it.
    /// This widens to the supertype to support returning any number of
    ///   possible NTs.
    /// By returning the matching NT,
    ///   we are able to generalize [`QName`] matching without having to
    ///   generate custom code per parser via
    ///   [`ele_parse`](crate::ele_parse).
    fn matches(qname: QName) -> Option<Self::NtSuper>;

    /// Number of
    ///   [`NodeMatcher`]s considered by this parser.
    fn matches_n() -> usize;

    /// Format matcher for display.
    ///
    /// This value may be rendered singularly or as part of a list of
    ///   values joined together by Sum NTs.
    /// This function receives the number of values to be formatted
    ///   as `n` and the current 0-indexed offset within that list
    ///   as `i`.
    /// This allows for zero-copy rendering of composable NTs.
    ///
    /// `i` must be incremented after the operation.
    fn fmt_matches(
        n: usize,
        i: &mut usize,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result;

    /// Whether the parser is in a state that can tolerate superstate
    ///   node preemption.
    ///
    /// For more information,
    ///   see the superstate.
    fn can_preempt_node(&self) -> bool;
}

/// Result of parsing an NT.
pub type NtParseResult<NT> = Result<
    <<NT as NtBase>::ParseState as ParseState>::Object,
    <NT as NtBase>::ParseError,
>;

/// Preemption status of the next expected node.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum NtExpectKind {
    /// Another NT's parser can preempt our own,
    ///   taking a token from us.
    /// If we cannot parse a token that we have been given,
    ///   we are also permitted to hand off the token to another parser
    ///     (dead state)
    ///     rather than failing outright.
    #[default] // TODO: remove; be explicit
    Preemptable,

    /// This parser _must_ handle the next token,
    ///   otherwise it must produce an error and initiate recovery.
    /// No other parser is permitted to preempt.
    NonPreemptable,
}

impl NtExpectKind {
    fn can_preempt_node(&self) -> bool {
        match self {
            Self::Preemptable => true,
            Self::NonPreemptable => false,
        }
    }
}

impl Display for NtExpectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Preemptable => write!(f, "preemptable"),
            Self::NonPreemptable => write!(f, "non-preemptable"),
        }
    }
}
