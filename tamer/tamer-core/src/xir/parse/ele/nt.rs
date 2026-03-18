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

use crate::{
    parse::prelude::*,
    xir::{OpenSpan, QName, flat::Depth},
};

mod node;
mod sum;

pub use node::{ChildNt, NodeMatcher, NodeNt, NodeNtState, NtError};
pub use sum::{SumNt, SumNtError, SumNtState};

/// A nonterminal for an [`ele_parse`](crate::ele_parse) grammar.
pub trait Nt: PartialEq + Debug + Sized
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

    /// An expecting state based on the status of another expecting state.
    fn from_expect(kind: NtExpectKind) -> Self::ParseState;

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
pub type NtParseResult<NT: Nt> = Result<
    <<NT as Nt>::ParseState as ParseState>::Object,
    <NT as Nt>::ParseError,
>;

/// Preemption status of the next expected node.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
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

    /// This NT has preempted parsing.
    /// It acts similar to a [`Self::NonPreemptable`].
    PreemptedParsing,
}

impl NtExpectKind {
    /// Whether parsing can be preempted while waiting for a token.
    fn can_preempt_node(&self) -> bool {
        match self {
            Self::Preemptable => true,
            Self::NonPreemptable => false,
            Self::PreemptedParsing => false,
        }
    }
}

impl Display for NtExpectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Preemptable => write!(f, "preemptable"),
            Self::NonPreemptable => write!(f, "non-preemptable"),
            Self::PreemptedParsing => {
                write!(f, "non-preemptable (this NT already preempted)")
            }
        }
    }
}

/// Metadata used to track active element.
///
/// After an NT is matched,
///   data on the [`Open`](crate::xir::flat::XirfToken::Open) token is used
///   to populate these data.
/// This is used primarily to match on an associated closing token at the
///   appropriate depth
///     and to provide diagnostic information.
///
/// Note on [`Copy`]
/// ================
/// Each of these values are relatively small,
///   and if we need to pass it around,
///   we will always reconstruct it to get around the borrow checker;
///     let's just keep things simple.
/// In some cases,
///   the borrow checker doesn't realize that only one branch is taken and
///   that a copy won't occur,
///     but this has been improving as Rust evolves!
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NtMeta {
    pub qname: QName,
    pub ospan: OpenSpan,
    pub depth: Depth,

    /// Whether the active NT is the cause of preemption.
    pub caused_preemption: PreemptionStatus,
}

/// Whether the NT is responsible for preempting parsing.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PreemptionStatus {
    /// The NT preempted whatever parsing was ongoing.
    ///
    /// This status applies _only_ to the NT that triggered preemption;
    ///   it does not apply to its children.
    PreemptedParsing,

    /// This NT is a valid child of its current context without any
    ///   preemption.
    ///
    /// Note that this designation also applies to root nodes
    ///   (which are children of the document).
    NoPreemption,
}

impl From<NtExpectKind> for PreemptionStatus {
    fn from(value: NtExpectKind) -> Self {
        use NtExpectKind::*;

        match value {
            Preemptable => Self::NoPreemption,
            NonPreemptable => Self::NoPreemption,
            PreemptedParsing => Self::PreemptedParsing,
        }
    }
}
