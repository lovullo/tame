// XIR element parser generator superstate
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

use crate::{
    diagnose::Diagnostic,
    parse::{StateStack, prelude::*},
    xir::flat::{RefinedText, XirfToken},
};

/// Maximum level of parser stack nesting.
///
/// The intent of this value is to ensure that TAMER will fail if something
///   goes terribly wrong,
///     e.g. if the system has a bug where lookahead causes the system to
///     recurse infinitely.
///
/// Unfortunately,
///   this limit _does not_ correspond to the level of XML nesting;
///     parsers composed of Sum NTs,
///       in particular,
///       push multiple parsers onto the stack for a single element.
pub const XIR_MAX_DEPTH: usize = 1024;

/// Superstate.
///
/// A superstate is responsible for aggregating all nonterminals and serving
///   as a trampoline to delegate parsing operations.
///
/// Conceptually,
///   a superstate acts as a runtime for the state machine defined by NT
///   interdependencies.
/// It represents the reification of such a state machine and all of its
///   transitions.
pub trait SuperState:
    ClosedParseState<
        Token = XirfToken<RefinedText>,
        Context = SuperStateContext<Self>,
    > + Default
{
    /// Type of error when failing to parse attribute values.
    type AttrValueError: Diagnostic + PartialEq;

    /// Whether the inner (active child) [`ParseState`] is in an accepting
    ///   state.
    ///
    /// [`ParseState`]: crate::parse::ParseState
    fn is_inner_accepting(&self, ctx: &<Self as ParseState>::Context) -> bool;

    /// Whether the inner parser is in a state that can tolerate superstate
    ///   node preemption.
    ///
    /// Node preemption allows us (the superstate) to ask for
    ///   permission from the inner parser to parse some token ourselves,
    ///     by asking whether the parser is in a state that would cause
    ///     semantic issues if we were to do so.
    ///
    /// For example,
    ///   if we were to preempt text nodes while an inner parser was still
    ///   parsing attributes,
    ///     then we would emit an object associated with that text before
    ///     the inner parser had a chance to conclude that attribute parsing
    ///     has completed and emit the opening object for that node;
    ///       the result would otherwise be an incorrect `Text, Open`
    ///       instead of the correct `Open, Text`,
    ///         which would effectively unparent the text.
    /// Similarly,
    ///   if we were to parse our own tokens while an inner parser was
    ///   performing error recovery in such a way as to ignore all child
    ///   tokens,
    ///     then we would emit an object in an incorrect context.
    fn can_preempt_node(&self) -> bool;

    /// Force current state into a non-preemptable expecting state.
    fn expect_non_preemptable(self) -> Self;
}

/// [`SuperState`] [`Context`] that gets propagated to each child parser.
pub type SuperStateContext<S> = Context<StateStack<S, XIR_MAX_DEPTH>>;
