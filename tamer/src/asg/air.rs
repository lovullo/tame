// ASG IR
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

//! Intermediate representation for construction of the
//!   [abstract semantic graph (ASG)](super) (AIR).
//!
//! AIR serves as an abstraction layer between higher-level parsers and the
//!   aggregate ASG.
//! It allows parsers to operate as a raw stream of data without having to
//!   worry about ownership of or references to the ASG,
//!     and allows for multiple such parsers to be joined.

use super::{
    graph::object::{
        Object, ObjectIndexRelTo, ObjectIndexTo, ObjectIndexToTree,
        ObjectRelTy, ObjectRelatable, Pkg, Root, Tpl,
    },
    Asg, AsgError, Expr, Ident, ObjectIndex,
};
use crate::{
    diagnose::Annotate,
    diagnostic_unreachable,
    f::Functor,
    parse::{prelude::*, StateStack},
    span::Span,
    sym::SymbolId,
};
use std::{
    collections::hash_map::Entry,
    fmt::{Debug, Display},
};

#[cfg(test)]
use super::graph::object::ObjectRelTo;

#[macro_use]
mod ir;
use fxhash::FxHashMap;
pub use ir::Air;

mod expr;
mod meta;
mod opaque;
mod pkg;
mod tpl;
use expr::AirExprAggregate;
use meta::AirMetaAggregate;
use opaque::AirOpaqueAggregate;
use pkg::AirPkgAggregate;
use tpl::AirTplAggregate;

pub type IdentSym = SymbolId;
pub type DepSym = SymbolId;

/// AIR parser state.
#[derive(Debug, PartialEq, Default)]
pub enum AirAggregate {
    /// Parser has not yet been initialized.
    #[default]
    Uninit,

    /// Parser is in the root context.
    ///
    /// As a parser,
    ///   this does nothing but await work.
    /// Its presence in the [`AirStack`] is used for the global environment.
    Root(ObjectIndex<Root>),

    /// Parsing a package.
    Pkg(AirPkgAggregate),

    /// Parsing an expression.
    ///
    /// This expects to inherit an [`AirExprAggregate`] from the prior state
    ///   so that we are not continuously re-allocating its stack for each
    ///   new expression root.
    PkgExpr(AirExprAggregate),

    /// Parser is in template parsing mode.
    ///
    /// All objects encountered until the closing [`Air::TplEnd`] will be
    ///   parented to this template rather than the parent [`Pkg`].
    /// See [`Air::TplStart`] for more information.
    PkgTpl(AirTplAggregate),

    /// Parsing metavariables.
    PkgMeta(AirMetaAggregate),

    /// Parsing opaque objects.
    ///
    /// This parser is intended for loading declarations from object files
    ///   without loading their corresponding definitions.
    PkgOpaque(AirOpaqueAggregate),
}

impl Display for AirAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirAggregate::*;

        match self {
            Uninit => write!(f, "awaiting AIR input"),
            Root(_) => write!(f, "awaiting input at root"),
            Pkg(pkg) => {
                write!(f, "defining a package: {pkg}")
            }
            PkgExpr(expr) => {
                write!(f, "defining a package expression: {expr}")
            }
            PkgTpl(tpl) => {
                write!(f, "building a template: {tpl}")
            }
            PkgMeta(meta) => {
                write!(f, "building metavariable: {meta}")
            }
            PkgOpaque(opaque) => {
                write!(f, "loading opaque objects: {opaque}")
            }
        }
    }
}

impl From<AirPkgAggregate> for AirAggregate {
    fn from(st: AirPkgAggregate) -> Self {
        Self::Pkg(st)
    }
}

impl From<AirExprAggregate> for AirAggregate {
    fn from(st: AirExprAggregate) -> Self {
        Self::PkgExpr(st)
    }
}

impl From<AirTplAggregate> for AirAggregate {
    fn from(st: AirTplAggregate) -> Self {
        Self::PkgTpl(st)
    }
}

impl From<AirMetaAggregate> for AirAggregate {
    fn from(st: AirMetaAggregate) -> Self {
        Self::PkgMeta(st)
    }
}

impl From<AirOpaqueAggregate> for AirAggregate {
    fn from(st: AirOpaqueAggregate) -> Self {
        Self::PkgOpaque(st)
    }
}

impl ParseState for AirAggregate {
    type Token = Air;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use ir::{AirSubsets::*, AirTodo::*};
        use AirAggregate::*;

        match (self, tok.into()) {
            // Initialize the parser with the graph root,
            //   or continue with a previous context that has already been
            //   initialized.
            // See `asg::air::test::resume_previous_parsing_context` for an
            //   explanation of why this is important.
            (Uninit, tok) => {
                let oi_root = ctx.asg_ref().root(tok.span());
                ctx.stack().continue_or_init(|| Root(oi_root), tok)
            }

            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            // Package
            //
            // Note that `ret_or_transfer` will return from the active frame
            //   if it is in an accepting state,
            //     and so encountering a properly nested `PkgClose` will pop
            //     frames off of the stack until reaching the still-active
            //     parent package frame.
            (
                st @ (Root(..) | PkgExpr(..) | PkgTpl(..) | PkgMeta(..)
                | PkgOpaque(..)),
                tok @ AirPkg(..),
            ) => ctx.ret_or_transfer(st, tok, AirPkgAggregate::new()),
            (Pkg(pkg), AirPkg(etok)) => ctx.proxy(pkg, etok),
            (Pkg(pkg), AirDoc(etok)) => ctx.proxy(pkg, etok),
            (st @ Pkg(..), tok @ AirBind(_)) => {
                ctx.try_ret_with_lookahead(st, tok)
            }

            // Expression
            (st @ (Pkg(_) | PkgTpl(_)), tok @ AirExpr(..)) => {
                ctx.ret_or_transfer(st, tok, AirExprAggregate::new())
            }
            (PkgExpr(expr), AirExpr(etok)) => ctx.proxy(expr, etok),
            (PkgExpr(expr), AirBind(etok)) => ctx.proxy(expr, etok),
            (PkgExpr(expr), AirDoc(etok)) => ctx.proxy(expr, etok),

            // Template
            (st @ (Pkg(_) | PkgExpr(_)), tok @ AirTpl(..)) => {
                ctx.ret_or_transfer(st, tok, AirTplAggregate::new())
            }
            (PkgTpl(tplst), AirTpl(ttok)) => ctx.proxy(tplst, ttok),
            (PkgTpl(tplst), AirBind(ttok)) => ctx.proxy(tplst, ttok),
            (PkgTpl(tplst), AirDoc(ttok)) => ctx.proxy(tplst, ttok),

            // Metavariables
            (st @ (PkgTpl(_) | PkgExpr(_)), tok @ AirMeta(..)) => {
                ctx.ret_or_transfer(st, tok, AirMetaAggregate::new())
            }
            (PkgMeta(meta), AirMeta(mtok)) => ctx.proxy(meta, mtok),
            (PkgMeta(meta), AirBind(mtok)) => ctx.proxy(meta, mtok),
            (PkgMeta(meta), AirDoc(mtok)) => ctx.proxy(meta, mtok),
            (PkgMeta(meta), tok @ (AirExpr(..) | AirTpl(..))) => {
                ctx.try_ret_with_lookahead(meta, tok)
            }

            // Opaque
            //
            // By having opaque object loading be its _own_ child parser,
            //   we ensure that the active package frame becomes held on the
            //   stack before loading e.g. opaque identifiers.
            // Since scope is determined by stack frames,
            //   this has the effect of ensuring that the package `st`
            //   becomes included in the identifier's scope.
            (st @ Pkg(_), tok @ AirIdent(..)) => {
                ctx.ret_or_transfer(st, tok, AirOpaqueAggregate::new())
            }
            (PkgOpaque(opaque), AirIdent(otok)) => ctx.proxy(opaque, otok),
            (
                PkgOpaque(_),
                tok @ (AirExpr(..) | AirBind(..) | AirTpl(..) | AirDoc(..)),
            ) => {
                // This is simply not expected at the time of writing,
                //   since this is used for importing object files.
                crate::diagnostic_panic!(
                    vec![
                        tok.span()
                            .internal_error("this is not an opaque identifier"),
                        tok.span().help(
                            "this may represent a problem with an object file"
                        )
                    ],
                    "expected opaque identifier, found {tok}",
                );
            }

            (
                st @ Root(_),
                tok @ (AirExpr(..) | AirBind(..) | AirTpl(..) | AirMeta(..)
                | AirDoc(..)),
            ) => Transition(st).err(AsgError::PkgExpected(tok.span())),

            // TODO: We will need to be more intelligent about this,
            //   since desugaring will produce metavariables in nested contexts,
            //     e.g. within an expression within a template.
            (st @ (Pkg(..) | PkgOpaque(..)), AirMeta(tok)) => {
                Transition(st).err(AsgError::UnexpectedMeta(tok.span()))
            }

            (
                st @ (Root(..) | PkgExpr(..) | PkgTpl(..) | PkgMeta(..)),
                AirIdent(tok),
            ) => {
                Transition(st).err(AsgError::UnexpectedOpaqueIdent(tok.name()))
            }
        }
    }

    fn is_accepting(&self, ctx: &Self::Context) -> bool {
        ctx.stack_ref().iter().all(|st| st.active_is_accepting(ctx))
            && self.active_is_accepting(ctx)
    }
}

impl AirAggregate {
    /// Whether the active parser is completed with active parsing.
    ///
    /// This method is used to determine whether control ought to be
    ///   transferred to a new child parser.
    ///
    /// If a child parser is active,
    ///   then its [`ParseState::is_accepting`] will be consulted.
    fn active_is_complete(&self, ctx: &<Self as ParseState>::Context) -> bool {
        use AirAggregate::*;

        match self {
            Uninit => false,

            // We can't be done with something we're not doing.
            // This is necessary to start the first child parser.
            Root(_) => false,

            Pkg(st) => st.is_accepting(ctx),
            PkgExpr(st) => st.is_accepting(ctx),
            PkgTpl(st) => st.is_accepting(ctx),
            PkgMeta(st) => st.is_accepting(ctx),
            PkgOpaque(st) => st.is_accepting(ctx),
        }
    }

    // Whether the parser is in an accepting state.
    fn active_is_accepting(&self, ctx: &<Self as ParseState>::Context) -> bool {
        use AirAggregate::*;

        match self {
            Uninit => false,

            // This must not recurse on `AirAggregate::is_accepting`,
            //   otherwise it'll be mutually recursive.
            Root(_) => true,

            Pkg(st) => st.is_accepting(ctx),
            PkgExpr(st) => st.is_accepting(ctx),
            PkgTpl(st) => st.is_accepting(ctx),
            PkgMeta(st) => st.is_accepting(ctx),
            PkgOpaque(st) => st.is_accepting(ctx),
        }
    }

    /// The rooting context for [`Ident`]s for the active parser.
    ///
    /// A value of [`None`] indicates that the current parser does not
    ///   support direct bindings,
    ///     but a parent context may
    ///       (see [`AirAggregateCtx::rooting_oi`]).
    fn active_rooting_oi(&self) -> Option<ObjectIndexToTree<Ident>> {
        use AirAggregate::*;

        match self {
            Uninit => None,

            // Root will serve as a pool of identifiers,
            //   but it can never _contain_ their definitions.
            // See `active_env_oi`.
            Root(_) => None,

            // Packages always serve as roots for identifiers
            //   (that is their entire purpose).
            Pkg(pkgst) => pkgst.active_pkg_oi().map(Into::into),

            // Expressions never serve as roots for identifiers;
            //   this will always fall through to the parent context.
            // Since the parent context is a package or a template,
            //   the next frame should succeed.
            PkgExpr(_) => None,

            // Identifiers bound while within a template definition context
            //   must bind to the eventual _expansion_ site,
            //     as if the body were pasted there.
            // Templates must therefore serve as containers for identifiers
            //   bound therein.
            PkgTpl(tplst) => tplst.active_tpl_oi().map(Into::into),

            // Identifiers cannot be rooted within metavariables since they
            //   contain only lexical information.
            PkgMeta(_) => None,

            // Loading of opaque objects happens within the context of the
            //   parent frame.
            // At the time of writing,
            //   that is only a package.
            PkgOpaque(_) => None,
        }
    }

    /// Active environment for identifier lookups.
    ///
    /// An environment is a superset of a container,
    ///   which is described by [`Self::active_rooting_oi`].
    /// For example,
    ///   [`Self::Root`] cannot own any identifiers,
    ///     but it can serve as a pool of references to them.
    fn active_env_oi(&self) -> Option<ObjectIndexTo<Ident>> {
        use AirAggregate::*;

        match self {
            Root(oi_root) => Some((*oi_root).into()),
            _ => self.active_rooting_oi().map(Into::into),
        }
    }

    /// The boundary associated with the active environment.
    ///
    /// If the active parser does not introduce its own scope
    ///   (if its environment is that of an ancestor frame)
    ///   then the boundary will be [`EnvBoundary::Transparent`].
    fn active_env_boundary(&self) -> EnvBoundary {
        use AirAggregate::*;
        use EnvBoundary::*;

        match self {
            // These are just parsing states,
            //   not environments.
            Uninit => Transparent,
            PkgOpaque(_) => Transparent,

            // Expressions and metadata are not containers,
            //   and do not introduce scope.
            PkgExpr(_) => Transparent,
            PkgMeta(_) => Transparent,

            // Packages and templates act as containers and so restrict
            //   identifier scope.
            Pkg(_) => Opaque,
            PkgTpl(_) => Opaque,

            // Root will absorb any identifiers that are Visibile
            //   immediately below it
            //     (which is `Pkg` in practice).
            Root(_) => Filter,
        }
    }

    /// Whether the active context represents an object that can be
    ///   lexically instantiated.
    ///
    /// Only containers that support _instantiation_ are able to contain
    ///   abstract identifiers.
    /// Instantiation triggers expansion,
    ///   which resolves metavariables and makes all abstract objects
    ///   therein concrete.
    fn is_lexically_instantiatible(&self) -> bool {
        use AirAggregate::*;

        match self {
            Uninit => false,

            // These objects cannot be instantiated,
            //   and so the abstract identifiers that they own would never
            //   be able to be made concrete.
            Root(_) => false,
            Pkg(_) => false,
            PkgExpr(_) => false,

            // Templates are the metalinguistic abstraction and are,
            //   at the time of writing,
            //   the only containers capable of instantiation.
            PkgTpl(_) => true,

            // Metavariables cannot own identifiers
            //   (they can only reference them).
            PkgMeta(_) => false,

            // If an object is opaque to us then we cannot possibly look
            //   into it to see what needs expansion.
            PkgOpaque(_) => false,
        }
    }
}

/// Behavior of an environment boundary when crossing environment upward
///   into a parent (less nested) environment.
///
/// An identifier's [`EnvScopeKind`] is a function of its current
///   [`EnvScopeKind`] and the boundary that it is crossing upward toward.
///
/// This system focuses heavily on identifier "shadows".
/// This boundary is therefore described in terms of material properties
///   with respect to light,
///     where [`EnvScopeKind`] represents a light source
///       (or absence thereof in the case of a shadow).
///
/// TAME does not explicitly track identifier scope at environments more
///   deeply nested than its definition site,
///     since the scope is always implicitly [`EnvScopeKind::Visible`] at
///     those levels.
///
/// Boundary rules are applied to an [`EnvScopeKind`] via
///   [`Self::cross_low_to_high`].
///
/// TODO: Diagrams are still only in test cases.
enum EnvBoundary {
    /// Identifier scope is unaffected by this boundary.
    ///
    /// From a boundary crossing function perspective,
    ///   this represents the identity function.
    Transparent,

    /// Identifier scope will end at this boundary,
    ///   casting a shadow from that point forward.
    ///
    /// Opaque boundaries are visualized as `|` in TAME's scope diagrams.
    Opaque,

    /// Visible identifiers will remain visible after crossing this
    ///   boundary,
    ///     but shadows will not cross.
    ///
    /// Filtering boundaries are visualized as `:` in TAME's scope
    ///   diagrams.
    Filter,
}

impl EnvBoundary {
    /// Apply boundary rules to the given `kind` when crossing the boundary
    ///   from a lower (more nested) environment toward a higher (less
    ///   nested) one.
    ///
    /// The definition (source code) of this function clearly states the
    ///   transformation rules and how they map to the scope diagram
    ///   representation.
    /// See [`Self`] for a summary.
    fn cross_low_to_high<T>(self, kind: EnvScopeKind<T>) -> EnvScopeKind<T> {
        use EnvBoundary::*;
        use EnvScopeKind::*;

        match (kind, self) {
            // x . x
            (kind, Transparent) => kind,

            // v | s
            (Visible(x), Opaque) => Shadow(x),
            // s | s
            (Shadow(x), Opaque) => Shadow(x),
            // _ | _
            (Hidden(x), Opaque) => Hidden(x),

            // v : v
            (Visible(x), Filter) => Visible(x),
            // s : _
            (Shadow(x), Filter) => Hidden(x),
            // _ : _
            (Hidden(x), Filter) => Hidden(x),
        }
    }
}

/// Additional parser context,
///   including the ASG and parser stack frames.
#[derive(Default)]
pub struct AirAggregateCtx {
    /// The ASG under construction by this parser.
    ///
    /// The ASG must be under exclusive ownership of this parser to ensure
    ///   that graph metadata
    ///     (e.g. indexes)
    ///     are accurate.
    asg: Asg,

    /// Held parser frames.
    stack: AirStack,

    /// The package currently being parsed,
    ///   if any.
    ///
    /// This is not necessarily the current compilation unit,
    ///   as the parser may be parsing imports.
    ooi_pkg: Option<ObjectIndex<Pkg>>,

    /// Cache of object scope at a given environment.
    ///
    // TODO: FxHashMap is used for other parts of TAMER,
    //   but here it's really used primarily for bucketing,
    //   so it may be worth profiling compared to alternatives.
    index: ScopeIndex,
}

impl Debug for AirAggregateCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The index provides far too much information to be useful,
        //   and slows down Parser trace generation.
        f.debug_struct("AirAggregateCtx")
            .field("asg", &self.asg)
            .field("stack", &self.stack)
            .field("ooi_pkg", &self.ooi_pkg)
            .field("index.len()", &self.index.len())
            .finish_non_exhaustive()
    }
}

/// Object scope index at a given environment.
type ScopeIndex = FxHashMap<
    (ObjectRelTy, SymbolId, ObjectIndex<Object>),
    EnvScopeKind<ObjectIndex<Object>>,
>;

/// Limit of the maximum number of held parser frames.
///
/// Note that this is the number of [`ParseState`]s held,
///   _not_ the depth of the graph at a given point.
/// The intent of this is to limit runaway recursion in the event of some
///   bug in the system;
///     while the input stream is certainly finite,
///       lookahead tokens cause recursion that does not provably
///       terminate.
///
/// This limit is arbitrarily large,
///   but hopefully such that no legitimate case will ever hit it.
const MAX_AIR_STACK_DEPTH: usize = 1024;

/// Held parser stack frames.
///
/// See [`AirAggregateCtx`] for more information.
pub type AirStack = StateStack<AirAggregate, MAX_AIR_STACK_DEPTH>;

impl AirAggregateCtx {
    fn asg_mut(&mut self) -> &mut Asg {
        self.as_mut()
    }

    fn asg_ref(&self) -> &Asg {
        self.as_ref()
    }

    fn stack(&mut self) -> &mut AirStack {
        &mut self.stack
    }

    fn stack_ref(&self) -> &AirStack {
        &self.stack
    }

    /// Return control to the parser atop of the stack if `st` is an
    ///   accepting state,
    ///     otherwise transfer control to a new parser `to`.
    ///
    /// This serves as a balance with the behavior of [`Self::proxy`].
    /// Rather than checking for an accepting state after each proxy,
    ///   or having the child parsers return to the top stack frame once
    ///     they have completed,
    ///   we leave the child parser in place to potentially handle more
    ///     tokens of the same type.
    /// For example,
    ///   adjacent expressions can re-use the same parser rather than having
    ///   to pop and push for each sibling.
    ///
    /// Consequently,
    ///   this means that a parser may be complete when we need to push and
    ///   transfer control to another parser.
    /// Before pushing,
    ///   we first check to see if the parser atop of the stack is in an
    ///   accepting state.
    /// If so,
    ///   then we are a sibling,
    ///   and so instead of proceeding with instantiating a new parser,
    ///   we return to the one atop of the stack and delegate to it.
    ///
    /// If `st` is _not_ in an accepting state,
    ///   that means that we are a _child_;
    ///     we then set aside the state `st` on the stack and transfer
    ///     control to the child `to`.
    ///
    /// See also [`Self::proxy`].
    fn ret_or_transfer<S: Into<AirAggregate>, SB: Into<AirAggregate>>(
        &mut self,
        st: S,
        tok: impl Token + Into<Air>,
        to: SB,
    ) -> TransitionResult<AirAggregate> {
        let st_super = st.into();

        if st_super.active_is_complete(self) {
            // TODO: error (this should never happen, so maybe panic instead?)
            self.stack().ret_or_dead(AirAggregate::Uninit, tok)
        } else {
            self.stack().transfer_with_ret(
                Transition(st_super),
                Transition(to.into()).incomplete().with_lookahead(tok),
            )
        }
    }

    /// Attempt to return to the previous stack frame,
    ///   using the provided token as a token of lookahead.
    ///
    /// If the provided `st` is in an accepting state,
    ///   then control will return to the frame atop of the stack.
    /// Otherwise,
    ///   an unexpected token was found,
    ///   and a dead state transition will be yielded,
    ///     leaving `st` unchanged.
    fn try_ret_with_lookahead<S: Into<AirAggregate>>(
        &mut self,
        st: S,
        tok: impl Token + Into<Air>,
    ) -> TransitionResult<AirAggregate> {
        let st_super = st.into();

        if st_super.active_is_complete(self) {
            // TODO: error (this should never happen, so maybe panic instead?)
            self.stack().ret_or_dead(AirAggregate::Uninit, tok)
        } else {
            Transition(st_super).dead(tok)
        }
    }

    /// Proxy `tok` to `st`,
    ///   returning to the state atop of the stack if parsing reaches a dead
    ///   state.
    ///
    /// See also [`Self::ret_or_transfer`].
    fn proxy<S: ParseState<Super = AirAggregate, Context = Self>>(
        &mut self,
        st: S,
        tok: impl Token + Into<S::Token>,
    ) -> TransitionResult<AirAggregate> {
        st.delegate_child(tok.into(), self, |_deadst, tok, ctx| {
            // TODO: error (this should never happen, so maybe panic instead?)
            ctx.stack().ret_or_dead(AirAggregate::Uninit, tok)
        })
    }

    /// Index the provided symbol `name` as representing the
    ///   [`ObjectIndex`] in the immediate environment `imm_env`.
    ///
    /// Intersecting shadows are permitted,
    ///   but the existing index will be kept in tact.
    /// This behavior may change in the future to permit diagnostic messages
    ///   that list _all_ identifiers in the event of a shadowing error.
    ///
    /// If indexing fails,
    ///   the _existing_ [`ObjectIndex`] will be returned,
    ///     leaving the caller to determine how to react.
    fn try_index<
        O: ObjectRelatable,
        OS: ObjectIndexRelTo<O>,
        S: Into<SymbolId>,
    >(
        index: &mut ScopeIndex,
        imm_env: OS,
        name: S,
        eoi: EnvScopeKind<ObjectIndex<O>>,
    ) -> Result<(), ObjectIndex<O>> {
        let sym = name.into();
        let ient = index.entry((O::rel_ty(), sym, imm_env.widen()));

        use Entry::*;
        use EnvScopeKind::*;

        match (ient, eoi) {
            (Vacant(_), Hidden(_)) => Ok(()),

            (Vacant(ent), Shadow(_) | Visible(_)) => {
                ent.insert(eoi.map(ObjectIndex::widen));
                Ok(())
            }

            (Occupied(ent), eoi) => match (ent.get(), eoi) {
                // This ought to be omitted from the index,
                //   as shown above.
                (Hidden(oi), _) => diagnostic_unreachable!(
                    vec![oi.internal_error(
                        "this should not have been indexed as Hidden"
                    )],
                    "unexpected Hidden scope index entry",
                ),

                (Visible(_), Hidden(_)) => Ok(()),

                (Shadow(_), Hidden(_) | Shadow(_)) => Ok(()),

                (Shadow(oi), Visible(_))
                | (Visible(oi), Shadow(_) | Visible(_)) => {
                    Err(oi.must_narrow_into::<O>())
                }
            },
        }
    }

    /// Index the provided symbol `name` as representing the
    ///   [`ObjectIndex`] in the immediate environment `imm_env`.
    ///
    /// An index does not require the existence of an edge,
    ///   but an index may only be created if an edge `imm_env->oi` _could_
    ///   be constructed.
    ///
    /// This index permits `O(1)` object lookups.
    /// The term "immediate environment" is meant to convey that this index
    ///   applies only to the provided `imm_env` node and does not
    ///   propagate to any other objects that share this environment.
    ///
    /// After an object is indexed it is not expected to be re-indexed
    ///   to another node.
    /// Debug builds contain an assertion that will panic in this instance.
    fn index<O: ObjectRelatable, OS: ObjectIndexRelTo<O>, S: Into<SymbolId>>(
        index: &mut ScopeIndex,
        imm_env: OS,
        name: S,
        eoi: EnvScopeKind<ObjectIndex<O>>,
    ) {
        let sym = name.into();
        let prev = Self::try_index(index, imm_env, sym, eoi);

        // We should never overwrite indexes
        #[allow(unused_variables)] // used only for debug
        #[allow(unused_imports)]
        if let Err(prev_oi) = prev {
            use crate::fmt::{DisplayWrapper, TtQuote};
            crate::debug_diagnostic_panic!(
                vec![
                    imm_env.widen().note("at this scope boundary"),
                    prev_oi.note("previously indexed identifier was here"),
                    eoi.internal_error(
                        "this identifier has already been indexed at the above scope boundary"
                    ),
                    eoi.help(
                        "this is a bug in the system responsible for analyzing \
                            identifier scope;"
                    ),
                    eoi.help(
                        "  you can try to work around it by duplicating the definition of "
                    ),
                    eoi.help(
                        format!(
                            "  {} as a _new_ identifier with a different name.",
                            TtQuote::wrap(sym),
                        )
                    ),
                ],
                "re-indexing of identifier at scope boundary",
            );
        }
    }

    /// Create a new rooted package of the given canonical name and record
    ///   it as the active package.
    ///
    /// A canonical package name is a path relative to the project root.
    ///
    /// This operation will fail if a package of the same name has already
    ///   been declared.
    fn pkg_begin(
        &mut self,
        start: Span,
        name: SPair,
    ) -> Result<ObjectIndex<Pkg>, AsgError> {
        let oi_root = self.asg.root(start);
        let oi_pkg = self.asg.create(Pkg::new_canonical(start, name)?);
        let eoi_pkg = EnvScopeKind::Visible(oi_pkg);

        Self::try_index(&mut self.index, oi_root, name, eoi_pkg).map_err(
            |oi_prev| {
                let prev = oi_prev.resolve(&self.asg);

                // unwrap note: a canonical name must exist for this error to
                //   have been thrown,
                //     but this will at least not blow up if something really
                //     odd happens.
                AsgError::PkgRedeclare(prev.canonical_name(), name)
            },
        )?;

        oi_pkg.root(&mut self.asg)?;
        self.ooi_pkg.replace(oi_pkg);

        Ok(oi_pkg)
    }

    /// Indicate that there is no longer any active package.
    fn pkg_clear(&mut self) {
        self.ooi_pkg.take();
    }

    /// The active package if any.
    fn pkg_oi(&self) -> Option<ObjectIndex<Pkg>> {
        self.ooi_pkg
    }

    /// The active container (rooting context) for [`Ident`]s.
    ///
    /// The integer value returned represents the stack offset at which the
    ///   rooting index was found,
    ///     with `0` representing the package.
    ///
    /// A value of [`None`] indicates that no bindings are permitted in the
    ///   current context.
    fn rooting_oi(&self) -> Option<(&AirAggregate, ObjectIndexToTree<Ident>)> {
        self.stack
            .iter()
            .rev()
            .find_map(|st| st.active_rooting_oi().map(|oi| (st, oi)))
    }

    /// The active container (rooting context) for _abstract_ [`Ident`]s.
    ///
    /// Only containers that support _instantiation_ are able to contain
    ///   abstract identifiers.
    /// Instantiation triggers expansion,
    ///   which resolves metavariables and makes all abstract objects
    ///   therein concrete.
    ///
    /// This utilizes [`Self::rooting_oi`] to determine the active rooting
    ///   context.
    /// If that context does not support instantiation,
    ///   [`None`] is returned.
    /// This method will _not_ continue looking further up the stack for a
    ///   context that is able to be instantiated,
    ///     since that would change the parent of the binding.
    fn instantiable_rooting_oi(
        &self,
    ) -> Option<(&AirAggregate, ObjectIndexToTree<Ident>)> {
        self.rooting_oi()
            .filter(|(st, _)| st.is_lexically_instantiatible())
    }

    /// The active dangling expression context for [`Expr`]s.
    ///
    /// A value of [`None`] indicates that expressions are not permitted to
    ///   dangle in the current context
    ///     (and so must be identified).
    fn dangling_expr_oi(&self) -> Option<ObjectIndexTo<Expr>> {
        use AirAggregate::*;

        self.stack.iter().rev().find_map(|st| match st {
            Uninit => None,

            // It should never be possible to define expressions directly in
            //   Root.
            Root(_) => None,

            // A dangling expression in a package context would be
            //   unreachable.
            // There should be no parent frame and so this will fail to find
            //   a value.
            Pkg(_) => None,

            // Expressions may always contain other expressions,
            //   and so this method should not be consulted in such a
            //   context.
            // Nonetheless,
            //   fall through to the parent frame and give a correct answer.
            PkgExpr(_) => None,

            // Templates serve as containers for dangling expressions,
            //   since they may expand into an context where they are not
            //   considered to be dangling.
            PkgTpl(tplst) => tplst.active_tpl_oi().map(Into::into),

            // No definitions can be made within metavariables.
            PkgMeta(_) => None,

            // Expressions are transparent definitions,
            //   not opaque,
            //   and so not permitted in this context.
            PkgOpaque(_) => None,
        })
    }

    /// The active expansion target (splicing context) for [`Tpl`]s.
    ///
    /// A value of [`None`] indicates that template expansion is not
    ///   permitted in this current context.
    fn expansion_oi(&self) -> Option<ObjectIndexTo<Tpl>> {
        use AirAggregate::*;

        self.stack.iter().rev().find_map(|st| match st {
            Uninit => None,
            Root(_) => None,
            Pkg(pkg_st) => pkg_st.active_pkg_oi().map(Into::into),
            PkgExpr(exprst) => exprst.active_expr_oi().map(Into::into),
            PkgTpl(tplst) => tplst.active_tpl_oi().map(Into::into),

            // Metavariables do not historically support template
            //   applications within their body.
            // Support for such a feature can be evaluated in the future.
            PkgMeta(_) => None,

            // Templates _could_ conceptually expand into opaque objects,
            //   but the source language of TAME provides no mechanism to do
            //   such a thing,
            //     and so it'd be best to leave this alone unless it's
            //     actually needed.
            PkgOpaque(_) => None,
        })
    }

    /// Root a concrete identifier using the [`Self::rooting_oi`] atop of
    ///   the stack.
    ///
    /// This definition will index the identifier into the proper
    ///   environments,
    ///     giving it scope.
    /// If the identifier is abstract,
    ///   it is important to use [`Self::defines_abstract`] instead so that
    ///   the metavariable that the identifier references will not be
    ///   indexed as the binding `name`.
    fn defines_concrete(
        &mut self,
        binding_name: SPair,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        let (_, oi_root) = self
            .rooting_oi()
            .ok_or(AsgError::InvalidBindContext(binding_name))?;

        self.lookup_lexical_or_missing(binding_name)
            .defined_by(self.asg_mut(), oi_root)
    }

    /// Define an abstract identifier within the context of a container that
    ///   is able to hold dangling objects.
    ///
    /// If the identifier is concrete,
    ///   then it is important to use [`Self::defines_concrete`] instead to
    ///   ensure that the identifier has its scope computed and indexed.
    ///
    /// TODO: This is about to evolve;
    ///   document further.
    fn defines_abstract(
        &mut self,
        meta_name: SPair,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        match self.instantiable_rooting_oi() {
            // The container cannot be instantiated and so there is no
            //   chance that this expression will be expanded in the future.
            None => {
                // Since we do not have an abstract container,
                //   the nearest container (if any) is presumably concrete,
                //   so let's reference that in the hope of making the error
                //     more informative.
                // Note that this _does_ re-search the stack,
                //   but this is an error case that should seldom occur.
                // If it's a problem,
                //   we can have `instantiable_rooting_oi` retain
                //   information.
                let rooting_span = self
                    .rooting_oi()
                    .map(|(_, oi)| oi.widen().resolve(self.asg_ref()).span());

                // Note that we _discard_ the attempted bind token
                //   and so remain in a dangling state.
                Err(AsgError::InvalidAbstractBindContext(
                    meta_name,
                    rooting_span,
                ))
            }

            // We root a new identifier in the instantiable container,
            //   but we do not index it,
            //   since its name is not known until instantiation.
            Some((_, oi_root)) => {
                let oi_meta_ident = self.lookup_lexical_or_missing(meta_name);

                oi_meta_ident
                    .new_abstract_ident(self.asg_mut(), meta_name.span())
                    .and_then(|oi| oi.defined_by(self.asg_mut(), oi_root))
            }
        }
    }

    /// Attempt to retrieve an identifier and its scope information from the
    ///   graph by name relative to the environment `env`.
    ///
    /// See [`Self::lookup`] for more information.
    #[cfg(test)]
    fn env_scope_lookup_raw<O: ObjectRelatable>(
        &self,
        env: impl ObjectIndexRelTo<O>,
        name: SPair,
    ) -> Option<EnvScopeKind<ObjectIndex<O>>> {
        self.lookup_raw(env, name)
    }

    /// Resolve an identifier at the scope of the provided environment.
    ///
    /// If the identifier is not in scope at `env`,
    ///   [`None`] will be returned.
    #[cfg(test)]
    fn env_scope_lookup<O: ObjectRelatable>(
        &self,
        env: impl ObjectIndexRelTo<O>,
        name: SPair,
    ) -> Option<ObjectIndex<O>> {
        self.env_scope_lookup_raw(env, name)
            .and_then(EnvScopeKind::in_scope)
            .map(EnvScopeKind::into_inner)
    }

    /// Resolve an identifier at the scope of the provided environment and
    ///   retrieve its definition.
    ///
    /// If the identifier is not in scope or does not have a definition,
    ///   [`None`] will be returned;
    ///     the caller cannot distinguish between the two using this method;
    ///     see [`Self::env_scope_lookup`] if that distinction is important.
    #[cfg(test)]
    fn env_scope_lookup_ident_dfn<O: ObjectRelatable>(
        &self,
        env: impl ObjectIndexRelTo<Ident>,
        name: SPair,
    ) -> Option<ObjectIndex<O>>
    where
        Ident: ObjectRelTo<O>,
    {
        self.env_scope_lookup::<Ident>(env, name)
            .and_then(|oi| oi.definition(self.asg_ref()))
    }

    /// Attempt to retrieve an identifier from the graph by name relative to
    ///   the immediate environment `imm_env`.
    ///
    /// Since only identifiers carry a name,
    ///   this method cannot be used to retrieve all possible objects on the
    ///   graph---for
    ///     that, see [`Asg::get`].
    #[inline]
    fn lookup<O: ObjectRelatable>(
        &self,
        imm_env: impl ObjectIndexRelTo<O>,
        id: SPair,
    ) -> Option<ObjectIndex<O>> {
        self.lookup_raw(imm_env, id)
            .and_then(EnvScopeKind::in_scope)
            .map(EnvScopeKind::into_inner)
    }

    /// Attempt to retrieve an identifier and its scope information from the
    ///   graph by name relative to the immediate environment `imm_env`.
    ///
    /// See [`Self::lookup`] for more information.
    #[inline]
    fn lookup_raw<O: ObjectRelatable>(
        &self,
        imm_env: impl ObjectIndexRelTo<O>,
        id: SPair,
    ) -> Option<EnvScopeKind<ObjectIndex<O>>> {
        // The type `O` is encoded into the index on [`Self::index`] and so
        //   should always be able to be narrowed into the expected type.
        // If this invariant somehow does not hold,
        //   then the system will panic when the object is resolved.
        // Maybe future Rust will have dependent types that allow for better
        //   static assurances.
        self.index
            .get(&(O::rel_ty(), id.symbol(), imm_env.widen()))
            .map(|&eoi| {
                eoi.map(|oi| oi.overwrite(id.span()).must_narrow_into::<O>())
            })
    }

    /// Attempt to locate a lexically scoped identifier in the current stack,
    ///   or create a new one if missing.
    ///
    /// Since shadowing is not permitted
    ///   (but local identifiers are),
    ///   we can reduce the cost of lookups for the majority of identifiers
    ///     by beginning at the root and continuing down into the narrowest
    ///     lexical scope until we find what we're looking for.
    ///
    /// Note that the global environment,
    ///   represented by the root,
    ///   is a pool of identifiers from all packages;
    ///     it does not form a hierarchy and local identifiers will not be
    ///     indexed outside of their package hierarchy,
    ///       so we'll have to continue searching for those.
    ///
    /// The provided name's span is used to seed the missing object with
    ///   some sort of context to aid in debugging why a missing object
    ///   was introduced to the graph.
    /// The provided span will be used by the returned [`ObjectIndex`] even
    ///   if an object exists on the graph,
    ///     which can be used for retaining information on the location that
    ///     requested the object.
    /// To retrieve the span of a previously declared object,
    ///   you must resolve the [`ObjectIndex`] and inspect it.
    fn lookup_lexical_or_missing(&mut self, name: SPair) -> ObjectIndex<Ident> {
        self.stack
            .iter()
            .filter_map(|st| st.active_env_oi())
            .find_map(|oi| self.lookup(oi, name))
            .unwrap_or_else(|| self.create_env_indexed_ident(name))
    }

    /// Index an identifier within its environment.
    ///
    /// TODO: More information as this is formalized.
    fn create_env_indexed_ident(&mut self, name: SPair) -> ObjectIndex<Ident> {
        let oi_ident = self.asg.create(Ident::declare(name));

        self.stack
            .iter()
            .rev()
            .filter_map(|frame| frame.active_env_oi().map(|oi| (oi, frame)))
            .fold(None, |oeoi, (imm_oi, frame)| {
                let eoi_next = oeoi
                    .map(|eoi| {
                        frame.active_env_boundary().cross_low_to_high(eoi)
                    })
                    .unwrap_or(EnvScopeKind::Visible(oi_ident));

                // TODO: Let's find this a better home.
                match eoi_next {
                    // There is no use in indexing something that will be
                    //   filtered out on retrieval.
                    EnvScopeKind::Hidden(_) => (),
                    _ => Self::index(&mut self.index, imm_oi, name, eoi_next),
                }

                Some(eoi_next)
            });

        oi_ident
    }

    /// Consume the context and yield the inner [`Asg`].
    ///
    /// This indicates that all package parsing has been completed and that
    ///   the ASG contains complete information about the program sources
    ///   for the requested compilation unit.
    pub fn finish(self) -> Asg {
        self.asg
    }
}

/// Property of identifier scope within a given environment.
///
/// An _environment_ is the collection of identifiers associated with a
///   container object.
/// Environments stack,
///   such that an environment inherits the identifiers of its parent.
///
/// The _scope_ of an identifier is defined by what environments can "see"
///   that identifier.
/// For the purposes of TAME's analysis,
///   we care only about the global environment and shadowing.
///
/// The relationship between identifier scope and environment can be
///   visualized as a two-dimensional table with the environments forming
///   layers along the x-axes,
///     and scopes slicing those layers along the y-axies.
///
/// TODO: Example visualization.
///
/// Root and Global Environment
/// ===========================
/// Identifiers are pooled without any defined hierarchy at the root.
///
/// An identifier that is part of a pool must be unique.
/// Since there is no hierarchy,
///   the system should not suggest that shadowing is not permitted and
///   should insteam emphasize that such an identifier must be unique
///   globally.
///
/// An identifier's scope can be further refined to provide more useful
///   diagnostic messages by descending into the package in which it is
///   defined and evaluating scope relative to the package.
#[derive(Debug, PartialEq, Copy, Clone)]
pub(super) enum EnvScopeKind<T = ObjectIndex<Object>> {
    /// This environment owns the identifier,
    ///   is descended from an environment that does,
    ///   or is a global pool of identifiers.
    Visible(T),

    /// Identifier in this environment is a shadow of a deeper environment.
    ///
    /// An identifier is said to cast a shadow on environments higher in its
    ///   hierarchy.
    /// Since shadowing is not permitted in TAME,
    ///   this can be used to present useful diagnostic information to the
    ///   user.
    ///
    /// A shadow can be used to check for identifier conflicts,
    ///   but it cannot be used for lookup;
    ///     this environment should be filtered out of this identifier's
    ///     scope.
    Shadow(T),

    /// The identifier is not in scope.
    Hidden(T),
}

impl<T> EnvScopeKind<T> {
    pub fn into_inner(self) -> T {
        use EnvScopeKind::*;

        match self {
            Shadow(x) | Visible(x) | Hidden(x) => x,
        }
    }

    /// Whether this represents an identifier that is in scope.
    pub fn in_scope(self) -> Option<Self> {
        use EnvScopeKind::*;

        match self {
            Visible(_) => Some(self),
            Shadow(_) | Hidden(_) => None,
        }
    }
}

impl<T> AsRef<T> for EnvScopeKind<T> {
    fn as_ref(&self) -> &T {
        use EnvScopeKind::*;

        match self {
            Shadow(x) | Visible(x) | Hidden(x) => x,
        }
    }
}

impl<T, U> Functor<T, U> for EnvScopeKind<T> {
    type Target = EnvScopeKind<U>;

    fn map(self, f: impl FnOnce(T) -> U) -> Self::Target {
        use EnvScopeKind::*;

        match self {
            Shadow(x) => Shadow(f(x)),
            Visible(x) => Visible(f(x)),
            Hidden(x) => Hidden(f(x)),
        }
    }
}

impl<T> From<EnvScopeKind<T>> for Span
where
    T: Into<Span>,
{
    fn from(kind: EnvScopeKind<T>) -> Self {
        kind.into_inner().into()
    }
}

impl AsMut<AirAggregateCtx> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut AirAggregateCtx {
        self
    }
}

impl AsRef<Asg> for AirAggregateCtx {
    fn as_ref(&self) -> &Asg {
        &self.asg
    }
}

impl AsMut<Asg> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut Asg {
        &mut self.asg
    }
}

impl AsMut<AirStack> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut AirStack {
        &mut self.stack
    }
}

impl From<Asg> for AirAggregateCtx {
    fn from(asg: Asg) -> Self {
        Self {
            asg,
            ..Default::default()
        }
    }
}

#[cfg(test)]
pub mod test;
