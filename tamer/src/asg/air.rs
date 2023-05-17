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
//!
//! AIR is _not_ intended to replace the API of the ASG---it
//!   is intended as a termination point for the parsing pipeline,
//!     and as such implements a subset of the ASG's API that is suitable
//!     for aggregating raw data from source and object files.
//! Given that it does so little and is so close to the [`Asg`] API,
//!   one might say that the abstraction is as light as air,
//!   but that would surely result in face-palming and so we're not going
//!     air such cringeworthy dad jokes here.

use super::{
    graph::object::{Object, ObjectIndexTo, ObjectIndexToTree, Pkg, Root, Tpl},
    Asg, AsgError, Expr, Ident, ObjectIndex,
};
use crate::{
    f::Functor,
    parse::{prelude::*, StateStack},
    span::Span,
    sym::SymbolId,
};
use std::fmt::{Debug, Display};

#[macro_use]
mod ir;
pub use ir::Air;

mod expr;
mod pkg;
mod tpl;
use expr::AirExprAggregate;
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
                write!(f, "building a template: {tpl}",)
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

impl ParseState for AirAggregate {
    type Token = Air;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;

    /// Destination [`Asg`] that this parser lowers into.
    ///
    /// This ASG will be yielded by [`crate::parse::Parser::finalize`].
    type PubContext = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use ir::{AirSubsets::*, AirTodo::*};
        use AirAggregate::*;

        match (self, tok.into()) {
            // Initialize the parser with the graph root.
            // The graph may contain multiple roots in the future to support
            //   cross-version analysis.
            (Uninit, tok) => Transition(Root(ctx.asg_mut().root(tok.span())))
                .incomplete()
                .with_lookahead(tok),

            (st, AirTodo(Todo(_))) => Transition(st).incomplete(),

            // Package
            (st @ (Root(..) | PkgExpr(..) | PkgTpl(..)), tok @ AirPkg(..)) => {
                ctx.ret_or_transfer(st, tok, AirPkgAggregate::new())
            }
            (Pkg(pkg), AirPkg(etok)) => ctx.proxy(pkg, etok),
            (Pkg(pkg), AirBind(etok)) => ctx.proxy(pkg, etok),
            (Pkg(pkg), AirIdent(etok)) => ctx.proxy(pkg, etok),
            (Pkg(pkg), AirDoc(etok)) => ctx.proxy(pkg, etok),

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

            (
                st @ Root(_),
                tok @ (AirExpr(..) | AirBind(..) | AirTpl(..) | AirDoc(..)),
            ) => Transition(st).err(AsgError::PkgExpected(tok.span())),

            (st @ (Root(..) | PkgExpr(..) | PkgTpl(..)), AirIdent(tok)) => {
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

    /// Adjust a [`EnvScopeKind`] while crossing an environment boundary
    ///   into `self`.
    ///
    /// An identifier is _visible_ at the environment in which it is defined.
    /// This identifier casts a _shadow_ to lower environments,
    ///   with the exception of the root.
    /// The _root_ will absorb adjacent visible identifiers into a _pool_,
    ///   which is distinct from the hierarchy that is otherwise created at
    ///   the package level and lower.
    fn env_cross_boundary_into<T>(
        &self,
        kind: EnvScopeKind<T>,
    ) -> EnvScopeKind<T> {
        use AirAggregate::*;
        use EnvScopeKind::*;

        match (self, kind) {
            // This is not an environment.
            (Uninit, kind) => kind,

            // Hidden is a fixpoint.
            (_, kind @ Hidden(_)) => kind,

            // Expressions do not introduce their own environment
            //   (they are not containers)
            //   and so act as an identity function.
            (PkgExpr(_), kind) => kind,

            // A visible identifier will always cast a shadow in one step.
            // A shadow will always be cast (propagate) until the root.
            (Pkg(_) | PkgTpl(_), Visible(x) | Shadow(x)) => Shadow(x),

            // Above we see that Visual will always transition to Shadow in
            //   one step.
            // Consequently,
            //   Visible at Root means that we're a package-level Visible,
            //     which must contribute to the pool.
            (Root(_), Visible(x)) => Visible(x),

            // If we're _not_ Visible at the root,
            //   then we're _not_ a package-level definition,
            //     and so we should _not_ contribute to the pool.
            (Root(_), Shadow(x)) => Hidden(x),
        }
    }
}

/// Additional parser context,
///   including the ASG and parser stack frames.
///
/// [`ObjectIndex`] lookups perform reverse linear searches beginning from
///   the last stack frame until a non-[`None`] value is found;
///     this creates an environment whereby inner contexts shadow outer.
/// Missing values create holes,
///   much like a prototype chain.
/// In practice,
///   this should only have to search the last two frames.
#[derive(Debug, Default)]
pub struct AirAggregateCtx(Asg, AirStack, Option<ObjectIndex<Pkg>>);

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
        let Self(_, stack, _) = self;
        stack
    }

    fn stack_ref(&self) -> &AirStack {
        let Self(_, stack, _) = self;
        stack
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

    /// Create a new rooted package and record it as the active package.
    fn pkg_begin(
        &mut self,
        start: Span,
        name: SPair,
    ) -> Result<ObjectIndex<Pkg>, AsgError> {
        let Self(asg, _, pkg) = self;

        let oi_root = asg.root(start);
        let oi_pkg = oi_root.create_pkg(asg, start, name)?;

        pkg.replace(oi_pkg);
        Ok(oi_pkg)
    }

    /// Indicate that there is no longer any active package.
    fn pkg_clear(&mut self) {
        let Self(_, _, pkg) = self;
        pkg.take();
    }

    /// The active package if any.
    fn pkg_oi(&self) -> Option<ObjectIndex<Pkg>> {
        match self {
            Self(_, _, oi) => *oi,
        }
    }

    /// The active container (rooting context) for [`Ident`]s.
    ///
    /// The integer value returned represents the stack offset at which the
    ///   rooting index was found,
    ///     with `0` representing the package.
    ///
    /// A value of [`None`] indicates that no bindings are permitted in the
    ///   current context.
    fn rooting_oi(&self) -> Option<ObjectIndexToTree<Ident>> {
        let Self(_, stack, _) = self;

        stack.iter().rev().find_map(|st| st.active_rooting_oi())
    }

    /// The active dangling expression context for [`Expr`]s.
    ///
    /// A value of [`None`] indicates that expressions are not permitted to
    ///   dangle in the current context
    ///     (and so must be identified).
    fn dangling_expr_oi(&self) -> Option<ObjectIndexTo<Expr>> {
        use AirAggregate::*;
        let Self(_, stack, _) = self;

        stack.iter().rev().find_map(|st| match st {
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
        })
    }

    /// The active expansion target (splicing context) for [`Tpl`]s.
    ///
    /// A value of [`None`] indicates that template expansion is not
    ///   permitted in this current context.
    fn expansion_oi(&self) -> Option<ObjectIndexTo<Tpl>> {
        use AirAggregate::*;
        let Self(_, stack, _) = self;

        stack.iter().rev().find_map(|st| match st {
            Uninit => None,
            Root(_) => None,
            Pkg(pkg_st) => pkg_st.active_pkg_oi().map(Into::into),
            PkgExpr(exprst) => exprst.active_expr_oi().map(Into::into),
            PkgTpl(tplst) => tplst.active_tpl_oi().map(Into::into),
        })
    }

    /// Root an identifier using the [`Self::rooting_oi`] atop of the stack.
    fn defines(&mut self, name: SPair) -> Result<ObjectIndex<Ident>, AsgError> {
        let oi_root = self
            .rooting_oi()
            .ok_or(AsgError::InvalidBindContext(name))?;

        Ok(self.lookup_lexical_or_missing(name).add_edge_from(
            self.asg_mut(),
            oi_root,
            None,
        ))
    }

    /// Attempt to locate a lexically scoped identifier,
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
    fn lookup_lexical_or_missing(&mut self, name: SPair) -> ObjectIndex<Ident> {
        let Self(asg, stack, _) = self;

        stack
            .iter()
            .filter_map(|st| st.active_env_oi())
            .find_map(|oi| asg.lookup(oi, name))
            .unwrap_or_else(|| self.create_env_indexed_ident(name))
    }

    /// Index an identifier within its environment.
    ///
    /// TODO: More information as this is formalized.
    fn create_env_indexed_ident(&mut self, name: SPair) -> ObjectIndex<Ident> {
        let Self(asg, stack, _) = self;
        let oi_ident = asg.create(Ident::declare(name));

        // TODO: This will need the active OI to support `AirIdent`s
        stack
            .iter()
            .rev()
            .filter_map(|frame| frame.active_env_oi().map(|oi| (oi, frame)))
            .fold(None, |oeoi, (imm_oi, frame)| {
                let eoi_next = oeoi
                    .map(|eoi| frame.env_cross_boundary_into(eoi))
                    .unwrap_or(EnvScopeKind::Visible(oi_ident));

                // TODO: Let's find this a better home.
                match eoi_next {
                    // There is no use in indexing something that will be
                    //   filtered out on retrieval.
                    EnvScopeKind::Hidden(_) => (),
                    _ => asg.index(imm_oi, name, eoi_next),
                }

                Some(eoi_next)
            });

        oi_ident
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
        match self {
            Self(asg, _, _) => asg,
        }
    }
}

impl AsMut<Asg> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut Asg {
        match self {
            Self(asg, _, _) => asg,
        }
    }
}

impl AsMut<AirStack> for AirAggregateCtx {
    fn as_mut(&mut self) -> &mut AirStack {
        match self {
            Self(_, stack, _) => stack,
        }
    }
}

impl From<AirAggregateCtx> for Asg {
    fn from(ctx: AirAggregateCtx) -> Self {
        match ctx {
            AirAggregateCtx(asg, _, _) => asg,
        }
    }
}

impl From<Asg> for AirAggregateCtx {
    fn from(asg: Asg) -> Self {
        Self(asg, Default::default(), None)
    }
}

#[cfg(test)]
mod test;
