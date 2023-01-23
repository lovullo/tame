// Objects represented on ASG
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

//! Objects represented by the ASG.
//!
//! Dynamic Object Types and Narrowing
//! ==================================
//! Unlike the functional lowering pipeline that precedes it,
//!   the ASG is a mutable, ever-evolving graph of dynamic data.
//! The ASG does not benefit from the same type-level guarantees that the
//!   rest of the system does at compile-time.
//!
//! However,
//!   we _are_ able to utilize the type system to ensure statically that
//!   there exists no code path that is able to generated an invalid graph
//!     (a graph that does not adhere to its ontology as described below).
//!
//! Any node on the graph can represent any type of [`Object`].
//! An [`ObjectIndex`] contains an index into the graph,
//!   _not_ a reference;
//!     it is therefore possible (though avoidable) for objects to be
//!       modified out from underneath references.
//! Consequently,
//!   we cannot trust that an [`ObjectIndex`] is what we expect it to be when
//!   performing an operation on the graph using that index,
//!     though the system is designed to uphold an invariant that the _type_
//!     of [`Object`] cannot be changed.
//!
//! To perform an operation on a particular type of object,
//!   we must first _narrow_ it.
//! Narrowing converts from the [`Object`] sum type into a more specific
//!   inner type,
//!     such as [`Ident`] or [`Expr`].
//! This operation _should_,
//!   if the compiler is operating correctly,
//!   always succeed,
//!     because the type of object should always match our expectations;
//!       the explicit narrowing is to ensure memory safety in case that
//!       assumption does not hold.
//! To facilitate this in a convenient way,
//!   operations returning an [`ObjectIndex`] will be associated with an
//!   [`ObjectKind`] that will be used to automatically perform narrowing on
//!   subsequent operations using that [`ObjectIndex`].
//!
//! Since a type mismatch represents a bug in the compiler,
//!   the API favors [`Result`]-free narrowing rather than burdening every
//!   caller with additional complexity---we
//!     will attempt to narrow and panic in the event of a failure,
//!       including a diagnostic message that helps to track down the issue
//!       using whatever [`Span`]s we have available.
//! [`ObjectIndex`] is associated with a span derived from the point of its
//!   creation to handle this diagnostic situation automatically.
//!
//! Edge Types and Narrowing
//! ------------------------
//! Unlike nodes,
//!   edges may reference [`Object`]s of many different types,
//!   as defined by the graph's ontology.
//!
//! The set [`ObjectKind`] types that may be related _to_
//!   (via edges)
//!   from other objects are the variants of [`ObjectRelTy`].
//! Each such [`ObjectKind`] must implement [`ObjectRelatable`],
//!   where [`ObjectRelatable::Rel`] is an enum whose variants represent a
//!   _subset_ of [`Object`]'s variants that are valid targets for edges
//!   from that object type.
//! If some [`ObjectKind`] `OA` is able to be related to another
//!   [`ObjectKind`] `OB`,
//!     then [`ObjectRelTo::<OB>`](ObjectRelTo) is implemented for `OA`.
//!
//! When querying the graph for edges using [`ObjectIndex::edges`],
//!   the corresponding [`ObjectRelatable::Rel`] type is provided,
//!   which may then be acted upon or filtered by the caller.
//! Unlike nodes,
//!   it is difficult to statically expect exact edge types in most code
//!   paths
//!     (beyond the `Rel` object itself),
//!     and so [`ObjectRel::narrow`] produces an [`Option`] of the inner
//!     [`ObjectIndex`],
//!       rather than panicing.
//! This `Option` is convenient to use with `Iterator::filter_map` to query
//!   for specific edge types.
//!
//! Using [`ObjectRelTo`],
//!   we are able to ensure statically that all code paths only add edges to
//!   the [`Asg`] that adhere to the ontology described above;
//!     it should therefore not be possible for an edge to exist on the
//!     graph that is not represented by [`ObjectRelatable::Rel`],
//!       provided that it is properly defined.
//! Since [`ObjectRel`] narrows into an [`ObjectIndex`],
//!   the system will produce runtime panics if there is ever any attempt to
//!   follow an edge to an unexpected [`ObjectKind`].

use super::Asg;
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    diagnostic_panic,
    f::Functor,
    span::{Span, UNKNOWN_SPAN},
};
use petgraph::graph::NodeIndex;
use std::{convert::Infallible, fmt::Display, marker::PhantomData};

pub mod expr;
pub mod ident;

use expr::Expr;
use ident::Ident;

/// An object on the ASG.
///
/// See the [module-level documentation](super) for more information.
#[derive(Debug, PartialEq)]
pub enum Object {
    /// Represents the root of all reachable identifiers.
    ///
    /// Any identifier not reachable from the root will not be linked into
    ///   the final executable.
    ///
    /// There should be only one object of this kind.
    Root,

    /// Identifier (a named object).
    Ident(Ident),

    /// Expression.
    ///
    /// An expression may optionally be named by one or more [`Ident`]s.
    Expr(Expr),
}

/// Object types corresponding to variants in [`Object`] that are able to
///   serve as targets of object relations
///     (edges on the graph).
///
/// These are used as small tags for [`ObjectRelatable`].
/// Rust unfortunately makes working with its internal tags difficult,
///   despite their efforts with [`std::mem::Discriminant`],
///     which requires a _value_ to produce.
///
/// TODO: `pub(super)` when the graph can be better encapsulated.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ObjectRelTy {
    Ident,
    Expr,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "root ASG node"),
            Self::Ident(ident) => Display::fmt(ident, f),
            Self::Expr(expr) => Display::fmt(expr, f),
        }
    }
}

impl Object {
    pub fn span(&self) -> Span {
        match self {
            Self::Root => UNKNOWN_SPAN,
            Self::Ident(ident) => ident.span(),
            Self::Expr(expr) => expr.span(),
        }
    }

    /// Retrieve an [`Ident`] reference,
    ///   or [`None`] if the object is not an identifier.
    pub fn as_ident_ref(&self) -> Option<&Ident> {
        match self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    /// Unwraps an object as an [`Ident`],
    ///   panicing if the object is of a different type.
    ///
    /// This should be used only when a panic would represent an internal
    ///   error resulting from state inconsistency on the graph.
    /// Ideally,
    ///   the graph would be typed in such a way to prevent this type of
    ///   thing from occurring in the future.
    pub fn unwrap_ident(self) -> Ident {
        self.into()
    }

    /// Unwraps an object as an [`&Ident`](Ident),
    ///   panicing if the object is of a different type.
    ///
    /// This should be used only when a panic would represent an internal
    ///   error resulting from state inconsistency on the graph.
    /// Ideally,
    ///   the graph would be typed in such a way to prevent this type of
    ///   thing from occurring in the future.
    pub fn unwrap_ident_ref(&self) -> &Ident {
        match self {
            Self::Ident(ident) => ident,
            x => panic!("internal error: expected Ident, found {x:?}"),
        }
    }

    /// Diagnostic panic after failing to narrow an object.
    ///
    /// This is an internal method.
    /// `expected` should contain "a"/"an".
    fn narrowing_panic(&self, expected: &str) -> ! {
        diagnostic_panic!(
            self.span()
                .internal_error(format!(
                    "expected this object to be {expected}"
                ))
                .into(),
            "expected {expected}, found {self}",
        )
    }
}

impl From<Ident> for Object {
    fn from(ident: Ident) -> Self {
        Self::Ident(ident)
    }
}

impl From<Expr> for Object {
    fn from(expr: Expr) -> Self {
        Self::Expr(expr)
    }
}

impl From<Object> for Ident {
    /// Narrow an object into an [`Ident`],
    ///   panicing if the object is not of that type.
    fn from(val: Object) -> Self {
        match val {
            Object::Ident(ident) => ident,
            _ => val.narrowing_panic("an identifier"),
        }
    }
}

impl From<Object> for Expr {
    /// Narrow an object into an [`Expr`],
    ///   panicing if the object is not of that type.
    fn from(val: Object) -> Self {
        match val {
            Object::Expr(expr) => expr,
            _ => val.narrowing_panic("an expression"),
        }
    }
}

impl AsRef<Object> for Object {
    fn as_ref(&self) -> &Object {
        self
    }
}

impl AsRef<Ident> for Object {
    fn as_ref(&self) -> &Ident {
        match self {
            Object::Ident(ref ident) => ident,
            _ => self.narrowing_panic("an identifier"),
        }
    }
}

impl AsRef<Expr> for Object {
    fn as_ref(&self) -> &Expr {
        match self {
            Object::Expr(ref expr) => expr,
            _ => self.narrowing_panic("an expression"),
        }
    }
}

/// An [`Object`]-compatbile entity.
///
/// See [`ObjectIndex`] for more information.
/// This type simply must be convertable both to and from [`Object`] so that
///   operations on the graph that retrieve its value can narrow into it,
///     and operations writing it back can expand it back into [`Object`].
///
/// Note that [`Object`] is also an [`ObjectKind`],
///   if you do not desire narrowing.
pub trait ObjectKind = Into<Object> where Object: Into<Self> + AsRef<Self>;

/// Index representing an [`Object`] stored on the [`Asg`](super::Asg).
///
/// Object references are integer offsets,
///   not pointers.
/// See the [module-level documentation][self] for more information.
///
/// The associated [`ObjectKind`] states an _expectation_ that,
///   when this [`ObjectIndex`] is used to perform an operation on the ASG,
///   that it will operate on an object of type `O`.
/// This type will be verified at runtime during any graph operation,
///   resulting in a panic if the expectation is not met;
///     see the [module-level documentation][self] for more information.
///
/// This object is associated with a [`Span`] that identifies the source
///   location from which this object was derived;
///     this is intended to be used to provide diagnostic information in the
///     event that the object somehow becomes unavailable for later
///       operations.
///
/// _The span is not accounted for in [`PartialEq`]_,
///   since it represents the context in which the [`ObjectIndex`] was
///   retrieved,
///     and the span associated with the underlying [`Object`] may evolve
///     over time.
#[derive(Debug)]
pub struct ObjectIndex<O: ObjectKind>(NodeIndex, Span, PhantomData<O>);

// Deriving this trait seems to silently fail at the time of writing
//   (2022-12-22, Rust 1.68.0-nightly).
impl<O: ObjectKind> Clone for ObjectIndex<O> {
    fn clone(&self) -> Self {
        Self(self.0, self.1, self.2)
    }
}

impl<O: ObjectKind> Copy for ObjectIndex<O> {}

impl<O: ObjectKind> ObjectIndex<O> {
    pub fn new<S: Into<Span>>(index: NodeIndex, span: S) -> Self {
        Self(index, span.into(), PhantomData::default())
    }

    /// Add an edge from `self` to `to_oi` on the provided [`Asg`].
    ///
    /// An edge can only be added if ontologically valid;
    ///   see [`ObjectRelTo`] for more information.
    ///
    /// See also [`Self::add_edge_to`].
    pub fn add_edge_to<OB: ObjectKind>(
        self,
        asg: &mut Asg,
        to_oi: ObjectIndex<OB>,
    ) -> Self
    where
        O: ObjectRelTo<OB>,
    {
        asg.add_edge(self, to_oi);
        self
    }

    /// Add an edge from `from_oi` to `self` on the provided [`Asg`].
    ///
    /// An edge can only be added if ontologically valid;
    ///   see [`ObjectRelTo`] for more information.
    ///
    /// See also [`Self::add_edge_to`].
    pub fn add_edge_from<OB: ObjectKind>(
        self,
        asg: &mut Asg,
        from_oi: ObjectIndex<OB>,
    ) -> Self
    where
        OB: ObjectRelTo<O>,
    {
        from_oi.add_edge_to(asg, self);
        self
    }

    /// Create an iterator over the [`ObjectIndex`]es of the outgoing edges
    ///   of `self`.
    ///
    /// Note that the [`ObjectKind`] `OB` indicates what type of
    ///   [`ObjectIndex`]es will be yielded by the returned iterator;
    ///     this method does nothing to filter non-matches.
    pub fn edges<'a>(
        self,
        asg: &'a Asg,
    ) -> impl Iterator<Item = <O as ObjectRelatable>::Rel> + 'a
    where
        O: ObjectRelatable + 'a,
    {
        asg.edges(self)
    }

    /// Resolve `self` to the object that it references.
    ///
    /// Panics
    /// ======
    /// If our [`ObjectKind`] `O` does not match the actual type of the
    ///   object on the graph,
    ///     the system will panic.
    pub fn resolve(self, asg: &Asg) -> &O {
        asg.expect_obj(self)
    }

    /// Curried [`Self::resolve`].
    pub fn cresolve<'a>(asg: &'a Asg) -> impl FnMut(Self) -> &'a O {
        move |oi| oi.resolve(asg)
    }

    /// Resolve the identifier and map over the resulting [`Object`]
    ///   narrowed to [`ObjectKind`] `O`,
    ///     replacing the object on the given [`Asg`].
    ///
    /// While the provided map may be pure,
    ///  this does mutate the provided [`Asg`].
    ///
    /// If the operation fails,
    ///   `f` is expected to provide an object
    ///     (such as the original)
    ///     to return to the graph.
    ///
    /// If this operation is [`Infallible`],
    ///   see [`Self::map_obj`].
    pub fn try_map_obj<E>(
        self,
        asg: &mut Asg,
        f: impl FnOnce(O) -> Result<O, (O, E)>,
    ) -> Result<Self, E> {
        asg.try_map_obj(self, f)
    }

    /// Resolve the identifier and infallibly map over the resulting
    ///   [`Object`] narrowed to [`ObjectKind`] `O`,
    ///     replacing the object on the given [`Asg`].
    ///
    /// If this operation is _not_ [`Infallible`],
    ///   see [`Self::try_map_obj`].
    pub fn map_obj(self, asg: &mut Asg, f: impl FnOnce(O) -> O) -> Self {
        // This verbose notation (in place of e.g. `unwrap`) is intentional
        //   to emphasize why it's unreachable and to verify our assumptions
        //   at every point.
        match self.try_map_obj::<Infallible>(asg, |o| Ok(f(o))) {
            Ok(oi) => oi,
            Err::<_, Infallible>(_) => unreachable!(),
        }
    }

    /// Lift [`Self`] into [`Option`] and [`filter`](Option::filter) based
    ///   on whether the [`ObjectRelatable::rel_ty`] of [`Self`]'s `O`
    ///   matches that of `OB`.
    ///
    /// More intuitively:
    ///   if `OB` is the same [`ObjectKind`] associated with [`Self`],
    ///     return [`Some(Self)`](Some).
    /// Otherwise,
    ///   return [`None`].
    fn filter_rel<OB: ObjectKind + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>>
    where
        O: ObjectRelatable,
    {
        let Self(index, span, _pd) = self;

        // Rust doesn't know that `OB` and `O` will be the same,
        //   but this will be the case.
        // If it weren't,
        //   then [`ObjectIndex`] protects us at runtime,
        //   so there are no safety issues here.
        Some(ObjectIndex::<OB>(index, span, PhantomData::default()))
            .filter(|_| O::rel_ty() == OB::rel_ty())
    }
}

impl ObjectIndex<Object> {
    /// Indicate that the [`Object`] referenced by this index must be
    ///   narrowed into [`ObjectKind`] `O` when resolved.
    ///
    /// This simply narrows the expected [`ObjectKind`].
    pub fn must_narrow_into<O: ObjectKind>(self) -> ObjectIndex<O> {
        match self {
            Self(index, span, _) => ObjectIndex::new(index, span),
        }
    }
}

impl<O: ObjectKind> Functor<Span> for ObjectIndex<O> {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self {
        match self {
            Self(index, span, ph) => Self(index, f(span), ph),
        }
    }
}

impl<O: ObjectKind> PartialEq for ObjectIndex<O> {
    /// Compare two [`ObjectIndex`]s' indices,
    ///   without concern for its associated [`Span`].
    ///
    /// See [`ObjectIndex`] for more information on why the span is not
    ///   accounted for in this comparison.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self(index_a, _, _), Self(index_b, _, _)) => index_a == index_b,
        }
    }
}

impl<O: ObjectKind> Eq for ObjectIndex<O> {}

impl<O: ObjectKind> From<ObjectIndex<O>> for NodeIndex {
    fn from(objref: ObjectIndex<O>) -> Self {
        match objref {
            ObjectIndex(index, _, _) => index,
        }
    }
}

impl<O: ObjectKind> From<ObjectIndex<O>> for Span {
    fn from(value: ObjectIndex<O>) -> Self {
        match value {
            ObjectIndex(_, span, _) => span,
        }
    }
}

/// Indicate that an [`ObjectKind`] `Self` can be related to
///   [`ObjectKind`] `OB` by creating an edge from `Self` to `OB`.
///
/// This trait defines a portion of the graph ontology,
///   allowing [`Self`] to be related to `OB` by creating a directed edge
///   from [`Self`] _to_ `OB`, as in:
///
/// ```text
///   (Self) -> (OB)
/// ```
///
/// While the data on the graph itself is dynamic and provided at runtime,
///   the systems that _construct_ the graph using the runtime data can be
///   statically analyzed by the type system to ensure that they only
///   construct graphs that adhere to this schema.
pub trait ObjectRelTo<OB: ObjectKind + ObjectRelatable> =
    ObjectRelatable where <Self as ObjectRelatable>::Rel: From<ObjectIndex<OB>>;

/// Identify [`Self::Rel`] as a sum type consisting of the subset of
///   [`Object`] variants representing the valid _target_ edges of
///   [`Self`].
///
/// This is used to derive [`ObjectRelTo``],
///   which can be used as a trait bound to assert a valid relationship
///   between two [`Object`]s.
pub trait ObjectRelatable: ObjectKind {
    /// Sum type representing a subset of [`Object`] variants that are valid
    ///   targets for edges from [`Self`].
    ///
    /// See [`ObjectRel`] for more information.
    type Rel: ObjectRel;

    /// The [`ObjectRelTy`] tag used to identify this [`ObjectKind`] as a
    ///   target of a relation.
    fn rel_ty() -> ObjectRelTy;

    /// Represent a relation to another [`ObjectKind`] that cannot be
    ///   statically known and must be handled at runtime.
    ///
    /// See [`ObjectRel`] for more information.
    fn new_rel_dyn(ty: ObjectRelTy, oi: ObjectIndex<Object>) -> Self::Rel;
}

/// A relationship to another [`ObjectKind`].
///
/// This trait is intended to be implemented by enums that represent the
///   subset of [`ObjectKind`]s that are able to serve as edge targets for
///   the [`ObjectRelatable`] that utilizes it as its
///   [`ObjectRelatable::Rel`].
///
/// As described in the [module-level documentation](super),
///   the concrete [`ObjectKind`] of an edge is generally not able to be
///   determined statically outside of code paths that created the
///   [`Object`] anew.
/// But we _can_ at least narrow the types of [`ObjectKind`]s to those
///   [`ObjectRelTo`]s that we know are valid,
///     since the system is restricted (statically) to those edges when
///     performing operations on the graph.
///
/// This [`ObjectRel`] represents that subset of [`ObjectKind`]s.
/// A caller may decide to dispatch based on the type of edge it receives,
///   or it may filter edges with [`Self::narrow`] in conjunction with
///   [`Iterator::filter_map`]
///     (for example).
/// Since the wrapped value is an [`ObjectIndex`],
///   the system will eventually panic if it attempts to reference a node
///   that is not of the type expected by the edge,
///     which can only happen if the edge has an incorrect [`ObjectRelTy`],
///     meaning the graph is somehow corrupt
///       (because system invariants were not upheld).
///
/// This affords us both runtime memory safety and static guarantees that
///   the system is not able to generate an invalid graph that does not
///   adhere to the prescribed ontology,
///     provided that invariants are properly upheld by the
///     [`asg`](crate::asg) module.
pub trait ObjectRel {
    /// Attempt to narrow into the [`ObjectKind`] `OB`.
    ///
    /// Unlike [`Object`] nodes,
    ///   _this operation does not panic_,
    ///   instead returning an [`Option`].
    /// If the relationship is of type `OB`,
    ///   then [`Some`] will be returned with an inner
    ///   [`ObjectIndex<OB>`](ObjectIndex).
    /// If the narrowing fails,
    ///   [`None`] will be returned instead.
    ///
    /// This return value is well-suited for [`Iterator::filter_map`] to
    ///   query for edges of particular kinds.
    fn narrow<OB: ObjectKind + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>>;
}

/// Subset of [`ObjectKind`]s that are valid targets for edges from
///   [`Ident`].
///
/// See [`ObjectRel`] for more information.
pub enum IdentRel {
    Ident(ObjectIndex<Ident>),
    Expr(ObjectIndex<Expr>),
}

impl ObjectRel for IdentRel {
    fn narrow<OB: ObjectKind + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>> {
        match self {
            Self::Ident(oi) => oi.filter_rel(),
            Self::Expr(oi) => oi.filter_rel(),
        }
    }
}

impl ObjectRelatable for Ident {
    type Rel = IdentRel;

    fn rel_ty() -> ObjectRelTy {
        ObjectRelTy::Ident
    }

    fn new_rel_dyn(ty: ObjectRelTy, oi: ObjectIndex<Object>) -> IdentRel {
        match ty {
            ObjectRelTy::Ident => IdentRel::Ident(oi.must_narrow_into()),
            ObjectRelTy::Expr => IdentRel::Expr(oi.must_narrow_into()),
        }
    }
}

impl From<ObjectIndex<Ident>> for IdentRel {
    fn from(value: ObjectIndex<Ident>) -> Self {
        Self::Ident(value)
    }
}

impl From<ObjectIndex<Expr>> for IdentRel {
    fn from(value: ObjectIndex<Expr>) -> Self {
        Self::Expr(value)
    }
}

/// Subset of [`ObjectKind`]s that are valid targets for edges from
///   [`Expr`].
///
/// See [`ObjectRel`] for more information.
pub enum ExprRel {
    Ident(ObjectIndex<Ident>),
    Expr(ObjectIndex<Expr>),
}

impl ObjectRel for ExprRel {
    fn narrow<OB: ObjectKind + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>> {
        match self {
            Self::Ident(oi) => oi.filter_rel(),
            Self::Expr(oi) => oi.filter_rel(),
        }
    }
}

impl ObjectRelatable for Expr {
    type Rel = ExprRel;

    fn rel_ty() -> ObjectRelTy {
        ObjectRelTy::Expr
    }

    fn new_rel_dyn(ty: ObjectRelTy, oi: ObjectIndex<Object>) -> ExprRel {
        match ty {
            ObjectRelTy::Ident => ExprRel::Ident(oi.must_narrow_into()),
            ObjectRelTy::Expr => ExprRel::Expr(oi.must_narrow_into()),
        }
    }
}

impl From<ObjectIndex<Ident>> for ExprRel {
    fn from(value: ObjectIndex<Ident>) -> Self {
        Self::Ident(value)
    }
}

impl From<ObjectIndex<Expr>> for ExprRel {
    fn from(value: ObjectIndex<Expr>) -> Self {
        Self::Expr(value)
    }
}

/// A container for an [`Object`] allowing for owned borrowing of data.
///
/// The purpose of allowing this owned borrowing is to permit a functional
///   style of object manipulation,
///     like the rest of the TAMER system,
///     despite the mutable underpinnings of the ASG.
/// This is accomplished by wrapping each object in an [`Option`] so that we
/// can [`Option::take`] its inner value temporarily.
///
/// This container has a critical invariant:
///   the inner [`Option`] must _never_ be [`None`] after a method exits,
///     no matter what branches are taken.
/// Methods operating on owned data enforce this invariant by mapping over
///   data and immediately placing the new value to the container before the
///   method completes.
/// This container will panic if this variant is not upheld.
///
/// TODO: Make this `pub(super)` when [`Asg`]'s public API is cleaned up.
#[derive(Debug, PartialEq)]
pub struct ObjectContainer(Option<Object>);

impl ObjectContainer {
    /// Retrieve an immutable reference to the inner [`Object`],
    ///   narrowed to expected type `O`.
    ///
    /// Panics
    /// ======
    /// This will panic if the object on the graph is not the expected
    ///   [`ObjectKind`] `O`.
    pub fn get<O: ObjectKind>(&self) -> &O {
        let Self(container) = self;

        container
            .as_ref()
            .diagnostic_unwrap(container_oops)
            .as_ref()
    }

    /// Attempt to modify the inner [`Object`],
    ///   narrowed to expected type `O`,
    ///   returning any error.
    ///
    /// See also [`Self::replace_with`] if the operation is [`Infallible`].
    ///
    /// Panics
    /// ======
    /// This will panic if the object on the graph is not the expected
    ///   [`ObjectKind`] `O`.
    pub fn try_replace_with<O: ObjectKind, E>(
        &mut self,
        f: impl FnOnce(O) -> Result<O, (O, E)>,
    ) -> Result<(), E> {
        let ObjectContainer(container) = self;

        let obj = container.take().diagnostic_unwrap(container_oops).into();

        // NB: We must return the object to the container in all code paths!
        let result = f(obj)
            .map(|obj| {
                container.replace(obj.into());
            })
            .map_err(|(orig, err)| {
                container.replace(orig.into());
                err
            });

        debug_assert!(container.is_some());
        result
    }

    /// Modify the inner [`Object`],
    ///   narrowed to expected type `O`.
    ///
    /// See also [`Self::try_replace_with`] if the operation can fail.
    ///
    /// Panics
    /// ======
    /// This will panic if the object on the graph is not the expected
    ///   [`ObjectKind`] `O`.
    pub fn replace_with<O: ObjectKind>(&mut self, f: impl FnOnce(O) -> O) {
        let _ = self.try_replace_with::<O, (O, Infallible)>(|obj| Ok(f(obj)));
    }
}

impl<I: Into<Object>> From<I> for ObjectContainer {
    fn from(obj: I) -> Self {
        Self(Some(obj.into()))
    }
}

fn container_oops() -> Vec<AnnotatedSpan<'static>> {
    // This used to be a real span,
    //   but since this invariant is easily verified and should absolutely
    //   never occur,
    //     there's no point in complicating the API.
    let span = UNKNOWN_SPAN;

    vec![
        span.help("this means that some operation used take() on the object"),
        span.help("  container but never replaced it with an updated object"),
        span.help(
            "  after the operation completed, which should not \
                    be possible.",
        ),
    ]
}