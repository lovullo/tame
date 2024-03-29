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
//! ![Visualization of ASG ontology](../../ontviz.svg)
//!
//! Dynamic Object Types and Narrowing
//! ==================================
//! Unlike the functional lowering pipeline that precedes it,
//!   the ASG is a mutable, ever-evolving graph of dynamic data.
//! The ASG does not benefit from the same static type-level guarantees that
//!   the rest of the system does at compile-time.
//!
//! However,
//!   we _are_ able to utilize the type system to statically ensure that
//!   there exists no code path that is able to generate an invalid graph
//!     (a graph that does not adhere to its ontology as described below).
//!
//! Every node on the graph can represent any type of [`Object`].
//! An [`ObjectIndex`] contains an index into the graph,
//!   _not_ a reference;
//!     it is therefore possible (though avoidable) for objects to be
//!       modified out from underneath an [`ObjectIndex`].
//! Consequently,
//!   we cannot be _absolutely certain_ that the [`Object`] referred to by
//!   an [`ObjectIndex`] is in fact what we expect it to be when performing
//!   an operation on the graph using that index;
//!     though the system is designed to uphold an invariant that the type
//!     of [`Object`] cannot be changed,
//!       it is conceivable that the system may contain,
//!         now or in the future,
//!         bugs that cause it to fail to uphold that invariant.
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
//! The set of [`ObjectKind`] types that may be related _to_
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
//!
//! In addition to these static guarantees,
//!   [`AsgRelMut`](super::AsgRelMut) is utilized by [`Asg`] to consult an
//!   object before an edge is added _from_ it,
//!     allowing objects to assert ownership over their edges and cache
//!     information about them as the graph is being built.

use super::{Asg, AsgError};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    diagnostic_panic,
    f::{Map, TryMap},
    parse::util::SPair,
    span::{Span, UNKNOWN_SPAN},
};
use petgraph::graph::NodeIndex;
use std::{
    convert::Infallible,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    marker::PhantomData,
};

#[macro_use]
mod rel;

pub mod doc;
pub mod expr;
pub mod ident;
pub mod meta;
pub mod pkg;
pub mod root;
pub mod tpl;

pub use doc::Doc;
pub use expr::Expr;
pub use ident::Ident;
pub use meta::Meta;
pub use pkg::Pkg;
pub use rel::{
    DynObjectRel, ObjectIndexRelTo, ObjectIndexTo, ObjectIndexToTree,
    ObjectIndexTreeRelTo, ObjectRel, ObjectRelFrom, ObjectRelTo, ObjectRelTy,
    ObjectRelatable, ObjectTreeRelTo,
};
pub use root::Root;
pub use tpl::Tpl;

/// Often-needed exports for [`ObjectKind`]s.
pub mod prelude {
    pub use super::{
        super::{super::error::AsgError, Asg, AsgRelMut},
        Object, ObjectIndex, ObjectIndexRelTo, ObjectKind, ObjectRel,
        ObjectRelFrom, ObjectRelTo, ObjectRelTy, ObjectRelatable,
        ObjectTreeRelTo,
    };
}

/// Given a list of [`ObjectKind`]s,
///   generate [`Object`],
///   associated types,
///   and various [`From`]/[`AsRef`] implementations for [`ObjectKind`]
///     widening and narrowing.
///
/// This macro must be applied only once.
macro_rules! object_gen {
    (
        $(
            $(#[$attr:meta])*
            $kind:ident,
        )+
    ) => {
        /// An object on the ASG.
        ///
        /// This is generic over its inner values to support using
        ///   [`Object`] as a sum type in a variety of different contexts
        ///   where [`ObjectKind`] may be used.
        /// The concrete [`ObjectInner`] that is stored on the ASG itself is
        ///   [`OnlyObjectInner`].
        ///
        /// See the [module-level documentation](super) for more
        ///   information.
        #[derive(Debug, PartialEq)]
        pub enum Object<T: ObjectInner = OnlyObjectInner> {
            $(
                $(#[$attr])*
                $kind(T::$kind),
            )+
        }

        /// Object types corresponding to variants in [`Object`].
        ///
        /// These are used as small tags for [`ObjectRelatable`].
        /// Rust unfortunately makes working with its internal tags
        ///   difficult,
        ///     despite their efforts with [`std::mem::Discriminant`],
        ///       which requires a _value_ to produce.
        ///
        /// TODO: Encapsulate within `crate::asg` when the graph can be better
        ///   encapsulated.
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum ObjectTy {
            $($kind,)+
        }

        impl ObjectTy {
            /// Assume that the provided [`ObjectIndex`] is of the type
            ///   associated with `self`,
            ///     and then determine whether that object can be related to
            ///     another object of a given type `OB`.
            ///
            /// This method should be kept private;
            ///   it is memory safe,
            ///     but incorrect assumptions will violate graph object
            ///     invariants and cause panics when the [`ObjectIndexTo`]
            ///     is later resolved.
            fn assuming_oi_maybe_rel_to_dyn<OB: ObjectRelatable>(
                &self,
                oi: ObjectIndex<Object>,
            ) -> Option<ObjectIndexTo<OB>> {
                match self {
                    $(
                        Self::$kind => {
                            $kind::oi_rel_to_dyn(oi.must_narrow_into())
                        }
                    )+
                }
            }
        }

        impl<T: ObjectInner> From<&Object<T>> for ObjectTy {
            fn from(obj: &Object<T>) -> Self {
                match obj {
                    $(Object::$kind(_) => ObjectTy::$kind,)+
                }
            }
        }

        /// Narrowed [`ObjectIndex`] types for each [`ObjectKind`].
        ///
        /// This allows for converting a dynamic
        ///   [`ObjectIndex<Object>`](ObjectIndex) into a statically known
        ///   [`ObjectKind`],
        ///     while also providing the ability to exhaustively match
        ///     against all such possibilities.
        #[derive(Debug, PartialEq, Eq)]
        pub enum ObjectIndexRefined {
            $(
                $kind(ObjectIndex<$kind>),
            )+
        }

        /// The collection of potential objects of [`Object`].
        pub trait ObjectInner {
            $(type $kind;)+
        }

        /// An [`ObjectInner`] where each constituent type implements
        ///   [`Display`].
        trait DisplayableObjectInner = ObjectInner
        where
            $(<Self as ObjectInner>::$kind: Display,)+;

        /// Concrete [`ObjectKind`]s and nothing more.
        #[derive(Debug, PartialEq)]
        pub struct OnlyObjectInner;

        impl ObjectInner for OnlyObjectInner {
            $(type $kind = $kind;)+
        }

        /// References to [`OnlyObjectInner`].
        ///
        /// This allows for `Object<&T>`.
        /// See [`Object::inner_as_ref`] for more information.
        #[derive(Debug, PartialEq)]
        pub struct RefObjectInner<'a>(PhantomData<&'a ()>);

        impl<'a> ObjectInner for RefObjectInner<'a> {
            $(type $kind = &'a $kind;)+
        }

        /// A [`RefObjectInner`] paired with an [`ObjectIndex`] that
        ///   represents it.
        ///
        /// This is desirable when an [`ObjectIndex`] is resolved but is
        ///   still needed for graph operations.
        /// The pair ensures that,
        ///   when the inner [`ObjectKind`] is narrowed,
        ///   the paired [`ObjectIndex`] will too be narrowed to the same
        ///     kind,
        ///       limiting logic bugs that may result from otherwise having
        ///       to manually narrow types.
        #[derive(Debug, PartialEq)]
        pub struct OiPairObjectInner<'a>(PhantomData<&'a ()>);

        impl<'a> ObjectInner for OiPairObjectInner<'a> {
            $(type $kind = (&'a $kind, ObjectIndex<$kind>);)+
        }

        impl<T: DisplayableObjectInner> Display for Object<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(Self::$kind(x) => Display::fmt(x, f),)+
                }
            }
        }

        $(
            impl From<$kind> for Object {
                fn from(x: $kind) -> Self {
                    Self::$kind(x)
                }
            }

            impl From<Object> for $kind {
                /// Narrow an object into an [`Root`],
                ///   panicing if the object is not of that type.
                fn from(obj: Object) -> Self {
                    match obj {
                        Object::$kind(x) => x,
                        _ => obj.narrowing_panic(stringify!($kind)),
                    }
                }
            }

            impl AsRef<$kind> for Object {
                fn as_ref(&self) -> &$kind {
                    match self {
                        Object::$kind(ref x) => x,
                        _ => self.narrowing_panic(stringify!($kind)),
                    }
                }
            }
        )+

        /// Generate boilerplate `match`es for individual [`Object`]
        ///   variants.
        ///
        /// Rust will infer the [`Object`]'s [`ObjectInner`] from the
        ///   surrounding context.
        macro_rules! map_object {
            ($obj:expr, $inner:ident => $map:expr) => {
                match $obj {
                    $(Object::$kind($inner) => Object::$kind($map),)+
                }
            };
        }
    }
}

object_gen! {
    /// Represents the root of all reachable identifiers.
    ///
    /// Any identifier not reachable from the root will not be linked into
    ///   the final executable.
    ///
    /// There should be only one object of this kind.
    Root,

    /// A package of identifiers.
    Pkg,

    /// Identifier (a named object).
    Ident,

    /// Expression.
    Expr,

    /// A template definition.
    Tpl,

    /// Metalinguistic variable (metavariable).
    Meta,

    /// Documentation.
    Doc,
}

impl Object<OnlyObjectInner> {
    pub fn span(&self) -> Span {
        match self {
            Self::Root(_) => UNKNOWN_SPAN,
            Self::Pkg(pkg) => pkg.span(),
            Self::Ident(ident) => ident.span(),
            Self::Expr(expr) => expr.span(),
            Self::Tpl(tpl) => tpl.span(),
            Self::Meta(meta) => meta.span(),
            Self::Doc(doc) => doc.span(),
        }
    }

    pub fn ty(&self) -> ObjectTy {
        self.into()
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

    /// Convert an `&Object<Inner>` into an `Object<&Inner>`.
    ///
    /// Since [`Object`] is the type stored directly on the [`Asg`],
    ///   this is necessary in order to use [`Object`] as a wrapper over
    ///   narrowed [`ObjectKind`]s.
    fn inner_as_ref(&self) -> Object<RefObjectInner> {
        map_object!(self, x => x)
    }

    /// Pair self with an [`ObjectIndex`] that represents it.
    ///
    /// This replaces the [`Span`] of the [`ObjectIndex`] with our span,
    ///   such that any (unlikely) lookup errors will highlight this object.
    ///
    /// This allows the [`ObjectIndex`] to be refined alongside the inner
    ///   [`ObjectKind`] so that callers can make use of the refined
    ///   [`ObjectIndex`] without having to explicitly narrow themselves.
    /// While isn't any more or less safe than the manual alternative,
    ///   it _does_ defend against logic bugs.
    ///
    /// Note that this makes no guarantees that the provided
    ///   [`ObjectIndex`] `oi` is _actually_ an index that represents this
    ///   object;
    ///     that onus is on the caller.
    /// Getting this wrong won't lead to memory safety issues,
    ///   but it will lead to bugs and confusion at best,
    ///   and panics at worst if `oi` references a different [`ObjectKind`].
    fn pair_oi(&self, oi: ObjectIndex<Object>) -> Object<OiPairObjectInner> {
        map_object!(
            self.inner_as_ref(),
            x => (x, oi.must_narrow_into().overwrite(self.span()))
        )
    }
}

impl From<&Object> for Span {
    fn from(val: &Object) -> Self {
        val.span()
    }
}

impl AsRef<Object> for Object {
    fn as_ref(&self) -> &Object {
        self
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
pub trait ObjectKind = Into<Object>
where
    Self: Debug + PartialEq,
    Object: Into<Self> + AsRef<Self>;

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
/// [`Self::resolve_span`] may be used to replace the inner [`Span`] with
///   that of the object that [`Self`] represents,
///     which is useful in a different diagnostic context.
///
/// _The span is not accounted for in [`PartialEq`] or [`Hash`]_,
///   since it represents the context in which the [`ObjectIndex`] was
///   retrieved,
///     and the span associated with the underlying [`Object`] may evolve
///     over time.
#[derive(Debug)]
pub struct ObjectIndex<O: ObjectKind>(NodeIndex, Span, PhantomData<O>);

impl<O: ObjectKind + ObjectRelatable> Display for ObjectIndex<O> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(ni, _, _) = self;
        write!(f, "ObjectIndex<{}>({ni:?})", O::rel_ty())
    }
}

impl Display for ObjectIndex<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(ni, _, _) = self;
        write!(f, "ObjectIndex<Object>({ni:?})")
    }
}

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

    /// The source location from which the request for the associated object
    ///   was derived.
    ///
    /// The span _does not necessarily represent_ the span of the target
    ///   [`Object`].
    /// If the object is being created,
    ///   then it may,
    ///   but it otherwise represents the location of whatever is
    ///   _requesting_ the object.
    pub fn span(&self) -> Span {
        match self {
            Self(_, span, _) => *span,
        }
    }

    /// Add an edge from `self` to `to_oi` on the provided [`Asg`].
    ///
    /// Since the only invariant asserted by [`ObjectIndexRelTo`] is that
    ///   it may be related to `OB`,
    ///     this method will only permit edges to `OB`;
    ///       nothing else about the inner object is statically known.
    ///
    /// See also [`Self::add_edge_from`].
    ///
    /// _This method must remain private_,
    ///   forcing callers to go through APIs for specific operations that
    ///   allow objects to enforce their own invariants.
    /// This is also the reason why this method is defined here rather than
    ///   on [`ObjectIndexRelTo`].
    fn add_edge_to<OB: ObjectRelatable>(
        self,
        asg: &mut Asg,
        to_oi: ObjectIndex<OB>,
        ctx_span: Option<Span>,
    ) -> Result<Self, AsgError>
    where
        Self: ObjectIndexRelTo<OB>,
    {
        asg.add_edge(self, to_oi, ctx_span).map(|()| self)
    }

    /// Add an edge from `from_oi` to `self` on the provided [`Asg`].
    ///
    /// An edge can only be added if ontologically valid;
    ///   see [`ObjectRelTo`] for more information.
    ///
    /// See also [`Self::add_edge_to`].
    ///
    /// _This method must remain private_,
    ///   forcing callers to go through APIs for specific operations that
    ///   allow objects to enforce their own invariants.
    fn add_edge_from<OA: ObjectIndexRelTo<O>>(
        self,
        asg: &mut Asg,
        from_oi: OA,
        ctx_span: Option<Span>,
    ) -> Result<Self, AsgError>
    where
        O: ObjectRelatable,
    {
        asg.add_edge(from_oi, self, ctx_span).map(|()| self)
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

    /// Iterate over the [`ObjectIndex`]es of the outgoing edges of `self`
    ///   that match the [`ObjectKind`] `OB`.
    ///
    /// This is simply a shorthand for applying [`ObjectRel::narrow`] via
    ///   [`Iterator::filter_map`].
    pub fn edges_filtered<'a, OB: ObjectKind + ObjectRelatable + 'a>(
        self,
        asg: &'a Asg,
    ) -> impl Iterator<Item = ObjectIndex<OB>> + 'a
    where
        O: ObjectRelTo<OB> + 'a,
    {
        self.edges(asg).filter_map(ObjectRel::narrow::<OB>)
    }

    /// Incoming edges to self filtered by [`ObjectKind`] `OI`.
    ///
    /// For filtering rationale,
    ///   see [`Asg::incoming_edges_filtered`].
    fn incoming_edges_filtered<'a, OI: ObjectKind + ObjectRelatable + 'a>(
        self,
        asg: &'a Asg,
    ) -> impl Iterator<Item = ObjectIndex<OI>> + 'a
    where
        O: ObjectRelFrom<OI> + 'a,
    {
        asg.incoming_edges_filtered(self)
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

    /// Replace our associated [`Span`] with the span of the resolved
    ///   [`Object`].
    ///
    /// This is useful to utilize [`ObjectIndex`] in a diagnostic context
    ///   without having to allocate space for additional metadata.
    pub fn resolve_span(self, asg: &Asg) -> ObjectIndexResolvedSpan<O> {
        ObjectIndexResolvedSpan(
            self.overwrite(self.widen().resolve(asg).span()),
        )
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

    /// Resolve the identifier and map over an inner `T` of the resulting
    ///   [`Object`] narrowed to [`ObjectKind`] `O`,
    ///     replacing the object on the given [`Asg`].
    ///
    /// This uses [`Self::try_map_obj`] to retrieve the object from
    ///   the [`Asg`].
    ///
    /// If this operation is [`Infallible`],
    ///   see [`Self::map_obj_inner`].
    pub fn try_map_obj_inner<T, E>(
        self,
        asg: &mut Asg,
        f: impl FnOnce(T) -> <O as TryMap<T>>::FnResult<E>,
    ) -> Result<Self, E>
    where
        O: TryMap<T, Result<E> = Result<O, (O, E)>>,
    {
        self.try_map_obj(asg, O::try_fmap(f))
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

    /// Resolve the identifier and map over an inner `T` of the resulting
    ///   [`Object`] narrowed to [`ObjectKind`] `O`,
    ///     replacing the object on the given [`Asg`].
    ///
    /// This uses [`Self::map_obj`] to retrieve the object from
    ///   the [`Asg`].
    ///
    /// If this operation is _not_ [`Infallible`],
    ///   see [`Self::try_map_obj_inner`].
    pub fn map_obj_inner<T>(self, asg: &mut Asg, f: impl FnOnce(T) -> T) -> Self
    where
        O: Map<T, Target = O>,
    {
        self.map_obj(asg, O::fmap(f))
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

    /// Root this object in the ASG's [`Root`] object.
    ///
    /// A rooted object is forced to be reachable.
    /// This should only be utilized when necessary for toplevel objects;
    ///   other objects should be reachable via their relations to other
    ///   objects.
    /// Forcing objects to be reachable can prevent them from being
    ///   optimized away if they are not used.
    pub fn root(self, asg: &mut Asg) -> Result<Self, AsgError>
    where
        Root: ObjectRelTo<O>,
    {
        asg.root(self.span())
            .add_edge_to(asg, self, None)
            .map(|_| self)
    }

    /// Whether this object has been rooted in the ASG's [`Root`] object.
    ///
    /// See [`Self::root`] for more information.
    pub fn is_rooted(&self, asg: &Asg) -> bool
    where
        Root: ObjectRelTo<O>,
    {
        asg.root(self.span()).has_edge_to(asg, *self)
    }

    /// Widen an [`ObjectKind`] `O` into [`Object`],
    ///   generalizing the index type.
    ///
    /// This generalization is useful in dynamic contexts,
    ///   but it discards type information that must be later re-checked and
    ///   verified.
    /// See [`Self::widen_dyn_ty`] to retain that type information.
    pub fn widen(self) -> ObjectIndex<Object> {
        match self {
            Self(index, span, _pd) => ObjectIndex::new(index, span),
        }
    }

    /// Widen an [`ObjectKind`] `O` into [`Object`] like [`Self::widen`],
    ///   but retain type information for later narrowing at runtime.
    pub fn widen_dyn_ty(self) -> (ObjectIndex<Object>, ObjectRelTy)
    where
        O: ObjectRelatable,
    {
        (self.widen(), O::rel_ty())
    }

    /// Attempt to look up a locally bound [`Ident`] via a linear search of
    ///   `self`'s edges.
    ///
    /// This should be used only when an index is not available,
    ///   and is currently restricted to tests.
    ///
    /// See [`ObjectIndexRelTo::lookup_local_linear`].
    //
    // TODO: This method exists here only as a fallback when Rust is unable
    //     to infer the proper type for [`ObjectIndexRelTo`].
    //   It can be removed once that is resolved;
    //     delete this method and compile to see.
    #[cfg(test)]
    pub fn lookup_local_linear(
        &self,
        asg: &Asg,
        name: SPair,
    ) -> Option<ObjectIndex<Ident>>
    where
        O: ObjectRelTo<Ident>,
    {
        ObjectIndexRelTo::lookup_local_linear(self, asg, name)
    }

    /// Retrieve the identifier for this object,
    ///   if any.
    ///
    /// If there is more than one identifier,
    ///   only one will be returned,
    ///   and the result of the operation is undefined.
    /// This can be problematic if certain optimizations have been performed
    ///   on the graph,
    ///     like common subexpression elimination,
    ///   in which case it's best not to rely on following edges in reverse.
    pub fn ident<'a>(&self, asg: &'a Asg) -> Option<ObjectIndex<Ident>>
    where
        O: ObjectRelFrom<Ident>,
    {
        self.incoming_edges_filtered(asg).next()
    }

    /// Indicate that the given identifier `oi` is defined by this object.
    pub fn defines(
        self,
        asg: &mut Asg,
        oi: ObjectIndex<Ident>,
    ) -> Result<Self, AsgError>
    where
        Self: ObjectIndexRelTo<Ident>,
    {
        oi.defined_by(asg, self).map(|_| self)
    }

    /// Describe this expression using a short independent clause.
    ///
    /// This is intended to be a concise description for use either as a
    ///   simple sentence or as part of a compound sentence.
    /// There should only be one such clause for any given object,
    ///   but that is not enforced here.
    pub fn add_desc_short(
        &self,
        asg: &mut Asg,
        clause: SPair,
    ) -> Result<Self, AsgError>
    where
        O: ObjectRelTo<Doc>,
    {
        let oi_doc = asg.create(Doc::new_indep_clause(clause));
        self.add_edge_to(asg, oi_doc, None)
    }

    /// Retrieve a description of this expression using a short independent
    ///   clause,
    ///     if one has been set.
    ///
    /// See [`Self::add_desc_short`] to add such a description.
    pub fn desc_short(&self, asg: &Asg) -> Option<SPair>
    where
        O: ObjectRelTo<Doc>,
    {
        self.edges_filtered::<Doc>(asg)
            .map(ObjectIndex::cresolve(asg))
            .find_map(Doc::indep_clause)
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

impl<O: ObjectKind> Map<Span> for ObjectIndex<O> {
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

impl<O: ObjectKind> Hash for ObjectIndex<O> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self(index, _, _) => index.hash(state),
        }
    }
}

impl<O: ObjectKind> From<ObjectIndex<O>> for NodeIndex {
    fn from(objref: ObjectIndex<O>) -> Self {
        match objref {
            ObjectIndex(index, _, _) => index,
        }
    }
}

impl<O: ObjectKind> From<ObjectIndex<O>> for usize {
    fn from(value: ObjectIndex<O>) -> Self {
        Into::<NodeIndex>::into(value).index()
    }
}

impl<O: ObjectKind> From<ObjectIndex<O>> for Span {
    fn from(value: ObjectIndex<O>) -> Self {
        match value {
            ObjectIndex(_, span, _) => span,
        }
    }
}

/// An [`ObjectKind`] whose associated [`Span`] has been resolved.
///
/// This exists simply to provide proof via the type system that resolution
///   occurred.
///
/// See [`ObjectIndex::resolve_span`].
#[derive(Debug, PartialEq, Eq)]
pub struct ObjectIndexResolvedSpan<O: ObjectKind>(ObjectIndex<O>);

impl<O: ObjectKind> ObjectIndexResolvedSpan<O> {
    pub fn oi(&self) -> ObjectIndex<O> {
        (*self).into()
    }

    pub fn span(&self) -> Span {
        self.oi().span()
    }
}

impl<O: ObjectKind> From<ObjectIndexResolvedSpan<O>> for ObjectIndex<O> {
    fn from(value: ObjectIndexResolvedSpan<O>) -> Self {
        match value {
            ObjectIndexResolvedSpan(oi) => oi,
        }
    }
}

impl<O: ObjectKind> From<ObjectIndexResolvedSpan<O>> for Span {
    fn from(value: ObjectIndexResolvedSpan<O>) -> Self {
        ObjectIndex::from(value).span()
    }
}

impl<O: ObjectKind> Clone for ObjectIndexResolvedSpan<O> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<O: ObjectKind> Copy for ObjectIndexResolvedSpan<O> {}

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
