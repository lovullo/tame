// Relationship between objects represented on ASG //
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

//! Relationship betwen objects on the ASG.
//!
//! See (parent module)[super] for more information.

use super::{
    Expr, Ident, Meta, Object, ObjectIndex, ObjectKind, OiPairObjectInner, Pkg,
    Root,
};
use crate::{
    asg::{graph::object::Tpl, Asg},
    f::Functor,
    span::Span,
};
use std::fmt::Display;

pub use super::ObjectTy as ObjectRelTy;

impl Display for ObjectRelTy {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // At the time of writing,
        //   this happens to be sufficient.
        std::fmt::Debug::fmt(self, f)
    }
}

/// Declare relations for an [`ObjectKind`].
///
/// This generates an [`ObjectRel`] type for the provided [`ObjectKind`] and
///   binds it to the kind using [`ObjectRelatable`].
///
/// Each relationship must be explicitly specified as either a `tree` or
///   `cross` edge.
/// For more information on cross edges,
///   see [`ObjectRel::is_cross_edge`].
macro_rules! object_rel {
    (
        $(#[$attr:meta])+
        $from:ident -> {
            $($ety:ident $kind:ident,)*
        }
    ) => {paste::paste! {
        /// Subset of [`ObjectKind`]s that are valid targets for edges from
        #[doc=concat!("[`", stringify!($from), "`].")]
        ///
        $(#[$attr])+
        ///
        /// See [`ObjectRel`] for more information.
        ///
        /// [`ObjectKind`]: crate::asg::ObjectKind
        #[derive(Debug, PartialEq, Eq)]
        pub enum [<$from Rel>] {
            $($kind(ObjectIndex<$kind>),)*
        }

        impl ObjectRel<$from> for [<$from Rel>] {
            fn narrow<OB: ObjectRelFrom<$from> + ObjectRelatable>(
                self,
            ) -> Option<ObjectIndex<OB>> {
                match self {
                    $(Self::$kind(oi) => oi.filter_rel(),)*
                }
            }

            /// The root of the graph by definition has no cross edges.
            fn is_cross_edge(&self) -> bool {
                match self {
                    $(
                        Self::$kind(..) => object_rel!(@is_cross_edge $ety),
                    )*

                    #[allow(unreachable_patterns)] // for empty Rel types
                    _ => unreachable!(
                        concat!(stringify!($from), "Rel is empty")
                    ),
                }
            }
        }

        impl ObjectRelatable for $from {
            type Rel = [<$from Rel>];

            fn rel_ty() -> ObjectRelTy {
                ObjectRelTy::$from
            }

            fn new_rel_dyn(
                ty: ObjectRelTy,
                #[allow(unused_variables)] // for empty Rel
                oi: ObjectIndex<Object>,
            ) -> Option<[<$from Rel>]> {
                match ty {
                    $(
                        ObjectRelTy::$kind => {
                            Some(Self::Rel::$kind(oi.must_narrow_into()))
                        },
                    )*
                    _ => None,
                }
            }
        }

        $(
            impl From<ObjectIndex<$kind>> for [<$from Rel>] {
                fn from(value: ObjectIndex<$kind>) -> Self {
                    Self::$kind(value)
                }
            }
        )*
    }};

    (@is_cross_edge cross) => { true };
    (@is_cross_edge tree)  => { false };
}

/// A dynamic relationship (edge) from one object to another before it has
///   been narrowed.
///
/// The source and target of this edge are usually [`ObjectIndex`]es,
///   but it is made generic (`S, T`) to support mapping while retaining
///   useful metadata,
///     e.g. to resolve an object while retaining the edge information.
#[derive(Debug, PartialEq)]
pub struct DynObjectRel<S = ObjectIndex<Object>, T = ObjectIndex<Object>>(
    (ObjectRelTy, ObjectRelTy),
    (S, T),
    Option<Span>,
);

impl<S, T> DynObjectRel<S, T> {
    pub(in super::super) fn new(
        from_ty: ObjectRelTy,
        to_ty: ObjectRelTy,
        src: S,
        target: T,
        ctx_span: Option<Span>,
    ) -> Self {
        Self((from_ty, to_ty), (src, target), ctx_span)
    }

    /// The type of the source edge.
    pub fn source_ty(&self) -> ObjectRelTy {
        match self {
            Self((ty, _), ..) => *ty,
        }
    }

    /// The type of the target edge.
    pub fn target_ty(&self) -> ObjectRelTy {
        match self {
            Self((_, ty), ..) => *ty,
        }
    }

    /// The source of this relationship.
    pub fn source(&self) -> &S {
        match self {
            Self(_, (oi, _), _) => oi,
        }
    }

    /// The target of this relationship.
    ///
    /// This type generally originates as [`ObjectIndex`] but can be mapped
    ///   over to retain the structured edge data.
    pub fn target(&self) -> &T {
        match self {
            Self(_, (_, oi), _) => oi,
        }
    }

    /// A [`Span`] associated with the _relationship_ between the source and
    ///   target objects,
    ///     if any.
    pub fn ctx_span(&self) -> Option<Span> {
        match self {
            Self(_, _, ctx_span) => *ctx_span,
        }
    }
}

impl<S> DynObjectRel<S, ObjectIndex<Object>> {
    /// Attempt to narrow the target into the [`ObjectRel`] of `O`.
    ///
    /// See [`ObjectRelatable::new_rel_dyn`] for more information.
    pub fn narrow_target<O: ObjectKind + ObjectRelatable>(
        &self,
    ) -> Option<O::Rel> {
        O::new_rel_dyn(self.target_ty(), *self.target())
    }

    /// Pair the target [`ObjectIndex`] with its resolved [`Object`].
    ///
    /// This allows the [`ObjectIndex`] to be refined alongside the inner
    ///   [`ObjectKind`] so that callers can make use of the refined
    ///   [`ObjectIndex`] without having to explicitly narrow themselves.
    /// While isn't any more or less safe than the manual alternative,
    ///   it _does_ defend against logic bugs.
    pub fn resolve_target_oi_pair(
        self,
        asg: &Asg,
    ) -> DynObjectRel<S, Object<OiPairObjectInner>> {
        self.map(|(soi, toi)| (soi, toi.resolve(asg).pair_oi(toi)))
    }

    /// Dynamically determine whether this edge represents a cross edge.
    ///
    /// This function is intended for _dynamic_ edge types,
    ///   which cannot be determined statically;
    ///     it should be used only in situations where the potential edge types
    ///     are unbounded,
    ///       e.g. on an iterator yielding generalized [`ObjectIndex`]es during
    ///       a full graph traversal.
    /// You should otherwise use [`ObjectRel::is_cross_edge`].
    ///
    /// For more information on cross edges,
    ///   see [`ObjectRel::is_cross_edge`].
    pub fn is_cross_edge(&self) -> bool {
        /// Generate cross-edge mappings between ObjectRelTy and the associated
        ///   ObjectRel.
        ///
        /// This is intended to both reduce boilerplate and to eliminate typos.
        ///
        /// This mess will be optimized away,
        ///   but exists so that cross edge definitions can exist alongside
        ///   other relationship definitions for each individual object type,
        ///     rather than having to maintain them in aggregate here.
        macro_rules! ty_cross_edge {
            ($($ty:ident),*) => {
                match self.source_ty() {
                    $(
                        ObjectRelTy::$ty => {
                            self.narrow_target::<$ty>().is_some_and(
                                |rel| rel.is_cross_edge()
                            )
                        },
                    )*
                }
            }
        }

        ty_cross_edge!(Root, Pkg, Ident, Expr, Tpl, Meta)
    }
}

impl<T> DynObjectRel<ObjectIndex<Object>, T> {
    /// Pair the source [`ObjectIndex`] with its resolved [`Object`].
    ///
    /// This allows the [`ObjectIndex`] to be refined alongside the inner
    ///   [`ObjectKind`] so that callers can make use of the refined
    ///   [`ObjectIndex`] without having to explicitly narrow themselves.
    /// While isn't any more or less safe than the manual alternative,
    ///   it _does_ defend against logic bugs.
    pub fn resolve_source_oi_pair(
        self,
        asg: &Asg,
    ) -> DynObjectRel<Object<OiPairObjectInner>, T> {
        self.map(|(soi, toi)| (soi.resolve(asg).pair_oi(soi), toi))
    }
}

impl DynObjectRel<ObjectIndex<Object>, ObjectIndex<Object>> {
    /// Pair the source and target [`ObjectIndex`]es with their respective
    ///   resolved [`Object`]s.
    ///
    /// See [`Self::resolve_target_oi_pair`] and
    ///   [`Self::resolve_source_oi_pair`] for more information.
    pub fn resolve_oi_pairs(
        self,
        asg: &Asg,
    ) -> DynObjectRel<Object<OiPairObjectInner>, Object<OiPairObjectInner>>
    {
        self.resolve_source_oi_pair(asg).resolve_target_oi_pair(asg)
    }
}

impl<S, T, U, V> Functor<(S, T), (U, V)> for DynObjectRel<S, T> {
    type Target = DynObjectRel<U, V>;

    fn map(self, f: impl FnOnce((S, T)) -> (U, V)) -> Self::Target {
        match self {
            Self(tys, x, ctx_span) => DynObjectRel(tys, f(x), ctx_span),
        }
    }
}

impl<T: Display> Display for DynObjectRel<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self((from_ty, to_ty), (s, t), _) = self;

        write!(f, "dynamic edge {from_ty}->{to_ty} with {s}->{t}",)
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

/// Reverse of [`ObjectRelTo`].
///
/// This is primarily useful for avoiding `where` clauses,
///   or for use in `impl Trait` specifications.
pub trait ObjectRelFrom<OA: ObjectKind + ObjectRelatable> =
    ObjectRelatable where <OA as ObjectRelatable>::Rel: From<ObjectIndex<Self>>;

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
    type Rel: ObjectRel<Self>;

    /// The [`ObjectRelTy`] tag used to identify this [`ObjectKind`] as a
    ///   target of a relation.
    fn rel_ty() -> ObjectRelTy;

    /// Represent a relation to another [`ObjectKind`] that cannot be
    ///   statically known and must be handled at runtime.
    ///
    /// A value of [`None`] means that the provided [`DynObjectRel`] is not
    ///   valid for [`Self`].
    /// If the caller is utilizing edge data that is already present on the graph,
    ///   then this means that the system is not properly upholding edge
    ///   invariants
    ///     (the graph's ontology)
    ///     and the system ought to panic;
    ///       this is a significant bug representing a problem with the
    ///       correctness of the system.
    ///
    /// See [`ObjectRel`] for more information.
    fn new_rel_dyn(
        ty: ObjectRelTy,
        oi: ObjectIndex<Object>,
    ) -> Option<Self::Rel>;
}

impl<O: ObjectKind + ObjectRelatable> ObjectIndex<O> {
    pub fn rel_ty(&self) -> ObjectRelTy {
        O::rel_ty()
    }
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
pub trait ObjectRel<OA: ObjectKind + ObjectRelatable>: Sized {
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
    fn narrow<OB: ObjectRelFrom<OA> + ObjectRelatable>(
        self,
    ) -> Option<ObjectIndex<OB>>;

    /// Attempt to narrow into the [`ObjectKind`] `OB`,
    ///   but rather than returning the narrowed type,
    ///   return `Option<Self>`.
    ///
    /// This can be used with [`Iterator::filter_map`].
    /// By not being a [`bool`] predicate,
    ///   we're able to provide a default trait implementation based on
    ///   [`Self::narrow`] without requiring that [`Self`] implement
    ///   [`Copy`].
    fn narrows_into<OB: ObjectRelFrom<OA> + ObjectRelatable>(
        self,
    ) -> Option<Self>
    where
        Self: From<ObjectIndex<OB>>,
    {
        self.narrow::<OB>().map(Into::into)
    }

    /// Whether this relationship represents an ontological cross edge.
    ///
    /// A _cross edge_ is an edge between two trees as described by the
    ///   graph's ontology.
    /// Many objects on the graph represent trees,
    ///   but contain references to other trees.
    /// Recognizing cross edges allows the system to understand when it is
    ///   following an edge between two trees,
    ///     which may require different behavior.
    ///
    /// This contrasts to cross edges in the context of a graph traversal,
    ///   where a tree is determined by a walk of the graph and may not take
    ///   into consideration the meaning of edges.
    ///
    /// _Because this is a property of the ontology rather than a structural
    ///   interpretation of the graph,
    ///     it must be manually verified by a human._
    /// An incorrect flagging of cross edges here will result in certain
    ///   traversals being incorrect.
    ///
    /// Implementation Context
    /// ======================
    /// It is important to understand why this method exists and how it may
    ///   be used so that implementations of this trait do the right thing
    ///   with regards to determining whether an edge ought to represent a
    ///   cross edge.
    ///
    /// For example,
    ///   when generating a representation of an [`Expr`],
    ///   a walk of the graph ought not consider an [`Ident`] reference to
    ///   be part of the expression tree,
    ///     otherwise the referenced expression would be inlined.
    /// Furthermore,
    ///   visiting the referenced [`Ident`] ought not inhibit a later walk,
    ///   since the walk must later traverse the [`Ident`] to reach the
    ///   [`Object`] that it represents.
    /// Similarly,
    ///   if the [`Ident`] has already been visited by a previous walk,
    ///   we want to _re-visit_ it to output a reference as part of the
    ///   referencing [`Expr`].
    ///
    /// However,
    ///   this behavior is not always desirable.
    /// In the case of a topological sort of the graph for linking,
    ///   cross edges ought to count as visitations since that dependency
    ///   must be calculated before the expression that needs it,
    ///     and we don't want to re-calculate it again later on.
    ///
    /// The cross-edge is therefore an ontological fact,
    ///   but its _interpretation_ is context-dependent.
    ///
    /// Note that the ontology is not intended to support back edges,
    ///   since they produce cycles,
    ///   except for exceptional situations
    ///     (e.g. function recursion which will hopefully be removed from
    ///       the language in the future).
    /// With that said,
    ///   if an edge could conceivably be a back edge and not be rejected
    ///   from circular dependency checks,
    ///     then do _not_ assume that it is a cross edge without further
    ///     analysis,
    ///       which may require introducing more context to this method.
    fn is_cross_edge(&self) -> bool;
}
