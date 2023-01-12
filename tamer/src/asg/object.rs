// Objects represented on ASG
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

//! Objects represented by the ASG.
//!
//! Dynamic Object Types and Narrowing
//! ==================================
//! Unlike the functional lowering pipeline that precedes it,
//!   the ASG is a mutable, ever-evolving graph of dynamic data.
//! The ASG does not benefit from the same type-level guarantees that the
//!   rest of the system does at compile-time.
//!
//! Any node on the graph can represent any type of [`Object`].
//! An [`ObjectIndex`] contains an index into the graph,
//!   _not_ a reference;
//!     consequently,
//!       it is possible (though avoidable) for objects to be modified out
//!       from underneath references.
//! Consequently,
//!   we cannot trust that an [`ObjectIndex`] is what we expect it to be when
//!   performing an operation on the graph using that index.
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

use super::{Asg, Expr, Ident};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan},
    diagnostic_panic,
    f::Functor,
    span::{Span, UNKNOWN_SPAN},
};
use petgraph::graph::NodeIndex;
use std::{convert::Infallible, fmt::Display, marker::PhantomData};

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

impl<'a> From<&'a Object> for &'a Ident {
    /// Narrow an object into an [`Ident`],
    ///   panicing if the object is not of that type.
    fn from(val: &'a Object) -> Self {
        match val {
            Object::Ident(ident) => ident,
            _ => val.narrowing_panic("an identifier"),
        }
    }
}

impl<'a> From<&'a Object> for &'a Expr {
    /// Narrow an object into an [`Expr`],
    ///   panicing if the object is not of that type.
    fn from(val: &'a Object) -> Self {
        match val {
            Object::Expr(expr) => expr,
            _ => val.narrowing_panic("an expression"),
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
pub trait ObjectKind = Into<Object>
where
    Object: Into<Self>,
    for<'a> &'a Object: Into<&'a Self>;

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
    pub fn edges<'a, OB: ObjectKind + 'a>(
        self,
        asg: &'a Asg,
    ) -> impl Iterator<Item = ObjectIndex<OB>> + 'a
    where
        O: ObjectRelTo<OB> + 'a,
    {
        asg.edges(self).map(ObjectIndex::must_narrow_into::<OB>)
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
pub trait ObjectRelTo<OB: ObjectKind>: ObjectKind {}

/// Indicate that an [`ObjectKind`] `Self` can be related to
///   [`ObjectKind`] `OB` by creating an edge from `OB` to `Self`.
///
/// _This trait exists for notational convenience and is intended only to
///   derive a blanket [`ObjectRelTo`] implementation._
/// This is because `impl`s are of the form `impl T for O`,
///   but it is more natural to reason about directed edges left-to-write as
///   `(From) -> (To)`;
///     this trait allows `impl ObjectRelFrom<OA> for OB` rather than the
///     equivalent `impl ObjectRelTo<OB> for OA`.
trait ObjectRelFrom<OA: ObjectKind>: ObjectKind {}

impl<OA: ObjectKind, OB: ObjectKind> ObjectRelTo<OB> for OA where
    OB: ObjectRelFrom<OA>
{
}

// This describes the object relationship portion of the ASG's ontology.
impl ObjectRelFrom<Ident> for Expr {}
impl ObjectRelFrom<Expr> for Expr {}

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
            .diagnostic_unwrap(container_oops())
            .into()
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

        let obj = container.take().diagnostic_unwrap(container_oops()).into();

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
