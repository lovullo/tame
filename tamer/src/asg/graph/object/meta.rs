// Metalinguistic objects represented on the ASG
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

//! Metalinguistic objects on the ASG.
//!
//! Metalinguistic variables[^w],
//!   called "metavariables" for short,
//!   have historically been a feature of the template system.
//! The canonical metavariable is the template parameter.
//!
//! [^w]: This term comes from logic; see
//!         <https://en.wikipedia.org/wiki/Metavariable_(logic)>.
//!       The term "metasyntactic" was originally used with TAMER,
//!         but that term generally has a different meaning in programming:
//!           <https://en.wikipedia.org/wiki/Metasyntactic_variable>.

use arrayvec::ArrayVec;

use super::{prelude::*, Doc, Ident};
use crate::{
    diagnose::Annotate,
    diagnostic_todo,
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{util::SPair, Token},
    span::Span,
};
use std::fmt::Display;

/// Metalinguistic variable (metavariable).
///
/// A metavariable is a lexical construct.
/// Its value is a lexeme that represents an [`Ident`],
///   whose meaning depends on the context in which the metavariable is
///   referenced.
/// Its lexeme may be composed of multiple [`Self::Lexeme`]s,
///   and may even be constructed dynamically based on the values of other
///   [`Meta`]s.
///
/// Metavariables are identified by being bound by an [`Ident`];
///   the symbol representing that identifier then acts as a metavariable.
#[derive(Debug, PartialEq, Eq)]
pub enum Meta {
    /// Metavariable represents a parameter without a value.
    ///
    /// A value must be provided at or before expansion,
    ///   generally via template application arguments.
    Required(Span),

    /// Metavariable has a concrete lexical value.
    ///
    /// This metavariable represents a literal and requires no further
    ///   reduction or processing.
    Lexeme(Span, SPair),

    /// Metavariable whose value is to be the concatenation of all
    ///   referenced metavariables.
    ///
    /// This object has no value on its own;
    ///   it must contain edges to other metavariables,
    ///     and the order of those edges on the ASG represents concatenation
    ///     order.
    ConcatList(Span),
}

impl Meta {
    /// Create a new metavariable without a value.
    ///
    /// Metavariables with no value cannot be used in an expansion context.
    /// Intuitively,
    ///   they act as required parameters.
    pub fn new_required(span: Span) -> Self {
        Self::Required(span)
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Required(span)
            | Self::ConcatList(span)
            | Self::Lexeme(span, _) => *span,
        }
    }

    /// Assign a lexeme to a metavariable.
    ///
    /// In a template definition context,
    ///   this acts as a default value for this metavariable.
    /// In an application context,
    ///   this has the effect of binding a value to this metavariable.
    pub fn assign_lexeme(self, lexeme: SPair) -> Self {
        match self {
            Self::Required(span) => Self::Lexeme(span, lexeme),

            Self::ConcatList(_) => diagnostic_todo!(
                vec![lexeme.note("while parsing this lexeme")],
                "append to ConcatList",
            ),

            Self::Lexeme(_, _) => diagnostic_todo!(
                vec![lexeme.note("while parsing this lexeme")],
                "Lexeme => ConcatList",
            ),
        }
    }

    /// Retrieve a concrete lexeme,
    ///   if any.
    ///
    /// If this metavariable represents a concatenation list,
    ///   this will return [`None`].
    /// This method _does not_ expand metavariables,
    ///   and does not have the context necessary to do so.
    pub fn lexeme(&self) -> Option<SPair> {
        match self {
            Self::Required(_) | Self::ConcatList(_) => None,
            Self::Lexeme(_, lex) => Some(*lex),
        }
    }
}

impl From<&Meta> for Span {
    fn from(meta: &Meta) -> Self {
        meta.span()
    }
}

impl Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Required(_) => {
                write!(f, "metalinguistic parameter with required value")
            }
            Self::ConcatList(_) => {
                write!(f, "metalinguistic concatenation list")
            }
            Self::Lexeme(_, spair) => {
                write!(f, "lexeme {}", TtQuote::wrap(spair))
            }
        }
    }
}

impl Functor<Span> for Meta {
    fn map(self, f: impl FnOnce(Span) -> Span) -> Self::Target {
        match self {
            Self::Required(span) => Self::Required(f(span)),
            Self::ConcatList(span) => Self::ConcatList(f(span)),
            Self::Lexeme(span, spair) => Self::Lexeme(f(span), spair),
        }
    }
}

object_rel! {
    /// Metavariables contain lexical data and references to other
    ///   metavariables.
    Meta -> {
        // References to other metavariables
        //   (e.g. `<param-value>` in XML-based sources).
        cross Ident,

        // Owned lexical values.
        //
        // These differ from the above references because they represent
        //   inline lexemes that have no identifier.
        tree Meta,

        // e.g. template paramater description.
        tree Doc,
    }
}

impl ObjectIndex<Meta> {
    /// Append a lexeme to this metavariable.
    ///
    /// If `self` is [`Meta::Required`],
    ///   this provides a value and reuses the object already allocated.
    ///
    /// If `self` is a single [`Meta::Lexeme`],
    ///   it is re-allocated to a separate [`Meta`] object along with the
    ///   provided `lexeme`,
    ///     and edges are added to both,
    ///     indicating concatenation.
    ///
    /// Metavariables with multiple values already represents concatenation
    ///   and a new edge will be added without changing `self`.
    pub fn append_lexeme(
        self,
        asg: &mut Asg,
        lexeme: SPair,
    ) -> Result<Self, AsgError> {
        use Meta::*;

        let mut rels = ArrayVec::<SPair, 2>::new();

        // We don't have access to `asg` within this closure because of
        //   `map_obj`;
        //     the above variable will be mutated by it to return extra
        //     information to do those operations afterward.
        // If we do this often,
        //   then let's create a `map_obj` that is able to return
        //   supplemental information or create additional relationships
        //     (so, a map over a subgraph rather than an object).
        self.map_obj(asg, |meta| match meta {
            // Storage is already allocated for this lexeme.
            Required(span) => Lexeme(span, lexeme),

            // We could technically allocate a new symbol and combine the
            //   lexeme now,
            //     but let's wait so that we can avoid allocating
            //     intermediate symbols.
            Lexeme(span, first_lexeme) => {
                // We're converting from a single lexeme stored on `self` to
                //   a `Meta` with edges to both individual lexemes.
                rels.push(first_lexeme);
                rels.push(lexeme);

                ConcatList(span)
            }

            // We're already representing concatenation so we need only add
            //   an edge to the new lexeme.
            ConcatList(span) => {
                rels.push(lexeme);
                ConcatList(span)
            }
        });

        for rel_lexeme in rels {
            let oi = asg.create(Meta::Lexeme(rel_lexeme.span(), rel_lexeme));
            self.add_edge_to(asg, oi, None)?;
        }

        Ok(self)
    }

    pub fn close(self, asg: &mut Asg, close_span: Span) -> Self {
        self.map_obj(asg, |meta| {
            meta.map(|open_span| {
                open_span.merge(close_span).unwrap_or(open_span)
            })
        })
    }

    // Append a reference to a metavariable identified by `oi_ref`.
    //
    // The value of the metavariable will not be known until expansion time,
    //   at which point its lexical value will be concatenated with those of
    //   any other references,
    //     in the order that they were added.
    //
    // It is expected that the value of `oi_ref` was produced via a lookup
    //   from the reference location and therefore contains the reference
    //   [`Span`];
    //     this is used to provide accurate diagnostic information.
    pub fn concat_ref(
        self,
        asg: &mut Asg,
        oi_ref: ObjectIndex<Ident>,
    ) -> Result<Self, AsgError> {
        use Meta::*;

        // We cannot mutate the ASG within `map_obj` below because of the
        //   held reference to `asg`,
        //     so this will be used to store data for later mutation.
        let mut pre = None;

        // References are only valid for a [`Self::ConcatList`].
        self.map_obj(asg, |meta| match meta {
            Required(span) | ConcatList(span) => ConcatList(span),

            Lexeme(span, lex) => {
                // We will move the lexeme into a _new_ object,
                //   and store a reference to it.
                pre.replace(Meta::Lexeme(lex.span(), lex));

                ConcatList(span)
            }
        });

        // This represents a lexeme that was extracted into a new `Meta`;
        //   we must add the edge before appending the ref since
        //   concatenation will occur during expansion in edge order.
        if let Some(orig) = pre {
            asg.create(orig).add_edge_from(asg, self, None)?;
        }

        // Having been guaranteed a `ConcatList` above,
        //   we now only need to append an edge that references what to
        //   concatenate.
        self.add_edge_to(asg, oi_ref, Some(oi_ref.span()))
    }
}

impl AsgObjectMut for Meta {}
