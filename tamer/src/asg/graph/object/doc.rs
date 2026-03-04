// Documentation represented on the ASG
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

//! Documentation on the ASG.
//!
//! TODO: Document TAME's stance on documentation and literate programming,
//!   much of which hasn't been able to be realized over the years.

use super::{Ident, Meta, ident::IdentDefinition, prelude::*};
use crate::{
    asg::graph::ProposedRel,
    parse::{Token, prelude::Annotate, util::SPair},
    span::Span,
};
use std::fmt::Display;

/// Documentation string.
///
/// TODO: This presently serves as a subject line,
///   e.g. a description or label,
///   but will evolve in the future.
#[derive(Debug, PartialEq, Eq)]
pub enum Doc {
    /// An (ideally) concise independent clause describing an object.
    IndepClause(SPair),

    /// Arbitrary text serving as documentation for sibling objects in a
    ///   literate style.
    ///
    /// TAMER does not presently ascribe any semantic meaning to this text,
    ///   and it may even be entirely whitespace.
    /// There are plans to improve upon this in the future.
    ///
    /// The intent is for this text to be mixed with sibling objects,
    ///   in a style similar to that of literate programming.
    Text(SPair),

    /// An [`IndepClause`](Self::IndepClause)whose text will not be known
    ///   until expansion.
    ///
    /// This object is expected to contain an edge to an identifier
    ///   representing a metavariable whose lexical value will serve as the
    ///   documentation text.
    ///
    /// The associated span is the location at which the documentation was
    ///   defined.
    AbstractIndepClause(Span),
}

impl Display for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "documentation string")
    }
}

impl Doc {
    /// Document an object using what is ideally a concise independent
    ///   clause.
    pub fn new_indep_clause(clause: SPair) -> Self {
        Self::IndepClause(clause)
    }

    /// Reference an identifier bound to a metavariable whose lexical value
    ///   will represent an independent clause as in
    ///   [`Self::new_indep_clause`].
    pub fn new_indep_clause_ref(ref_span: Span) -> Self {
        Self::AbstractIndepClause(ref_span)
    }

    /// Arbitrary text serving as documentation for sibling objects in a
    ///   literate style.
    pub fn new_text(text: SPair) -> Self {
        Self::Text(text)
    }

    pub fn concrete_indep_clause(&self) -> Option<SPair> {
        match self {
            Self::IndepClause(spair) => Some(*spair),
            Self::Text(_) => None,

            // An independent clause _will_ be available at some point,
            //   but is not yet.
            Self::AbstractIndepClause(_) => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::IndepClause(spair) | Self::Text(spair) => spair.span(),
            Self::AbstractIndepClause(span) => *span,
        }
    }
}

object_rel! {
    /// Documentation strings cannot contain children,
    ///   but they can be abstract.
    Doc -> {
        // References to identifiers representing metavariables are
        //   permitted for documentation generation.
        cross Ident {
            fn pre_add_edge(
                asg: &mut Asg,
                rel: ProposedRel<Self, Ident>,
                commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                match rel.to_oi.definition(asg) {
                    // Missing; we'll have to check once we support
                    //   notification on definition.
                    None => Ok(commit(asg)),

                    // Only metavariable references are permitted.
                    Some(IdentDefinition::Meta(_)) => {
                        Ok(commit(asg))
                    },

                    Some(
                        IdentDefinition::Expr(_)
                        | IdentDefinition::Tpl(_)
                    ) => Err(AsgError::InvalidDocRef(
                        // This is a cross edge and so should always be
                        //   available
                        rel.ref_span.unwrap_or(rel.from_oi.span()),
                        rel.to_oi.resolve(asg).span(),
                    ))
                }
            }
        },

        // TODO: This is not actually used.
        //   There seems to be a bug in the `min_specialization` feature
        //   that results in too narrow of a type inference on
        //   `ObjectIndexRelTo<OB> for ObjectIndexTo<OB>`,
        //     expecting `ProposedRel<Doc, Ident>` instead of
        //     `<ProposedRel<Doc, OB>` when there is only one `AsgRelMut`
        //     impl defined
        //       (when there's only `Ident` above).
        //  Remove this rel to see if it's still a problem,
        //    noting that the nightly version of Rust must be manually
        //    bumped.
        cross Meta {
            fn pre_add_edge(
                _asg: &mut Asg,
                rel: ProposedRel<Self, Meta>,
                _commit: impl FnOnce(&mut Asg),
            ) -> Result<(), AsgError> {
                diagnostic_panic!(
                    vec![
                        rel.from_oi.span()
                            .internal_error("cannot create edge from here..."),
                        rel.to_oi.span()
                            .internal_error("...to here")
                    ],
                    "unsupported edge (see object/doc.rs for more information)"
                )
            }
        },
    }
}

impl ObjectIndex<Doc> {
    /// The identifier that is expected to resolve to a metavariable whose
    ///   lexical value will become the documentation string.
    ///
    /// This is applicable to [`Doc::AbstractIndepClause`].
    /// If no such reference exists,
    ///   [`None`] will be returned.
    pub fn abstract_indep_clause_ref(
        &self,
        asg: &Asg,
    ) -> Option<ObjectIndex<Ident>> {
        self.edges_filtered::<Ident>(asg).next()
    }
}
