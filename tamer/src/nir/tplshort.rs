// Shorthand template application desugaring for NIR
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

//! Shorthand template application desugaring for NIR.
//!
//! A shorthand template application looks something like this,
//!   in XML form:
//!
//! ```xml
//!   <t:foo bar="baz">
//!     <c:sum />
//!   </t:foo>
//!
//!   <!-- desugars into -->
//!   <apply-template name="_foo_">
//!     <with-param name="@bar@" value="baz" />
//!     <with-param name="@values@">
//!       <c:sum />
//!     </with-param>
//!   </apply-template>
//! ```
//!
//! The shorthand syntax makes templates look like another language
//!   primitive,
//!     with the exception of the namespace prefix.
//!
//! Note how desugaring also wraps template names in `'_'` and param names
//!   in `'@'`.
//! These naming requirements were intended to eliminate conflicts with
//!   other types of identifiers and to make it obvious when templates and
//!   metavariables were being used,
//!     but it works against the goal of making template applications look
//!     like language primitives.
//! Shorthand form was added well after the long `apply-template` form.
//!
//! This shorthand version does not permit metavariables for template or
//!   param names,
//!     so the long form is still a useful language feature for more
//!     sophisticated cases.
//!
//! This was originally handled in the XSLT compiler in
//!   `:src/current/include/preproc/template.xsl`.
//! You may need to consult the Git history if this file is no longer
//!   available or if the XSLT template was since removed.

use arrayvec::ArrayVec;

use super::{Nir, NirEntity};
use crate::{
    parse::prelude::*,
    sym::{GlobalSymbolIntern, GlobalSymbolResolve},
};
use std::convert::Infallible;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum TplShortDesugar {
    /// Waiting for shorthand template application,
    ///   passing tokens along in the meantime.
    #[default]
    Scanning,
}

impl Display for TplShortDesugar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Scanning => {
                write!(f, "awaiting shorthand template application")
            }
        }
    }
}

impl ParseState for TplShortDesugar {
    type Token = Nir;
    type Object = Nir;
    type Error = Infallible;
    type Context = Stack;

    fn parse_token(
        self,
        tok: Self::Token,
        stack: &mut Self::Context,
    ) -> TransitionResult<Self::Super> {
        use TplShortDesugar::*;

        if let Some(obj) = stack.pop() {
            return Transition(self).ok(obj).with_lookahead(tok);
        }

        match (self, tok) {
            // Shorthand template applications are identified by a `Some`
            //   QName in the `TplApply` entity.
            //
            // The name of the template _without_ the underscore padding is
            //   the local part of the QName.
            (Scanning, Nir::Open(NirEntity::TplApply(Some(qname)), span)) => {
                // TODO: Determine whether caching these has any notable
                //   benefit over repeated heap allocations,
                //     comparing packages with very few applications and
                //     packages with thousands
                //       (we'd still have to hit the heap for the cache).
                let tpl_name =
                    format!("_{}_", qname.local_name().lookup_str()).intern();

                stack.push(Nir::Ref(SPair(tpl_name, span)));

                Transition(Scanning)
                    .ok(Nir::Open(NirEntity::TplApply(None), span))
            }

            // Shorthand template params' names do not contain the
            //   surrounding `@`s.
            (
                Scanning,
                Nir::Open(NirEntity::TplParam(Some((name, val))), span),
            ) => {
                let pname = format!("@{name}@").intern();

                stack.push(Nir::Close(NirEntity::TplParam(None), span));
                stack.push(Nir::Text(val));
                stack.push(Nir::BindIdent(SPair(pname, name.span())));

                Transition(Scanning)
                    .ok(Nir::Open(NirEntity::TplParam(None), span))
            }

            // Any tokens that we don't recognize will be passed on unchanged.
            (st, tok) => Transition(st).ok(tok),
        }
    }

    fn is_accepting(&self, stack: &Self::Context) -> bool {
        matches!(self, Self::Scanning) && stack.is_empty()
    }
}

type Stack = ArrayVec<Nir, 3>;

#[cfg(test)]
mod test;
