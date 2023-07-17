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
//! Desugaring is performed by [`TplShortDesugar`].
//!
//! A shorthand template application looks something like this,
//!   in XML form:
//!
//! ```xml
//!   <t:foo bar="baz">
//!     <c:sum />
//!   </t:foo>
//!
//!   <!-- the above desugars into the below -->
//!
//!   <apply-template name="_foo_">
//!     <with-param name="@bar@" value="baz" />
//!     <with-param name="@values@" value="___dsgr-01___" />
//!   </apply-template>
//!
//!   <template name="___dsgr-01___">
//!     <c:sum />
//!   </template>
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
//! The body of a shorthand template becomes the body of a new template,
//!   and its id referenced as the lexical value of the param `@values@`.
//! (This poor name is a historical artifact.)
//! Since the template is closed
//!   (has no free metavariables),
//!   it will be expanded on reference,
//!     inlining its body into the reference site.
//! This is a different and generalized approach to the `param-copy`
//!   behavior of the XLST-based TAME.
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
//! The XSLT-based compiler did not produce a separate template for
//!   `@values@`.

use arrayvec::ArrayVec;

use super::{Nir, NirEntity};
use crate::{
    fmt::TtQuote,
    parse::prelude::*,
    span::Span,
    sym::{
        st::raw::L_TPLP_VALUES, GlobalSymbolIntern, GlobalSymbolResolve,
        SymbolId,
    },
};

use Nir::*;
use NirEntity::*;

/// Desugar shorthand template applications into long-form template
///   applications.
///
/// This parser is able to handle nested applications without having to
///   track nesting by taking advantage of the parsing that NIR has already
///   performed.
///
/// See the [module-level documentation](super) for more information.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum TplShortDesugar {
    /// Waiting for shorthand template application,
    ///   passing tokens along in the meantime.
    ///
    /// This state is also used when parsing the body of a shorthand
    ///   template application.
    #[default]
    Scanning,

    /// A shorthand template application associated with the provided
    ///   [`SPair`] was encountered and shorthand params are being
    ///   desugared.
    DesugaringParams(SPair),
}

impl Display for TplShortDesugar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Scanning => {
                write!(f, "awaiting shorthand template application")
            }
            Self::DesugaringParams(_) => {
                write!(f, "desugaring shorthand template application params")
            }
        }
    }
}

diagnostic_infallible! {
    pub enum TplShortDesugarError {}
}

impl ParseState for TplShortDesugar {
    type Token = Nir;
    type Object = Nir;
    type Error = TplShortDesugarError;
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
            (Scanning, Open(TplApplyShort(qname), span)) => {
                // TODO: Determine whether caching these has any notable
                //   benefit over repeated heap allocations,
                //     comparing packages with very few applications and
                //     packages with thousands
                //       (we'd still have to hit the heap for the cache).
                let tpl_name =
                    format!("_{}_", qname.local_name().lookup_str()).intern();

                // TODO: This should be emitted _after_ params to save work
                //   for `crate::asg:graph::visit::ontree::SourceCompatibleTreeEdgeOrder`.
                let name = SPair(tpl_name, span);
                stack.push(Ref(name));

                Transition(DesugaringParams(name)).ok(Open(TplApply, span))
            }

            // Shorthand template params' names do not contain the
            //   surrounding `@`s.
            (DesugaringParams(ospan), Open(TplParamShort(name, val), span)) => {
                let pname = format!("@{name}@").intern();

                // note: reversed (stack)
                stack.push(Close(TplParam, span));
                stack.push(Text(val));
                stack.push(BindIdentMeta(SPair(pname, name.span())));
                Transition(DesugaringParams(ospan)).ok(Open(TplParam, span))
            }

            // A child element while we're desugaring template params
            //   means that we have reached the body,
            //     which is to desugar into `@values@`.
            // We generate a name for a new template,
            //   set `@values@` to the name of the template,
            //   close our active template application,
            //   and then place the body into that template.
            //
            // TODO: This does not handle nested template applications.
            (DesugaringParams(name), tok @ Open(..)) => {
                let ospan = name.span();
                let gen_name = gen_tpl_name_at_offset(ospan);
                let gen_desc = values_tpl_desc(name);

                // The spans are awkward here because we are streaming,
                //   and so don't have much choice but to use the opening
                //   span for everything.
                // If this ends up being unhelpful for diagnostics,
                //   we can have AIR do some adjustment through some
                //   yet-to-be-defined means.
                //
                // note: reversed (stack)
                stack.push(Desc(gen_desc));
                stack.push(BindIdent(SPair(gen_name, ospan)));
                stack.push(Open(Tpl, ospan));

                // Application ends here,
                //   and the new template (above) will absorb both this
                //   token `tok` and all tokens that come after.
                stack.push(Close(TplApply, ospan));
                stack.push(Close(TplParam, ospan));
                stack.push(Text(SPair(gen_name, ospan)));
                stack.push(BindIdentMeta(SPair(L_TPLP_VALUES, ospan)));

                // Note that we must have `tok` as lookahead instead of
                //   pushing directly on the stack in case it's a
                //   `TplApplyShort`.
                Transition(Scanning)
                    .ok(Open(TplParam, ospan))
                    .with_lookahead(tok)
            }

            // If we're scanning and find a closing shorthand application,
            //   then we must have just finished with a shorthand body.
            (Scanning, Close(TplApplyShort(_), span)) => {
                Transition(Scanning).ok(Close(Tpl, span))
            }

            // If we complete the shorthand template during param parsing,
            //   then we have no body and must close the application.
            (DesugaringParams(_), Close(TplApplyShort(_), span)) => {
                Transition(Scanning).ok(Close(TplApply, span))
            }

            // Any tokens that we don't recognize will be passed on unchanged.
            (st, tok) => Transition(st).ok(tok),
        }
    }

    fn is_accepting(&self, stack: &Self::Context) -> bool {
        matches!(self, Self::Scanning) && stack.is_empty()
    }
}

type Stack = ArrayVec<Nir, 7>;

/// Generate a deterministic template identifier name that is unique
///   relative to the offset in the source context (file) of the given
///   [`Span`].
///
/// Hygiene is not a concern since identifiers cannot be redeclared,
///   so conflicts with manually-created identifiers will result in a
///   compilation error
///     (albeit a cryptic one);
///       the hope is that the informally-compiler-reserved `___` convention
///       mitigates that unlikely occurrence.
/// Consequently,
///   we _must_ intern to ensure that error can occur
///     (we cannot use [`GlobalSymbolIntern::clone_uninterned`]).
#[inline]
fn gen_tpl_name_at_offset(span: Span) -> SymbolId {
    format!("___dsgr-{:x}___", span.offset()).intern()
}

/// Generate a description for the template generated from the body of a
///   shorthand template application.
///
/// Descriptions are required on templates,
///   but we should also provide something that is useful in debugging.
/// The description will contain the name of the template being applied and
///   will share the same span as the provided [`SPair`] `applying`'s,
///     having been derived from it.
fn values_tpl_desc(applying: SPair) -> SPair {
    SPair(
        format!(
            "Desugared body of shorthand template application of {}",
            TtQuote::wrap(applying.symbol())
        )
        .intern(),
        applying.span(),
    )
}

#[cfg(test)]
mod test;
