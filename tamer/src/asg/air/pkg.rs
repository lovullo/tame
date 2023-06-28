// ASG IR package parsing
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

//! AIR package parser.
//!
//! See the [parent module](super) for more information.

use super::{
    super::{graph::object::Pkg, AsgError, ObjectIndex},
    ir::AirLiteratePkg,
    AirAggregate, AirAggregateCtx,
};
use crate::{diagnostic_todo, parse::prelude::*};

/// Package parsing with support for loaded identifiers.
///
/// This supports non-nested package definitions of source files,
///   as well as declaring opaque identifiers loaded from object files via
///   [`AirIdent`](super::ir::AirIdent).
#[derive(Debug, PartialEq)]
pub enum AirPkgAggregate {
    /// Ready for an expression;
    ///   expression stack is empty.
    Ready,

    /// Expecting a package-level token.
    Toplevel(ObjectIndex<Pkg>),
}

impl Display for AirPkgAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirPkgAggregate::*;

        match self {
            Ready => {
                write!(f, "expecting package definition")
            }
            Toplevel(_) => {
                write!(f, "expecting package header or an expression")
            }
        }
    }
}

impl ParseState for AirPkgAggregate {
    type Token = AirLiteratePkg;
    type Object = ();
    type Error = AsgError;
    type Context = AirAggregateCtx;
    type Super = AirAggregate;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self::Super> {
        use super::ir::{AirDoc::*, AirPkg::*};
        use AirLiteratePkg::*;
        use AirPkgAggregate::*;

        match (self, tok) {
            (st @ (Ready | Toplevel(..)), AirPkg(PkgStart(span, name))) => {
                if let Some(first) =
                    ctx.pkg_oi().map(|oi| oi.resolve(ctx.asg_ref()))
                {
                    let first_span = first.span();
                    let first_name = first.canonical_name();

                    Transition(st).err(AsgError::NestedPkgStart(
                        (span, name),
                        (first_span, first_name),
                    ))
                } else {
                    match ctx.pkg_begin(span, name) {
                        Ok(oi_pkg) => Transition(Toplevel(oi_pkg)).incomplete(),
                        Err(e) => Transition(Ready).err(e),
                    }
                }
            }

            (Toplevel(oi_pkg), AirPkg(PkgEnd(span))) => {
                oi_pkg.close(ctx.asg_mut(), span);
                ctx.pkg_clear();
                Transition(Ready).incomplete()
            }

            (Toplevel(oi_pkg), tok @ AirDoc(DocIndepClause(..))) => {
                diagnostic_todo!(
                    vec![
                        oi_pkg.note("for this package"),
                        tok.internal_error(
                            "this package description is not yet supported"
                        )
                    ],
                    "package-level short description is not yet supported by TAMER",
                )
            }

            (Toplevel(oi_pkg), AirDoc(DocText(text))) => {
                oi_pkg.append_doc_text(ctx.asg_mut(), text);
                Transition(Toplevel(oi_pkg)).incomplete()
            }

            // Package import
            (Toplevel(oi_pkg), AirPkg(PkgImport(namespec))) => oi_pkg
                .import(ctx.asg_mut(), namespec)
                .map(|_| ())
                .transition(Toplevel(oi_pkg)),

            (Ready, AirPkg(PkgImport(namespec))) => {
                Transition(Ready).err(AsgError::InvalidPkgImport(namespec))
            }

            (Ready, AirPkg(PkgEnd(span))) => {
                Transition(Ready).err(AsgError::InvalidPkgEndContext(span))
            }

            // Token may refer to a parent context.
            (st @ Ready, tok @ AirDoc(..)) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(self, Self::Ready)
    }
}

impl AirPkgAggregate {
    pub fn new() -> Self {
        Self::Ready
    }

    /// The [`ObjectIndex`] of the package being parsed,
    ///   if any.
    pub fn active_pkg_oi(&self) -> Option<ObjectIndex<Pkg>> {
        use AirPkgAggregate::*;

        match self {
            Ready => None,
            Toplevel(oi_pkg) => Some(*oi_pkg),
        }
    }
}
