// Lower `xmlo` object file into AIR
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

//! Lower [`xmlo` IR](crate::obj::xmlo) into [AIR`](crate::asg::air).

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use fxhash::FxHashSet;

use crate::{
    asg::{air::Air, IdentKind, Source},
    diagnose::{AnnotatedSpan, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    obj::xmlo::{SymAttrs, SymType},
    parse::{util::SPair, ParseState, ParseStatus, Transition, Transitionable},
    span::Span,
    sym::SymbolId,
};

use super::XmloToken::{self, *};

/// Persistent `xmlo` lowering context to be shared among all `xmlo` files.
///
/// TODO: Continue refactoring this into [`XmloToAir`] and the ASG itself.
#[derive(Debug)]
pub struct XmloAirContext {
    /// Relative paths to imported packages that have been discovered.
    ///
    /// The caller will use these to perform recursive loads.
    /// This is contained within an [`Option`] so that the caller can `take`
    ///   ownership over its contents.
    pub found: Option<FxHashSet<SymbolId>>,

    /// Program name once discovered.
    ///
    /// This will be set by the first package encountered.
    pub prog_name: Option<SymbolId>,

    /// Relative path to project root once discovered.
    ///
    /// This will be set by the first package encountered.
    pub relroot: Option<SymbolId>,

    /// Whether this is the first package encountered.
    ///
    /// This defaults to [`true`] and is updated to [`false`] at EOH.
    first: bool,
}

impl Default for XmloAirContext {
    fn default() -> Self {
        Self {
            found: None,
            prog_name: None,
            relroot: None,
            first: true,
        }
    }
}

impl XmloAirContext {
    /// Whether this is the first discovered package.
    #[inline]
    fn is_first(&self) -> bool {
        self.first
    }
}

type PackageSPair = SPair;

/// State machine for lowering into the [`Asg`](crate::asg::Asg) via
///   [`Air`].
#[derive(Debug, PartialEq, Eq, Default)]
pub enum XmloToAir {
    #[default]
    PackageExpected,
    PackageFound(Span),
    Package(PackageSPair),
    SymDep(PackageSPair, SPair),
    SymDepEnded(PackageSPair, Span),
    /// End of header (EOH) reached.
    Done(Span),
}

impl ParseState for XmloToAir {
    type Token = XmloToken;
    type Object = Air;
    type Error = XmloAirError;

    type Context = XmloAirContext;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use XmloToAir::*;

        match (self, tok) {
            (PackageExpected, PkgStart(span)) => {
                Transition(PackageFound(span)).incomplete()
            }

            (PackageExpected, tok) => Transition(PackageExpected).dead(tok),

            (PackageFound(span), PkgName(name)) => {
                if ctx.is_first() {
                    ctx.prog_name = Some(name.symbol());
                }

                Transition(Package(name)).ok(Air::PkgStart(span, name))
            }

            (st @ Package(..), PkgRootPath(relroot)) => {
                if ctx.is_first() {
                    ctx.relroot = Some(relroot.symbol());
                }

                Transition(st).incomplete()
            }

            // Eligibility classes are rooted as soon as they are
            //   encountered on the root node,
            //     which will result in a missing identifier until its
            //     definition is encountered later within the same file.
            // TODO: Let's remove the need for this special root handling
            //   here.
            (Package(name), PkgEligClassYields(pkg_elig)) => {
                // The span for this is a bit awkward,
                //   given that rooting is automatic,
                //   but it it should never have to be utilized in
                //     diagnostics unless there is a compiler bug.
                Transition(Package(name)).ok(Air::IdentRoot(pkg_elig))
            }

            (st @ (PackageFound(..) | Package(..)), PkgProgramFlag(_)) => {
                // TODO: Unused
                Transition(st).incomplete()
            }

            (Package(pkg_name) | SymDep(pkg_name, ..), SymDepStart(sym)) => {
                Transition(SymDep(pkg_name, sym)).incomplete()
            }

            (SymDep(pkg_name, sym), Symbol(dep_sym)) => {
                Transition(SymDep(pkg_name, sym))
                    .ok(Air::IdentDep(sym, dep_sym))
            }

            (
                Package(pkg_name),
                SymDecl(
                    _name,
                    SymAttrs {
                        src: Some(sym_src), ..
                    },
                ),
            ) => {
                ctx.found.get_or_insert(Default::default()).insert(sym_src);
                Transition(Package(pkg_name)).incomplete()
            }

            (Package(pkg_name), SymDecl(name, attrs)) => {
                let extern_ = attrs.extern_;

                // TODO: This attr/source separation is a mess,
                //   the IR is a mess,
                //   and spans are not retained.
                (&attrs)
                    .try_into()
                    .map(|kindval| {
                        let mut src: Source = attrs.into();

                        // This used to come from SymAttrs in the old XmloReader.
                        if src.pkg_name.is_none() {
                            src.pkg_name.replace(pkg_name.symbol());
                        }

                        // Existing convention is to omit @src of local package
                        // (in this case, the program being linked)
                        if ctx.is_first() {
                            src.pkg_name = None;
                        }

                        if extern_ {
                            ParseStatus::Object(Air::IdentExternDecl(
                                name, kindval, src,
                            ))
                        } else {
                            ParseStatus::Object(Air::IdentDecl(
                                name, kindval, src,
                            ))
                        }
                    })
                    .transition(Package(pkg_name))
            }

            (Package(pkg_name) | SymDep(pkg_name, _), SymDepEnd(span)) => {
                Transition(SymDepEnded(pkg_name, span)).incomplete()
            }

            (
                Package(pkg_name)
                | SymDep(pkg_name, _)
                | SymDepEnded(pkg_name, _),
                Fragment(name, text),
            ) => {
                Transition(Package(pkg_name)).ok(Air::IdentFragment(name, text))
            }

            // We don't need to read any further than the end of the
            //   header (symtable, sym-deps, fragments).
            (Package(..) | SymDep(..) | SymDepEnded(..), Eoh(span)) => {
                // It's important to set this _after_ we're done processing,
                //   otherwise our `first` checks above will be inaccurate.
                ctx.first = false;

                // Note that this uses `incomplete` because we have nothing
                //   to yield,
                //     but we are in fact done.
                Transition(Done(span)).ok(Air::PkgEnd(span))
            }

            (
                st @ Package(..),
                tok @ (PkgStart(..) | PkgName(..) | Symbol(..)),
            ) => Transition(st).dead(tok),

            (
                st @ (PackageFound(..) | SymDep(..) | SymDepEnded(..)
                | Done(..)),
                tok,
            ) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        matches!(*self, Self::Done(_))
    }

    fn eof_tok(&self, _ctx: &Self::Context) -> Option<Self::Token> {
        use XmloToAir::*;

        match self {
            // We are able to stop parsing immediately after symbol
            //   dependencies have ended if the caller wishes to ignore
            //   fragments.
            // Pretend that we received an `Eoh` token in this case so that
            //   we can conclude parsing.
            SymDepEnded(_, span) => Some(XmloToken::Eoh(*span)),

            Package(_)
            | PackageExpected
            | PackageFound(_)
            | SymDep(_, _)
            | Done(_) => None,
        }
    }
}

impl Display for XmloToAir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XmloToAir::*;

        match self {
            PackageExpected => write!(f, "expecting package definition"),
            PackageFound(_) => write!(f, "package found, awaiting definition"),
            Package(name) => {
                write!(f, "expecting package `/{name}` declarations")
            }
            SymDep(pkg_name, sym) => {
                write!(f, "expecting dependency for symbol `/{pkg_name}/{sym}`")
            }
            SymDepEnded(pkg_name, _) => {
                write!(
                    f,
                    "expecting fragments or end of header for package {}",
                    TtQuote::wrap(pkg_name)
                )
            }
            Done(_) => write!(f, "done lowering xmlo into AIR"),
        }
    }
}

impl TryFrom<SymAttrs> for IdentKind {
    type Error = XmloAirError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: SymAttrs) -> Result<Self, Self::Error> {
        Self::try_from(&attrs)
    }
}

impl TryFrom<&SymAttrs> for IdentKind {
    type Error = XmloAirError;

    /// Attempt to raise [`SymAttrs`] into an [`IdentKind`].
    ///
    /// Certain [`IdentKind`] require that certain attributes be present,
    ///   otherwise the conversion will fail.
    fn try_from(attrs: &SymAttrs) -> Result<Self, Self::Error> {
        let ty = attrs.ty.as_ref().ok_or(Self::Error::MissingType)?;

        macro_rules! ident {
            ($to:expr) => {
                Ok($to)
            };
            ($to:expr, dim) => {
                Ok($to(attrs.dim.ok_or(Self::Error::MissingDim)?))
            };
            ($to:expr, dtype) => {
                Ok($to(attrs.dtype.ok_or(Self::Error::MissingDtype)?))
            };
            ($to:expr, dim, dtype) => {
                Ok($to(
                    attrs.dim.ok_or(Self::Error::MissingDim)?,
                    attrs.dtype.ok_or(Self::Error::MissingDtype)?,
                ))
            };
        }

        match ty {
            SymType::Cgen => ident!(Self::Cgen, dim),
            SymType::Class => ident!(Self::Class, dim),
            SymType::Const => ident!(Self::Const, dim, dtype),
            SymType::Func => ident!(Self::Func, dim, dtype),
            SymType::Gen => ident!(Self::Gen, dim, dtype),
            SymType::Lparam => ident!(IdentKind::Lparam, dim, dtype),
            SymType::Param => ident!(IdentKind::Param, dim, dtype),
            SymType::Rate => ident!(IdentKind::Rate, dtype),
            SymType::Tpl => ident!(IdentKind::Tpl),
            SymType::Type => ident!(IdentKind::Type, dtype),
            SymType::MapHead => ident!(IdentKind::MapHead),
            SymType::Map => ident!(IdentKind::Map),
            SymType::MapTail => ident!(IdentKind::MapTail),
            SymType::RetMapHead => ident!(IdentKind::RetMapHead),
            SymType::RetMap => ident!(IdentKind::RetMap),
            SymType::RetMapTail => ident!(IdentKind::RetMapTail),
            SymType::Meta => ident!(IdentKind::Meta),
            SymType::Worksheet => ident!(IdentKind::Worksheet),
        }
    }
}

impl From<SymAttrs> for Source {
    /// Raise Legacy IR [`SymAttrs`].
    ///
    /// This simply extracts a subset of fields from the source attributes.
    fn from(attrs: SymAttrs) -> Self {
        Source {
            pkg_name: attrs.pkg_name,
            src: attrs.src,
            generated: attrs.generated,
            parent: attrs.parent,
            yields: attrs.yields,
            desc: attrs.desc,
            from: attrs.from,
            virtual_: attrs.virtual_,
            override_: attrs.override_,
        }
    }
}

/// Error populating graph with `xmlo`-derived data.
///
/// TODO: Spans are needed!
#[derive(Debug, PartialEq)]
pub enum XmloAirError {
    /// Symbol type was not provided.
    MissingType,

    /// Number of symbol dimensions were not provided.
    MissingDim,

    /// Symbol dtype was not provided.
    MissingDtype,

    /// Eligibility classification references unknown identifier.
    ///
    /// This is generated by the compiler and so should never happen.
    /// (That's not to say that it won't, but it shouldn't.)
    BadEligRef(SymbolId),
}

impl Display for XmloAirError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::MissingType => write!(fmt, "missing symbol type"),
            Self::MissingDim => write!(fmt, "missing dim"),
            Self::MissingDtype => write!(fmt, "missing dtype"),
            Self::BadEligRef(name) => write!(
                fmt,
                "internal error: package elig references nonexistant symbol `{}`",
                name,
            ),
        }
    }
}

impl Diagnostic for XmloAirError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use XmloAirError::*;

        match self {
            // TODO: Missing spans!
            MissingType | MissingDim | MissingDtype | BadEligRef(_) => vec![],
        }
    }
}

impl Error for XmloAirError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod test;
