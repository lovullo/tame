// Lower `xmlo` object file into AIR
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

//! Lower [`xmlo` IR](crate::obj::xmlo) into [AIR`](crate::asg::air).

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use fxhash::FxHashSet;

use crate::{
    asg::{air::AirToken, IdentKind, Source},
    diagnose::{AnnotatedSpan, Diagnostic},
    obj::xmlo::{SymAttrs, SymType},
    parse::{ParseState, ParseStatus, Transition, Transitionable},
    span::Span,
    sym::SymbolId,
};

use super::XmloToken;

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

type PackageName = SymbolId;

/// State machine for lowering into the [`Asg`](crate::asg::Asg) via
///   [`AirToken`].
#[derive(Debug, PartialEq, Eq, Default)]
pub enum XmloToAir {
    #[default]
    PackageExpected,
    Package(PackageName, Span),
    SymDep(PackageName, Span, SymbolId),
    /// End of header (EOH) reached.
    Done(Span),
}

impl ParseState for XmloToAir {
    type Token = XmloToken;
    type Object = AirToken;
    type Error = XmloAirError;

    type Context = XmloAirContext;

    fn parse_token(
        self,
        tok: Self::Token,
        ctx: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use XmloToAir::*;

        match (self, tok) {
            (PackageExpected, XmloToken::PkgName(name, span)) => {
                if ctx.is_first() {
                    ctx.prog_name = Some(name);
                }

                Transition(Package(name, span)).incomplete()
            }

            (st @ Package(..), XmloToken::PkgRootPath(relroot, _)) => {
                if ctx.is_first() {
                    ctx.relroot = Some(relroot);
                }

                Transition(st).incomplete()
            }

            // Eligibility classes are rooted as soon as they are
            //   encountered on the root node,
            //     which will result in a missing identifier until its
            //     definition is encountered later within the same file.
            // TODO: Let's remove the need for this special root handling
            //   here.
            (
                Package(pkg_name, span),
                XmloToken::PkgEligClassYields(pkg_elig, _),
            ) => Transition(Package(pkg_name, span))
                .ok(AirToken::IdentRoot(pkg_elig)),

            (
                st @ (PackageExpected | Package(..)),
                XmloToken::PkgProgramFlag(_),
            ) => {
                // TODO: Unused
                Transition(st).incomplete()
            }

            (
                Package(pkg_name, span) | SymDep(pkg_name, span, ..),
                XmloToken::SymDepStart(sym, _),
            ) => Transition(SymDep(pkg_name, span, sym)).incomplete(),

            (SymDep(pkg_name, span, sym), XmloToken::Symbol(dep_sym, _)) => {
                Transition(SymDep(pkg_name, span, sym))
                    .ok(AirToken::IdentDep(sym, dep_sym))
            }

            (
                Package(pkg_name, span),
                XmloToken::SymDecl(
                    _sym,
                    SymAttrs {
                        src: Some(sym_src), ..
                    },
                    _span,
                ),
            ) => {
                ctx.found.get_or_insert(Default::default()).insert(sym_src);
                Transition(Package(pkg_name, span)).incomplete()
            }

            (
                Package(pkg_name, span),
                XmloToken::SymDecl(sym, attrs, _span),
            ) => {
                let extern_ = attrs.extern_;

                // TODO: This attr/source separation is a mess,
                //   the IR is a mess,
                //   and spans are not retained.
                (&attrs)
                    .try_into()
                    .and_then(|kindval| {
                        let mut src: Source = attrs.into();

                        // This used to come from SymAttrs in the old XmloReader.
                        if src.pkg_name.is_none() {
                            src.pkg_name.replace(pkg_name);
                        }

                        // Existing convention is to omit @src of local package
                        // (in this case, the program being linked)
                        if ctx.is_first() {
                            src.pkg_name = None;
                        }

                        if extern_ {
                            Ok(ParseStatus::Object(AirToken::IdentExternDecl(
                                sym, kindval, src,
                            )))
                        } else {
                            Ok(ParseStatus::Object(AirToken::IdentDecl(
                                sym, kindval, src,
                            )))
                        }
                    })
                    .transition(Package(pkg_name, span))
            }

            (
                Package(pkg_name, span) | SymDep(pkg_name, span, _),
                XmloToken::Fragment(sym, text, _),
            ) => Transition(Package(pkg_name, span))
                .ok(AirToken::IdentFragment(sym, text)),

            // We don't need to read any further than the end of the
            //   header (symtable, sym-deps, fragments).
            (Package(..) | SymDep(..), XmloToken::Eoh(span)) => {
                // It's important to set this _after_ we're done processing,
                //   otherwise our `first` checks above will be inaccurate.
                ctx.first = false;

                // Note that this uses `incomplete` because we have nothing
                //   to yield,
                //     but we are in fact done.
                Transition(Done(span)).incomplete()
            }

            (st, tok) => Transition(st).dead(tok),
        }
    }

    fn is_accepting(&self) -> bool {
        matches!(*self, Self::Done(_))
    }
}

impl Display for XmloToAir {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use XmloToAir::*;

        match self {
            PackageExpected => write!(f, "expecting package definition"),
            Package(name, _) => {
                write!(f, "expecting package `/{name}` declarations")
            }
            SymDep(pkg_name, _, sym) => {
                write!(f, "expecting dependency for symbol `/{pkg_name}/{sym}`")
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
mod test {
    use super::*;
    use crate::{
        asg::{FragmentText, IdentKind},
        num::{Dim, Dtype},
        obj::xmlo::{SymAttrs, SymType},
        parse::Parsed,
        span::{dummy::*, UNKNOWN_SPAN},
        sym::GlobalSymbolIntern,
    };

    type Sut = XmloToAir;

    #[test]
    fn data_from_package_event() {
        let name = "name".into();
        let relroot = "some/path".into();

        let toks = vec![
            XmloToken::PkgName(name, S1),
            XmloToken::PkgRootPath(relroot, S2),
            XmloToken::Eoh(S3),
        ]
        .into_iter();

        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // PkgName
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // PkgRootPath
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Eoh

        let ctx = sut.finalize().unwrap();

        assert_eq!(Some(name), ctx.prog_name);
        assert_eq!(Some(relroot), ctx.relroot);
    }

    #[test]
    fn adds_elig_as_root() {
        let name = "name-root".into();
        let elig_sym = "elig".into();

        let toks = vec![
            XmloToken::PkgName(name, S1),
            XmloToken::PkgEligClassYields(elig_sym, S2),
            XmloToken::Eoh(S3),
        ];

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete, // PkgName
                Parsed::Object(AirToken::IdentRoot(elig_sym)),
                Parsed::Incomplete, // Eoh
            ]),
            Sut::parse(toks.into_iter()).collect(),
        );
    }

    #[test]
    fn adds_sym_deps() {
        let sym_from = "from".into();
        let sym_to1 = "to1".into();
        let sym_to2 = "to2".into();

        let toks = vec![
            XmloToken::PkgName("name".into(), S1),
            XmloToken::SymDepStart(sym_from, S2),
            XmloToken::Symbol(sym_to1, S3),
            XmloToken::Symbol(sym_to2, S4),
            XmloToken::Eoh(S1),
        ];

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete, // PkgName
                Parsed::Incomplete, // SymDepStart
                Parsed::Object(AirToken::IdentDep(sym_from, sym_to1)),
                Parsed::Object(AirToken::IdentDep(sym_from, sym_to2)),
                Parsed::Incomplete, // Eoh
            ]),
            Sut::parse(toks.into_iter()).collect(),
        );
    }

    #[test]
    fn sym_decl_with_src_not_added_and_populates_found() {
        let sym = "sym".into();
        let src_a = "src_a".into();
        let src_b = "src_b".into();

        let toks = vec![
            XmloToken::PkgName("name".into(), S1),
            XmloToken::SymDecl(
                sym,
                SymAttrs {
                    src: Some(src_a),
                    ..Default::default()
                },
                S2,
            ),
            XmloToken::SymDecl(
                sym,
                SymAttrs {
                    src: Some(src_b),
                    ..Default::default()
                },
                S3,
            ),
            XmloToken::Eoh(S1),
        ];

        let mut sut = Sut::parse(toks.into_iter());

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // PkgName
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // SymDecl (@src)
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // SymDecl (@src)
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Eoh

        let ctx = sut.finalize().unwrap();
        let mut founds = ctx.found.unwrap().into_iter().collect::<Vec<_>>();

        // Just to remove nondeterminism in case the iteration order happens
        //   to change.
        founds.sort();

        assert_eq!(vec![src_a, src_b], founds);
    }

    #[test]
    fn sym_decl_added_to_graph() {
        let sym_extern = "sym_extern".into();
        let sym_non_extern = "sym_non_extern".into();
        let sym_map = "sym_map".into();
        let sym_retmap = "sym_retmap".into();
        let pkg_name = "pkg name".into();

        let toks = vec![
            XmloToken::PkgName("name".into(), S1),
            XmloToken::SymDecl(
                sym_extern,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    extern_: true,
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                S1,
            ),
            XmloToken::SymDecl(
                sym_non_extern,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                S2,
            ),
            XmloToken::SymDecl(
                sym_map,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Map),
                    ..Default::default()
                },
                S3,
            ),
            XmloToken::SymDecl(
                sym_retmap,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::RetMap),
                    ..Default::default()
                },
                S4,
            ),
            XmloToken::Eoh(S1),
        ];

        let mut sut = Sut::parse(toks.into_iter());

        // Note that each of these will have their package names cleared
        //   since this is considered to be the first package encountered.
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // PkgName
        assert_eq!(
            Some(Ok(Parsed::Object(AirToken::IdentExternDecl(
                sym_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                }
            )))),
            sut.next(),
        );
        assert_eq!(
            Some(Ok(Parsed::Object(AirToken::IdentDecl(
                sym_non_extern,
                IdentKind::Meta,
                Source {
                    pkg_name: None,
                    ..Default::default()
                }
            )))),
            sut.next(),
        );
        assert_eq!(
            Some(Ok(Parsed::Object(AirToken::IdentDecl(
                sym_map,
                IdentKind::Map,
                Source {
                    pkg_name: None,
                    ..Default::default()
                }
            )))),
            sut.next(),
        );
        assert_eq!(
            Some(Ok(Parsed::Object(AirToken::IdentDecl(
                sym_retmap,
                IdentKind::RetMap,
                Source {
                    pkg_name: None,
                    ..Default::default()
                }
            )))),
            sut.next(),
        );
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // Eoh

        let ctx = sut.finalize().unwrap();

        // Both above symbols were local (no `src`),
        //   but note that we don't care if it's None or initialized with a
        //   length of 0.
        assert!(ctx.found.unwrap_or_default().len() == 0);
    }

    // See above test, where pkg_name was cleared.
    #[test]
    fn sym_decl_pkg_name_retained_if_not_first() {
        let sym = "sym".into();
        let pkg_name = "pkg name".into();

        // This is all that's needed to not consider this to be the first
        //   package,
        //     so that pkg_name is retained below.
        let ctx = XmloAirContext {
            first: false,
            ..Default::default()
        };

        let toks = vec![
            XmloToken::PkgName(pkg_name, S1),
            XmloToken::SymDecl(
                sym,
                SymAttrs {
                    pkg_name: Some(pkg_name),
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            ),
            XmloToken::Eoh(S1),
        ];

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete, // PkgName
                Parsed::Object(AirToken::IdentDecl(
                    sym,
                    IdentKind::Meta,
                    Source {
                        pkg_name: Some(pkg_name),
                        ..Default::default()
                    }
                )),
                Parsed::Incomplete, // Eoh
            ]),
            Sut::parse_with_context(toks.into_iter(), ctx).collect(),
        );
    }

    // This used to be set in SymAttrs by XmloReader,
    //   but that's no longer true with the new reader.
    #[test]
    fn sym_decl_pkg_name_set_if_empty_and_not_first() {
        let sym = "sym".into();
        let pkg_name = "pkg name".into();

        let ctx = XmloAirContext {
            first: false,
            ..Default::default()
        };

        let toks = vec![
            XmloToken::PkgName(pkg_name, S1),
            XmloToken::SymDecl(
                sym,
                SymAttrs {
                    // No name
                    ty: Some(SymType::Meta),
                    ..Default::default()
                },
                UNKNOWN_SPAN,
            ),
            XmloToken::Eoh(S1),
        ];

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete, // PkgName
                Parsed::Object(AirToken::IdentDecl(
                    sym,
                    IdentKind::Meta,
                    Source {
                        pkg_name: Some(pkg_name), // Name inherited
                        ..Default::default()
                    },
                )),
                Parsed::Incomplete, // Eoh
            ]),
            Sut::parse_with_context(toks.into_iter(), ctx).collect(),
        );
    }

    #[test]
    fn ident_kind_conversion_error_propagates() {
        let sym = "sym".into();
        let bad_attrs = SymAttrs::default();

        let toks = vec![
            XmloToken::PkgName("name".into(), S1),
            XmloToken::SymDecl(sym, bad_attrs, S2),
            XmloToken::Eoh(S1),
        ];

        Sut::parse(toks.into_iter())
            .collect::<Result<Vec<_>, _>>()
            .expect_err("expected IdentKind conversion error");
    }

    #[test]
    fn sets_fragment() {
        let sym = "sym".into();
        let frag = FragmentText::from("foo");

        let toks = vec![
            XmloToken::PkgName("name".into(), S1),
            XmloToken::Fragment(sym, frag.clone(), S2),
            XmloToken::Eoh(S1),
        ];

        assert_eq!(
            Ok(vec![
                Parsed::Incomplete, // PkgName
                Parsed::Object(AirToken::IdentFragment(sym, frag)),
                Parsed::Incomplete, // Eoh
            ]),
            Sut::parse(toks.into_iter()).collect(),
        );
    }

    macro_rules! test_kind {
        ($name:ident, $src:expr => $dest:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    Ok($dest),
                    SymAttrs {
                        ty: Some($src),
                        ..Default::default()
                    }
                    .try_into()
                );
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim) => {
            #[test]
            fn $name() {
                let dim = Dim::Vector;

                assert_eq!(
                    Ok($dest(Dim::Vector)),
                    SymAttrs {
                        ty: Some($src),
                        dim: Some(dim),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(XmloAirError::MissingDim, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dtype) => {
            #[test]
            fn $name() {
                let dtype = Dtype::Float;

                assert_eq!(
                    Ok($dest(dtype)),
                    SymAttrs {
                        ty: Some($src),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dtype
                let result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(XmloAirError::MissingDtype, result);
            }
        };

        ($name:ident, $src:expr => $dest:expr, dim, dtype) => {
            #[test]
            fn $name() {
                let dim = Dim::Vector;
                let dtype = Dtype::Float;

                assert_eq!(
                    Ok($dest(Dim::Vector, dtype)),
                    SymAttrs {
                        ty: Some($src),
                        dim: Some(dim),
                        dtype: Some(dtype),
                        ..Default::default()
                    }
                    .try_into()
                );

                // no dim
                let dim_result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    dtype: Some(dtype),
                    ..Default::default()
                })
                .expect_err("must fail when missing dim");

                assert_eq!(XmloAirError::MissingDim, dim_result);

                // no dtype
                let dtype_result = IdentKind::try_from(SymAttrs {
                    ty: Some($src),
                    dim: Some(dim),
                    ..Default::default()
                })
                .expect_err("must fail when missing dtype");

                assert_eq!(XmloAirError::MissingDtype, dtype_result);
            }
        };
    }

    test_kind!(cgen, SymType::Cgen => IdentKind::Cgen, dim);
    test_kind!(class, SymType::Class => IdentKind::Class, dim);
    test_kind!(r#const, SymType::Const => IdentKind::Const, dim, dtype);
    test_kind!(func, SymType::Func => IdentKind::Func, dim, dtype);
    test_kind!(gen, SymType::Gen => IdentKind::Gen, dim, dtype);
    test_kind!(lparam, SymType::Lparam => IdentKind::Lparam, dim, dtype);
    test_kind!(param, SymType::Param => IdentKind::Param, dim, dtype);
    test_kind!(rate, SymType::Rate => IdentKind::Rate, dtype);
    test_kind!(tpl, SymType::Tpl => IdentKind::Tpl);
    test_kind!(r#type, SymType::Type => IdentKind::Type, dtype);
    test_kind!(maphead, SymType::MapHead => IdentKind::MapHead);
    test_kind!(map, SymType::Map => IdentKind::Map);
    test_kind!(maptail, SymType::MapTail => IdentKind::MapTail);
    test_kind!(retmaphead, SymType::RetMapHead => IdentKind::RetMapHead);
    test_kind!(retmap, SymType::RetMap => IdentKind::RetMap);
    test_kind!(retmaptail, SymType::RetMapTail => IdentKind::RetMapTail);
    test_kind!(meta, SymType::Meta => IdentKind::Meta);
    test_kind!(worksheet, SymType::Worksheet => IdentKind::Worksheet);

    #[test]
    fn source_from_sym_attrs() {
        let nsym: SymbolId = "name".intern();
        let ssym: SymbolId = "src".intern();
        let psym: SymbolId = "parent".intern();
        let ysym: SymbolId = "yields".intern();
        let fsym: SymbolId = "from".intern();

        let attrs = SymAttrs {
            pkg_name: Some(nsym),
            src: Some(ssym),
            generated: true,
            parent: Some(psym),
            yields: Some(ysym),
            desc: Some("sym desc".into()),
            from: Some(fsym),
            virtual_: true,
            override_: true,
            ..Default::default()
        };

        assert_eq!(
            Source {
                pkg_name: Some(nsym),
                src: Some(ssym),
                generated: attrs.generated,
                parent: attrs.parent,
                yields: attrs.yields,
                desc: Some("sym desc".into()),
                from: Some(fsym),
                virtual_: true,
                override_: true,
            },
            attrs.into(),
        );
    }
}
