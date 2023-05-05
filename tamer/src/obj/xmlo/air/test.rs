// Tests lowering `xmlo` object file into AIR
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

use super::*;
use crate::{
    asg::{FragmentText, IdentKind},
    num::{Dim, Dtype},
    obj::xmlo::{SymAttrs, SymType},
    parse::Parsed,
    span::dummy::*,
    sym::GlobalSymbolIntern,
};

type Sut = XmloToAir;

use Parsed::{Incomplete, Object as O};

#[test]
fn data_from_package_event() {
    let name = "name".into();
    let relroot = "some/path".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),
          PkgRootPath(SPair(relroot, S4)),
        Eoh(S4),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(name, S2))),
              Incomplete,         // PkgRootPath
            O(Air::PkgEnd(S4)),
        ]),
        sut.by_ref().collect(),
    );

    let ctx = sut.finalize().unwrap().into_context();

    assert_eq!(Some(name), ctx.prog_name);
    assert_eq!(Some(relroot), ctx.relroot);
}

#[test]
fn adds_elig_as_root() {
    let name = "name-root".into();
    let elig_sym = "elig".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),
          PkgEligClassYields(SPair(elig_sym, S3)),
        Eoh(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(name, S2))),
              O(Air::IdentRoot(SPair(elig_sym, S3))),
            O(Air::PkgEnd(S4)), // Eoh
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn adds_sym_deps() {
    let name = "name".into();
    let sym_from = "from".into();
    let sym_to1 = "to1".into();
    let sym_to2 = "to2".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),

          SymDepStart(SPair(sym_from, S3)),
          Symbol(SPair(sym_to1, S4)),
          Symbol(SPair(sym_to2, S5)),
        Eoh(S6),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(name, S2))),
              Incomplete, // SymDepStart
              O(Air::IdentDep(SPair(sym_from, S3), SPair(sym_to1, S4))),
              O(Air::IdentDep(SPair(sym_from, S3), SPair(sym_to2, S5))),
            O(Air::PkgEnd(S6)),
        ]),
        Sut::parse(toks.into_iter()).collect(),
    );
}

#[test]
fn sym_decl_with_src_not_added_and_populates_found() {
    let name = "name".into();
    let sym = "sym".into();
    let src_a = "src_a".into();
    let src_b = "src_b".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),

          SymDecl(
              SPair(sym, S3),
              SymAttrs {
                  src: Some(src_a),
                  ..Default::default()
              },
          ),
          SymDecl(
              SPair(sym, S4),
              SymAttrs {
                  src: Some(src_b),
                  ..Default::default()
              },
          ),
        Eoh(S5),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(name, S2))),
              Incomplete, // SymDecl (@src)
              Incomplete, // SymDecl (@src)
            O(Air::PkgEnd(S5)),
        ]),
        sut.by_ref().collect(),
    );

    let ctx = sut.finalize().unwrap().into_context();
    let mut founds = ctx.found.unwrap().into_iter().collect::<Vec<_>>();

    // Just to remove nondeterminism in case the iteration order happens
    //   to change.
    founds.sort();

    assert_eq!(vec![src_a, src_b], founds);
}

#[test]
fn sym_decl_added_to_graph() {
    let name = "name".into();
    let sym_extern = "sym_extern".into();
    let sym_non_extern = "sym_non_extern".into();
    let sym_map = "sym_map".into();
    let sym_retmap = "sym_retmap".into();
    let pkg_name = "pkg name".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),

          SymDecl(
              SPair(sym_extern, S3),
              SymAttrs {
                  pkg_name: Some(pkg_name),
                  extern_: true,
                  ty: Some(SymType::Meta),
                  ..Default::default()
              },
          ),
          SymDecl(
              SPair(sym_non_extern, S4),
              SymAttrs {
                  pkg_name: Some(pkg_name),
                  ty: Some(SymType::Meta),
                  ..Default::default()
              },
          ),
          SymDecl(
              SPair(sym_map, S5),
              SymAttrs {
                  pkg_name: Some(pkg_name),
                  ty: Some(SymType::Map),
                  ..Default::default()
              },
          ),
          SymDecl(
              SPair(sym_retmap, S6),
              SymAttrs {
                  pkg_name: Some(pkg_name),
                  ty: Some(SymType::RetMap),
                  ..Default::default()
              },
          ),
        Eoh(S7),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    // Note that each of these will have their package names cleared
    //   since this is considered to be the first package encountered.
    assert_eq!(Some(Ok(O(Air::PkgStart(S1)))), sut.next()); // PkgStart
    assert_eq!(Some(Ok(O(Air::BindIdent(SPair(name, S2))))), sut.next());
    assert_eq!(
        Some(Ok(O(Air::IdentExternDecl(
            SPair(sym_extern, S3),
            IdentKind::Meta,
            Source {
                pkg_name: None,
                ..Default::default()
            }
        )))),
        sut.next(),
    );
    assert_eq!(
        Some(Ok(O(Air::IdentDecl(
            SPair(sym_non_extern, S4),
            IdentKind::Meta,
            Source {
                pkg_name: None,
                ..Default::default()
            }
        )))),
        sut.next(),
    );
    assert_eq!(
        Some(Ok(O(Air::IdentDecl(
            SPair(sym_map, S5),
            IdentKind::Map,
            Source {
                pkg_name: None,
                ..Default::default()
            }
        )))),
        sut.next(),
    );
    assert_eq!(
        Some(Ok(O(Air::IdentDecl(
            SPair(sym_retmap, S6),
            IdentKind::RetMap,
            Source {
                pkg_name: None,
                ..Default::default()
            }
        )))),
        sut.next(),
    );

    assert_eq!(Some(Ok(O(Air::PkgEnd(S7)))), sut.next());

    let ctx = sut.finalize().unwrap().into_context();

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

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(pkg_name, S2)),

          SymDecl(
              SPair(sym, S3),
              SymAttrs {
                  pkg_name: Some(pkg_name),
                  ty: Some(SymType::Meta),
                  ..Default::default()
              },
          ),
        Eoh(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(pkg_name, S2))),

              O(Air::IdentDecl(
                  SPair(sym, S3),
                  IdentKind::Meta,
                  Source {
                      pkg_name: Some(pkg_name),
                      ..Default::default()
                  }
              )),
            O(Air::PkgEnd(S4)),
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

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(pkg_name, S2)),

          SymDecl(
              SPair(sym, S3),
              SymAttrs {
                  // No name
                  ty: Some(SymType::Meta),
                  ..Default::default()
              },
          ),
        Eoh(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(pkg_name, S2))),

              O(Air::IdentDecl(
                  SPair(sym, S3),
                  IdentKind::Meta,
                  Source {
                      pkg_name: Some(pkg_name), // Name inherited
                      ..Default::default()
                  },
              )),
            O(Air::PkgEnd(S4)),
        ]),
        Sut::parse_with_context(toks.into_iter(), ctx).collect(),
    );
}

#[test]
fn ident_kind_conversion_error_propagates() {
    let sym = "sym".into();
    let bad_attrs = SymAttrs::default();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair("name".into(), S2)),
          SymDecl(SPair(sym, S3), bad_attrs),
        Eoh(S1),
    ];

    Sut::parse(toks.into_iter())
        .collect::<Result<Vec<_>, _>>()
        .expect_err("expected IdentKind conversion error");
}

#[test]
fn sets_fragment() {
    let name = "name".into();
    let sym = "sym".into();
    let frag = FragmentText::from("foo");

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          PkgName(SPair(name, S2)),
          Fragment(SPair(sym, S3), frag.clone()),
        Eoh(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(Air::PkgStart(S1)),
              O(Air::BindIdent(SPair(name, S2))),
              O(Air::IdentFragment(SPair(sym, S3), frag)),
            O(Air::PkgEnd(S4)),
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
