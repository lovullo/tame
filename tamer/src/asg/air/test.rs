// Tests for ASG IR
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

//! These are tested as if they are another API directly atop of the ASG,
//!   since that is how they are used.

use super::{super::Ident, *};
use crate::{
    asg::{
        graph::object::{ObjectRel, ObjectRelFrom, ObjectRelatable},
        IdentKind, ObjectIndexRelTo, Source, TransitionError,
    },
    parse::{ParseError, Parsed, Parser},
    span::dummy::*,
};

type Sut = AirAggregate;

use Air::*;
use Parsed::Incomplete;

#[test]
fn ident_decl() {
    let id = SPair("foo".into(), S2);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl".into()),
        ..Default::default()
    };

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          IdentDecl(id, kind.clone(), src.clone()),
          // Attempt re-declaration.
          IdentDecl(id, kind.clone(), src.clone()),
        PkgEnd(S3),
    ].into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete), // PkgStart
              Ok(Incomplete), // IdentDecl
              // Redeclare identifier
              Err(ParseError::StateError(AsgError::IdentTransition(
                  TransitionError::Redeclare(id, S2)
              ))),
              // RECOVERY: Ignore redeclaration
            Ok(Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<Result<Parsed<()>, _>>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S2, kind.clone(), src.clone())
            .as_ref(),
    );
}

#[test]
fn ident_extern_decl() {
    let id = SPair("foo".into(), S2);
    let re_id = SPair("foo".into(), S3);
    let kind = IdentKind::Tpl;
    let different_kind = IdentKind::Meta;
    let src = Source {
        src: Some("test/decl-extern".into()),
        ..Default::default()
    };

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          IdentExternDecl(id, kind.clone(), src.clone()),
          // Redeclare with a different kind
          IdentExternDecl(re_id, different_kind.clone(), src.clone()),
        PkgEnd(S4),
    ].into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete), // PkgStart
              Ok(Incomplete), // IdentDecl
              // Redeclare identifier with a different kind
              Err(ParseError::StateError(AsgError::IdentTransition(
                  TransitionError::ExternResolution(
                      id,
                      kind.clone(),
                      (different_kind, S3)
                  )
              ))),
              // RECOVERY: Ignore redeclaration
            Ok(Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<Result<Parsed<()>, _>>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id).extern_(S2, kind, src.clone()).as_ref(),
    );
}

#[test]
fn ident_dep() {
    let id = SPair("foo".into(), S2);
    let dep = SPair("dep".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          IdentDep(id, dep),
        PkgEnd(S4),
    ].into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete, // PkgStart
              Incomplete, // IdentDep
            Incomplete, // PkgEnd
        ]),
        sut.by_ref().collect(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let dep_node = root_lookup(&asg, dep).expect("dep was not added to graph");

    assert!(ident_node.has_edge_to(&asg, dep_node));
}

#[test]
fn ident_fragment() {
    let id = SPair("frag".into(), S2);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/frag".into()),
        ..Default::default()
    };
    let frag = "fragment text".into();

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          // Identifier must be declared before it can be given a
          //   fragment.
          IdentDecl(id, kind.clone(), src.clone()),
          IdentFragment(id, frag),
          // Reset fragment (error)
          IdentFragment(id, frag),
          // RECOVERY: Ignore reset
        PkgEnd(S4),
    ] .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete),   // PkgStart
              Ok(Incomplete), // IdentDecl
              Ok(Incomplete), // IdentFragment
              // Reset fragment
              Err(ParseError::StateError(AsgError::IdentTransition(
                  TransitionError::BadFragmentDest(id)
              ))),
              // RECOVERY: Ignore reset
            Ok(Incomplete),   // PkgEnd
        ],
        sut.by_ref().collect::<Vec<Result<Parsed<()>, _>>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S2, kind.clone(), src.clone())
            .and_then(|resolved| resolved.set_fragment(frag))
            .as_ref(),
    );
}

// Adding a root before the identifier exists should add a
//   `Ident::Missing`.
#[test]
fn ident_root_missing() {
    let id = SPair("toroot".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          IdentRoot(id),
        PkgEnd(S3),
    ].into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete, // PkgStart
              Incomplete, // IdentRoot
            Incomplete, // PkgEnd
        ]),
        sut.by_ref().collect(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The identifier did not previously exist,
    //   and so a missing node is created as a placeholder.
    assert_eq!(&Ident::Missing(id), ident);

    // And that missing identifier should be rooted.
    assert!(ident_node.is_rooted(&asg));
}

#[test]
fn ident_root_existing() {
    let id = SPair("toroot".into(), S2);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/root-existing".into()),
        ..Default::default()
    };

    // Ensure that it won't auto-root based on the kind,
    //   otherwise we won't be testing the right thing.
    assert!(!kind.is_auto_root());

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          IdentDecl(id, kind.clone(), src.clone()),
          IdentRoot(SPair(id.symbol(), S3)),
        PkgEnd(S3),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete, // PkgStart
              Incomplete, // IdentDecl
              Incomplete, // IdentRoot
            Incomplete, // PkgEnd
        ]),
        sut.by_ref().collect(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The previously-declared identifier...
    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S2, kind.clone(), src.clone())
            .as_ref()
    );

    // ...should have been subsequently rooted.
    assert!(ident_node.is_rooted(&asg));
}

#[test]
fn declare_kind_auto_root() {
    let auto_kind = IdentKind::Worksheet;
    let no_auto_kind = IdentKind::Tpl;

    // Sanity check, in case this changes.
    assert!(auto_kind.is_auto_root());
    assert!(!no_auto_kind.is_auto_root());

    let id_auto = SPair("auto_root".into(), S2);
    let id_no_auto = SPair("no_auto_root".into(), S3);

    let src = Source {
        src: Some("src/pkg".into()),
        ..Default::default()
    };

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1),
          // auto-rooting
          IdentDecl(id_auto, auto_kind, src.clone()),
          // non-auto-rooting
          IdentDecl(id_no_auto, no_auto_kind, src),
        PkgEnd(S4),
    ].into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete, // PkgStart
              Incomplete, // IdentDecl
              Incomplete, // IdentDecl
            Incomplete, // PkgEnd
        ]),
        sut.by_ref().collect(),
    );

    let asg = sut.finalize().unwrap().into_context();

    let oi_auto = root_lookup(&asg, id_auto).unwrap();
    let oi_no_auto = root_lookup(&asg, id_no_auto).unwrap();

    assert!(oi_auto.is_rooted(&asg));
    assert!(!oi_no_auto.is_rooted(&asg));
}

#[test]
fn pkg_is_rooted() {
    let toks = vec![PkgStart(S1), PkgEnd(S2)];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let oi_root = asg.root(S3);
    let pkg = oi_root
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("missing rooted package")
        .resolve(&asg);

    assert_eq!(pkg.span(), S1.merge(S2).unwrap());
}

#[test]
fn close_pkg_without_open() {
    let toks = vec![
        PkgEnd(S1),
        // RECOVERY: Try again.
        PkgStart(S2),
        PkgEnd(S3),
    ];

    assert_eq!(
        vec![
            Err(ParseError::StateError(AsgError::InvalidPkgEndContext(S1))),
            // RECOVERY
            Ok(Incomplete), // PkgStart
            Ok(Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn nested_open_pkg() {
    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          BindIdent(SPair("foo".into(), S2)),

          // Cannot nest package
          PkgStart(S3),
          // RECOVERY
        PkgEnd(S4),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete), // PkgStart
              Ok(Incomplete), // BindIdent
              Err(ParseError::StateError(AsgError::NestedPkgStart(S3, S1))),
              // RECOVERY
            Ok(Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn pkg_canonical_name() {
    let name = SPair("foo/bar".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          BindIdent(name),
        PkgEnd(S3),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let oi_root = asg.root(S1);
    let oi_pkg = oi_root
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find package from root");

    assert_eq!(Some(name), oi_pkg.resolve(&asg).canonical_name());

    // We should be able to find the same package by its index.
    let oi_pkg_indexed = asg.lookup(oi_root, name);
    assert_eq!(
        Some(oi_pkg),
        oi_pkg_indexed,
        "package was not indexed at Root"
    );
}

// This isn't supposed to happen in practice,
//   especially with normal usage of TAME where names are generated from
//   filenames.
#[test]
fn pkg_cannot_redeclare() {
    let name = SPair("foo/bar".into(), S2);
    let name2 = SPair("foo/bar".into(), S5);
    let namefix = SPair("foo/fix".into(), S6);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          BindIdent(name),
        PkgEnd(S3),

        PkgStart(S4),
          // Attempt to define a package of the same name.
          BindIdent(name2),

          // RECOVERY: Use a proper name.
          BindIdent(namefix),
        PkgEnd(S7),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete),   // PkgStart
              Ok(Incomplete), // BindIdent
            Ok(Incomplete),   // PkgEnd

            Ok(Incomplete),   // PkgStart
              Err(ParseError::StateError(
                  AsgError::PkgRedeclare(name, name2)
              )),
              // RECOVERY: Ignore the attempted name
              Ok(Incomplete), // BindIdent
            Ok(Incomplete),   // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // The second package should be available under the recovery name.
    let oi_root = asg.root(S1);
    let oi_pkg = asg
        .lookup::<Pkg>(oi_root, namefix)
        .expect("failed to locate package by its recovery name");
    assert_eq!(S4.merge(S7).unwrap(), oi_pkg.resolve(&asg).span());
}

#[test]
fn pkg_cannot_rename() {
    let name = SPair("foo/bar".into(), S2);
    let name2 = SPair("bad/rename".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          BindIdent(name),
          // Attempt to provide a name a second time.
          BindIdent(name2),
          // RECOVERY: Just ignore it.
        PkgEnd(S4),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    assert_eq!(
        vec![
            Ok(Incomplete), // PkgStart
            Ok(Incomplete), // BindIdent
            Err(ParseError::StateError(AsgError::PkgRename(name, name2))),
            // RECOVERY: Ignore the attempted rename
            Ok(Incomplete), // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let asg = sut.finalize().unwrap().into_context();

    // The original name should have been kept.
    let oi_root = asg.root(S1);
    let oi_pkg = asg
        .lookup::<Pkg>(oi_root, name)
        .expect("failed to locate package by its original name");
    assert_eq!(S1.merge(S4).unwrap(), oi_pkg.resolve(&asg).span());
}

#[test]
fn pkg_import() {
    let pathspec = SPair("foo/bar".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          RefIdent(pathspec),
        PkgEnd(S3),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context();

    let import = asg
        .root(S1)
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find package from root")
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find imported package")
        .resolve(&asg);

    assert_eq!(Some(pathspec), import.import_path());
}

// Documentation can be mixed in with objects in a literate style.
#[test]
fn pkg_doc() {
    let doc_a = SPair("first".into(), S2);
    let id_import = SPair("import".into(), S3);
    let doc_b = SPair("first".into(), S4);

    #[rustfmt::skip]
    let toks = vec![
        DocText(doc_a),

        // Some object to place in-between the two
        //   documentation blocks.
        RefIdent(id_import),

        DocText(doc_b),
    ];

    let asg = asg_from_toks(toks);

    let oi_pkg = asg
        .root(S1)
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find package from root");

    assert_eq!(
        vec![S4, S3, S2], // (edges reversed by Petgraph)
        oi_pkg
            .edges(&asg)
            .map(|rel| rel.widen().resolve(&asg).span())
            .collect::<Vec<_>>(),
    );
}

/// Parse using [`Sut`] when the test does not care about the outer package.
pub fn parse_as_pkg_body<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Parser<Sut, impl Iterator<Item = Air> + Debug>
where
    <I as IntoIterator>::IntoIter: Debug,
{
    use std::iter;

    Sut::parse(
        iter::once(PkgStart(S1))
            .chain(toks.into_iter())
            .chain(iter::once(PkgEnd(S1))),
    )
}

pub fn asg_from_toks<I: IntoIterator<Item = Air>>(toks: I) -> Asg
where
    I::IntoIter: Debug,
{
    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));
    sut.finalize().unwrap().into_context()
}

fn root_lookup(asg: &Asg, name: SPair) -> Option<ObjectIndex<Ident>> {
    asg.lookup(asg.root(S1), name)
}

pub fn pkg_lookup(asg: &Asg, name: SPair) -> Option<ObjectIndex<Ident>> {
    let oi_pkg = asg
        .root(S1)
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("missing rooted package");

    asg.lookup(oi_pkg, name)
}

pub fn pkg_expect_ident_oi<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    asg: &Asg,
    name: SPair,
) -> ObjectIndex<O> {
    // Duplicates logic of `pkg_get_ident_oi`,
    //   but in doing so,
    //   provides better assertion messages.
    pkg_lookup(asg, name)
        .expect(&format!("missing ident: `{name}`"))
        .edges(asg)
        .next()
        .expect(&format!("missing definition for ident `{name}`"))
        .narrow()
        .expect(&format!("ident `{name}` was not of expected ObjectKind"))
}

pub fn pkg_expect_ident_obj<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    asg: &Asg,
    name: SPair,
) -> &O {
    pkg_expect_ident_oi(asg, name).resolve(asg)
}
