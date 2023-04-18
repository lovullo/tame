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
        IdentKind, ObjectIndexRelTo, Source,
    },
    parse::{ParseError, Parsed, Parser},
    span::dummy::*,
};
use std::assert_matches::assert_matches;

type Sut = AirAggregate;

#[test]
fn ident_decl() {
    let id = SPair("foo".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl".into()),
        ..Default::default()
    };

    let toks = vec![Air::IdentDecl(id, kind.clone(), src.clone())].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare the same identifier.
    let bad_toks =
        vec![Air::IdentDecl(SPair(id.symbol(), S2), kind, src)].into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_extern_decl() {
    let id = SPair("foo".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/decl-extern".into()),
        ..Default::default()
    };

    let toks =
        vec![Air::IdentExternDecl(id, kind.clone(), src.clone())].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id).extern_(S1, kind, src.clone()).as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   redeclare with a different kind.
    let different_kind = IdentKind::Meta;
    let bad_toks = vec![Air::IdentExternDecl(
        SPair(id.symbol(), S2),
        different_kind,
        src,
    )]
    .into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

#[test]
fn ident_dep() {
    let id = SPair("foo".into(), S1);
    let dep = SPair("dep".into(), S2);

    let toks = vec![Air::IdentDep(id, dep)].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let dep_node = root_lookup(&asg, dep).expect("dep was not added to graph");

    assert!(ident_node.has_edge_to(&asg, dep_node));
}

#[test]
fn ident_fragment() {
    let id = SPair("frag".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/frag".into()),
        ..Default::default()
    };
    let frag = "fragment text".into();

    let toks = vec![
        // Identifier must be declared before it can be given a
        //   fragment.
        Air::IdentDecl(id, kind.clone(), src.clone()),
        Air::IdentFragment(id, frag),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentFragment

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to graph");
    let ident = asg.get(ident_node).unwrap();

    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
            .and_then(|resolved| resolved.set_fragment(frag))
            .as_ref(),
    );

    // Re-instantiate the parser and test an error by attempting to
    //   re-set the fragment.
    let bad_toks = vec![Air::IdentFragment(id, frag)].into_iter();
    let mut sut = Sut::parse_with_context(bad_toks, asg);

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
    );
}

// Adding a root before the identifier exists should add a
//   `Ident::Missing`.
#[test]
fn ident_root_missing() {
    let id = SPair("toroot".into(), S1);

    let toks = vec![Air::IdentRoot(id)].into_iter();
    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

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
    let id = SPair("toroot".into(), S1);
    let kind = IdentKind::Tpl;
    let src = Source {
        src: Some("test/root-existing".into()),
        ..Default::default()
    };

    // Ensure that it won't auto-root based on the kind,
    //   otherwise we won't be testing the right thing.
    assert!(!kind.is_auto_root());

    let toks = vec![
        Air::IdentDecl(id, kind.clone(), src.clone()),
        Air::IdentRoot(SPair(id.symbol(), S2)),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentRoot

    let asg = sut.finalize().unwrap().into_context();

    let ident_node =
        root_lookup(&asg, id).expect("identifier was not added to the graph");
    let ident = asg.get(ident_node).unwrap();

    // The previously-declared identifier...
    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S1, kind.clone(), src.clone())
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

    let id_auto = SPair("auto_root".into(), S1);
    let id_no_auto = SPair("no_auto_root".into(), S2);

    let toks = [
        // auto-rooting
        Air::IdentDecl(id_auto, auto_kind, Default::default()),
        // non-auto-rooting
        Air::IdentDecl(id_no_auto, no_auto_kind, Default::default()),
    ]
    .into_iter();

    let mut sut = Sut::parse(toks);

    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());
    assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

    let asg = sut.finalize().unwrap().into_context();

    let oi_auto = root_lookup(&asg, id_auto).unwrap();
    let oi_no_auto = root_lookup(&asg, id_no_auto).unwrap();

    assert!(oi_auto.is_rooted(&asg));
    assert!(!oi_no_auto.is_rooted(&asg));
}

#[test]
fn pkg_is_rooted() {
    let toks = vec![Air::PkgStart(S1), Air::PkgEnd(S2)];

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
        Air::PkgEnd(S1),
        // RECOVERY: Try again.
        Air::PkgStart(S2),
        Air::PkgEnd(S3),
    ];

    assert_eq!(
        vec![
            Err(ParseError::StateError(AsgError::InvalidPkgEndContext(S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgStart
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn nested_open_pkg() {
    let toks = vec![
        Air::PkgStart(S1),
        Air::PkgStart(S2),
        // RECOVERY
        Air::PkgEnd(S3),
    ];

    assert_eq!(
        vec![
            Ok(Parsed::Incomplete), // PkgStart
            Err(ParseError::StateError(AsgError::NestedPkgStart(S2, S1))),
            // RECOVERY
            Ok(Parsed::Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn pkg_import() {
    let pathspec = SPair("foo/bar".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        Air::PkgStart(S1),
          Air::RefIdent(pathspec),
        Air::PkgEnd(S3),
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

    assert_eq!(pathspec, import.pathspec());
}

// Documentation can be mixed in with objects in a literate style.
#[test]
fn pkg_doc() {
    let doc_a = SPair("first".into(), S2);
    let id_import = SPair("import".into(), S3);
    let doc_b = SPair("first".into(), S4);

    #[rustfmt::skip]
    let toks = vec![
        Air::DocText(doc_a),

        // Some object to place in-between the two
        //   documentation blocks.
        Air::RefIdent(id_import),

        Air::DocText(doc_b),
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
        iter::once(Air::PkgStart(S1))
            .chain(toks.into_iter())
            .chain(iter::once(Air::PkgEnd(S1))),
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

pub fn pkg_get_ident_oi<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    asg: &Asg,
    name: SPair,
) -> Option<ObjectIndex<O>> {
    pkg_lookup(asg, name)
        .and_then(|oi| oi.edges(asg).next())
        .and_then(|oi| oi.narrow())
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

pub fn pkg_get_ident_obj<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    asg: &Asg,
    name: SPair,
) -> Option<&O> {
    pkg_get_ident_oi(asg, name).map(|oi| oi.resolve(asg))
}

pub fn pkg_expect_ident_obj<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    asg: &Asg,
    name: SPair,
) -> &O {
    pkg_expect_ident_oi(asg, name).resolve(asg)
}
