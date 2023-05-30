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

mod scope;

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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to graph");
    let ident = ctx.asg_ref().get(ident_node).unwrap();

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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to graph");
    let ident = ctx.asg_ref().get(ident_node).unwrap();

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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to graph");
    let dep_node = root_lookup(&ctx, dep).expect("dep was not added to graph");

    assert!(ident_node.has_edge_to(ctx.asg_ref(), dep_node));
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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to graph");
    let ident = ctx.asg_ref().get(ident_node).unwrap();

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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to the graph");
    let ident = ctx.asg_ref().get(ident_node).unwrap();

    // The identifier did not previously exist,
    //   and so a missing node is created as a placeholder.
    assert_eq!(&Ident::Missing(id), ident);

    // And that missing identifier should be rooted.
    assert!(ident_node.is_rooted(ctx.asg_ref()));
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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let ident_node =
        root_lookup(&ctx, id).expect("identifier was not added to the graph");
    let ident = ctx.asg_ref().get(ident_node).unwrap();

    // The previously-declared identifier...
    assert_eq!(
        Ok(ident),
        Ident::declare(id)
            .resolve(S2, kind.clone(), src.clone())
            .as_ref()
    );

    // ...should have been subsequently rooted.
    assert!(ident_node.is_rooted(ctx.asg_ref()));
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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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

    let ctx = sut.finalize().unwrap().into_private_context();

    let oi_auto = root_lookup(&ctx, id_auto).unwrap();
    let oi_no_auto = root_lookup(&ctx, id_no_auto).unwrap();

    assert!(oi_auto.is_rooted(ctx.asg_ref()));
    assert!(!oi_no_auto.is_rooted(ctx.asg_ref()));
}

#[test]
fn pkg_is_rooted() {
    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg".into(), S1)),
        PkgEnd(S2),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context().finish();

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
        PkgStart(S2, SPair("/pkg".into(), S2)),
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
    let name_a = SPair("/pkg-a".into(), S2);
    let name_b = SPair("/pkg-b".into(), S4);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, name_a),
          // Cannot nest package
          PkgStart(S3, name_b),
          // RECOVERY
        PkgEnd(S5),
    ];

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete), // PkgStart
              Err(ParseError::StateError(AsgError::NestedPkgStart(
                  (S3, name_b), (S1, name_a),
              ))),
              // RECOVERY
            Ok(Incomplete), // PkgEnd
        ],
        Sut::parse(toks.into_iter()).collect::<Vec<_>>(),
    );
}

#[test]
fn pkg_canonical_name() {
    let name = SPair("/foo/bar".into(), S2);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, name),
        PkgEnd(S3),
    ];

    let ctx = air_ctx_from_toks(toks);

    let oi_root = ctx.asg_ref().root(S1);
    let oi_pkg = oi_root
        .edges_filtered::<Pkg>(ctx.asg_ref())
        .next()
        .expect("cannot find package from root");

    assert_eq!(name, oi_pkg.resolve(ctx.asg_ref()).canonical_name());

    // We should be able to find the same package by its index.
    let oi_pkg_indexed = ctx
        .env_scope_lookup_raw(oi_root, name)
        .map(|eoi| eoi.into_inner());

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
    let name = SPair("/foo/bar".into(), S2);
    let name2 = SPair("/foo/bar".into(), S5);
    let namefix = SPair("/foo/fix".into(), S7);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, name),
        PkgEnd(S3),

        // Attempt to define a package of the same name.
        PkgStart(S4, name2),

        // RECOVERY: Use a proper name.
        PkgStart(S6, namefix),
        PkgEnd(S8),
    ];

    let mut sut = Sut::parse(toks.into_iter());

    assert_eq!(
        #[rustfmt::skip]
        vec![
            Ok(Incomplete),   // PkgStart
            Ok(Incomplete),   // PkgEnd

            Err(ParseError::StateError(
                AsgError::PkgRedeclare(name, name2)
            )),

            // RECOVERY: Retry with a proper name
            Ok(Incomplete),   // PkgStart
            Ok(Incomplete),   // PkgEnd
        ],
        sut.by_ref().collect::<Vec<_>>(),
    );

    let ctx = sut.finalize().unwrap().into_private_context();

    // The second package should be available under the recovery name.
    let oi_root = ctx.asg_ref().root(S1);
    let oi_pkg = ctx
        .env_scope_lookup_raw::<Pkg>(oi_root, namefix)
        .map(|eoi| eoi.into_inner())
        .expect("failed to locate package by its recovery name");

    assert_eq!(S6.merge(S8).unwrap(), oi_pkg.resolve(ctx.asg_ref()).span());
}

#[test]
fn pkg_import_canonicalized_against_current_pkg() {
    let pkg_name = SPair("/foo/bar".into(), S2);
    let pkg_rel = SPair("baz/quux".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, pkg_name),
          PkgImport(pkg_rel),
        PkgEnd(S3),
    ];

    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));

    let asg = sut.finalize().unwrap().into_context().finish();

    let import = asg
        .root(S1)
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find package from root")
        .edges_filtered::<Pkg>(&asg)
        .next()
        .expect("cannot find imported package")
        .resolve(&asg);

    // TODO
    assert_eq!(SPair("/foo/baz/quux".into(), S3), import.canonical_name());
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
        PkgImport(id_import),

        DocText(doc_b),
    ];

    let asg = asg_from_pkg_body_toks(toks);

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

// Package imports will trigger parsing,
//   but the intent is to re-use the previous parsing context so that we can
//   continue to accumulate into the same graph along with the same scope
//   index.
#[test]
fn resume_previous_parsing_context() {
    let name_foo = SPair("foo".into(), S2);
    let name_bar = SPair("bar".into(), S5);
    let name_baz = SPair("baz".into(), S6);
    let kind = IdentKind::Tpl;
    let src = Source::default();

    // We're going to test with opaque objects as if we are the linker.
    // This is the first parse.
    #[rustfmt::skip]
    let toks = vec![
        // The first package will reference an identifier from another
        //   package.
        PkgStart(S1, SPair("/pkg-a".into(), S1)),
          IdentDep(name_foo, name_bar),
        PkgEnd(S3),
    ];

    let ctx = air_ctx_from_toks(toks);

    // We consumed the parser above and retrieved its context.
    // This is the token stream for the second parser,
    //   which will re-use the above context.
    #[rustfmt::skip]
    let toks = vec![
        // This package will define that identifier,
        //   which should also find the identifier having been placed into
        //   the global environment.
        PkgStart(S4, SPair("/pkg-b".into(), S4)),
          IdentDecl(name_bar, kind.clone(), src.clone()),

          // This is a third identifier that is unique to this package.
          // This is intended to catch the following situation,
          //   where `P` is the ParseState and `S` is the stack.
          //
          //   1. P:Uninit  S:[]
          //   2. P:Root    S:[]
          //   3. P:Pkg     S:[Root]
          //   ---- next parser ---
          //   4. P:Uninit  S:[Root]
          //   5. P:Root    S:[Root]         <-- new Root
          //   6. P:Pkg     S:[Root, Root]
          //                     ^     ^
          //                      `-----\
          //                          Would try to index at oi_root
          //                          _twice_, which would panic.
          //
          // AirAggregate is designed to resume from the top of the stack
          //   when initializing to avoid this scenario.
          // So here's what it's expected to do instead:
          //
          //  [...]
          //   ---- next parser ---
          //   4. P:Uninit  S:[Root]
          //   5. P:Root    S:[]             <-- pop existing Root
          //   6. P:Pkg     S:[Root]
          IdentDecl(name_baz, kind.clone(), src),
        PkgEnd(S7),
    ];

    // We _resume_ parsing with the previous context.
    let mut sut = Sut::parse_with_context(toks.into_iter(), ctx);
    assert!(sut.all(|x| x.is_ok()));

    // The ASG should have been constructed from _both_ of the previous
    //   individual parsers,
    //     having used the shared context.
    let ctx = sut.finalize().unwrap().into_private_context();

    // Both should have been added to the same graph.
    let oi_foo = root_lookup(&ctx, name_foo).expect("missing foo");
    let oi_bar = root_lookup(&ctx, name_bar).expect("missing bar");

    assert!(oi_foo.has_edge_to(ctx.asg_ref(), oi_bar));

    // And it should have been resolved via the _second_ package,
    //   which is parsed separately,
    //   as part of the same graph and with the same indexed identifiers.
    // If there were not a shared index between the two parsers,
    //   then it would have retained an original `Missing` Ident and created
    //   a new resolved one.
    assert_eq!(Some(&kind), oi_bar.resolve(ctx.asg_ref()).kind());
}

/////// Tests above; plumbing begins below ///////

/// Parse using [`Sut`] when the test does not care about the outer package.
pub fn parse_as_pkg_body<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Parser<Sut, impl Iterator<Item = Air> + Debug>
where
    <I as IntoIterator>::IntoIter: Debug,
{
    use std::iter;

    Sut::parse(
        iter::once(PkgStart(S1, SPair("/pkg".into(), S1)))
            .chain(toks.into_iter())
            .chain(iter::once(PkgEnd(S1))),
    )
}

pub(super) fn asg_from_pkg_body_toks<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Asg
where
    I::IntoIter: Debug,
{
    // Equivalent to `into_{private_=>}context` in this function.
    air_ctx_from_pkg_body_toks(toks).finish()
}

pub(super) fn air_ctx_from_pkg_body_toks<I: IntoIterator<Item = Air>>(
    toks: I,
) -> <Sut as ParseState>::Context
where
    I::IntoIter: Debug,
{
    let mut sut = parse_as_pkg_body(toks);
    assert!(sut.all(|x| x.is_ok()));
    sut.finalize().unwrap().into_private_context()
}

/// Create and yield a new [`Asg`] from an [`Air`] token stream.
pub fn asg_from_toks<I: IntoIterator<Item = Air>>(toks: I) -> Asg
where
    I::IntoIter: Debug,
{
    // Equivalent to `into_{private_=>}context` in this function.
    air_ctx_from_toks(toks).finish()
}

/// Create and yield a new [`Asg`] from an [`Air`] token stream.
pub(super) fn air_ctx_from_toks<I: IntoIterator<Item = Air>>(
    toks: I,
) -> <Sut as ParseState>::Context
where
    I::IntoIter: Debug,
{
    let mut sut = Sut::parse(toks.into_iter());
    assert!(sut.all(|x| x.is_ok()));
    sut.finalize().unwrap().into_private_context()
}

fn root_lookup(
    ctx: &<AirAggregate as ParseState>::Context,
    name: SPair,
) -> Option<ObjectIndex<Ident>> {
    ctx.env_scope_lookup_raw(ctx.asg_ref().root(S1), name)
        .map(|eoi| eoi.into_inner())
}

pub fn pkg_lookup(
    ctx: &<AirAggregate as ParseState>::Context,
    name: SPair,
) -> Option<ObjectIndex<Ident>> {
    let oi_pkg = ctx
        .asg_ref()
        .root(S1)
        .edges_filtered::<Pkg>(ctx.asg_ref())
        .next()
        .expect("missing rooted package");

    ctx.env_scope_lookup(oi_pkg, name)
}

pub fn pkg_expect_ident_oi<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    ctx: &<AirAggregate as ParseState>::Context,
    name: SPair,
) -> ObjectIndex<O> {
    // Duplicates logic of `pkg_get_ident_oi`,
    //   but in doing so,
    //   provides better assertion messages.
    pkg_lookup(ctx, name)
        .expect(&format!("missing ident: `{name}`"))
        .edges(ctx.asg_ref())
        .next()
        .expect(&format!("missing definition for ident `{name}`"))
        .narrow()
        .expect(&format!("ident `{name}` was not of expected ObjectKind"))
}

pub fn pkg_expect_ident_obj<O: ObjectRelatable + ObjectRelFrom<Ident>>(
    ctx: &<AirAggregate as ParseState>::Context,
    name: SPair,
) -> &O {
    pkg_expect_ident_oi(ctx, name).resolve(ctx.asg_ref())
}
