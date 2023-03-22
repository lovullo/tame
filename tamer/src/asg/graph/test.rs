// Tests for graph abstraction
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

use super::super::error::AsgError;
use super::*;
use crate::{num::Dim, span::dummy::*, sym::GlobalSymbolIntern};
use std::{assert_matches::assert_matches, convert::Infallible};

type Sut = Asg;

#[test]
fn create_with_capacity() {
    let node_capacity = 100;
    let edge_capacity = 300;
    let sut = Sut::with_capacity(node_capacity, edge_capacity);

    let (nc, ec) = sut.graph.capacity();
    assert!(nc >= node_capacity);
    assert!(ec >= edge_capacity);
    assert!(sut.index.capacity() >= node_capacity);
}

#[test]
fn declare_new_unique_idents() -> AsgResult<()> {
    let mut sut = Sut::new();

    // NB: The index ordering is important!  We first use a larger
    // index to create a gap, and then use an index within that gap
    // to ensure that it's not considered an already-defined
    // identifier.
    let syma = "syma".into();
    let symb = "symab".into();

    let nodea = sut.declare(
        SPair(syma, S1),
        IdentKind::Meta,
        Source {
            desc: Some("a".into()),
            ..Default::default()
        },
    )?;

    let nodeb = sut.declare(
        SPair(symb, S2),
        IdentKind::Worksheet,
        Source {
            desc: Some("b".into()),
            ..Default::default()
        },
    )?;

    assert_ne!(nodea, nodeb);

    let givena = sut.get_ident(nodea).unwrap();
    assert_eq!(SPair(syma, S1), givena.name());
    assert_eq!(Some(&IdentKind::Meta), givena.kind());
    assert_eq!(
        Some(&Source {
            desc: Some("a".into()),
            ..Default::default()
        },),
        givena.src()
    );

    let givenb = sut.get_ident(nodeb).unwrap();
    assert_eq!(SPair(symb, S2), givenb.name());
    assert_eq!(Some(&IdentKind::Worksheet), givenb.kind());
    assert_eq!(
        Some(&Source {
            desc: Some("b".into()),
            ..Default::default()
        }),
        givenb.src()
    );

    Ok(())
}

#[test]
fn declare_kind_auto_root() -> AsgResult<()> {
    let mut sut = Sut::new();

    let auto_kind = IdentKind::Worksheet;
    // Sanity check, in case this changes.
    assert!(auto_kind.is_auto_root());

    let auto_root_node = sut.declare(
        SPair("auto_root".into(), S1),
        auto_kind,
        Default::default(),
    )?;

    // Should have been automatically added as a root.
    assert!(sut
        .graph
        .contains_edge(sut.root_node, auto_root_node.into()));

    let no_auto_kind = IdentKind::Tpl;
    assert!(!no_auto_kind.is_auto_root());

    let no_auto_root_node = sut.declare(
        SPair("no_auto_root".into(), S2),
        no_auto_kind,
        Default::default(),
    )?;

    // Non-auto-roots should _not_ be added as roots automatically.
    assert!(!sut
        .graph
        .contains_edge(sut.root_node, no_auto_root_node.into()));

    Ok(())
}

#[test]
fn lookup_by_symbol() -> AsgResult<()> {
    let mut sut = Sut::new();

    let id = SPair("lookup".into(), S1);
    let node = sut.declare(
        id,
        IdentKind::Meta,
        Source {
            generated: true,
            ..Default::default()
        },
    )?;

    assert_eq!(Some(node), sut.lookup_global(id));

    Ok(())
}

#[test]
fn declare_fails_if_transition_fails() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "symdup".into();
    let src = Source {
        desc: Some("orig".into()),
        ..Default::default()
    };

    // Set up an object to fail redeclaration.
    let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;
    let result =
        sut.declare(SPair(sym, S2), IdentKind::Meta, Source::default());

    assert_matches!(result, Err(AsgError::IdentTransition(..)));

    // The node should have been restored.
    assert_eq!(Some(&src), sut.get_ident(node).unwrap().src());

    Ok(())
}

#[test]
fn declare_extern_returns_existing() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "symext".into();
    let src = Source::default();
    let kind = IdentKind::Class(Dim::Matrix);
    let node = sut.declare_extern(SPair(sym, S1), kind.clone(), src.clone())?;

    let resrc = Source {
        desc: Some("redeclare".into()),
        ..Default::default()
    };
    let redeclare =
        sut.declare_extern(SPair(sym, S2), kind.clone(), resrc.clone())?;

    assert_eq!(node, redeclare);

    Ok(())
}

// Builds upon declare_returns_existing.
#[test]
fn declare_extern_fails_if_transition_fails() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "symdup".into();
    let src = Source {
        desc: Some("orig".into()),
        ..Default::default()
    };

    let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

    // Changes kind, which is invalid.
    let result = sut.declare_extern(
        SPair(sym, S2),
        IdentKind::Worksheet,
        Source::default(),
    );

    assert_matches!(result, Err(AsgError::IdentTransition(..)));

    // The node should have been restored.
    assert_eq!(Some(&src), sut.get_ident(node).unwrap().src());

    Ok(())
}

#[test]
fn add_fragment_to_ident() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "tofrag".into();
    let src = Source {
        generated: true,
        ..Default::default()
    };
    let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

    let fragment = "a fragment".intern();
    let node_with_frag = sut.set_fragment(SPair(sym, S2), fragment)?;

    // Attaching a fragment should _replace_ the node, not create a
    // new one
    assert_eq!(
        node, node_with_frag,
        "fragment node does not match original node"
    );

    let obj = sut.get_ident(node).unwrap();

    assert_eq!(SPair(sym, S1), obj.name());
    assert_eq!(Some(&IdentKind::Meta), obj.kind());
    assert_eq!(Some(&src), obj.src());
    assert_eq!(Some(fragment), obj.fragment());

    Ok(())
}

#[test]
fn add_fragment_to_ident_fails_if_transition_fails() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "failfrag".into();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    // The failure will come from terr below, not this.
    let node = sut.declare(SPair(sym, S1), IdentKind::Meta, src.clone())?;

    // The first set will succeed.
    sut.set_fragment(SPair(sym, S2), "".into())?;

    // This will fail.
    let result = sut.set_fragment(SPair(sym, S3), "".into());

    // The node should have been restored.
    let obj = sut.get_ident(node).unwrap();

    assert_eq!(SPair(sym, S1), obj.name());
    assert_matches!(result, Err(AsgError::IdentTransition(..)));

    Ok(())
}

#[test]
fn add_ident_dep_to_ident() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = "sym".into();
    let dep = "dep".into();

    let symnode =
        sut.declare(SPair(sym, S1), IdentKind::Meta, Source::default())?;
    let depnode =
        sut.declare(SPair(dep, S2), IdentKind::Meta, Source::default())?;

    sut.add_dep(symnode, depnode);
    assert!(sut.has_dep(symnode, depnode));

    // sanity check if we re-add a dep
    sut.add_dep(symnode, depnode);
    assert!(sut.has_dep(symnode, depnode));

    Ok(())
}

// same as above test
#[test]
fn add_dep_lookup_existing() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let _ = sut.declare(sym, IdentKind::Meta, Source::default())?;
    let _ = sut.declare(dep, IdentKind::Meta, Source::default())?;

    let (symnode, depnode) = sut.add_dep_lookup_global(sym, dep);
    assert!(sut.has_dep(symnode, depnode));

    Ok(())
}

#[test]
fn add_dep_lookup_missing() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    // both of these are missing
    let (symnode, depnode) = sut.add_dep_lookup_global(sym, dep);
    assert!(sut.has_dep(symnode, depnode));

    assert_eq!(sym, sut.get_ident(symnode).unwrap().name());
    assert_eq!(dep, sut.get_ident(depnode).unwrap().name());

    Ok(())
}

#[test]
fn declare_return_missing_symbol() -> AsgResult<()> {
    let mut sut = Sut::new();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    // both of these are missing, see add_dep_lookup_missing
    let (symnode, _) = sut.add_dep_lookup_global(sym, dep);

    let src = Source {
        desc: Some("redeclare missing".into()),
        ..Default::default()
    };

    // Check with a declared value
    let declared = sut.declare(sym, IdentKind::Meta, src.clone())?;

    assert_eq!(symnode, declared);

    let obj = sut.get_ident(declared).unwrap();

    assert_eq!(sym, obj.name());
    assert_eq!(Some(&IdentKind::Meta), obj.kind());
    assert_eq!(Some(&src), obj.src());

    Ok(())
}

#[test]
fn try_map_narrows_and_modifies() {
    let mut sut = Sut::new();

    let id_a = SPair("foo".into(), S1);
    let id_b = SPair("bar".into(), S2);

    let oi = sut.create(Ident::Missing(id_a));

    // This is the method under test.
    // It should narrow to an `Ident` because `oi` was `create`'d with
    //   an `Ident`.
    let oi_new = sut
        .try_map_obj(oi, |ident| {
            assert_eq!(ident, Ident::Missing(id_a));

            // Replace the identifier
            Ok::<_, (_, Infallible)>(Ident::Missing(id_b))
        })
        .unwrap();

    // These would not typically be checked by the caller;
    //   they are intended for debugging.
    assert_eq!(S1, oi.into());
    assert_eq!(S2, oi_new.into());

    // A change in span does not change its equivalence.
    assert_eq!(oi_new, oi);

    // Ensure that the graph was updated with the new object from the
    //   above method.
    assert_eq!(&Ident::Missing(id_b), sut.get(oi).unwrap());
}

#[test]
fn try_map_failure_restores_original_object() {
    let mut sut = Sut::new();

    let id_a = SPair("foo".into(), S1);

    let err = "uh oh";

    let oi = sut.create(Ident::Missing(id_a));

    // This will fail to modify the object.
    let oi_new = sut.try_map_obj(oi, |ident| {
        assert_eq!(ident, Ident::Missing(id_a));

        Err((ident, err))
    });

    assert_eq!(Err(err), oi_new);

    // Ensure that the original object was retained.
    assert_eq!(&Ident::Missing(id_a), sut.get(oi).unwrap());
}
