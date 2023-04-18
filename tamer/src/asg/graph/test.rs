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

use super::*;
use crate::span::dummy::*;
use std::convert::Infallible;

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
