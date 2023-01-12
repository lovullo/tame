// Benchmarking of sorting of ASG into xmle sections
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

#![feature(test)]

extern crate tamer;
extern crate test;

use test::Bencher;

pub(crate) use tamer::{
    asg::{DefaultAsg, IdentKind, Source},
    ld::xmle::{lower::sort, Sections},
    num::Dtype,
    parse::util::SPair,
    span::UNKNOWN_SPAN,
    sym::GlobalSymbolIntern,
};

type TestAsg = DefaultAsg;

fn interned_n(n: u16) -> Vec<SPair> {
    (0..n)
        .map(|i| SPair(i.to_string().intern(), UNKNOWN_SPAN))
        .collect()
}

#[bench]
fn sort_1_with_1_000_existing_supernode(bench: &mut Bencher) {
    let mut sut = TestAsg::new();
    let xs = interned_n(1_000);

    let orefs = xs
        .iter()
        .map(|sym| {
            sut.declare(
                *sym,
                IdentKind::Rate(Dtype::Integer),
                Source::default(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();

    let root = orefs[0];

    // All edges from a single node.
    orefs.iter().skip(1).for_each(|to| {
        sut.add_dep(root, *to);
    });

    sut.add_root(root);

    bench.iter(|| {
        drop(sort(&sut, Sections::new()));
    });
}

#[bench]
fn sort_1_with_1_000_existing_one_edge_per_node_one_path(bench: &mut Bencher) {
    let mut sut = TestAsg::new();
    let xs = interned_n(1_000);

    let orefs = xs
        .iter()
        .map(|sym| {
            sut.declare(
                *sym,
                IdentKind::Rate(Dtype::Integer),
                Source::default(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();

    // Note that there's no `cycle` call on the iterator, like the
    // above tests, to make sure we don't create a cycle on the
    // graph.
    orefs
        .iter()
        .zip(orefs.iter().skip(1))
        .for_each(|(from, to)| {
            sut.add_dep(*from, *to);
        });

    let root = orefs[0];

    sut.add_root(root);

    bench.iter(|| {
        drop(sort(&sut, Sections::new()));
    });
}
