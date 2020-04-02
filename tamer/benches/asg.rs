// Abstract semantic graph benchmarks
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
//
// Note that the baseline tests have a _suffix_ rather than a prefix so that
// they are still grouped with the associated test in the output, since it's
// sorted lexically by function name.

#![feature(test)]

extern crate tamer;
extern crate test;

use test::Bencher;

mod base {
    use super::*;
    use tamer::global;
    use tamer::ir::asg::{
        Asg, DataType, DefaultAsg, IdentKind, IdentObject, SortableAsg, Source,
    };
    use tamer::sym::{DefaultInterner, Interner, Symbol};

    type Sut<'i> = DefaultAsg<'i, IdentObject<'i>, global::PkgIdentSize>;
    type SutProg<'i> = DefaultAsg<'i, IdentObject<'i>, global::ProgIdentSize>;

    fn interned_n<'i>(
        interner: &'i DefaultInterner<'i>,
        n: u16,
    ) -> Vec<&'i Symbol<'i>> {
        (0..n).map(|i| interner.intern(&i.to_string())).collect()
    }

    #[bench]
    fn declare_1_000(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| sut.declare(i, IdentKind::Meta, Source::default()))
                .for_each(drop);
        });
    }

    #[bench]
    fn declare_1_000_full_inital_capacity(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(1024, 1024);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| sut.declare(i, IdentKind::Meta, Source::default()))
                .for_each(drop);
        });
    }

    // The Ix size affects memory, but how about performance?
    #[bench]
    fn declare_1_000_prog_ident_size(bench: &mut Bencher) {
        let mut sut = SutProg::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| sut.declare(i, IdentKind::Meta, Source::default()))
                .for_each(drop);
        });
    }

    #[bench]
    fn declare_extern_1_000(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| {
                    sut.declare_extern(i, IdentKind::Meta, Source::default())
                })
                .for_each(drop);
        });
    }

    #[bench]
    fn resolve_extern_1_000(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        xs.iter().for_each(|sym| {
            let _ = sut.declare_extern(sym, IdentKind::Meta, Source::default());
        });

        // Bench only the resolution, not initial declare.
        bench.iter(|| {
            xs.iter()
                .map(|sym| sut.declare(sym, IdentKind::Meta, Source::default()))
                .for_each(drop);
        });
    }

    // N.B.: This benchmark isn't easily comparable to the others because
    // `set_fragment` takes ownership over a string, and so we have to clone
    // strings for each call.
    #[bench]
    fn set_fragment_1_000_with_new_str(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        // Bench only the resolution, not initial declare.
        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| sut.set_fragment(*oref, "".into())) // see N.B.
                .for_each(drop);
        });
    }

    #[bench]
    fn lookup_1_000(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        xs.iter().for_each(|sym| {
            let _ = sut.declare(&sym, IdentKind::Meta, Source::default());
        });

        bench.iter(|| {
            xs.iter().map(|sym| sut.lookup(sym).unwrap()).for_each(drop);
        });
    }

    #[bench]
    fn get_1_000(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| sut.get(*oref).unwrap())
                .for_each(drop);
        });
    }

    // All dependencies on a single node.  Petgraph does poorly with
    // supernodes at the time of writing, relatively speaking.
    #[bench]
    fn add_dep_1_000_to_single_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let root = orefs[0];

        // Note that this adds all edges to one node
        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| sut.add_dep(root, *oref))
                .for_each(drop);
        });
    }

    // Same as above but only one edge per node.
    #[bench]
    fn add_dep_1_000_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        bench.iter(|| {
            orefs
                .iter()
                .zip(orefs.iter().cycle().skip(1))
                .map(|(from, to)| sut.add_dep(*from, *to))
                .for_each(drop);
        });
    }

    #[bench]
    fn has_dep_1_000_single_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let root = orefs[0];

        orefs.iter().for_each(|oref| {
            sut.add_dep(root, *oref);
        });

        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| sut.has_dep(root, *oref))
                .for_each(drop);
        });
    }

    // Same as above but only one edge per node.
    #[bench]
    fn has_dep_1_000_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        orefs.iter().zip(orefs.iter().cycle().skip(1)).for_each(
            |(from, to)| {
                sut.add_dep(*from, *to);
            },
        );

        bench.iter(|| {
            orefs
                .iter()
                .zip(orefs.iter().cycle().skip(1))
                .map(|(from, to)| sut.has_dep(*from, *to))
                .for_each(drop);
        });
    }

    #[bench]
    fn add_dep_lookup_1_000_missing_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        bench.iter(|| {
            xs.iter()
                .zip(xs.iter().cycle().skip(1))
                .map(|(from, to)| sut.add_dep_lookup(from, to))
                .for_each(drop);
        });
    }

    #[bench]
    fn add_dep_lookup_1_000_existing_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        xs.iter().for_each(|sym| {
            let _ = sut.declare(sym, IdentKind::Meta, Source::default());
        });

        bench.iter(|| {
            xs.iter()
                .zip(xs.iter().cycle().skip(1))
                .map(|(from, to)| sut.add_dep_lookup(from, to))
                .for_each(drop);
        });
    }

    #[bench]
    fn sort_1_with_1_000_existing_supernode(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(
                    sym,
                    IdentKind::Rate(DataType::Integer),
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

        bench.iter(|| {
            drop(sut.sort(&[root]));
        });
    }

    #[bench]
    fn sort_1_with_1_000_existing_one_edge_per_node_one_path(
        bench: &mut Bencher,
    ) {
        let mut sut = Sut::with_capacity(0, 0);
        let interner = DefaultInterner::new();
        let xs = interned_n(&interner, 1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                sut.declare(
                    sym,
                    IdentKind::Rate(DataType::Integer),
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

        bench.iter(|| {
            drop(sut.sort(&[root]));
        });
    }
}

mod object {
    use super::*;

    mod ident {
        use super::*;
        use tamer::ir::asg::{
            IdentKind, IdentObject, IdentObjectData, IdentObjectState, Source,
        };
        use tamer::sym::{DefaultInterner, Interner};

        type Sut<'i> = IdentObject<'i>;

        #[bench]
        fn declare_1_000(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000).map(|_| Sut::declare(&sym)).for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_missing(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .resolve(IdentKind::Meta, Source::default())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn extern_1_000_missing(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .extern_(IdentKind::Meta, Source::default())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_extern(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .extern_(IdentKind::Meta, Source::default())
                            .unwrap()
                            .resolve(IdentKind::Meta, Source::default())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_override(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .resolve(
                                IdentKind::Meta,
                                Source {
                                    virtual_: true,
                                    ..Default::default()
                                },
                            )
                            .unwrap()
                            .resolve(
                                IdentKind::Meta,
                                Source {
                                    override_: true,
                                    ..Default::default()
                                },
                            )
                    })
                    .for_each(drop);
            });
        }

        // Override encountered before virtual
        #[bench]
        fn resolve_1_000_override_virt_after_override(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .resolve(
                                IdentKind::Meta,
                                Source {
                                    override_: true,
                                    ..Default::default()
                                },
                            )
                            .unwrap()
                            .resolve(
                                IdentKind::Meta,
                                Source {
                                    virtual_: true,
                                    ..Default::default()
                                },
                            )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn set_fragment_1_000_resolved_with_new_str(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(&sym)
                            .resolve(IdentKind::Meta, Source::default())
                            .unwrap()
                            .set_fragment("".into())
                    })
                    .for_each(drop);
            });
        }

        // No need to do all of the others, since they're all the same thing.
        #[bench]
        fn declared_name_1_000(bench: &mut Bencher) {
            let interner = DefaultInterner::new();
            let sym = interner.intern("sym");

            bench.iter(|| {
                (0..1000).map(|_| Sut::declare(&sym).name()).for_each(drop);
            });
        }
    }
}
