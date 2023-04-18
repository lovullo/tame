// Abstract semantic graph benchmarks
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
    use tamer::{
        asg::{
            AsgError, DefaultAsg, Ident, IdentKind, ObjectIndex,
            ObjectIndexRelTo, Source,
        },
        parse::util::SPair,
        span::UNKNOWN_SPAN,
        sym::GlobalSymbolIntern,
    };

    type Sut = DefaultAsg;

    fn declare(
        asg: &mut Sut,
        name: SPair,
        kind: IdentKind,
        src: Source,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        let oi_root = asg.root(name);
        oi_root.declare(asg, name, kind, src)
    }

    fn declare_extern(
        asg: &mut Sut,
        name: SPair,
        kind: IdentKind,
        src: Source,
    ) -> Result<ObjectIndex<Ident>, AsgError> {
        lookup_or_missing(asg, name).declare_extern(asg, name, kind, src)
    }

    fn lookup(asg: &mut Sut, name: SPair) -> Option<ObjectIndex<Ident>> {
        let oi_root = asg.root(name);
        asg.lookup(oi_root, name)
    }

    fn lookup_or_missing(asg: &mut Sut, name: SPair) -> ObjectIndex<Ident> {
        let oi_root = asg.root(name);
        oi_root.lookup_or_missing(asg, name)
    }

    fn interned_n(n: u16) -> Vec<SPair> {
        (0..n)
            .map(|i| SPair(i.to_string().intern(), UNKNOWN_SPAN))
            .collect()
    }

    #[bench]
    fn declare_1_000(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| {
                    declare(&mut sut, *i, IdentKind::Meta, Source::default())
                })
                .for_each(drop);
        });
    }

    #[bench]
    fn declare_1_000_full_inital_capacity(bench: &mut Bencher) {
        let mut sut = Sut::with_capacity(1024, 1024);
        let xs = interned_n(1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| {
                    declare(&mut sut, *i, IdentKind::Meta, Source::default())
                })
                .for_each(drop);
        });
    }

    #[bench]
    fn declare_1_000_prog_ident_size(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| {
                    declare(&mut sut, *i, IdentKind::Meta, Source::default())
                })
                .for_each(drop);
        });
    }

    #[bench]
    fn declare_extern_1_000(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        bench.iter(|| {
            xs.iter()
                .map(|i| {
                    declare_extern(
                        &mut sut,
                        *i,
                        IdentKind::Meta,
                        Source::default(),
                    )
                })
                .for_each(drop);
        });
    }

    #[bench]
    fn resolve_extern_1_000(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        xs.iter().for_each(|sym| {
            let _ = declare_extern(
                &mut sut,
                *sym,
                IdentKind::Meta,
                Source::default(),
            );
        });

        // Bench only the resolution, not initial declare.
        bench.iter(|| {
            xs.iter()
                .map(|sym| {
                    declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                })
                .for_each(drop);
        });
    }

    // N.B.: This benchmark isn't easily comparable to the others because
    // `set_fragment` takes ownership over a string, and so we have to clone
    // strings for each call.
    #[bench]
    fn set_fragment_1_000_with_new_str(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        xs.iter().for_each(|sym| {
            declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                .unwrap();
        });

        // Bench only the resolution, not initial declare.
        bench.iter(|| {
            xs.iter()
                .map(|sym| {
                    lookup_or_missing(&mut sut, *sym)
                        .set_fragment(&mut sut, "".into())
                }) // see N.B.
                .for_each(drop);
        });
    }

    #[bench]
    fn lookup_1_000(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        xs.iter().for_each(|sym| {
            let _ = declare(&mut sut, *sym, IdentKind::Meta, Source::default());
        });

        bench.iter(|| {
            xs.iter()
                .map(|sym| lookup(&mut sut, *sym).unwrap())
                .for_each(drop);
        });
    }

    #[bench]
    fn get_1_000(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                declare(&mut sut, *sym, IdentKind::Meta, Source::default())
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
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let root = orefs[0];

        // Note that this adds all edges to one node
        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| root.add_opaque_dep(&mut sut, *oref))
                .for_each(drop);
        });
    }

    // Same as above but only one edge per node.
    #[bench]
    fn add_dep_1_000_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        bench.iter(|| {
            orefs
                .iter()
                .zip(orefs.iter().cycle().skip(1))
                .map(|(from, to)| from.add_opaque_dep(&mut sut, *to))
                .for_each(drop);
        });
    }

    #[bench]
    fn has_dep_1_000_single_node(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let root = orefs[0];

        orefs.iter().for_each(|oref| {
            root.add_opaque_dep(&mut sut, *oref);
        });

        bench.iter(|| {
            orefs
                .iter()
                .map(|oref| root.has_edge_to(&sut, *oref))
                .for_each(drop);
        });
    }

    // Same as above but only one edge per node.
    #[bench]
    fn has_dep_1_000_one_edge_per_node(bench: &mut Bencher) {
        let mut sut = Sut::new();
        let xs = interned_n(1_000);

        let orefs = xs
            .iter()
            .map(|sym| {
                declare(&mut sut, *sym, IdentKind::Meta, Source::default())
                    .unwrap()
            })
            .collect::<Vec<_>>();

        orefs.iter().zip(orefs.iter().cycle().skip(1)).for_each(
            |(from, to)| {
                from.add_opaque_dep(&mut sut, *to);
            },
        );

        bench.iter(|| {
            orefs
                .iter()
                .zip(orefs.iter().cycle().skip(1))
                .map(|(from, to)| from.has_edge_to(&sut, *to))
                .for_each(drop);
        });
    }
}

mod object {
    use super::*;

    mod ident {
        use super::*;
        use tamer::{
            asg::{Ident, IdentKind, Source},
            parse::util::SPair,
            span::UNKNOWN_SPAN as S0,
        };

        type Sut = Ident;

        #[bench]
        fn declare_1_000(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000).map(|_| Sut::declare(sym)).for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_missing(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym).resolve(
                            S0,
                            IdentKind::Meta,
                            Source::default(),
                        )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn extern_1_000_missing(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym).extern_(
                            S0,
                            IdentKind::Meta,
                            Source::default(),
                        )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_extern(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym)
                            .extern_(S0, IdentKind::Meta, Source::default())
                            .unwrap()
                            .resolve(S0, IdentKind::Meta, Source::default())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn resolve_1_000_override(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym)
                            .resolve(
                                S0,
                                IdentKind::Meta,
                                Source {
                                    virtual_: true,
                                    ..Default::default()
                                },
                            )
                            .unwrap()
                            .resolve(
                                S0,
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
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym)
                            .resolve(
                                S0,
                                IdentKind::Meta,
                                Source {
                                    override_: true,
                                    ..Default::default()
                                },
                            )
                            .unwrap()
                            .resolve(
                                S0,
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
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000)
                    .map(|_| {
                        Sut::declare(sym)
                            .resolve(S0, IdentKind::Meta, Source::default())
                            .unwrap()
                            .set_fragment("".into())
                    })
                    .for_each(drop);
            });
        }

        // No need to do all of the others, since they're all the same thing.
        #[bench]
        fn declared_name_1_000(bench: &mut Bencher) {
            let sym = SPair("sym".into(), S0);

            bench.iter(|| {
                (0..1000).map(|_| Sut::declare(sym).name()).for_each(drop);
            });
        }
    }
}
