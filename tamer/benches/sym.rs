// String internment benchmarks and baselines
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

use std::rc::Rc;
use tamer::sym::*;
use test::Bencher;

fn gen_strs(n: usize) -> Vec<String> {
    (0..n)
        .map(|n| n.to_string() + "foobarbazquuxlongsymbol")
        .collect()
}

mod interner {
    use super::*;
    use std::collections::hash_map::RandomState;
    use std::collections::HashSet;
    use std::hash::BuildHasher;

    pub struct HashSetSut<S = RandomState>
    where
        S: BuildHasher,
    {
        pub map: HashSet<Rc<str>, S>,
    }

    impl<S> HashSetSut<S>
    where
        S: BuildHasher + Default,
    {
        #[inline]
        fn new() -> Self {
            Self {
                map: HashSet::with_hasher(Default::default()),
            }
        }

        pub fn intern(&mut self, value: &str) -> Rc<str> {
            if !self.map.contains(value) {
                self.map.insert(value.into());
            }

            self.map.get(value).unwrap().clone()
        }
    }

    /// This is our baseline with a raw Rc<str>.
    #[bench]
    fn with_all_new_rc_str_1000_baseline(bench: &mut Bencher) {
        let strs = gen_strs(1000);

        bench.iter(|| {
            let mut sut = HashSetSut::<RandomState>::new();
            strs.iter().map(|s| sut.intern(&s)).for_each(drop);
        });
    }

    #[bench]
    fn with_all_new_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000);

        bench.iter(|| {
            let sut = ArenaInterner::<RandomState, u32>::new();
            strs.iter().map(|s| sut.intern(&s)).for_each(drop);
        });
    }

    #[bench]
    fn with_all_new_uninterned_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000);

        bench.iter(|| {
            let sut = ArenaInterner::<RandomState, u32>::new();
            strs.iter().map(|s| sut.clone_uninterned(&s)).for_each(drop);
        });
    }

    #[bench]
    /// This is our baseline with a raw Rc<str>.
    fn with_one_new_rc_str_1000_baseline(bench: &mut Bencher) {
        bench.iter(|| {
            let mut sut = HashSetSut::<RandomState>::new();
            (0..1000).map(|_| sut.intern("first")).for_each(drop);
        });
    }

    #[bench]
    fn with_one_new_1000(bench: &mut Bencher) {
        bench.iter(|| {
            let sut = ArenaInterner::<RandomState, u32>::new();
            (0..1000).map(|_| sut.intern("first")).for_each(drop);
        });
    }

    #[bench]
    fn index_lookup_unique_1000(bench: &mut Bencher) {
        let sut = ArenaInterner::<RandomState, u32>::new();
        let strs = gen_strs(1000);

        let syms = strs.iter().map(|s| sut.intern(s)).collect::<Vec<_>>();

        bench.iter(|| {
            syms.iter().map(|si| sut.index_lookup(*si)).for_each(drop);
        });
    }

    mod fx {
        use super::*;
        use fxhash::FxBuildHasher;

        /// This is our baseline with a raw Rc<str>.
        #[bench]
        fn with_all_new_rc_str_1000_baseline(bench: &mut Bencher) {
            let strs = gen_strs(1000);
            bench.iter(|| {
                let mut sut = HashSetSut::<FxBuildHasher>::new();
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }

        // For comparison with uninterned symbols.
        #[bench]
        fn with_all_new_owned_string_1000_baseline(bench: &mut Bencher) {
            let strs = gen_strs(1000);

            bench.iter(|| {
                let _sut = ArenaInterner::<FxBuildHasher, u32>::new();
                strs.iter().map(|s| String::from(s)).for_each(drop);
            });
        }

        #[bench]
        fn with_all_new_1000(bench: &mut Bencher) {
            let strs = gen_strs(1000);

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }

        // Unlike the above, which already has a UTF-8 string, this actually
        // performs a conversion and check from `&[u8]`.
        #[bench]
        fn with_all_new_1000_utf8_checked(bench: &mut Bencher) {
            let strs = gen_strs(1000);
            let bs: Vec<&[u8]> = strs.iter().map(|s| s.as_bytes()).collect();

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                bs.iter()
                    .map(|b| sut.intern(std::str::from_utf8(&b).unwrap()))
                    .for_each(drop);
            });
        }

        // Using `Interner::intern_utf8`.
        #[bench]
        fn with_all_new_1000_intern_utf8(bench: &mut Bencher) {
            let strs = gen_strs(1000);
            let bs: Vec<&[u8]> = strs.iter().map(|s| s.as_bytes()).collect();

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                bs.iter()
                    .map(|b| sut.intern_utf8(&b).unwrap())
                    .for_each(drop);
            });
        }

        #[bench]
        fn with_all_new_1000_utf8_unchecked(bench: &mut Bencher) {
            let strs = gen_strs(1000);
            let bs: Vec<&[u8]> = strs.iter().map(|s| s.as_bytes()).collect();

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                bs.iter()
                    .map(|b| unsafe { sut.intern_utf8_unchecked(&b) })
                    .for_each(drop);
            });
        }

        #[bench]
        fn with_all_new_uninterned_1000(bench: &mut Bencher) {
            let strs = gen_strs(1000);

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                strs.iter().map(|s| sut.clone_uninterned(&s)).for_each(drop);
            });
        }

        #[bench]
        /// This is our baseline with a raw Rc<str>.
        fn with_one_new_rc_str_1000_baseline(bench: &mut Bencher) {
            bench.iter(|| {
                let mut sut: HashSetSut<FxBuildHasher> = HashSetSut {
                    map: HashSet::with_hasher(Default::default()),
                };
                (0..1000).map(|_| sut.intern("first")).for_each(drop);
            });
        }

        #[bench]
        fn with_one_new_1000(bench: &mut Bencher) {
            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                (0..1000).map(|_| sut.intern("first")).for_each(drop);
            });
        }

        // Unlike the above, which already has a UTF-8 string, this actually
        // performs a conversion and check from `&[u8]`.
        #[bench]
        fn with_one_new_1000_utf8_checked(bench: &mut Bencher) {
            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                (0..1000)
                    .map(|_| sut.intern(std::str::from_utf8(b"first").unwrap()))
                    .for_each(drop);
            });
        }

        // Using `Interner::intern_utf8`.
        #[bench]
        fn with_one_new_1000_intern_utf8(bench: &mut Bencher) {
            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                (0..1000)
                    .map(|_| sut.intern_utf8(b"first").unwrap())
                    .for_each(drop);
            });
        }

        #[bench]
        fn with_one_new_1000_utf8_unchecked(bench: &mut Bencher) {
            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::new();
                (0..1000)
                    .map(|_| unsafe { sut.intern_utf8_unchecked(b"first") })
                    .for_each(drop);
            });
        }

        /// Since Fx is the best-performing, let's build upon it to demonstrate
        /// the benefits of with_capacity
        #[bench]
        fn with_all_new_1000_with_capacity(bench: &mut Bencher) {
            let n = 1000;
            let strs = gen_strs(n);

            bench.iter(|| {
                let sut = ArenaInterner::<FxBuildHasher, u32>::with_capacity(n);
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }
    }

    // Note that these tests don't drop the global interner in-between.
    mod global {
        use super::*;
        use tamer::sym::GlobalSymbolIntern;

        #[bench]
        fn with_all_new_1000(bench: &mut Bencher) {
            let strs = gen_strs(1000);

            bench.iter(|| {
                strs.iter()
                    .map::<SymbolId, _>(|s| s.intern())
                    .for_each(drop);
            });
        }

        #[bench]
        fn with_one_new_1000(bench: &mut Bencher) {
            bench.iter(|| {
                (0..1000)
                    .map::<SymbolId, _>(|_| "onenew".intern())
                    .for_each(drop);
            });
        }

        #[bench]
        fn with_one_new_1000_utf8_unchecked(bench: &mut Bencher) {
            bench.iter(|| {
                (0..1000)
                    .map::<SymbolId, _>(|_| unsafe {
                        (b"onenewu8").intern_utf8_unchecked()
                    })
                    .for_each(drop);
            });
        }
    }
}
