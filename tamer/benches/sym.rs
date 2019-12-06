// String internment benchmarks and baselines
//
//  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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

mod symbol {
    use super::*;

    /// This is our baseline.  We should never be any slower than this.
    #[bench]
    fn new_raw_rc_1000_baseline(bench: &mut Bencher) {
        let s = "foo bar baz";

        bench.iter(|| {
            (0..1000)
                .map(|_| {
                    let _: Rc<str> = s.into();
                })
                .for_each(drop);
        });
    }

    /// Using the `SymbolRc` wrapper should perform no differently than the
    /// above test with `Rc<str>`.
    #[bench]
    fn new_symbol_rc_1000(bench: &mut Bencher) {
        let s = "foo bar baz";

        bench.iter(|| {
            (0..1000).map(|_| SymbolRc::new(s)).for_each(drop);
        });
    }

    /// Rc uses pointer comparisons when possible.  We want to match that.
    #[bench]
    fn eq_check_rc_baseline(bench: &mut Bencher) {
        bench.iter(|| {
            let a: Rc<str> = "foobarbazquux".into();
            let b: Rc<str> = "foobarbazquux".into();
            let c: Rc<str> = "foobarbazquuxx".into();

            let _ = a == b;
            let _ = a == c;
        });
    }

    #[bench]
    fn eq_check_symbol_rc_baseline(bench: &mut Bencher) {
        bench.iter(|| {
            let a: SymbolRc = "foobarbazquux".into();
            let b: SymbolRc = "foobarbazquux".into();
            let c: SymbolRc = "foobarbazquuxx".into();

            let _ = a == b;
            let _ = a == c;
        });
    }
}

mod hash_set {
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

    fn gen_strs(n: usize) -> Vec<String> {
        (0..n)
            .map(|n| n.to_string() + "foobarbazquuxlongsymbol")
            .collect()
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
            let mut sut = HashSetInterner::<SymbolRc>::new();
            strs.iter().map(|s| sut.intern(&s)).for_each(drop);
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
            let mut sut = HashSetInterner::<SymbolRc>::new();
            (0..1000).map(|_| sut.intern("first")).for_each(drop);
        });
    }

    mod fnv {
        use super::*;
        use ::fnv::FnvBuildHasher;

        /// This is our baseline with a raw Rc<str>.
        #[bench]
        fn with_all_new_rc_str_1000_baseline(bench: &mut Bencher) {
            let strs = gen_strs(1000);
            bench.iter(|| {
                let mut sut = HashSetSut::<FnvBuildHasher>::new();
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }

        #[bench]
        fn with_all_new_1000(bench: &mut Bencher) {
            let strs = gen_strs(1000);

            bench.iter(|| {
                let mut sut =
                    HashSetInterner::<SymbolRc, FnvBuildHasher>::new();
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }

        #[bench]
        /// This is our baseline with a raw Rc<str>.
        fn with_one_new_rc_str_1000_baseline(bench: &mut Bencher) {
            bench.iter(|| {
                let mut sut: HashSetSut<FnvBuildHasher> = HashSetSut {
                    map: HashSet::with_hasher(Default::default()),
                };
                (0..1000).map(|_| sut.intern("first")).for_each(drop);
            });
        }

        #[bench]
        fn with_one_new_1000(bench: &mut Bencher) {
            bench.iter(|| {
                let mut sut =
                    HashSetInterner::<SymbolRc, FnvBuildHasher>::new();
                (0..1000).map(|_| sut.intern("first")).for_each(drop);
            });
        }

        /// Since FNV is the best-performing, let's build upon it to demonstrate
        /// the benefits of with_capacity
        #[bench]
        fn with_all_new_1000_with_capacity(bench: &mut Bencher) {
            let n = 1000;
            let strs = gen_strs(n);

            bench.iter(|| {
                let mut sut =
                    HashSetInterner::<SymbolRc, FnvBuildHasher>::with_capacity(
                        n,
                    );
                strs.iter().map(|s| sut.intern(&s)).for_each(drop);
            });
        }
    }
}
