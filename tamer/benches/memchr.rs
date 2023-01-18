// Comparisons between Rust built-ins and memchr.
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

#![feature(test)]

//! Comparisons between Rust built-ins and memchr.
//!
//! The intent of these benchmarks are to determine how significant of a
//!   benefit the more traditional approach in C (`memchr`) provides over
//!   Rust's built-ins in various situations.
//!
//! See the [`memchr`] crate for more information.

extern crate memchr;
extern crate tamer;
extern crate test;

use test::Bencher;

fn gen_strs(n: usize, suffix: &str) -> Vec<String> {
    (0..n).map(|n| n.to_string() + suffix).collect()
}

mod small_str {
    use super::*;

    mod one {
        use super::*;

        #[bench]
        fn rust_contains_one_char_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foobar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.contains(':')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_byte_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foobar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.as_bytes().contains(&b':')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foobar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(memchr::memchr(b':', s.as_bytes()).is_none())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_char_mid_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foo:bar");

            bench.iter(|| {
                strs.iter().map(|s| assert!(s.contains(':'))).for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_byte_mid_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foo:bar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.as_bytes().contains(&b':')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr_mid_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foo:bar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(memchr::memchr(b':', s.as_bytes()).is_some())
                    })
                    .for_each(drop);
            });
        }
    }

    mod two {
        use super::*;

        #[bench]
        fn rust_contains_two_char_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foobar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.contains(&[':', '>'][..])))
                    .for_each(drop)
            });
        }

        #[bench]
        fn memchr2_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foobar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(
                            memchr::memchr2(b':', b'>', s.as_bytes()).is_none()
                        )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_two_char_mid_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foo>bar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.contains(&[':', '>'][..])))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr2_mid_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, "foo>bar");

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(
                            memchr::memchr2(b':', b'>', s.as_bytes()).is_some()
                        )
                    })
                    .for_each(drop);
            });
        }
    }
}

mod large_str {
    use super::*;

    // Granted, this isn't large compared to some of the strings the linker
    // deals with, but the linker also isn't searching those strings.
    const LG_STR: &'static str = r#"
This is a line of a longer string to test efficiency of searches.
: It contains a unique char near the beginning.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
@ And a unique char near the end.
This is a line of a longer string to test efficiency of searches.
This is a line of a longer string to test efficiency of searches.
"#;

    mod one {
        use super::*;

        #[bench]
        fn rust_contains_one_char_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.contains('_')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_byte_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.as_bytes().contains(&b'_')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(memchr::memchr(b'_', s.as_bytes()).is_none())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_char_early_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter().map(|s| assert!(s.contains(':'))).for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_byte_early_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.as_bytes().contains(&b':')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr_early_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(memchr::memchr(b':', s.as_bytes()).is_some())
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_char_late_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter().map(|s| assert!(s.contains('@'))).for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_one_byte_late_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.as_bytes().contains(&b'@')))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr_late_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(memchr::memchr(b'@', s.as_bytes()).is_some())
                    })
                    .for_each(drop);
            });
        }
    }

    mod two {
        use super::*;

        #[bench]
        fn rust_contains_two_char_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(!s.contains(&['_', '!'][..])))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr2_non_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(
                            memchr::memchr2(b'_', b'!', s.as_bytes()).is_none()
                        )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_two_char_early_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.contains(&['_', ':'][..])))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr2_early_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(
                            memchr::memchr2(b'_', b':', s.as_bytes()).is_some()
                        )
                    })
                    .for_each(drop);
            });
        }

        #[bench]
        fn rust_contains_two_char_late_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| assert!(s.contains(&['_', '@'][..])))
                    .for_each(drop);
            });
        }

        #[bench]
        fn memchr2_late_match(bench: &mut Bencher) {
            let strs = gen_strs(1000, LG_STR);

            bench.iter(|| {
                strs.iter()
                    .map(|s| {
                        assert!(
                            memchr::memchr2(b'_', b'@', s.as_bytes()).is_some()
                        )
                    })
                    .for_each(drop);
            });
        }
    }
}
