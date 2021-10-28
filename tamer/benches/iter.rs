// Comparisons between Rust built-ins and memchr.
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

use tamer::iter::into_iter_while_ok;
use test::Bencher;

mod trip {
    use super::*;

    #[bench]
    fn baseline_map_while(bench: &mut Bencher) {
        let mut data = (0..100).map(Ok).collect::<Vec<_>>();
        data.push(Err("trip"));

        bench.iter(|| {
            data.clone()
                .into_iter()
                .map_while(Result::ok)
                .for_each(drop);
        });
    }

    // Note that these aren't comparable feature-wise, since the above
    // doesn't give us access to the underlying iterator.  This is just to
    // ensure that performance is at least pretty close, despite it doing a
    // little more work.  (And it is.)
    #[bench]
    fn trip_iter(bench: &mut Bencher) {
        let mut data = (0..100).map(Ok).collect::<Vec<_>>();
        data.push(Err("trip"));

        bench.iter(|| {
            into_iter_while_ok(data.clone(), |iter| {
                iter.for_each(drop);
            })
        });
    }
}
