## TAMER Makefile
#
#  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##

.DELETE_ON_ERROR:

.PHONY: all fix fmt check-fmt bench

CARGO_BUILD_FLAGS=@CARGO_BUILD_FLAGS@

all:
	@CARGO@ build $(CARGO_BUILD_FLAGS)

doc: html
html-am:
	@CARGO@ doc

# note that 'cargo check' is something else; see 'cargo --help'
test: check
check-am: check-fmt
	@CARGO@ test

check-fmt:
	@CARGO@ fmt -- --check

bench:
	@CARGO@ @CARGO_BENCH_FLAGS@ bench

fix: fmt
fmt:
	@CARGO@ fmt
