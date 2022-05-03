## TAMER Makefile
#
#  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
#
#  This file is part of TAME.
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

# `Makefile.am` is processed by Automake into `Makefile.in`, which is then
# processed by Autoconf into the final `Makefile`.
#
# The `@`-delimited variables are defined dynamically in `configure.ac`,
# which is processed by Autoconf into `configure`; they are populated based
# on the environment in which the `configure` script is invoked.

.DELETE_ON_ERROR:

.PHONY: all fix fmt check-fmt bench bench-build

CARGO_BUILD_FLAGS=@CARGO_BUILD_FLAGS@

.DEFAULT: bin

all: bin doc bench-build

.PHONY: bin
bin:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ build $(CARGO_BUILD_FLAGS) @FEATURES@

doc: html
html-am:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ test --doc @FEATURES@
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_DOC_FLAGS@ doc --document-private-items @FEATURES@

# note that 'cargo check' is something else; see 'cargo --help'
test: check
check-am: check-fmt
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ test @FEATURES@

check-fmt:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ fmt -- --check

bench:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_BENCH_PRE_FLAGS@ bench $(CARGO_BENCH_FLAGS) @FEATURES@

# Build but do not run benches (to ensures we didn't break them)
bench-build:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_BENCH_PRE_FLAGS@ build --benches @FEATURES@

fix: fmt
fmt:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ fmt

clean-am:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ clean

.PHONY: fetch
fetch:
	@CARGO@ +@RUST_TC@ --locked fetch

