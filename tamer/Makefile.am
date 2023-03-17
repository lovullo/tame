## TAMER Makefile
#
#  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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

SHELL = /bin/bash -o pipefail
.DELETE_ON_ERROR:

.PHONY: all fix fmt check-fmt bench bench-build FORCE

CARGO_BUILD_FLAGS=@CARGO_BUILD_FLAGS@
RUSTFLAGS=@RUSTFLAGS@

ontviz_svg := target/doc/tamer/asg/ontviz.svg

.DEFAULT: bin

all: bin doc bench-build

.PHONY: bin
bin:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ build $(CARGO_BUILD_FLAGS) @FEATURES@

doc: html
html-am: rustdoc $(ontviz_svg)

.PHONY: rustdoc
rustdoc:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ test --doc @FEATURES@
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_DOC_FLAGS@ doc --document-private-items @FEATURES@

$(ontviz_svg): FORCE
	build-aux/asg-ontviz | $(DOT) -Tsvg > $@

# note that 'cargo check' is something else; see 'cargo --help'
test: check
check-am: check-lint check-fmt check-cargo check-docgen check-system

.PHONY: check-cargo
check-cargo:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ test --quiet @FEATURES@

.PHONY: check-docgen
check-docgen:
	build-aux/asg-ontviz >/dev/null

.PHONY: check-system
check-system: bin
	tests/run-tests

.PHONY: check-lint
check-lint:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ clippy @FEATURES@

.PHONY: check-fmt
check-fmt:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ fmt -- --check

bench:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_BENCH_PRE_FLAGS@ bench $(CARGO_BENCH_FLAGS) @FEATURES@

# Build but do not run benches (to ensures we didn't break them)
bench-build:
	RUSTFLAGS="$(RUSTFLAGS)" @CARGO@ +@RUST_TC@ @CARGO_FLAGS@ @CARGO_BENCH_PRE_FLAGS@ build --benches @FEATURES@

fix: fmt
fmt:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ fmt

clean-am:
	@CARGO@ +@RUST_TC@ @CARGO_FLAGS@ clean

.PHONY: fetch
fetch:
	@CARGO@ +@RUST_TC@ --locked fetch

