<!---
  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

  Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License,
  Version 1.3 or any later version published by the Free Software
  Foundation; with no Invariant Sections, no Front-Cover Texts, and no
  Back-Cover Texts.  A copy of the license is included the file
  COPYING.FDL.
-->
TAME in Rust (TAMER)
====================
TAME was written to help tame the complexity of developing comparative
insurance rating systems.  This project aims to tame the complexity and
performance issues of TAME itself.  TAMER is therefore more tame than TAME.

TAME was originally written in XSLT.  For more information about the
project, see the [parent `README.md`](../README.md).


## Building
To bootstrap from the source repository, run `./bootstrap`.

To configure the build for your system, run `./configure`.  To build, run
`make`.  To run tests, run `make check`.

You may also invoke `cargo` directly, which `make` will do for you using
options provided to `configure`.

*Note that the default development build results in terrible runtime
performance!*  See [#Build Flags][] below for instructions on how to
generate a release binary.


### Build Flags
The environment variable `CARGO_BUILD_FLAGS` can be used to provide
additional arguments to `cargo build` when invoked via `make`.  This can be
provided optionally during `configure` and can be overridden when invoking
`make`.  For example:

```sh
# release build
$ ./configure && make CARGO_BUILD_FLAGS=--release
$ ./configure CARGO_BUILD_FLAGS=--release && make

# dev build
$ ./configure && make
$ ./configure CARGO_BUILD_FLAGS=--release && make CARGO_BUILD_FLAGS=
```


## Hacking
This section contains advice for those developing TAMER.


### Running Tests
Developers should be using test-driven development (TDD).  `make check` will
run all necessary tests.


### Code Format
Rust provides `rustfmt` that can automatically format code for you.  This
project mandates its use and therefore eliminates personal preference in
code style (for better or worse).

Formatting checks are run during `make check` and, on failure, will output
the diff that would be applied if you ran `make fmt` (or `make fix`); this
will run `cargo fmt` for you (and will use the binaries configured via
`configure`).

Since developers should be doing test-driven development (TDD) and therefore
should be running `make check` frequently, the hope is that frequent
feedback on formatting issues will allow developers to quickly adjust their
habits to avoid triggering formatting errors at all.

If you want to automatically fix formatting errors and then run tests:

```sh
$ make fmt check
```


## Benchmarking
Benchmarks serve two purposes: external integration tests (which are subject
to module visibility constraints) and actual benchmarking.  To run
benchmarks, invoke `make bench`.

Note that link-time optimizations (LTO) are performed on the binary for
benchmarking so that its performance reflects release builds that will be
used in production.

The `configure` script will automatically detect whether the `test` feature
is unstable (as it was as of the time of writing) and, if so, will
automatically fall back to invoking nightly (by running `cargo +nightly
bench`).

If you do not have nightly, run you install it via `rustup install nightly`.
