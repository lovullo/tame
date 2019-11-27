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
