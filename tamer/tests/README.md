# System and Integration Tests
Rust files in this directory will be recognized by Cargo and will be
automatically compiled and run by `make check`.

Shell scripts prefixed with `test-` will be recognized by our test harness
and run on `make check`.  These scripts should be preferred when confidence
in the system end-to-end is required, since they invoke the binaries just
the same as the user or build process would.

Unit and integration tests written in Rust are located alongside the modules
they test in [`../src/`](../src/).  Benchmarks are in
[`../benches`](../benches).

