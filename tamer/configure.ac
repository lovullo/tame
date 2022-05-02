# For use by automake and autoconf
#
#  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

m4_include([build-aux/m4/ax_compare_version.m4])

# version string is dynamically generated
m4_define([ver], m4_esyscmd(build-aux/vergen))
m4_if(ver, [], [m4_exit(1)])

AC_INIT([tame], [ver], [dev@lovullo.com])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign])

# provide more granular version numbers based on the version string, using
# the format MAJOR.MINOR.REV[-SUFFIX], where SUFFIX can itself contain
# dashes (and often will)
m4_define([ver_split],
  m4_split(
    patsubst(AC_PACKAGE_VERSION, [^\([^-]+\)-], [\1.]),
    [\.]))
AC_SUBST(MAJOR, m4_argn(1, ver_split))
AC_SUBST(MINOR, m4_argn(2, ver_split))
AC_SUBST(REV, m4_argn(3, ver_split))
AC_SUBST(SUFFIX, m4_argn(4, ver_split))

AC_ARG_VAR([CARGO], [Rust Cargo executable])
AC_CHECK_PROGS(CARGO, [cargo])

# Rust toolchain (stable/nightly) is hard-coded for now, since decisions
# regarding whether to use it are nuanced.
AC_SUBST([RUST_TC], nightly)

test -n "$CARGO" || AC_MSG_ERROR([cargo not found])

# There is no reason the build should _ever_ access the network.
# This both helps with reproducibility and helps to mitigate supply chain
# attacks by requiring developers to explicitly indicate their intent to
# fetch a network resource (by invoking cargo manually).
AC_SUBST([CARGO_FLAGS], "--frozen --offline")

# This is a nightly version at the time of writing
rustc_ver_req=1.62

AC_CHECK_PROGS(RUSTC, [rustc])
AC_MSG_CHECKING([rustc $RUST_TC version >= $rustc_ver_req])
rustc_version=$("$RUSTC" "+$RUST_TC" --version | cut -d' ' -f2)
AX_COMPARE_VERSION([$rustc_version], [ge], [$rustc_ver_req],
                   [AC_MSG_RESULT([yes ($rustc_version)])],
                   [AC_MSG_RESULT([no ($rustc_version)])
                    AC_MSG_ERROR([If using rustup, run `rustup update'])])

AC_ARG_VAR([CARGO_BUILD_FLAGS],
           [Flags to be passed to `cargo build' when invoked via Make])

# All currently-used doc features are stable (this used to be used for
# intra-doc links)
AC_SUBST([CARGO_DOC_FLAGS], [])

# The `test` feature is required for benchmarking.  If unavailable, then
# it's still an unstable feature and we'll need to use nightly.  We don't
# check for nightly here, though---if it's missing, then cargo will tell the
# user what to do.
AC_MSG_CHECKING([`test` feature support on $RUST_TC])
AS_IF(["$RUSTC" "+$RUST_TC" --crate-type lib build-aux/bench_check.rs &>/dev/null],
      [AC_MSG_RESULT(available)],
      [AC_MSG_RESULT([no (nightly required)])
       AC_SUBST([CARGO_BENCH_PRE_FLAGS], [+nightly])])

# Cargo commands may be available but not necessarily installed for the
# active toolchain.  Let's check that.
AC_MSG_CHECKING([whether cargo-fmt is available on $RUST_TC])
AS_IF([cargo "+$RUST_TC" fmt --help &>/dev/null],
      [AC_MSG_RESULT(yes)],
      [AC_MSG_RESULT(no)
       cargo "+$RUST_TC" fmt --help  # run again so user can see output
       AC_MSG_ERROR([missing cargo-fmt on $RUST_TC])])

# Cargo commands may be available but not necessarily installed for the
# active toolchain.  Let's check that.
AC_MSG_CHECKING([whether cargo-doc is available on $RUST_TC])
AS_IF([cargo "+$RUST_TC" $CARGO_DOC_FLAGS doc --help &>/dev/null],
      [AC_MSG_RESULT(yes)],
      [AC_MSG_RESULT(no)
       # run again so user can see output
       cargo "+$RUST_TC" $CARGO_DOC_FLAGS doc --help 2>&1 | sed 's/^.*: //'
       AC_MSG_WARN([missing cargo-doc on $RUST_TC])
       AC_MSG_WARN([`make html` will not work])])

AC_ARG_VAR([FEATURES],
           [Cargo features flags for TAMER, comma-delimited])

test -z "$FEATURES" || {
  AC_MSG_RESULT([requested features: $FEATURES])
  FEATURES="--features $FEATURES"
}

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
