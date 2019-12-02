# For use by automake and autoconf
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

test -n "$CARGO" || AC_MSG_ERROR([cargo not found])

rustc_ver_req=1.39.0

AC_CHECK_PROGS(RUSTC, [rustc])
AC_MSG_CHECKING([rustc version >= $rustc_ver_req])
rustc_version=$("$RUSTC" --version | cut -d' ' -f2)
AX_COMPARE_VERSION([$rustc_version], [ge], [$rustc_ver_req],
                   [AC_MSG_RESULT([yes ($rustc_version)])],
                   [AC_MSG_ERROR([no ($rustc_version)])])

AC_ARG_VAR([CARGO_BUILD_FLAGS],
           [Flags to be passed to `cargo build' when invoked via Make])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT

