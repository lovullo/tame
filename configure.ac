# For use by automake and autoconf
#
#  Copyright (C) 2015 LoVullo Associates, Inc.
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

AC_INIT([tame], [1.0.0], [dev@lovullo.com])
AC_CONFIG_AUX_DIR([tools])
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

AC_ARG_VAR([JAVA], [The Java executable])
AC_CHECK_PROGS(JAVA, [java])

AC_ARG_VAR([SAXON_CP], [Saxon class path])

: ${HOXSL=hoxsl}
AC_ARG_VAR([HOXSL], [Path to hoxsl])

AC_MSG_CHECKING([hoxsl path HOXSL=$HOXSL])
AS_IF(test ! -d "$HOXSL",
  AC_MSG_ERROR([hoxsl path '$HOXSL' does not exist!]))
AC_MSG_RESULT(found)

AC_CONFIG_FILES([Makefile doc/Makefile src/init.xsl VERSION])

AC_OUTPUT

