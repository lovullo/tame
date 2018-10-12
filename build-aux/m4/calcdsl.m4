# Common build configuration for TAME-based build systems
#
#   Copyright (C) 2017 R-T Specialty, LLC.
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# To use, include this in your configure.ac:
#   m4_define(`calc_root', path/to/calc/root)
#   m4_include([path/to/calc/root/build-aux/m4/calcdsl.m4])
##

# We use an M4 value (calc_root, specifically); this allows us to cleanly
# reference it
m4_pattern_allow([defn])

# Initialize Automake, indicating that we use non-standard conventions
AC_CONFIG_AUX_DIR(m4_defn(`calc_root')/build-aux)
AM_INIT_AUTOMAKE([foreign])

# Configuration values that can be provided via environment variables or the
# command line at configure- or build-time.
AC_ARG_VAR([JAVA], [The Java executable])
AC_ARG_VAR([ANT], [Apache Ant])
AC_ARG_VAR([DSLC_JAR], [Path to DSL Compiler JAR])
AC_ARG_VAR([TAME], [The TAME compiler])
AC_ARG_VAR([RATER_CLASSPATH], [DSL Compiler Saxon class path])
AC_ARG_VAR([PROGUI_TEST_PATH], [Path to JavaScript tests for Program UI])

# Auto-discover Java and Ant paths
AC_CHECK_PROGS(JAVA, [java])
AC_CHECK_PROGS(ANT, [ant])

AS_IF([test "$JAVA"],,
  [AC_MSG_ERROR([missing java])])
AS_IF([test "$ANT"],,
  [AC_MSG_ERROR([missing ant])])

# Automake runs before shell is available, thus the separate m4 variable
CALCROOT="m4_defn(`calc_root')"

# Checks to ensure that dslc is built, and gives instructions on how to
# build it otherwise.  We do not want to build that for them---that can be
# added to a bootstrap script, but isn't permissible in build scripts.
AS_IF([test ! "$DSLC_JAR"],
  [AC_CHECK_FILE([$CALCROOT/src/dslc.jar],
    [AC_SUBST([DSLC_JAR], [$CALCROOT/src/dslc.jar])],
    [AC_MSG_ERROR(
      [Please run `make` in $CALCROOT to build the DSL compiler.])])],
  [])

# TAME is the compiler (whereas dslc invokes it, keeps things in memory, etc)
AS_IF([test ! "$TAME"],
  [AC_CHECK_FILE([$CALCROOT/tame/bin/tame],
    [AC_SUBST([TAME], [$CALCROOT/tame/bin/tame])],
    [AC_MSG_ERROR(
      [TAME not found])])],
  [])

# @program@ in *.in files will be replaced with the program name provided by AC_INIT
AC_SUBST([program], AC_PACKAGE_NAME)

# Final files to be output by `configure'.  The path before the colon is the
# destination name; after the colon is the source.
AC_CONFIG_FILES(Makefile:m4_defn(`calc_root')/build-aux/Makefile.in)

# Generate configure script
AC_OUTPUT

# we want this to run as part of the configure script, not during M4
# expansion
"$CALCROOT/build-aux/suppmk-gen"

AC_MSG_NOTICE([complete

You may now run `make` to build.])
