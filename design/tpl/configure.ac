# Autoconf configuration
#
#  Copyright (C) 2021 Ryan Specialty, LLC
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

AC_INIT([tpl], [0.0.0], [])

# TODO: Check for pdflatex and required packages

use_appendix=false
AC_ARG_ENABLE(
  [appendix],
  [AS_HELP_STRING([--enable-appendix],
                  [enable appendix (default no)])],
  [test "x$enableval" == xyes && use_appendix=true])

AC_SUBST([ENABLE_APPENDIX], [$use_appendix])

AC_SUBST([AUTOGENERATED],
         ["THIS FILE IS AUTOGENERATED!  DO NOT MODIFY!  See *.in."])

# generate files from their *.in counterparts
AC_CONFIG_FILES([conf.tex])
AC_OUTPUT

