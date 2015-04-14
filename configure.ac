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

AC_INIT([tame], [0.0.0], [dev@lovullo.com])
AC_CONFIG_AUX_DIR([tools])
AM_INIT_AUTOMAKE([foreign])

AC_ARG_VAR([JAVA], [The Java executable])
AC_CHECK_PROGS(JAVA, [java])

AC_CONFIG_FILES([Makefile doc/Makefile])

AC_OUTPUT
