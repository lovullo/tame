## TAME Makefile
#
#  Copyright (C) 2015 R-T Specialty, LLC.
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

SUBDIRS = src/current/src doc progtest

path_src  = src
path_test = test
path_aux  = build-aux

# all source files will be run through hoxsl; see `applies' target
apply_src := $(path_src)/graph.xsl $(path_src)/symtable.xsl \
             $(path_src)/symtable/symbols.xsl $(path_test)/graph-test.xsl
apply_dest := $(apply_src:%.xsl=%.xsl.apply)

# needed by test runner
export SAXON_CP
export DSLC_CLASSPATH


.DELETE_ON_ERROR:

.PHONY: all all-nodoc check test texis applies FORCE

.DEFAULT_GOAL = all

all: applies progtest
all-nodoc: applies progtest

# the "applies" are hoxsl-generated stylesheets containing definitions to
# permit partial function application
applies: $(apply_dest)
%.apply: %
	$(JAVA) -jar "$(SAXON_CP)" \
	    -xsl:"$(HOXSL)/src/transform/apply-gen.xsl" \
	    "$<" > "$@"

test: check
check: | applies
	for test in $(path_aux)/test/test-*; do ./$$test || exit 1; done
	$(path_test)/runner

progtest: FORCE
	$(MAKE) -C progtest

clean-local:
	$(RM) $(apply_dest)
