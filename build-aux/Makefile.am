# @configure_input@
#
# TAME Makefile
#
#   Copyright (C) 2018 R-T Specialty, LLC.
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
# This fragment exists as a kluge to provide support for running a command
# after all targets have been run (in this case, dslc).
#
# A list of everything to be compiled is output into .cqueue, which is then
# picked up by dslc; this avoids the overhead of starting the JVM,
# recompiling XSL stylesheets, etc, which is quite substantial.
#
# !!! Unfortunately, this does not yet support parallel job execution.

path_rates         := $(path_suppliers)/rates
path_map           := map
path_c1map         := $(path_map)/c1
path_dsl           := rater
path_ui            := ui
path_tests         := test
path_suppliers     := suppliers
path_lv            := lovullo
path_srv           := srv
path_lvroot        := lvroot
path_intralov_root := "intralov-root/@program@"

.PHONY: FORCE prepare program-data-copy lvroot program-ui-immediate test

JAVA_HEAP_SIZE ?= 5120M
JAVA_STACK_SIZE ?= 5M

.DELETE_ON_ERROR:

# less verbose output; all to runlog
define saneout
  time -f 'total time: %E' awk ' \
    BEGIN { e=0; w=0; } \
    { printf "[%d] ", systime() >> ".runlog"; print >> ".runlog"; } \
    /^~~~~\[begin /,/^~~~~\[end / { next } \
    /^rm / { next } \
    /^Exception|^\t+at / { \
      if ( /^E/ ) { \
        print; \
        print "Stack trace written to .runlog"; \
      } \
      next; \
     } \
    /[Ww]arning:|[Nn]otice:/ { printf "\033[0;33m"; w++; out=1; } \
    /[Ff]atal:/ { printf "\033[0;31m"; out=1; } \
    /!|[Ee]rror:/ { printf "\033[0;31m"; e++; out=1; } \
    /internal:/ { printf "\033[0;35m"; out=1; } \
    /internal error:/ { printf "\033[1m"; out=1; } \
    /^[^[]/ || out { print; printf "\033[0;0m"; out=0; } \
    END { printf "%d error(s); %d warning(s).\n", e, w; } \
  '
endef

define _go
  touch .cqueue \
  && ( test -s .cqueue || echo "Nothing to be done for \`$@'." ) \
  && echo "$(JAVA_HEAP_SIZE) $(JAVA_STACK_SIZE)" \ \
  && CLASSPATH="$(RATER_CLASSPATH):rater/src/dslc.jar" \
     $(JAVA) -Xmx$(JAVA_HEAP_SIZE) -Xss$(JAVA_STACK_SIZE) \
	   com.lovullo.dslc.DslCompiler < .cqueue 2>&1 \
     | $(saneout); \
  exit $${PIPESTATUS[0]}; \
  @>.cqueue
endef

SHELL = /bin/bash -O extglob

all: program-data-copy

program-ui-immediate:
	@>.cqueue
	@$(MAKE) --no-print-directory -f Makefile.2 program-ui-immediate
	@$(MAKE) program-data-copy
	@$(_go)

program-data-copy:
	@>.cqueue
	@$(MAKE) --no-print-directory -f Makefile.2 standalones program-ui c1map
	@$(_go)
	mkdir -p "$(path_lv)/src/node/program/rater/programs/@program@"
	mkdir -p "$(path_lv)/src/node/program/classify"
	mkdir -p "$(path_lv)/src/node/program/ui/custom"
	mkdir -p "$(path_lv)/src/_gen/scripts/program/@program@"
	mkdir -p "$(path_lv)/src/_gen/views/scripts/quote/@program@"
	mkdir -p "$(path_lv)/src/lib/c1/interfaces/c1/contract/@program@"
	mkdir -p "$(path_lv)/misc/rater/programs"
	mkdir -p "$(path_lv)/src/www/scripts/program"
	cp -v .version.xml \
	  "$(path_lv)/misc/rater/programs/.version-@program@.xml"
	cp -v "$(path_ui)/custom.js" \
	  "$(path_lv)/src/www/scripts/program/@program@.js"
	cp -v "$(path_ui)/"!(custom|package|include).js \
	  "$(path_lv)/src/node/program/ui/custom/"
	cp -v "$(path_srv)/rater.js" \
	  "$(path_lv)/src/node/program/rater/programs/@program@.js"
	cp -v "$(path_ui)/package.js" \
	  "$(path_lv)/src/node/program/classify/@program@.js"
	cp -v "$(path_ui)/"{Program,include,package}.js \
	  "$(path_lv)/src/_gen/scripts/program/@program@/"
	cp -vr "$(path_ui)/html/"* \
	  "$(path_lv)/src/_gen/views/scripts/quote/@program@/"
	cp -v "$(path_suppliers)/"*.js \
	  "$(path_lv)/src/node/program/rater/programs/@program@"
	test ! -d "$(path_c1map)" || cp -v "$(path_c1map)/"*.php \
	  "$(path_lv)/src/lib/c1/interfaces/c1/contract/@program@/"
	ant -f "$(path_lv)/build.xml" js-mod-order

# TODO: merge this and the above
lvroot: prepare
	mkdir -p "$(path_lvroot)/src/node/program/rater/programs/@program@"
	mkdir -p "$(path_lvroot)/src/node/program/classify"
	mkdir -p "$(path_lvroot)/src/node/program/ui/custom"
	mkdir -p "$(path_lvroot)/src/_gen/scripts/program/@program@"
	mkdir -p "$(path_lvroot)/src/_gen/views/scripts/quote/@program@"
	mkdir -p "$(path_lvroot)/src/www/scripts/program"
	mkdir -p "$(path_lvroot)/src/lib/c1/interfaces/c1/contract/@program@"
	cp -v "$(path_srv)/rater.js" \
	  "$(path_lvroot)/src/node/program/rater/programs/@program@.js"
	cp -v "$(path_suppliers)/"*.js \
	  "$(path_lvroot)/src/node/program/rater/programs/@program@"
	cp -v "$(path_ui)/package.js" \
	  "$(path_lvroot)/src/node/program/classify/@program@.js"
	cp -v "$(path_ui)/"{Program,include,package}.js \
	  "$(path_lvroot)/src/_gen/scripts/program/@program@/"
	cp -vr "$(path_ui)/html/"* \
	  "$(path_lvroot)/src/_gen/views/scripts/quote/@program@/"
	cp -v "$(path_ui)/custom.js" \
	  "$(path_lvroot)/src/www/scripts/program/@program@.js"
	cp -v "$(path_ui)/"*Ui.js \
	  "$(path_lvroot)/src/node/program/ui/custom/"
	test ! -d "$(path_c1map)" || cp -v "$(path_c1map)/"*.php \
	  "$(path_lvroot)/src/lib/c1/interfaces/c1/contract/@program@/"

intralov-root: summary-html
	mkdir -p "$(path_intralov_root)/"{rater/scripts,suppliers}
	ln -fL $(path_dsl)/summary.css "$(path_intralov_root)/rater"
	ln -fL $(path_dsl)/scripts/*.js "$(path_intralov_root)/rater/scripts/"
	ln -fL $(path_suppliers)/*.{html,js} "$(path_intralov_root)/suppliers"


# because of the crazy wildcard target below, we want to ignore
# some Automake-generated stuff
%.am:
%.m4:
%.ac:

%: prepare
	@if [[ "$@" != [Mm]akefile ]]; then \
		$(MAKE) --no-print-directory -f Makefile.2 $@; \
		$(_go); \
	fi

clean:
	$(MAKE) --no-print-directory -f Makefile.2 clean

prepare: FORCE
	@>.cqueue

test: check
check-am: standalones ui/package.js
	@$(path_dsl)/build-aux/progtest-runner $(path_suppliers) $(path_tests)
	@$(path_dsl)/build-aux/progtest-runner ui/package.xml $(path_tests)/ui

FORCE: ;
