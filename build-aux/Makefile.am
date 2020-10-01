# @configure_input@
#
# TAME Makefile
#
#   Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
##

path_rates         := $(path_suppliers)/rates
path_map           := map
path_c1map         := $(path_map)/c1
path_dsl           := $(CALCROOT)
path_tame          := $(path_dsl)/tame
path_ui            := ui
path_tests         := test
path_suppliers     := suppliers
path_lv            := lovullo
path_srv           := srv
path_lvroot        := lvroot
path_c1root        := c1root
path_common        := common
path_intralov_root := "intralov-root/@program@"

src_suppliers := $(wildcard $(path_suppliers)/*.xml)
src_map       := $(wildcard $(path_map)/*.xml)
src_c1map     := $(wildcard $(path_c1map)/*.xml)

src_common := $(shell find $(path_common) -name '*.xml')
xmlo_common := $(patsubst %.xml, %.xmlo, $(src_common))

# Intended to be (optionally) overridden from the command line
SUPPLIERS=$(src_suppliers) $(path_ui)/package.xml
suppliers_strip=$(patsubst %.xml, %.strip.js, $(SUPPLIERS))

dest_summary_html := $(patsubst \
	$(path_suppliers)/%.xml, \
	$(path_suppliers)/%.html, \
	$(src_suppliers))
dest_standalone := $(patsubst \
	$(path_suppliers)/%.xml, \
	$(path_suppliers)/%.js, \
	$(src_suppliers))
dest_standalone_strip := $(patsubst \
	$(path_suppliers)/%.js, \
	$(path_suppliers)/%.strip.js, \
	$(dest_standalone))
dest_map := $(patsubst \
	$(path_map)/%.xml, \
	$(path_map)/%.xmle, \
	$(src_map))
dest_c1map := $(patsubst \
	$(path_c1map)/%.xml, \
	$(path_c1map)/%.php, \
	$(src_c1map))

program_fragments=$(shell \
  find $(path_ui)/program/ -name '*.xml' 2>/dev/null \
    | LC_ALL=C sort \
    | tr '\n' ' ' \
)

compiled_suppliers := $(src_suppliers:.xml=.xmlo)
linked_suppliers   := $(src_suppliers:.xml=.xmle)

comma := ,
extless_supp_delim := $(subst .xml,,$(subst .xml ,$(comma),$(src_suppliers)))

ant = @ANT@ -e

.PHONY: FORCE default program-data-copy lvroot c1root test \
        default clean interp-rate-tables summary-html c1map standalones common \
        strip program-ui version FORCE

default: program-ui c1map FORCE

.DELETE_ON_ERROR:

# keep all intermediate files for easy introspection
.SECONDARY:

SHELL = /bin/bash -O extglob -O nullglob

# propagate to tame{,d}
export TAME_CMD_WAITTIME
export TAMED_STALL_SECONDS
export TAMED_JAVA_OPTS

TAMED_SPAWNER_PID=$(shell echo $$PPID)
export TAMED_SPAWNER_PID

# Optional timestamping for TAME commands
TS_FMT=%s
tame__ts_0 = :
tame__ts_1 = @printf '[%($(TS_FMT))T] '
TAME_TS = $(tame__ts_$(TS))

all: program-data-copy

# Building all common files is useful in a distributed pipeline so that
# suppliers can be concurrently built without rebuilding common dependencies
common: $(xmlo_common)

program-ui: ui/package.strip.js ui/Program.js ui/html/index.phtml

# Handle an intermediate step as we transition to the new compiler
%.xmli: %.xml
	$(path_tame)/tamer/target/release/tamec --emit xmlo -o $@ $<

%.graphml: %.xmlo
	$(TAME_TS)
	$(path_tame)/tamer/target/release/tameld --emit graphml -o $@ $<

# Individual dependencies appear in suppliers.mk (see below)
%.xmlo: %.xmli $(path_tame)/.rev-xmlo
	$(TAME_TS)
	$(TAME) compile $< $@

# Note the `$()' here to prevent Automake from inlining this file---it is
# to be generated when imports change, which can be at any time.
include $()suppliers.mk

summary-html: $(dest_summary_html) ;

%.html: %.js %.xmle
	$(TAME_TS)
	$(TAME) summary $*.xmle $@

standalones: $(dest_standalone)
strip: $(dest_standalone_strip) ui/package.strip.js
%.xmle: %.xmlo $(path_tame)/.rev-xmle
	$(TAME_TS)
	$(path_tame)/tamer/target/release/tameld --emit xmle -o $@ $<
%.js: %.xmle
	$(TAME_TS)
	$(TAME) standalone $< $@
%.strip.js: %.js
	cp $< $@
	$(path_tame)/tools/strip $@

# C1 XML (specific recipes are in suppliers.mk)
c1map: $(dest_c1map)

%.dot: %.xmlo
	$(TAME_TS)
	$(TAME) dot $< $@
%.dote: %.xmle
	$(TAME_TS)
	$(TAME) dot $< $@

%.neo4j: %.xmlo
	$(TAME) neo4j $< $@
%.neo4je: %.xmle
	$(TAME) neo4j $< $@


%.svg: %.dote
	dot -Tsvg "$<" > "$@"
%.svg: %.dot
	dot -Tsvg "$<" > "$@"

%.xml: %.dat
	rater/tools/tdat2xml $< > $@

%.xml: %.typelist
	rater/tame/build-aux/list2typedef $(*F) < $< > $@

%.csvo: %.csvm
	rater/tools/csvm2csv $< > $@
%.csvo: %.csvi
	rater/tools/csvi $< > $@
%.csvo: %.csv
	cp $< $@

# TODO: This is necessary right now because of the current depgen
# process.  Once that is eliminated in favor of individual dependency files
# (e.g. the %.d convention), this can go away since dependency generation
# can properly take place for the various file formats.
%.xml: %.csvo rater/core/vector/table.xmlo
	rater/tools/csv2xml $< > $@

version: .version.xml
.version.xml: FORCE
	git log HEAD^.. -1 --pretty=format:'<version>%h</version>' > .version.xml

ui/program.expanded.xml: ui/program.xml $(program_fragments)
	$(TAME_TS)
	$(TAME) progui-expand $< $@
ui/Program.js: ui/program.expanded.xml ui/package.js
	$(TAME_TS)
	$(TAME) progui-class $< $@ include-path=$$(pwd)/ui/
ui/html/index.phtml: ui/program.expanded.xml
	$(TAME_TS)
	$(TAME) progui-html $< $@ out-path=./
ui/package-dfns.xmlo: ui/package-dfns.xml
ui/package-dfns.xml: ui/program.expanded.xml
	$(TAME_TS)
	$(TAME) progui-pkg $< $@
ui/package-map.xmlo: ui/package-map.xml ui/package-dfns.xmlo
ui/package-map.xml: ui/program.expanded.xml ui/package-dfns.xml
	$(TAME_TS)
	$(TAME) progui-pkg-map $< $@

# for the time being, this does not depend on clean-rate-tables because $(ant) will
specs:
	$(MAKE) -C doc/specs

# for the time being, this does not depend on clean-rate-tables because ant will
# run it
clean:
	find -L $(path_suppliers) $(path_map) $(path_c1map) common/ rater/core rater/lv \( \
		-name '*.xmlo' \
		-o -name '*.xmle' \
		-o -name '*.xmli' \
		-o -name '*.js' \
		-o -name '*.html' \
		-o -name '*.dep' \
		-o -name '*.tmp' \
		-o -name '*.php' \
	\) -exec rm -v {} \;
	rm -rf $(path_ui)/package-dfns.* \
		$(path_ui)/package-map.* \
		$(path_ui)/program.expanded.xml \
		$(path_ui)/include.js \
		$(path_ui)/Program.js \
		$(path_ui)/html
	find . -path '*/tables/*.csvm' -o -path '*/territories/*.dat' \
		| sed 's/\.csvm$$/\.xml/; s/\.dat$$/\.xml/' \
		| xargs rm -fv

# generates a Makefile that will properly build all package dependencies; note
# that territory and rate packages also have includes; see top of this file for
# an explanation
suppliers.mk:
	$(ant) pkg-dep
	test ! -f $(path_ui)/program.dep || mv $(path_ui)/program.dep $(path_ui)/package-dfns.dep
	$(path_dsl)/tame/build-aux/gen-make $(SRCPATHS) > $@
	test ! -d $(path_c1map) || $(path_dsl)/tame/build-aux/gen-c1make $(path_c1map)/*.xml >> $@

# TODO: There is a potential for conflict in copying files to
# src/node/programs/rater/programs/@program@.  Note that the `for' loop is
# used here to handle the situation where no such files exist.
program-data-copy: standalones program-ui c1map .version.xml
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
	for f in "$(path_srv)/"!(rater).js; do \
	  cp -v "$$f" "$(path_lv)/src/node/program/rater/programs/@program@/"; \
	done
	cp -v "$(path_ui)/package.strip.js" \
	  "$(path_lv)/src/node/program/classify/@program@.js"
	cp -v "$(path_ui)/"{Program,include,package}.js \
	  "$(path_lv)/src/_gen/scripts/program/@program@/"
	cp -vr "$(path_ui)/html/"* \
	  "$(path_lv)/src/_gen/views/scripts/quote/@program@/"
	cp -v "$(path_suppliers)/"*.js \
	  "$(path_lv)/src/node/program/rater/programs/@program@"
	test ! -d "$(path_c1map)" || cp -v "$(path_c1map)/"*.php \
	  "@C1_IMPORT_MAPDEST@/@program@/"
	ant -f "$(path_lv)/build.xml" js-mod-order

# TODO: merge this and the above
lvroot: summary-html program-ui c1map strip
	mkdir -p "$(path_lvroot)/src/node/program/rater/programs/@program@"
	mkdir -p "$(path_lvroot)/src/node/program/classify"
	mkdir -p "$(path_lvroot)/src/node/program/ui/custom"
	mkdir -p "$(path_lvroot)/src/_gen/scripts/program/@program@"
	mkdir -p "$(path_lvroot)/src/_gen/views/scripts/quote/@program@"
	mkdir -p "$(path_lvroot)/src/www/scripts/program"
	mkdir -p "$(path_lvroot)/src/lib/c1/interfaces/c1/contract/@program@"
	cp -v "$(path_srv)/rater.js" \
	  "$(path_lvroot)/src/node/program/rater/programs/@program@.js"
	for f in "$(path_srv)/"!(rater).js; do \
	  cp -v "$$f" "$(path_lvroot)/src/node/program/rater/programs/@program@/"; \
	done
	cp -v "$(path_suppliers)/"*.js \
	  "$(path_lvroot)/src/node/program/rater/programs/@program@"
	cp -v "$(path_ui)/package.strip.js" \
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

# used by newer systems (note that lvroot still contains the c1map files so
# as not to break BC)
c1root: c1map
	mkdir -p "$(path_c1root)/src/RSG/ImportBundle/Lib/interfaces/c1/contract/@program@/"
	cp -v "$(path_c1map)/"*.php \
	  "$(path_c1root)/src/RSG/ImportBundle/Lib/interfaces/c1/contract/@program@/"

intralov-root: summary-html
	mkdir -p "$(path_intralov_root)/"{rater/scripts,suppliers}
	ln -fL $(path_dsl)/summary.css "$(path_intralov_root)/rater"
	ln -fL $(path_dsl)/scripts/*.js "$(path_intralov_root)/rater/scripts/"
	ln -fL $(path_suppliers)/*.{html,js} "$(path_intralov_root)/suppliers"

# Suppliers to check may be overridden using SUPPLIERS.  Multiple suppliers
# should be space-delimited.  Note that the UI is considered to be a special
# type of supplier (ui/package.xml) and is included by default in the value
# of SUPPLIERS.
check-am: $(suppliers_strip)
	$(path_dsl)/build-aux/progtest-runner $(path_tests) $(SUPPLIERS)
test: check

kill-tamed: tamed-die
tamed-die:
	$(TAME_TS)
	$(TAME) --kill

me-a-sandwich:
	@test $$EUID -eq 0 \
	  && echo 'You actually ran me as root? Are you insane!?' \
	  || echo 'Make it yourself.'

FORCE: ;

# optionally include a "program.mk" file if it is
# present in the project's root directory
-include program.mk

