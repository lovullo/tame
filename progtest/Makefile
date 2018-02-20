# tame-progtest Makefile
#
#  Copyright (C) 2018 R-T Specialty, LLC.
#
#  This file is part of TAME.
#
#  TAME is free software: you can redistribute it and/or modify
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

.PHONY: check test modindex dist browserify FORCE

namespaces=$(shell find src/ -type d)
nsindex=$(addsuffix /index.js, $(namespaces))

.DEFAULT_GOAL: all

# used be parent Makefile to prevent doc/ building `all'
all-nodoc: all
all: tame-progtest.js

modindex: $(nsindex)
%/index.js: FORCE
	$(CURDIR)/build-aux/gen-index "$*" > "$@"

test: check
check:
	PATH="$(PATH):$(CURDIR)/node_modules/mocha/bin" \
		mocha --harmony_destructuring --recursive test/

browserify: tame-progtest.js
tame-progtest.js: check modindex
	$(CURDIR)/node_modules/.bin/browserify \
		--debug \
		-r $(CURDIR)/src/index.js:progtest \
		$(CURDIR)/src/index.js \
		-o "$@"

dist: tame-progtest.js
