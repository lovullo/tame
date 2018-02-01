# hoxsl manual Makefile
#
# Responsible for building the project documentation.
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

path_src   := ../src
path_tools := ../tools

stylesheets := $(shell find "$(path_src)" \
                         -name '*.xsl' \
                         -a \! -path "$(path_src)"/current/\* )
stexi       := $(stylesheets:.xsl=.texi)

info_TEXINFOS = tame.texi
tame_TEXINFOS = todo.texi license.texi $(stexi)

MAKEINFOHTML=$(MAKEINFO) --html

xsltexis: $(stexi)
%.texi: %.xsl
	$(JAVA) -jar "$(SAXON_CP)" \
	    -xsl:"$(path_tools)/literate-xsl/src/texinfo.xsl" \
	    "$<" > "$@"

# Privacy warning: if scripts are enabled, then you are subject to the
# normal tracking risks of loading external scripts.  If this is from your
# own domain or local filesystem, then you may be leaking personal
# information.
html-local:
	find tame.html/ -name '*.html' -type f \
		| xargs sed -i \
		's#\(<script.*\)\?</body>#<script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script></body>#'

clean-local:
	-rm -f $(stexi)
