# hoxsl manual Makefile
#
# Responsible for building the project documentation.
#
#  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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
tame_TEXINFOS = usage.texi concept.texi preproc.texi license.texi \
                config.texi macros.texi $(stexi) tame.css

MAKEINFOHTML=$(MAKEINFO) --html --css-include tame.css

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
		's#\(<script.*\)\?</body>#<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML" integrity="sha512-tOav5w1OjvsSJzePRtt2uQPFwBoHt1VZcUq8l8nm5284LEKE9FSJBQryzMBzHxY5P0zRdNqEcpLIRVYFNgu1jw==" crossorigin="anonymous"></script></body>#'

clean-local:
	-rm -f $(stexi)
