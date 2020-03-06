<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Processes object file dependency graph

  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

    This file is part of TAME.

    TAME is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Entry point for object file DOT generation

  To render the graph, we first declare all nodes associated with all referenced
  symbols in the symbol tree (it's important to check against preproc:sym-deps,
  since it's likely that not all imported symbols are used); this allows setting
  attributes for the symbol nodes on the graph without having to worry about
  duplicate code later on.

  After that, we simply recurse through the dependency list and neighbor the
  nodes.
-->
<xsl:template match="lv:package[ preproc:sym-deps ]" priority="5">
  <xsl:apply-templates select="." mode="dot:head" />

  <xsl:variable name="sym-deps" select="preproc:sym-deps" />

  <!-- pre-style all referenced nodes (the symbol table is likely to contain
       references to symbols that were imported but not used) -->
  <xsl:apply-templates mode="dot:defnode" select="
      preproc:symtable/preproc:sym[
        @name=$sym-deps/preproc:sym-dep/preproc:sym-ref/@name
        or @name=$sym-deps/preproc:sym-dep/preproc:sym-ref/@parent
      ]
    " />

  <!-- output graph description -->
  <xsl:apply-templates select="preproc:sym-deps" mode="dot:depout" />

  <xsl:apply-templates select="." mode="dot:tail" />
</xsl:template>


</xsl:stylesheet>

