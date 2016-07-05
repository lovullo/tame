<?xml version="1.0"?>
<!--
  Tests dependency graph

  Copyright (C) 2016 LoVullo Associates, Inc.

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
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:graph="http://www.lovullo.com/tame/graph"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:foo="http://www.lovullo.com/_junk">

<!-- SUT -->
<import href="../src/graph.xsl" />

<import href="graph-test.xsl.apply" />


<variable name="foo:document" as="element( foo:root )">
  <foo:root>
    <preproc:symtable>
      <preproc:sym name="local" />
      <preproc:sym name="local-dep-a" />
      <preproc:sym name="local-dep-b" />

      <preproc:sym name="external"
                   src="not-here" />

      <!-- says remote, but has local symbol deps -->
      <preproc:sym name="external-but-not"
                   src="not-here" />

      <preproc:sym name="missing-deps" />
    </preproc:symtable>

    <preproc:sym-deps>
      <preproc:sym-dep name="local">
        <preproc:sym-ref name="local-dep-a" />
        <preproc:sym-ref name="local-dep-b" />
      </preproc:sym-dep>

      <preproc:sym-dep name="local-dep-a">
        <preproc:sym-ref name="local-dep-a-dep" />
      </preproc:sym-dep>

      <preproc:sym-dep name="external-but-not">
        <preproc:sym-ref name="dep-external-but-not" />
      </preproc:sym-dep>
    </preproc:sym-deps>
  </foo:root>
</variable>

<function name="foo:lookup">
  <param name="yield"  as="element()" />
  <param name="symbol" as="element( preproc:sym )" />

  <!-- stub graph -->
  <preproc:sym-deps>
    <preproc:sym-dep name="{$symbol/@name}">
      <sequence select="$yield" />
    </preproc:sym-dep>
  </preproc:sym-deps>
</function>

</stylesheet>
