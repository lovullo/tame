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


<!-- a graph that is easier to mentally grasp when reversed -->
<variable name="foo:reverse-graph" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="A">
      <preproc:sym-ref name="B" />
      <preproc:sym-ref name="C" cattr="cvalue" />
    </preproc:sym-dep>

    <preproc:sym-dep name="B">
      <preproc:sym-ref name="C" cattr="cvalue" cattr2="cvalue2" />
    </preproc:sym-dep>

    <preproc:sym-dep name="C">
      <preproc:sym-ref name="D" />
    </preproc:sym-dep>

    <!-- produces a cycle -->
    <preproc:sym-dep name="D">
      <preproc:sym-ref name="B" />
    </preproc:sym-dep>

    <!-- disconnected -->
    <preproc:sym-dep name="X">
      <preproc:sym-ref name="Z" />
    </preproc:sym-dep>

    <preproc:sym-dep name="Z" />
  </preproc:sym-deps>
</variable>


<variable name="foo:graph-empty" as="element( preproc:sym-deps )">
  <preproc:sym-deps />
</variable>


<variable name="foo:graph-vtwo" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="a">
      <preproc:sym-ref name="a" attr1="foo" />
      <preproc:sym-ref name="b" attr2="bar" />
    </preproc:sym-dep>

    <!-- test empty for merge -->
    <preproc:sym-dep name="b" />
  </preproc:sym-deps>
</variable>


<variable name="foo:graph-vthree" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="a">
      <preproc:sym-ref name="b" attr1="foo" />
      <preproc:sym-ref name="c" />
    </preproc:sym-dep>

    <preproc:sym-dep name="b">
      <preproc:sym-ref name="c" attr3="baz" />
    </preproc:sym-dep>

    <preproc:sym-dep name="c">
      <preproc:sym-ref name="a" />
    </preproc:sym-dep>

    <!-- disconnected -->
    <preproc:sym-dep name="d">
      <preproc:sym-ref name="e" />
    </preproc:sym-dep>

    <preproc:sym-dep name="e" />
  </preproc:sym-deps>
</variable>


<!-- result of merging the above two -->
<variable name="foo:graph-vtwo-vthree" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="a">
      <preproc:sym-ref name="b" attr1="foo" attr2="bar" />
      <preproc:sym-ref name="c" />
      <preproc:sym-ref name="a" attr1="foo" />
    </preproc:sym-dep>

    <preproc:sym-dep name="b">
      <preproc:sym-ref name="c" attr3="baz" />
    </preproc:sym-dep>

    <preproc:sym-dep name="c">
      <preproc:sym-ref name="a" />
    </preproc:sym-dep>

    <!-- disconnected -->
    <preproc:sym-dep name="d">
      <preproc:sym-ref name="e" />
    </preproc:sym-dep>

    <preproc:sym-dep name="e" />
  </preproc:sym-deps>
</variable>


<variable name="foo:graph-with-dupes" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="dup">
      <preproc:sym-ref name="a" />
    </preproc:sym-dep>

    <preproc:sym-dep name="dup">
      <preproc:sym-ref name="b" />
      <preproc:sym-ref name="b" />
      <preproc:sym-ref name="b" />
    </preproc:sym-dep>
  </preproc:sym-deps>
</variable>


<variable name="foo:graph-deduped" as="element( preproc:sym-deps )">
  <preproc:sym-deps>
    <preproc:sym-dep name="dup">
      <preproc:sym-ref name="a" />
      <preproc:sym-ref name="b" />
    </preproc:sym-dep>
  </preproc:sym-deps>
</variable>


<variable name="foo:expected-lookup" as="element()">
  <preproc:sym-ref name="foo" lookup="ok" />
</variable>


<variable name="foo:sym-list" as="element()+">
  <preproc:sym name="foo" />
  <preproc:sym name="bar" />
  <preproc:sym name="baz" />
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


<function name="foo:lookup-sym">
  <param name="yield"  as="element()" />
  <param name="symbol" as="element( preproc:sym )" />

  <preproc:sym-dep name="{$symbol/@name}">
    <sequence select="$yield" />
  </preproc:sym-dep>
</function>

</stylesheet>
