<?xml version="1.0"?>
<!--
  Tests dynamic function reference

  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

    This file is part of TAME.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-->

<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:eseq="http://www.lovullo.com/tame/preproc/expand/eseq"
            xmlns:foo="http://www.lovullo.com/_junk">


  <import href="../../../src/preproc/expand/expand-sequence.xsl" />


  <variable name="foo:empty-eseq" as="element( foo:empty )">
    <foo:empty />
  </variable>

  <variable name="foo:a" as="element( foo:seq )">
    <foo:seq foo="bar">
      <foo:node head="true">
        <foo:head />
      </foo:node>
      <foo:node tail="true" />
    </foo:seq>
  </variable>


  <variable name="foo:expanded" as="element( foo:seq )">
    <foo:seq foo="baz">
      <sequence select="eseq:expand-node( $foo:a/foo:node[1] ),
                        eseq:expand-node( $foo:a/foo:node[2] )" />
    </foo:seq>
  </variable>

  <!-- used to assert indirectly that our own @code{is-expanded}
       predicate is being used -->
  <variable name="foo:unexpandable" as="element( foo:seq )">
    <foo:seq>
      <foo:node no-expand="true" />
    </foo:seq>
  </variable>


  <function name="eseq:is-expandable" as="xs:boolean"
            override="yes">
    <param name="node" as="node()" />

    <sequence select="$node instance of element()
                      and not( $node/@foo:expanded )" />
  </function>


  <function name="eseq:expand-node" as="node()*"
            override="yes">
    <param name="node" as="node()" />

    <choose>
      <when test="$node/@no-expand">
        <sequence select="$node" />
      </when>

      <otherwise>
        <apply-templates mode="foo:expand-node"
                         select="$node" />
      </otherwise>
    </choose>
  </function>


  <template mode="foo:expand-node"
            match="element()">
    <copy>
      <attribute name="foo:expanded"
                 select="'true'" />

      <sequence select="@*|node()" />
    </copy>
  </template>
</stylesheet>
