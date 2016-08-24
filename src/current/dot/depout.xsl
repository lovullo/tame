<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Outputs dependency relationship to a directed graph

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

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">



<!--
  Container for all the dependencies
-->
<xsl:template match="preproc:sym-deps" mode="dot:depout" priority="5">
  <xsl:apply-templates mode="dot:depout" />
</xsl:template>


<!--
  Treat the entry point (lv:yield) as the root node
-->
<xsl:template match="preproc:sym-dep[ @name='___yield' ]" mode="dot:depout" priority="6">
  <xsl:value-of select="@name" />
  <xsl:text> [root=ctr,fontsize=24,style=bold,label="Yield"]; </xsl:text>

  <xsl:apply-templates mode="dot:depout" />
</xsl:template>


<!--
  Constant and generated symbols will not be rendered
-->
<xsl:template mode="dot:depout" priority="5" match="
    preproc:sym-dep[
      @name=ancestor::lv:package/preproc:symtable/preproc:sym[
        @type='const'
        or @preproc:generated='true'
      ]/@name
    ]
  ">
</xsl:template>


<!--
  Container for symbol dependencies

  That is: this node represents a symbol, and its children are its dependencies.
-->
<xsl:template match="preproc:sym-dep" mode="dot:depout" priority="3">
  <xsl:apply-templates mode="dot:depout" />
</xsl:template>


<!--
  Do not output relationships to primitives or constants; or generated
-->
<xsl:template mode="dot:depout" priority="5" match="
    preproc:sym-ref[
      @type='type'
      and (
        @name='integer'
        or @name='float'
        or @name='boolean'
      )
      or @type='const'
      or @type='lparam'
      or @type='map' or @type='retmap'
      or @type='map:head' or @type='map:tail'
      or @type='retmap:head' or @type='retmap:tail'
    ]
   ">

  <!-- skip -->
</xsl:template>


<!--
  Process generated symbol deps as our own

  Generated symbols are not known by the user, so they should be treated as part
  of the unit from which they are generated.
-->
<xsl:template mode="dot:depout" priority="4" match="
    preproc:sym-ref[
      @name=ancestor::lv:package/preproc:symtable/preproc:sym[
        @preproc:generated='true'
      ]/@name
    ]
  ">

  <xsl:param name="usedby" select="parent::preproc:sym-dep/@name" />

  <xsl:variable name="name" select="@name" />

  <!-- process the generated symbol's deps as our own -->
  <xsl:apply-templates mode="dot:depout" select="
      ancestor::preproc:sym-deps/preproc:sym-dep[
        @name=$name
      ]/preproc:sym-ref
    ">
    <xsl:with-param name="usedby" select="$usedby" />
  </xsl:apply-templates>
</xsl:template>


<!--
  Process references with parents as if they were their parent symbol

  Symbols with defined parents are generated as part of that parent and will
  therefore be treated as a single unit.
-->
<xsl:template match="preproc:sym-ref[ @parent ]" mode="dot:depout" priority="3">
  <xsl:param name="usedby" select="parent::preproc:sym-dep/@name" />

  <xsl:variable name="parent" select="@parent" />

  <xsl:apply-templates mode="dot:depout" select="
      ancestor::lv:package/preproc:symtable/preproc:sym[ @name=$parent ]
    ">
    <xsl:with-param name="usedby" select="$usedby" />
  </xsl:apply-templates>
</xsl:template>


<!--
  Trigger processing of symbol associated with the ref
-->
<xsl:template match="preproc:sym-ref" mode="dot:depout" priority="2">
  <xsl:param name="usedby" select="parent::preproc:sym-dep/@name" />

  <xsl:variable name="name" select="@name" />

  <xsl:apply-templates mode="dot:depout" select="
      ancestor::lv:package/preproc:symtable/preproc:sym[ @name=$name ]
    ">
    <xsl:with-param name="usedby" select="$usedby" />
  </xsl:apply-templates>
</xsl:template>


<!--
  Output symbol reference to directed graph

  The symbol (dependency) is referenced as a node adjacent to the node of the
  symbol that uses it. The edge is directed toward the dependency and shall be
  read as "uses".

  For example: "foo uses bar":
     (foo) -> (bar)
-->
<xsl:template match="preproc:sym" mode="dot:depout" priority="5">
  <xsl:param name="usedby" />

  <xsl:variable name="attr">
    <xsl:call-template name="dot:render-attr-list">
      <xsl:with-param name="attr-list">
        <xsl:apply-templates select="." mode="dot:attr-extern" />
        <xsl:apply-templates select="." mode="dot:attr-color" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:variable>

  <xsl:text>"</xsl:text>
    <xsl:value-of select="$usedby" />
  <xsl:text disable-output-escaping="yes">" -&gt; "</xsl:text>
    <xsl:value-of select="@name" />
  <xsl:text>"</xsl:text>

    <xsl:if test="not( $attr='' )">
      <xsl:text> [</xsl:text>
        <xsl:value-of select="$attr" />
      <xsl:text>]</xsl:text>
    </xsl:if>

  <xsl:text>;</xsl:text>

  <xsl:value-of select="$dot:nl" />
</xsl:template>


<!--
  Bail out if asked to render something unexpected
-->
<xsl:template match="*" mode="dot:depout" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>error: what do I do!?: unexpected </xsl:text>
    <xsl:value-of select="name()" />
  </xsl:message>
</xsl:template>


<!--
  Extra comments and attributes are ignored

  text() is ignored, otherwise, extra whitespace corresponding to the
  indentation of nodes appears in the output.

  Ignoring attributes is just in case of an xpath whoopsie, but probably isn't
  necessary, and is probably dangerous (because it may veil bugs).
-->
<xsl:template match="@*|text()" mode="dot:depout" priority="1">
  <!-- ignore -->
</xsl:template>


</xsl:stylesheet>

