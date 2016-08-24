<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Outputs graph visualization of dependencies in DOT format

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

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<xsl:import href="./attr-color.xsl" />
<xsl:import href="./attr-shape.xsl" />
<xsl:import href="./attr-extern.xsl" />
<xsl:import href="./attr-keep.xsl" />


<!--
  External nodes should be styled as such
-->
<xsl:template mode="dot:defnode-attr" priority="5" match="
    preproc:sym[ @src and not( @src='' ) ]
  ">

  <dot:attr name="label">
    <xsl:value-of select="@src" />
    <xsl:text>/\n</xsl:text>
    <xsl:value-of select="@name" />
  </dot:attr>

  <dot:attr name="tooltip">
    <xsl:value-of select="@src" />
    <xsl:text>/</xsl:text>
    <xsl:value-of select="@name" />
  </dot:attr>
</xsl:template>



<!--
  Default node attributes
-->
<xsl:template match="preproc:sym" mode="dot:defnode-attr" priority="1">
</xsl:template>


<!--
  Render an attribute list as a comma-delimited string

  Expects a tree of dot:attr nodes where @name is the name of the attribute, and
  its normalized text is the value. The value will be quoted; double quotes must
  be manually escaped prior to calling this template.
-->
<xsl:template name="dot:render-attr-list">
  <xsl:param name="attr-list" />

  <xsl:for-each select="$attr-list/dot:attr">
    <xsl:if test="position() > 1">
      <xsl:text>, </xsl:text>
    </xsl:if>

    <xsl:value-of select="@name" />
    <xsl:text>="</xsl:text>
      <xsl:value-of select="normalize-space( text() )" />
    <xsl:text>"</xsl:text>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
