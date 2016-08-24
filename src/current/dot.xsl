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

  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:dot="http://www.lovullo.com/calc/dot">

<xsl:import href="dot/depout.xsl" />
<xsl:import href="dot/defnode.xsl" />

<!-- supported sources (entry points) -->
<xsl:include href="dot/pkg-obj.xsl" />
<xsl:include href="dot/pkg-exec.xsl" />

<xsl:output method="text" />


<!--
  Newline character
-->
<xsl:variable name="dot:nl" select="'&#10;'" />


<!--
  Immediately fails on unrecognized source type
-->
<xsl:template match="lv:package" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>[dot] fatal: this is not an object/executable file: </xsl:text>
    <xsl:text>no symbol dependencies found</xsl:text>
  </xsl:message>
</xsl:template>


<!--
  Beginning of a DOT document
-->
<xsl:template match="lv:package" mode="dot:head">
  <xsl:text>/* dependency graph of </xsl:text>
    <xsl:value-of select="@name" />
  <xsl:text> */</xsl:text>

  <xsl:text>digraph "</xsl:text>
    <xsl:value-of select="@name" />
    <xsl:text>" { </xsl:text>

    <xsl:text>graph [rankdir="LR", ranksep="2"]; </xsl:text>
</xsl:template>


<!--
  End of a DOT document
-->
<xsl:template match="lv:package" mode="dot:tail">
  <xsl:text>}</xsl:text>
</xsl:template>


</xsl:stylesheet>

