<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Entry point for compilation

  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

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
  for each task that requires such output.

  Also performs validation.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:util="http://www.lovullo.com/util"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:compiler="http://www.lovullo.com/rater/compiler"
  xmlns:calc-compiler="http://www.lovullo.com/calc/compiler"
  xmlns:ext="http://www.lovullo.com/ext">

<xsl:output
  indent="yes"
  omit-xml-declaration="yes"
  />

<!-- processing utilities -->
<xsl:include href="include/util.xsl" />

<!-- compiler -> JS -->
<xsl:include href="include/preprocess.xsl" />
<xsl:include href="compiler/validate.xsl" />
<xsl:include href="compiler/js.xsl" />
<xsl:include href="compiler/map.xsl" />

<!-- TODO: move into compiler/ -->
<xsl:include href="c1map.xsl" />

<!-- contains get-symbol-map -->
<xsl:include href="include/display.xsl" />

<!-- allows disabling of time-consuming validation -->
<xsl:param name="preproc-cache-validate" select="'true'" />


<!--
  Simply copy the preprocessor output; the caller is responsible for outputting
  this to a document.
-->
<xsl:template match="/lv:rater|/lv:package" priority="5">
  <!-- XXX: Duplicate code; see summary -->
  <xsl:variable name="processed">
    <xsl:apply-templates select="." mode="preproc:compile" />
  </xsl:variable>

  <!-- fail on preprocessor errors -->
  <xsl:call-template name="preproc:err-chk">
    <xsl:with-param name="processed" select="$processed" />
  </xsl:call-template>

  <!-- validation must have passed; output the nodes -->
  <xsl:copy-of select="$processed" />
</xsl:template>


<xsl:template name="preproc:err-chk">
  <xsl:param name="processed" />

  <xsl:for-each select="$processed//preproc:error">
    <xsl:message terminate="yes">
      <xsl:text>!!! preprocessor error: </xsl:text>
      <xsl:value-of select="." />
    </xsl:message>
  </xsl:for-each>
</xsl:template>


<xsl:template match="*" mode="preproc:handle-errors" priority="1">
  <!-- do nothing -->
</xsl:template>


<xsl:template match="lvm:program-map|lvm:return-map" priority="5">
  <xsl:apply-templates select="." mode="lvmc:compile" />
</xsl:template>


<xsl:template match="w:worksheet" priority="5">
  <xsl:apply-templates select="." mode="w:compile" />
</xsl:template>


<!-- any unhandled nodes should be an error -->
<xsl:template match="*" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>fatal: source file is invalid: unexpected node </xsl:text>
    <xsl:value-of select="name()" />
  </xsl:message>
</xsl:template>

</xsl:stylesheet>
