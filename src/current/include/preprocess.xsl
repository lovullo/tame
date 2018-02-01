<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Entry point for preprocessor

  Copyright (C) 2016 R-T Specialty, LLC.

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

  This progress is aggressive; the resulting tree will follow the structure of
  the original XML, but will be heavily augmented and some parts rewritten.
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:util="http://www.lovullo.com/util">


<xsl:include href="depgen.xsl" />
<xsl:include href="preproc/package.xsl" />


<!--
  Raters themselves get special treatment
-->
<xsl:template match="lv:rater" mode="preproc:compile" priority="9">
  <xsl:param name="orig-root" select="." />
  <xsl:param name="stopshort" />

  <xsl:message>
    <xsl:text>[preproc] [rater]</xsl:text>
  </xsl:message>

  <!-- handle package preprocessing -->
  <xsl:variable name="pkg-result">
    <xsl:call-template name="preproc:pkg-compile">
      <xsl:with-param name="orig-root" select="$orig-root" />
      <xsl:with-param name="stopshort" select="$stopshort" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:copy-of select="$pkg-result" />
</xsl:template>

</xsl:stylesheet>
