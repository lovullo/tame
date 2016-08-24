<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Output package dependencies in plain text

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

  Intended for parsing by external (e.g. shell) scripts.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvp="http://www.lovullo.com">

<xsl:output method="text" />

<xsl:variable name="nl" select="'&#10;'" />


<xsl:template match="lv:rater|lv:package">
  <!-- output deps, one per line -->
  <xsl:for-each select="lv:import[ @package ]">
    <xsl:value-of select="concat( @package, $nl )" />
  </xsl:for-each>
</xsl:template>

<xsl:template match="lvp:program">
  <!-- output deps, one per line -->
  <xsl:for-each select="lvp:import[ @package ]">
    <xsl:value-of select="concat( @package, $nl )" />
  </xsl:for-each>
</xsl:template>


<xsl:template match="lvm:program-map|lvm:return-map">
  <!-- output deps, one per line -->
  <xsl:for-each select="lvm:import[ @path ]">
    <xsl:value-of select="concat( @path, $nl )" />
  </xsl:for-each>
</xsl:template>


<xsl:template match="w:worksheet">
  <xsl:value-of select="concat( @package, $nl )" />
</xsl:template>


<xsl:template match="*">
  <!-- do nothing -->
</xsl:template>

</xsl:stylesheet>
