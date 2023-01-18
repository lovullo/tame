<?xml version="1.0" encoding="utf-8"?>
<!--
  Transforms values

  Copyright (C) 2014-2023 Ryan Specialty, LLC.

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
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">


<xsl:template match="lvmp:var[ @transform='proper-case' ]" priority="5"
  mode="lvmp:transform">

  <xsl:param name="value" />

  <xsl:text>ucwords(strtolower(</xsl:text>
    <xsl:copy-of select="$value" />
  <xsl:text>))</xsl:text>
</xsl:template>



<xsl:template match="lvmp:var[ not( @transform ) or @transform='' ]"
    mode="lvmp:transform" priority="3">

  <xsl:param name="value" />

  <!-- no transformation; do nothing -->
  <xsl:copy-of select="$value" />
</xsl:template>


<xsl:template match="lvmp:var" mode="lvmp:transform" priority="2">
  <xsl:message terminate="yes">
    <xsl:text>error: unknown transformation `</xsl:text>
      <xsl:value-of select="@transform" />
    <xsl:text>' for `</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>'</xsl:text>
  </xsl:message>
</xsl:template>


<xsl:template match="lvmp:*" mode="lvmp:transform" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>internal error: unexpected node for transformation: </xsl:text>
    <xsl:copy-of select="." />
  </xsl:message>
</xsl:template>

</xsl:stylesheet>

