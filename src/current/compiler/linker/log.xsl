<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Logging functions

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:log="http://www.lovullo.com/logger">

<xsl:template name="log:info">
  <xsl:param name="name" />
  <xsl:param name="msg" />

  <xsl:message>
    <xsl:if test="$name">
      <xsl:text>[</xsl:text>
        <xsl:value-of select="$name" />
      <xsl:text>] </xsl:text>
    </xsl:if>

    <xsl:value-of select="$msg" />
  </xsl:message>
</xsl:template>

<xsl:template name="log:debug">
  <xsl:param name="name" />
  <xsl:param name="msg" />

  <xsl:message>
    <xsl:if test="$name">
      <xsl:text>[</xsl:text>
        <xsl:value-of select="$name" />
      <xsl:text>] </xsl:text>
    </xsl:if>

    <xsl:value-of select="$msg" />
  </xsl:message>
</xsl:template>

<xsl:template name="log:warn">
  <xsl:param name="name" />
  <xsl:param name="msg" />

  <xsl:message>
    <xsl:if test="$name">
      <xsl:text>[</xsl:text>
        <xsl:value-of select="$name" />
      <xsl:text>] warning: </xsl:text>
    </xsl:if>

    <xsl:value-of select="$msg" />
  </xsl:message>
</xsl:template>

<xsl:template name="log:error">
  <xsl:param name="name" />
  <xsl:param name="msg" />
  <xsl:param name="terminate" select="'yes'" />

  <xsl:message terminate="{$terminate}">
    <xsl:if test="$msg">
      <xsl:if test="$name">
        <xsl:text>[</xsl:text>
          <xsl:value-of select="$name" />
        <xsl:text>] error: </xsl:text>
      </xsl:if>

      <xsl:value-of select="$msg" />
    </xsl:if>
  </xsl:message>
</xsl:template>

<xsl:template name="log:internal-error">
  <xsl:param name="name" />
  <xsl:param name="msg" />
  <xsl:param name="terminate" select="'yes'" />

  <xsl:message terminate="{$terminate}">
    <xsl:if test="$name">
      <xsl:text>[</xsl:text>
        <xsl:value-of select="$name" />
      <xsl:text>] internal error: </xsl:text>
    </xsl:if>

    <xsl:value-of select="$msg" />
  </xsl:message>
</xsl:template>

</xsl:stylesheet>

