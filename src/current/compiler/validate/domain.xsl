<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Domain validations

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

  TODO: For core domains, validate src package path as well. (Right now,
  param types are polluting, and so this is not a problem.)
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvv="http://www.lovullo.com/rater/validate"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Assert that VALUE falls within the provided domain

  SYM-DOMAIN should be a symbol resolving to the domain definition.
-->
<xsl:template name="lvv:domain-check">
  <xsl:param name="value" />
  <xsl:param name="sym-domain" />

  <xsl:if test="not( $sym-domain )">
    <xsl:message terminate="yes">
      <xsl:text>internal error: no domain symbol provided; </xsl:text>
      <xsl:text>caller: </xsl:text>
      <xsl:copy-of select="." />
    </xsl:message>
  </xsl:if>

  <!-- generate node to simplify xpath expressions -->
  <xsl:variable name="sym-validate">
    <lvv:chk value="{$value}">
      <xsl:copy-of select="$sym-domain" />
    </lvv:chk>
  </xsl:variable>

  <xsl:apply-templates mode="lvv:domain-check"
    select="$sym-validate/lvv:chk">

    <xsl:with-param name="self-pkg" select="
      $sym-domain/ancestor::lv:package" />
  </xsl:apply-templates>
</xsl:template>


<!--
  Core type checks

  - Integers must simply match when rounded;
  - Floats must be any type of number; and
  - Booleans may be only 1 or 0.
-->
<xsl:template match="
  lvv:chk[
    preproc:sym/@type = 'type'
    and (
      (
        preproc:sym/@name = 'integer'
        and not(
          @value = floor( @value )
        )
      )
      or (
        preproc:sym/@name = 'float'
        and not(
          @value = number( @value )
        )
      )
      or (
        preproc:sym/@name = 'boolean'
        and not(
          number( @value ) = 0
          or number( @value ) = 1
        )
      )
    )
  ]"
  mode="lvv:domain-check" priority="5">

  <xsl:call-template name="lvv:domain-fail" />
</xsl:template>


<!--
  Domain assertions on user-defined types
-->
<xsl:template match="
    lvv:chk[
      preproc:sym/@type='type'
      and not (
        preproc:sym/@name = 'integer'
        or preproc:sym/@name = 'float'
        or preproc:sym/@name = 'boolean'
      )
    ]
  "
  mode="lvv:domain-check" priority="5">

  <xsl:param name="self-pkg" />

  <xsl:variable name="chkval" select="@value" />

  <xsl:variable name="domain">
    <xsl:call-template name="lvv:get-domain-by-sym">
      <xsl:with-param name="sym" select="preproc:sym" />
      <xsl:with-param name="self-pkg" select="$self-pkg" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$domain/lv:domain/lv:element[ @value = $chkval ]">
      <lvv:ok type="domain-check" />
    </xsl:when>

    <xsl:otherwise>
      <xsl:call-template name="lvv:domain-fail" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  No validation failure
-->
<xsl:template match="lvv:chk"
  mode="lvv:domain-check" priority="2">

  <lvv:ok type="domain-check" />
</xsl:template>


<!--
  We passed ourselves something unexpected
-->
<xsl:template match="*"
  mode="lvv:domain-chk" priority="1">

  <xsl:message terminate="yes">
    <xsl:text>internal error: unexpected node for lvv:domain-chk: </xsl:text>
    <xsl:copy-of select="." />
  </xsl:message>
</xsl:template>


<!--
  Mark validation as a failure, outputting the assertion

  TODO: Once domains are used as the primary source instead of typedefs,
  check to ensure that the symbol is an actual domain symbol.
-->
<xsl:template name="lvv:domain-fail">
  <lvv:fail type="domain-check">
    <xsl:copy-of select="." />
  </lvv:fail>
</xsl:template>


<xsl:template name="lvv:get-domain-by-sym">
  <xsl:param name="sym" />
  <xsl:param name="self-pkg" select="ancestor::lv:package" />

  <!-- package containing symbol -->
  <xsl:variable name="pkg" select="
      if ( $sym/@src and not( $sym/@src='' ) ) then
        document( concat( $sym/@src, '.xmlo' ), $__entry-root )
          /lv:package
      else
        $self-pkg
    " />

  <!-- attempt to locate domain of the given name -->
  <xsl:variable name="domain" select="
    $pkg/lv:domain[ @name = $sym/@name ]" />

  <xsl:if test="not( $domain )">
    <xsl:message terminate="yes">
      <xsl:text>error: no domain found for </xsl:text>
      <xsl:value-of select="$sym/@src" />
      <xsl:text>/</xsl:text>
      <xsl:value-of select="$sym/@name" />
    </xsl:message>
  </xsl:if>

  <xsl:copy-of select="$domain" />
</xsl:template>

</xsl:stylesheet>

