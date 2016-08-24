<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Parameter validations
-->

<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvv="http://www.lovullo.com/rater/validate"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Param type must be known

  TODO: Doesn't the symbol table lookup process handle this?
-->
<xsl:template match="
  lv:param[
    not(
      @type=root(.)/preproc:symtable/preproc:sym[
        @type
      ]/@name
    )
  ]"
  mode="lvv:validate" priority="5">

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Unknown param type'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>'</xsl:text>
      <xsl:value-of select="@type" />
      <xsl:text>' is undefined for param </xsl:text>
      <xsl:value-of select="@name" />
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<!--
  Default must be within the domain of the param

  Note that this template priority is less than the template that checks to
  ensure that the param type exists in the first place.
-->
<xsl:template match="lv:param[ @default ]"
  mode="lvv:validate" priority="4">

  <xsl:variable name="type" select="@type" />

  <!-- default must be within its domain -->
  <xsl:variable name="result">
    <xsl:call-template name="lvv:domain-check">
      <xsl:with-param name="value" select="@default" />
      <xsl:with-param name="sym-domain" select="
        root(.)/preproc:symtable/preproc:sym[
          @name = $type
        ]" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="not( $result/lvv:ok )">
    <xsl:variable name="fail" select="$result/lvv:fail/lvv:chk" />

    <!-- if we didn't succeed, but we didn't fail, then we did something we
         weren't supposed to -->
    <xsl:if test="not( $fail )">
      <xsl:message terminate="yes">
        <xsl:text>internal error: in limbo processing param `</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>' @default</xsl:text>
      </xsl:message>
    </xsl:if>

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'param @default domain violation'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:text>param `</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>' @default of `</xsl:text>
          <xsl:value-of select="$fail/@value" />
        <xsl:text>' is not within its domain of </xsl:text>
        <xsl:value-of select="$fail/preproc:sym/@src" />
        <xsl:text>/</xsl:text>
        <xsl:value-of select="$fail/preproc:sym/@name" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
</xsl:template>


<!--
  Fallback for no validation issues
-->
<xsl:template match="lv:param" mode="lvv:validate" priority="2">
</xsl:template>

</xsl:stylesheet>

