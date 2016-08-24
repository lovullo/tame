<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<xsl:template match="lv:package" mode="preproc:compile-fragments">
  <xsl:copy>
    <xsl:sequence select="@*, *" />

    <!-- compile each fragment in the symbol table -->
    <preproc:fragments>
      <xsl:for-each select="preproc:symtable/preproc:sym">
        <xsl:variable name="result">
          <xsl:apply-templates select="." mode="preproc:compile-fragments" />
        </xsl:variable>

        <xsl:if test="$result != ''">
          <preproc:fragment id="{@name}">
            <xsl:value-of select="$result" />
          </preproc:fragment>
        </xsl:if>
      </xsl:for-each>
    </preproc:fragments>
  </xsl:copy>
</xsl:template>


<xsl:template match="preproc:sym[ @src ]" mode="preproc:compile-fragments" priority="9">
  <!-- do not compile external symbols -->
</xsl:template>

<xsl:template match="preproc:sym" mode="preproc:compile-fragments" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>[jsc] fatal: unknown symbol type for `</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>': </xsl:text>
    <xsl:value-of select="@type" />
  </xsl:message>
</xsl:template>


<xsl:template match="preproc:sym[ @type='rate' ]" mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <!-- could be one of two places -->
  <xsl:apply-templates mode="compile" select="
    $pkg/lv:rate[ @yields=$name ]
    , $pkg/lv:rate-group/lv:rate[ @yields=$name ]
   " />
</xsl:template>
<xsl:template match="preproc:sym[ @type='gen' ]" mode="preproc:compile-fragments" priority="5">
  <!-- compiled by above -->
</xsl:template>

<xsl:template match="preproc:sym[ @type='class' ]" mode="preproc:compile-fragments" priority="5">
  <!-- name is prefixed with :class: -->
  <xsl:variable name="as" select="substring-after( @name, ':class:' )" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:apply-templates select="$pkg/lv:classify[ @as=$as ]" mode="compile" />
</xsl:template>
<xsl:template match="preproc:sym[ @type='cgen' ]" mode="preproc:compile-fragments" priority="5">
  <!-- compiled by above -->
</xsl:template>

<xsl:template match="preproc:sym[ @type='func' ]" mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:apply-templates select="$pkg/lv:function[ @name=$name ]" mode="compile" />
</xsl:template>

<xsl:template match="preproc:sym[ @type='param' ]" mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:apply-templates select="$pkg/lv:param[ @name=$name ]" mode="compile" />
</xsl:template>

<xsl:template match="preproc:sym[ @type='type' ]" mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <!-- a typedef can stand on its own or exist within another typedef -->
  <xsl:apply-templates mode="compile" select="
      $pkg/lv:typedef[ @name=$name ]
      , $pkg//lv:typedef//lv:typedef[ @name=$name ]
    " />
</xsl:template>

<xsl:template match="preproc:sym[ @type='const' ]" mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:apply-templates select="$pkg/lv:const[ @name=$name ]" mode="compile" />
</xsl:template>

<xsl:template match="preproc:sym[ @type='tpl' ]" mode="preproc:compile-fragments" priority="5">
  <!-- templates are for the preprocessor only -->
</xsl:template>

<xsl:template match="preproc:sym[ @type='lparam' ]" mode="preproc:compile-fragments" priority="5">
  <!-- they're local and therefore compiled as part of the containing block -->
</xsl:template>

<xsl:template match="preproc:sym[ @type='meta' ]"
              mode="preproc:compile-fragments" priority="5">
  <xsl:variable name="name" select="substring-after( @name, ':meta:' )" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:variable name="node" as="element( lv:prop )"
                select="$pkg/lv:meta/lv:prop[ @name=$name ]" />
  <xsl:apply-templates mode="compile"
                       select="$node" />
</xsl:template>

</xsl:stylesheet>
