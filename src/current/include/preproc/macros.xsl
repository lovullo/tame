<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Handles macro preprocessing

  Copyright (C) 2016, 2017 LoVullo Associates, Inc.

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
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:ext="http://www.lovullo.com/ext">


<xsl:include href="template.xsl" />
<xsl:include href="eligclass.xsl" />


<!--
  Perform a macro expansion pass

  This will continue to recurse until no preproc:repass nodes are found; this
  allos macros to expand into macros for further processing.
-->
<xsl:template match="*" mode="preproc:macropass" priority="1"
              as="node()+">
  <xsl:variable name="result" as="node()+">
    <xsl:apply-templates select="." mode="preproc:macros" />
  </xsl:variable>

  <xsl:variable name="nodeset" select="$result" />

  <xsl:variable name="repass"
                select="$nodeset//preproc:repass" />

  <!-- halt if we are in error -->
  <xsl:for-each select="$nodeset//preproc:error">
    <xsl:message terminate="yes">
      <xsl:text>!!! [preproc] error: </xsl:text>
      <xsl:value-of select="." />
    </xsl:message>
  </xsl:for-each>

  <xsl:choose>
    <!-- if it was indicated that we must do so, recurse -->
    <xsl:when test="$repass and not( $repass[ @need-sym ] )">

      <!-- record the repass to keep a count -->
      <!-- TODO: reintroduce
      <preproc:repass-record />
      -->

      <xsl:message>[preproc] *REPASS*</xsl:message>

      <!-- perform the repass -->
      <xsl:apply-templates select="$nodeset" mode="preproc:macropass">
        <xsl:with-param name="clear-tpl-step"
                        tunnel="yes"
                        select="false()" />
      </xsl:apply-templates>
    </xsl:when>

    <!-- no more passes needed; strip any cruft and we're done -->
    <xsl:otherwise>
      <xsl:apply-templates mode="preproc:strip-tpl-cruft"
                           select="$nodeset" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="*" mode="preproc:macros" priority="1">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:apply-templates mode="preproc:macros" />
  </xsl:copy>
</xsl:template>


<!--
  Remove repass nodes left over from the previous pass

  Otherwise, we would recurse indefinately.
-->
<xsl:template match="preproc:repass" mode="preproc:macros" priority="5">
  <!-- remove; no longer needed -->
</xsl:template>


<xsl:template match="preproc:tpl-step" mode="preproc:macros" priority="5">
  <xsl:param name="clear-tpl-step"
             tunnel="yes"
             select="true()" />

  <xsl:choose>
    <xsl:when test="$clear-tpl-step">
      <!-- strip -->
    </xsl:when>

    <xsl:otherwise>
      <xsl:copy-of select="." />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  lv:rater is just a special type of package
-->
<xsl:template match="lv:rater" mode="preproc:macros" priority="9">
  <lv:package program="true">
    <xsl:sequence select="@*, *" />
  </lv:package>

  <preproc:repass src="lv:rater" />
</xsl:template>



<!--
  FOR PERFORMANCE ONLY:

  These nodes (usually) contain nothing that can be processed on the macro pass,
  so recursion is unnecessary; note the low priority.
-->
<xsl:template match="lv:typedef"
              mode="preproc:macros" priority="2">

  <xsl:sequence select="." />
</xsl:template>


<!--
  Expand values beginning with `#' into constants

  It is a nuisance to have separate params (e.g. templates) for constants
  and values.
-->
<xsl:template mode="preproc:macros"
              match="c:value-of[ starts-with( @name, '#' ) ]"
              priority="7">
  <c:const value="{substring-after( @name, '#' )}"
           type="float"
           desc="Generated short-hand constant" />
</xsl:template>


<!--
  Expand index values beginning with `#' into constants

  It is a nuisance to have separate params (e.g. templates) for constants
  and values.
-->
<xsl:template mode="preproc:macros"
              match="c:value-of[ starts-with( @index, '#' ) ]"
              priority="7">
  <xsl:copy>
    <xsl:copy-of select="@*[ not( name() = 'index' ) ]" />

    <c:index>
      <c:const value="{substring-after( @index, '#' )}"
               type="float"
               desc="Generated short-hand constant" />
    </c:index>
  </xsl:copy>
</xsl:template>


<!--
  It does not make sense to try to take an index of a scalar
-->
<xsl:template mode="preproc:macros"
              match="c:value-of[
                       @index
                       and starts-with( @name, '#' ) ]"
              priority="9">
  <preproc:error>
    <xsl:text>Cannot take index of scalar value: </xsl:text>
    <xsl:value-of select="@name" />
  </preproc:error>
</xsl:template>


<!--
  Classifications containing only an lv:any child node can be converted into
  existential classifications
-->
<xsl:template match="lv:classify[ lv:any and count(*) = 1 ]" mode="preproc:macros" priority="8">
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:attribute name="any" select="'true'" />

    <xsl:sequence select="lv:any/*" />
  </xsl:copy>

  <preproc:repass src="lv:classify any" />
</xsl:template>


<xsl:template match="lv:classify[ .//lv:any|.//lv:all ]" mode="preproc:macros" priority="6">
  <xsl:variable name="result">
    <xsl:apply-templates select="." mode="preproc:class-groupgen" />
  </xsl:variable>

  <xsl:apply-templates select="$result/lv:classify" mode="preproc:class-extract" />

  <preproc:repass src="lv:classify any|all" />
</xsl:template>


<xsl:template match="lv:classify" mode="preproc:class-groupgen" priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:apply-templates mode="preproc:class-groupgen" />
  </xsl:copy>
</xsl:template>


<xsl:template mode="preproc:class-groupgen" priority="9"
    match="lv:any[ not( element() ) ]
           |lv:all[ not( element() ) ]">
  <!-- useless; remove -->
</xsl:template>


<xsl:template match="lv:any|lv:all" mode="preproc:class-groupgen" priority="5">
  <!-- this needs to be unique enough that there is unlikely to be a conflict
       between generated ids in various packages; generate-id is not enough for
       cross-package guarantees (indeed, I did witness conflicts), so there is
       a random seed passed into the stylesheet externally -->
  <xsl:variable name="id" select="concat( $__rseed, generate-id(.) )" />

  <xsl:variable name="parent-name" select="ancestor::lv:classify/@as" />
  <xsl:variable name="yields" select="concat( 'is', $id )" />

  <xsl:variable name="external" as="xs:string?"
                select="ancestor::lv:classify/@external" />

  <!-- this will be raised outside of the parent classification during
       post-processing -->
  <lv:classify as="{$id}" yields="{$yields}"
               preproc:generated="true"
               preproc:generated-from="{$parent-name}"
               external="{$external}"
               desc="(generated from predicate group of {$parent-name}">
    <xsl:if test="local-name() = 'any'">
      <xsl:attribute name="any" select="'true'" />
    </xsl:if>

    <xsl:apply-templates mode="preproc:class-groupgen" />
  </lv:classify>

  <!-- this will remain in its place -->
  <lv:match on="{$yields}" value="TRUE" preproc:generated="true" />
</xsl:template>


<!-- retain everything else -->
<xsl:template match="*" mode="preproc:class-groupgen" priority="1">
  <xsl:sequence select="." />
</xsl:template>


<xsl:template match="lv:classify" mode="preproc:class-extract" priority="5">
  <xsl:apply-templates select="lv:classify" mode="preproc:class-extract" />

  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:apply-templates mode="preproc:class-filter" />
  </xsl:copy>
</xsl:template>


<xsl:template match="*" mode="preproc:class-extract" priority="1">
  <!-- ignore non-class -->
</xsl:template>


<xsl:template match="lv:classify" mode="preproc:class-filter" priority="5">
  <!-- remove -->
</xsl:template>


<xsl:template match="*" mode="preproc:class-filter" priority="1">
  <xsl:sequence select="." />
</xsl:template>


<!--
  Sections exist purely for organization and documentation.  Move all
  nodes out of it, so that we do not complicate parsing.
-->
<xsl:template mode="preproc:macros" priority="2"
              match="lv:section">
  <xsl:apply-templates select="*" mode="preproc:macros" />
</xsl:template>


<!--
  lv:yield is simply another rate block with a special name that is recognized
  by the linker
-->
<xsl:template match="lv:yield" mode="preproc:macros" priority="5">
  <lv:rate yields="___yield" local="true">
    <xsl:apply-templates mode="preproc:macros" />
  </lv:rate>
</xsl:template>


<!-- this situation may occur both manually and from lv:rate-each-template -->
<xsl:template match="lv:rate-each[ lv:apply-template ]" mode="preproc:macros" priority="9">
  <xsl:variable name="apply">
    <preproc:apply>
      <xsl:apply-templates select="lv:apply-template" mode="preproc:macros" />
    </preproc:apply>
  </xsl:variable>

  <xsl:choose>
    <!-- did the template apply? (note that we only check for a single one,
         since that's all that we should have) -->
    <xsl:when test="$apply/preproc:apply/lv:apply-template">
      <xsl:sequence select="." />

      <xsl:message>
        <xsl:text>[preproc] waiting to expand rate-each </xsl:text>
        <xsl:value-of select="@yields" />
        <xsl:text> (immediate template(s) need expansion)...</xsl:text>
      </xsl:message>
    </xsl:when>

    <xsl:otherwise>
      <!-- it applied! -->
      <xsl:copy>
        <xsl:sequence select="@*, *[ not( local-name()='apply-template' ) ]" />
        <xsl:sequence select="$apply/preproc:apply/*" />
      </xsl:copy>

      <!-- we'll process this block next time around -->
      <preproc:repass src="lv:rate-each lv:apply-template" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Convenience macro that expands to a lv:rate block summing over the magic
  _CMATCH_ set with the product of its value

  The intent here is to reduce highly repetitive code.
-->
<xsl:template match="lv:rate-each" mode="preproc:macros" priority="5">
  <!-- TODO: debug flag
  <xsl:message>
    <xsl:text>[preproc] expanding rate-each </xsl:text>
    <xsl:value-of select="@yields" />
    <xsl:text>...</xsl:text>
  </xsl:message>
  -->

  <lv:rate>
    <xsl:sequence select="@*[
        not( local-name() = 'index' )
        and not( local-name() = 'generates' )
      ]" />

    <xsl:if test="not( @yields )">
      <!-- if @generates is not supplied either, then we cannot continue -->
      <xsl:choose>
        <xsl:when test="not( @generates )">
          <!-- TODO: some means of identifying this...the error isn't terribly
               helpful... :x -->
          <preproc:error>
            <xsl:text>rate-each must provide either @yields or @generates</xsl:text>
          </preproc:error>
        </xsl:when>

        <xsl:otherwise>
          <xsl:attribute name="yields"
                         select="concat( '_', @generates )" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>

    <xsl:sequence select="./lv:class" />

    <c:sum of="_CMATCH_" index="{@index}" sym="{@gensym}">
      <xsl:if test="@dim">
        <xsl:copy-of select="@dim" />
      </xsl:if>

      <!-- copy @generates, if it exists (has the benefit of copying nothing
           if it does not exist) -->
      <xsl:sequence select="@generates" />

      <xsl:attribute name="desc">
        <xsl:text>Set of individual </xsl:text>
        <xsl:value-of select="@yields" />
        <xsl:text> premiums</xsl:text>
      </xsl:attribute>

      <c:product>
        <c:value-of name="_CMATCH_" index="{@index}">
          <xsl:attribute name="label">
            <xsl:text>Zero if not </xsl:text>
              <xsl:value-of select="@class" />
            <xsl:text>, otherwise one</xsl:text>
          </xsl:attribute>
        </c:value-of>

        <xsl:apply-templates
          select="*[
              not(
                local-name() = 'class'
              )
            ]"
            mode="preproc:macros" />
      </c:product>
    </c:sum>
  </lv:rate>
</xsl:template>

</xsl:stylesheet>
