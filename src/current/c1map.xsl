<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Generates PHP code that works with the LoVullo ConceptOne import system

  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.

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

  This map expects that the data are available in the bucket provided by the
  quote and therefore validates against a provided Program UI source file. Data
  external to the bucket may be provided if it is indicated as such.

  Each map source file is independent; variables and values do not bleed into
  one-another, unless explicitly passed.
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">

<xsl:output
  indent="yes"
  omit-xml-declaration="yes"
  />

<!-- newline -->
<xsl:variable name="lvmp:nl" select="'&#10;'" />

<xsl:include href="c1map/c1nodes.xsl" />
<xsl:include href="c1map/valparse.xsl" />
<xsl:include href="c1map/render.xsl" />

<!--
  Represents the root of the source document that processing was initiated upon
-->
<xsl:variable name="orig-root" select="/" />


<!--
  The root node
-->
<xsl:template match="lvm:c1-map" priority="5">
  <!-- populated by includes, if any -->
  <xsl:param name="args" />

  <xsl:message select="$args" />

  <!-- get the name from the first C1 node -->
  <xsl:variable name="name" select="
      if ( @id ) then
        @id
      else
        concat( translate( c1:*[1]/name(), '_', '' ), 'Composer' )
    " />

  <!-- TODO: this default value maintains BC, but it ought to be specified;
       remove this in the future -->
  <xsl:variable name="namespace" as="xs:string"
                select="if ( @namespace ) then
                            @namespace
                          else
                            concat(
                              'lovullo\c1\interfaces\c1\contract\',
                              @program )" />

  <!-- deprecation warning for above -->
  <xsl:if test="not( @namespace )">
    <xsl:message select="concat(
                           'warning: missing lvm:c1-map/@namespace; defaulting to `',
                           replace( $namespace, '\\', '\\\\' ), '''' )" />
  </xsl:if>

  <!-- preprocessed result -->
  <xsl:variable name="pp-result">
    <lvmp:root program="{@program}" name="{$name}" namespace="{$namespace}">
      <!-- introduce outer scope for variables -->
      <lvmp:scope id="">
        <xsl:apply-templates />
      </lvmp:scope>
    </lvmp:root>
  </xsl:variable>

  <!-- final processing -->
  <xsl:variable name="result">
    <xsl:apply-templates select="$pp-result/lvmp:root" mode="lvmp:render" />
  </xsl:variable>

  <!-- remove escapes -->
  <xsl:value-of disable-output-escaping="yes" select="$result" />
</xsl:template>


<!--
  Include another source map file relative to the path of the original source
  file

  The attributes @name and @for-each are special and cannot be used as
  arguments to the template. The special @for-each attribute will be copied
  into each of the children of the root node in the template as @lvm:for-each.
  For example, if the template consists of

    <c1-map>
      <Product>
      </Product>
    </c1-map>

  then <lvm:include for-each="foo" /> would produce

    <c1-map>
      <Product lvm:for-each="foo">
      </Product>
    </c1-map>
-->
<xsl:template match="lvm:include" priority="5">
  <xsl:message>[c1map] +<xsl:value-of select="@name" /></xsl:message>

  <xsl:variable name="src" select="
      document( concat( @name, '.xml' ), $orig-root )/lvm:c1-map
    " />

  <xsl:if test="not( $src )">
    <xsl:message terminate="yes">fatal: c1-map node not found</xsl:message>
  </xsl:if>

  <!-- process the body of the c1-map; we don't want to process the root node,
       as that would start processing from scratch, prematurely rendering the result -->
  <lvmp:scope id="/{@name}">
    <!-- arguments are included as attributes -->
    <xsl:variable name="args" select="
        @*[ not( local-name()='name' or local-name()='for-each' ) ]
      " />
    <xsl:variable name="for-each" select="@for-each" />

    <!-- augment the XML with our own mappings -->
    <xsl:variable name="augmented">
      <lvm:c1-map>
        <xsl:copy-of select="$src/@*" />

        <xsl:for-each select="$src/lvm:param">
          <xsl:call-template name="lvmp:param-to-map">
            <xsl:with-param name="args" select="$args" />
            <xsl:with-param name="param" select="." />
            <xsl:with-param name="context" select="/lvm:c1-map" />
          </xsl:call-template>
        </xsl:for-each>


        <xsl:choose>
          <!-- if @for-each was provided, then apply to all first-level
               children of the included template -->
          <xsl:when test="$for-each">
            <!-- we will need to expose the mapping -->
            <!-- TODO: need to validate that it actually exists -->
            <lvm:external name="{$for-each}" />

            <xsl:for-each select="$src/*">
              <xsl:copy>
                <xsl:copy-of select="@*" />
                <xsl:attribute name="lvm:for-each" select="$for-each" />
                <xsl:copy-of select="*|text()" />
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>

          <!-- no @for-each; just do a quick copy of all the nodes -->
          <xsl:otherwise>
            <xsl:copy-of select="$src/*" />
          </xsl:otherwise>
        </xsl:choose>
      </lvm:c1-map>
    </xsl:variable>

    <xsl:apply-templates select="$augmented/lvm:c1-map/*" />
  </lvmp:scope>

  <xsl:message>[c1map] -<xsl:value-of select="@name" /></xsl:message>
</xsl:template>


<!--
  Processes a template param into mappings

  This will generate the mappings necessary to process the template as though
  it was hard-coded with the imported mappings.

  The {} brace syntax denotes a variable, but mixing values and inline
  variables are not supported.
-->
<xsl:template name="lvmp:param-to-map">
  <xsl:param name="args" />
  <xsl:param name="param" />
  <xsl:param name="context" />

  <xsl:variable name="name" select="$param/@name" />
  <xsl:variable name="arg" select="$args[ local-name()=$name ]" />
  <xsl:variable name="argvar" select="substring-after( $arg, '{' )" />

  <xsl:if test="$argvar and not( $argvar='' )">
    <xsl:variable name="varname" select="substring-before( $argvar, '}' )" />

    <lvmp:translate name="{$name}" to="{$varname}" />

    <xsl:variable name="predot" select="substring-before( $varname, '.' )" />

    <xsl:choose>
      <!-- no dot; output the entire thing -->
      <xsl:when test="$predot = ''">
        <lvm:external name="{$varname}" />
      </xsl:when>

      <!-- multi-level var -->
      <xsl:otherwise>
        <lvm:external name="{$predot}" dict="true" lvmp:no-validate="true" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>

  <!-- TODO: no need to do this if the above conditional matches -->
  <lvm:map to="{$name}" lvmp:allow-default="true">
    <xsl:copy-of select="$param/@dict" />
    <xsl:copy-of select="$param/@default" />

    <xsl:choose>
      <xsl:when test="$arg">
        <!-- determines if we have a variable -->

        <xsl:choose>
          <xsl:when test="$argvar and not( $argvar='' )">
            <xsl:attribute name="from"
              select="substring-before( $argvar, '}' )" />
          </xsl:when>

          <!-- static value -->
          <xsl:otherwise>
            <xsl:attribute name="value" select="$arg" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>

      <!-- required param -->
      <xsl:when test="$param/@required">
        <xsl:message terminate="yes">
          <xsl:text>error: missing required template argument `</xsl:text>
            <xsl:value-of select="$name" />
          <xsl:text>'</xsl:text>
        </xsl:message>
      </xsl:when>

      <!-- otherwise, we have no value -->
      <xsl:otherwise>
        <xsl:attribute name="value" select="''" />
      </xsl:otherwise>
    </xsl:choose>
  </lvm:map>
</xsl:template>


<!--
  Common actions performed by nearly every mapping
-->
<xsl:template name="lvmp:map-common">
  <!-- may or may not be set -->
  <xsl:copy-of select="@dict" />
  <xsl:copy-of select="@link" />
  <xsl:copy-of select="@transform" />

  <xsl:apply-templates select="@default" />
</xsl:template>


<xsl:template match="lvm:param" priority="4">
  <!-- processed above on import; no longer needed -->
</xsl:template>

<xsl:template match="lvm:map[@from]" priority="4">
  <lvmp:var name="{@to}" from="{@from}" src="map">
    <xsl:call-template name="lvmp:map-common" />
  </lvmp:var>
</xsl:template>

<xsl:template match="lvm:map[lvm:from]" priority="4">
  <lvmp:var name="{@to}" from="{lvm:from/@name}">
    <xsl:call-template name="lvmp:map-common" />
  </lvmp:var>
</xsl:template>

<xsl:template match="lvm:map[@value]" priority="4">
  <!-- it does not make sense to have a string value be a dictionary -->
  <xsl:if test="@dict">
    <xsl:message terminate="yes">
      <xsl:text>error: cannot have @dict on static mapping `</xsl:text>
        <xsl:value-of select="@to" />
      <xsl:text>'</xsl:text>
    </xsl:message>
  </xsl:if>

  <!-- nor does a default make sense -->
  <xsl:if test="@default and not( @lvmp:allow-default='true' )">
    <xsl:message terminate="yes">
      <xsl:text>error: cannot have @default on static mapping `</xsl:text>
        <xsl:value-of select="@to" />
      <xsl:text>'</xsl:text>
    </xsl:message>
  </xsl:if>

  <lvmp:var name="{@to}" value="{@value}">
    <!-- we may use defaults internally -->
    <xsl:call-template name="lvmp:map-common" />
  </lvmp:var>
</xsl:template>

<xsl:template match="lvm:pass" priority="4">
  <lvmp:var name="{@name}" from="{@name}" src="map">
    <xsl:call-template name="lvmp:map-common" />
  </lvmp:var>
</xsl:template>

<xsl:template match="lvm:pass[@name='line_code'] | lvm:map[@to='line_code']" priority="9">
  <xsl:message terminate="yes">error: "line_code" is a reserved word</xsl:message>
</xsl:template>

<xsl:template match="lvm:external" priority="4">
  <lvmp:var name="{@name}" from="{@name}" src="external">
    <xsl:call-template name="lvmp:map-common" />
  </lvmp:var>
</xsl:template>

<xsl:template match="lvm:*/@default">
  <lvmp:default>
    <xsl:apply-templates select="." mode="lvm:valparse" />
  </lvmp:default>
</xsl:template>


<xsl:template match="lvmp:translate" priority="4">
  <!-- added by pre-processor during include; ignore -->
</xsl:template>


<!--
  Override default behavior of c1 nodes when iteration is requested
-->
<xsl:template match="c1:*[ @lvm:for-each ]"
  mode="lvmp:c1-node-result" priority="5">

  <lvmp:for-each name="{@lvm:for-each}">
    <xsl:choose>
      <xsl:when test="lvm:when">
        <lvmp:condition when="true">
          <lvmp:when>
            <xsl:call-template name="lvmp:gen-val">
              <xsl:with-param name="name" select="lvm:when/@name" />
            </xsl:call-template>
          </lvmp:when>
          <lvmp:cmp>
            <xsl:apply-templates mode="lvm:valparse"
                                 select="lvm:when/@eq" />
          </lvmp:cmp>

          <xsl:apply-templates select="@*|*" />
        </lvmp:condition>
      </xsl:when>

      <xsl:otherwise>
        <xsl:apply-templates select="@*|*" />
      </xsl:otherwise>
    </xsl:choose>
  </lvmp:for-each>
</xsl:template>


<xsl:template match="lvm:if" priority="4">
  <lvmp:condition>
    <lvmp:when>
      <xsl:call-template name="lvmp:gen-val">
        <xsl:with-param name="name" select="@name" />
      </xsl:call-template>
    </lvmp:when>
    <lvmp:cmp>
      <xsl:apply-templates mode="lvm:valparse"
                           select="@eq" />
    </lvmp:cmp>

    <xsl:apply-templates />
  </lvmp:condition>
</xsl:template>


<xsl:template match="lvm:if[ @for-each | @lvm:for-each ]" priority="9">
  <xsl:message terminate="yes">error: cannot iterate on conditional</xsl:message>
</xsl:template>


<xsl:template match="lvm:when" priority="4">
  <!-- handled as part of @lvm:for-each -->
</xsl:template>


<!--
  Unhandled node character data

  Note that, if a node contains newlines, then there will be text preceding and
  following its children. For example:

    <foo>
      <bar>
    </foo>

  In the above, the `foo' node has the text "\n  ", followed by the node `bar',
  followed by the text "\n" (assuming that `foo' starts in column 1).
-->
<xsl:template match="text()" priority="1">
  <!-- do not output whitespace from source files -->
</xsl:template>


<!--
  Bail out on unhandled nodes.
-->
<xsl:template match="*" priority="1">
  <xsl:message>
    <xsl:text>[c1map] fatal: unexpected node </xsl:text>
    <xsl:apply-templates select="." mode="lvmp:node-out" />
    <xsl:text>:</xsl:text>
  </xsl:message>

  <xsl:message terminate="yes" select="." />
</xsl:template>

<xsl:template match="*" mode="lvmp:node-out">
  <xsl:variable name="parent" select="parent::*" />
  <xsl:if test="$parent">
    <xsl:apply-templates select="$parent" mode="lvmp:node-out" />
  </xsl:if>

  <xsl:text>/</xsl:text>
  <xsl:value-of select="name()" />
</xsl:template>

</xsl:stylesheet>
