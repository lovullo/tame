<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Package eligibility class generation

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

  Here, the term "eligibility" means whether the package is eligible to be used
  in a result set basead on the values of its params within their respective
  domains and other factors such as the results of terminating classifications
  and the eligibility of imported packages.

  The goal of the eligibility classification is to create a cascading failure in
  the event of bad data.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc">



<!--
  Trigger eligibility class generation
-->
<xsl:template match="lv:package[ not( @preproc:elig-class-yields ) ]"
              as="element( lv:package )"
              priority="5"
              mode="preproc:expand-elig-class">
  <xsl:param name="orig-root" as="element( lv:package )" />

  <xsl:variable name="elig-class" as="element( lv:classify )">
    <xsl:apply-templates select="." mode="preproc:gen-elig-class">
      <xsl:with-param name="orig-root" select="$orig-root" />
    </xsl:apply-templates>
  </xsl:variable>

  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="preproc:elig-class"
      select="$elig-class/@as" />

    <xsl:attribute name="preproc:elig-class-yields"
      select="$elig-class/@yields" />

    <xsl:sequence select="$elig-class" />
    <xsl:apply-templates mode="preproc:macros" />
  </xsl:copy>
</xsl:template>


<xsl:template match="lv:package" as="element( lv:package )"
              priority="1"
  mode="preproc:expand-elig-class">

  <!-- already processed -->
  <xsl:sequence select="." />
</xsl:template>



<!--
  Generate eligibility classification asserting all data integrity aspects of a
  package

  The eligibility classification will yield a scalar.
-->
<xsl:template match="lv:package" as="element( lv:classify )"
              mode="preproc:gen-elig-class">
  <xsl:param name="orig-root" as="element( lv:package )" />

  <xsl:message>[preproc/eligclass] generating eligibility class</xsl:message>


  <!-- class-ify name -->
  <xsl:variable name="as" as="xs:string"
                select="preproc:gen-elig-class-name( @name )" />
  <xsl:variable name="yields" as="xs:string"
                select="preproc:gen-elig-class-yields( @name )" />


  <lv:classify as="{$as}" yields="{$yields}"
    desc="{@name} package is eligible">

    <!-- TODO: this should really be a compile-time value -->
    <xsl:if test="@keep-elig-class = 'true'">
      <xsl:attribute name="keep" select="'true'" />
    </xsl:if>

    <!-- each of our imported packages' elig classes must be truthful -->
    <xsl:apply-templates mode="preproc:gen-elig-class-matches"
      select="lv:import">

      <xsl:with-param name="orig-root" select="$orig-root" />
    </xsl:apply-templates>

    <!-- param values must be within their domain -->
    <!-- XXX: does not work when param is undefined due to no mapping
    <xsl:apply-templates mode="preproc:gen-elig-param-class"
      select="lv:param" />
    -->

    <!-- terminating classifications must not have matched -->
    <xsl:apply-templates mode="preproc:gen-elig-term-class"
      select="preproc:symtable/preproc:sym[ @type='class' ]" />
  </lv:classify>
</xsl:template>


<!--
  Generate eligibility classification name for package
-->
<xsl:function name="preproc:gen-elig-class-name"
              as="xs:string">
  <xsl:param name="name" />

  <xsl:sequence select="
      concat( '--elig-',
        translate(
          translate( $name, '.', '' ),
          '/', '-'
        )
      )
      " />
</xsl:function>


<!--
  Generate eligibility result scalar name for package
-->
<xsl:function name="preproc:gen-elig-class-yields"
              as="xs:string">
  <xsl:param name="name" />

  <xsl:sequence select="
      concat(
        'isElig',
        translate(
          translate( $name, '.', '' ),
          '/-', '' ) )" />
</xsl:function>


<!--
  Generate matches on eligibility of imported packages

  For each imported package, its eligibility classification must be true.
-->
<xsl:template match="lv:import[ @package ]"
              as="element( lv:match )?"
              priority="5"
              mode="preproc:gen-elig-class-matches">
  <xsl:param name="orig-root" as="element( lv:package )" />

  <!-- FIXME: path may not yet be resolved due to preprocessing order -->
  <xsl:variable name="pkg-path" as="xs:string">
    <xsl:call-template name="__apply-relroot">
      <xsl:with-param name="path" select="@package" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="pkg" as="element( lv:package )"
                select="document( concat( $pkg-path, '.xmlo' ),
                                  $__entry-root )
                          /lv:package" />

  <xsl:if test="not( $pkg )">
    <xsl:message terminate="yes">
      <xsl:text>[preproc/eligclass] error: could not load `</xsl:text>
        <xsl:value-of select="$pkg-path" />
      <xsl:text>' object file</xsl:text>
    </xsl:message>
  </xsl:if>

  <xsl:variable name="chk" as="xs:string?"
                select="$pkg/@preproc:elig-class-yields" />

  <xsl:choose>
    <xsl:when test="not( $chk ) or ( $chk = '' )">
      <!-- TODO: make this an error once we make maps part of the
           conventional build process -->
      <xsl:message>
        <xsl:text>[preproc/eligclass] internal: empty eligibility </xsl:text>
        <xsl:text>class for `</xsl:text>
          <xsl:value-of select="$pkg/@name" />
        <xsl:text>'; skipping</xsl:text>
      </xsl:message>
    </xsl:when>

    <xsl:otherwise>
      <!-- use eligibility class as stated by the package -->
      <lv:match on="{$chk}" value="TRUE" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="lv:import" priority="1"
  mode="preproc:gen-elig-class-matches">

  <!-- do nothing -->
</xsl:template>


<!--
  Param values must be within their domain

  This is a trivial operation.
-->
<xsl:template match="lv:param"
              as="element( lv:any )"
              mode="preproc:gen-elig-param-class" priority="5">

  <lv:any>
    <lv:match on="{@name}" anyOf="{@type}" />

    <!-- TODO: defaults should always be within the domain! -->
    <xsl:if test="@default">
      <lv:match on="{@name}" anyOf="empty" />
    </xsl:if>
  </lv:any>
</xsl:template>



<!--
  Terminating classification dependencies

  All terminiating classifications defined in the package must yield false
  for the package to be eligible.

  N.B. This checks to ensure @extclass is not set; this prevents errors when
  the eligibility classification attempts to pull in a terminating
  classification marked as external to the classifier. There may or may not
  be something we want to do about this in the future.
-->
<xsl:template match="preproc:sym[
                       not( @src )
                       and not( @pollute='true' )
                       and @type='class'
                       and @terminate='true'
                       and not( @extclass='true' )
                     ]"
              as="element( lv:match )"
              priority="5"
              mode="preproc:gen-elig-term-class">

  <lv:match on="{@yields}" value="FALSE" />
</xsl:template>


<xsl:template match="preproc:sym" priority="1"
  mode="preproc:gen-elig-term-class">

  <!-- do nothing -->
</xsl:template>

</xsl:stylesheet>

