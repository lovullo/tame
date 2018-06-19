<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Describes how ConceptOne nodes are handled in the output.

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

  Only nodes in the C1 XML namespace will be included in the output; all other
  nodes will be in error, except for nodes as part of the c1 map namespace,
  which are processed and will not be included in the output.

  The output is an array format used to generate the final XML at runtime; this
  format was not developed in conjunction with this project and is separate, so
  be sure that this compiler is updated if the format changes.
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">

<!--
  Nodes with attributes or children are recursively processed and have the
  form:
    '>Name' => array( <recurse> )
-->
<!--
<xsl:template match="c1:*[
                       node()
                       or ( @* and not( count( @* ) = count( @lvm:* )))]" priority="5">
<xsl:template match="c1:*[(*) or ( @* and not( count( @* ) = count( @lvm:* )))]" priority="5">
-->
<xsl:template match="c1:*[*|@*]" priority="5">
  <!-- make the output a little bit sane -->
  <xsl:value-of select="$lvmp:nl" />

  <!-- defer node rendering; allows us to easily determine if there are
       siblings of the same name within a node boundary -->
  <lvmp:node name="{name()}"/>
<!--
  <xsl:if test="not(@lvm:escape-param)">
    <lvmp:node name="{name()}"/>
  </xsl:if>
  <xsl:if test="@lvm:escape-param">
    <lvmp:node name="{name()}" escape-param="{@lvm:escape-param}"/>
  </xsl:if>
-->
  <xsl:text> => </xsl:text>

<!--
  <xsl:if test="not(@lvm:escape-param)">
      <lvmp:node-boundary>
        <xsl:apply-templates select="." mode="lvmp:c1-node-result" />
      </lvmp:node-boundary>
  </xsl:if>
  <xsl:if test="@lvm:escape-param)">
-->
      <lvmp:node-boundary escape-param="{@lvm:escape-param}">
        <xsl:apply-templates select="." mode="lvmp:c1-node-result" />
      </lvmp:node-boundary>
<!--
  </xsl:if>
-->
</xsl:template>


<!--
  The default behavior of c1 nodes is to simply output the nodes as-is, with
  variable substitutions.
-->
<xsl:template match="c1:*" mode="lvmp:c1-node-result" priority="1">
  <xsl:text>array( </xsl:text>
    <xsl:apply-templates select="@*|*" />
    <xsl:if test="text() != '' and not(.[*])">
        <xsl:text>'text()' => </xsl:text>
        <xsl:apply-templates select="text()" mode="lvm:valparse" />
        <xsl:text></xsl:text>
    </xsl:if>
  <xsl:text>) </xsl:text>
</xsl:template>


<!--
  Text-only nodes are of the form:
    '>Name' => 'value'
-->
<xsl:template match="c1:*[text()]" priority="4">
  <!-- defer node rendering; allows us to easily determine if there are
       siblings of the same name within a node boundary -->
<!--
-->
<xsl:message select="'text-only node:'"/>
<xsl:message select="."/>
  <lvmp:node name="{name()}" escape-param="{@lvm:escape-param}"/>
  <xsl:text> => </xsl:text>

  <xsl:text></xsl:text>
    <!-- TODO: escape single quotes -->
<!--
<xsl:message select="'{{{'"/>
<xsl:message>
  <xsl:apply-templates select="text()" mode="lvm:valparse"/>
</xsl:message>
<xsl:message select="'}}}'"/>
-->
    <xsl:apply-templates select="text()" mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>


<!--
  Attributes are of the format:
    '[Name]' => 'value'
-->
<xsl:template match="c1:*/@*" priority="5">
  <xsl:text>'[</xsl:text>
    <xsl:value-of select="name()" />
  <xsl:text>]' => </xsl:text>
    <xsl:apply-templates select="." mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>


<!-- alternative attribute format for special situations -->
<xsl:template match="lvm:attribute" priority="5">
  <xsl:text>'[</xsl:text>
    <xsl:value-of select="@name" />
  <xsl:text>]' => </xsl:text>
    <xsl:apply-templates select="@value" mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>



<xsl:template match="c1:*/@lvm:*" priority="6">
  <!-- discard all system attributes -->
  <xsl:message select="'\ndiscarding:'"/>
  <xsl:message select="."/>
  <!-- TODO: error once everything is properly implemented -->
<!--
  <xsl:text>'</xsl:text><xsl:value-of select="name()" />
  <xsl:text>' => '[BLORT</xsl:text>
  <xsl:apply-templates select="." mode="lvm:valparse"/>
  <xsl:text>BLORT]' </xsl:text>
-->
</xsl:template>


<!--
<xsl:template match="c1:*[text()][ @lvm:escape-param ]" priority="7">
  <lvmp:node name="{name()}" />
  <xsl:text> => </xsl:text>

  <xsl:text></xsl:text>
    <xsl:apply-templates select="text()" mode="lvm:valparse" />
  <xsl:text>, BLORT</xsl:text>

</xsl:template>
-->

</xsl:stylesheet>
