<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Renders the final PHP code

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
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">


<xsl:import href="transform.xsl" />


<xsl:template match="lvmp:root" mode="lvmp:render" priority="5">
  <xsl:text>&lt;?php </xsl:text>
  <xsl:value-of select="$lvmp:nl" />

  <!-- TODO: add program id to namespace -->
  <xsl:text>namespace lovullo\c1\interfaces\c1\contract\</xsl:text>
    <xsl:value-of select="@program" />
    <xsl:text>;</xsl:text>
  <xsl:value-of select="$lvmp:nl" />

  <xsl:text>class </xsl:text>
    <xsl:value-of select="@name" />
  <xsl:text> {</xsl:text>
  <xsl:value-of select="$lvmp:nl" />

    <xsl:text>public function compose( $contract ) {</xsl:text>
    <xsl:value-of select="$lvmp:nl" />
      <xsl:text>    return array(</xsl:text>
      <xsl:value-of select="$lvmp:nl" />
        <!-- render the preprocessed content -->
        <xsl:apply-templates mode="lvmp:render" />
      <xsl:value-of select="$lvmp:nl" />
      <xsl:text>    );</xsl:text>
      <xsl:value-of select="$lvmp:nl" />
    <xsl:text>  }</xsl:text>
    <xsl:value-of select="$lvmp:nl" />

  <xsl:text>}</xsl:text>
  <xsl:value-of select="$lvmp:nl" />
  <xsl:text>?&gt;</xsl:text>
</xsl:template>


<xsl:template match="text()" mode="lvmp:render" priority="5">
  <xsl:value-of select="." />
</xsl:template>


<xsl:template name="lvmp:value" match="lvmp:value" mode="lvmp:render" priority="5">
  <xsl:param name="name" select="@ref" />
  <xsl:param name="scope" select="ancestor::lvmp:scope[1]" />
  <xsl:param name="var" select="$scope/lvmp:var[ @name=$name ][1]" />
  <xsl:param name="from" select="$var/@from" />
  <xsl:param name="value" select="$var/@value" />
  <xsl:param name="default" select="$var/lvmp:default" />

  <!-- provide error if the variable could not be found in the current scope -->
  <xsl:call-template name="lvmp:check-var">
    <xsl:with-param name="name" select="$name" />
    <xsl:with-param name="scope" select="$scope" />
  </xsl:call-template>

  <xsl:choose>
    <!-- mapping was provided -->
    <xsl:when test="$from and not( $from='' )">
      <xsl:apply-templates select="$var" mode="lvmp:transform">
        <xsl:with-param name="value">
          <xsl:apply-templates select="." mode="lvmp:render-value">
            <xsl:with-param name="var" select="$var" />
            <xsl:with-param name="from" select="$from" />
            <xsl:with-param name="scope" select="$scope" />
            <xsl:with-param name="default" select="$default" />
          </xsl:apply-templates>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:when>

    <!-- static string mapping -->
    <xsl:when test="$value and not( $value='' )">
      <!-- TODO: escape single quote -->
      <xsl:text>'</xsl:text>
        <xsl:value-of select="$value" />
      <xsl:text>'</xsl:text>
    </xsl:when>

    <!-- if no @from was provided, then it forces the use of the default
         value -->
    <xsl:when test="$default">
      <xsl:text>$contract-&gt;checkDefaultValue( '</xsl:text>
        <xsl:value-of select="$name" />
      <xsl:text>', </xsl:text>
      <xsl:apply-templates select="$default" mode="lvmp:render" />
      <xsl:text> )</xsl:text>
    </xsl:when>

    <!-- if no default is available, then this node shall never be set -->
    <xsl:otherwise>
      <xsl:text>null</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template mode="lvmp:render" priority="7"
  match="lvmp:value[ lvmp:value ]">

  <xsl:param name="name" select="@ref" />
  <xsl:param name="scope" select="ancestor::lvmp:scope[1]" />
  <xsl:param name="var" select="$scope/lvmp:var[ @name=$name ][1]" />
  <xsl:param name="from" select="$var/@from" />
  <xsl:param name="value" select="$var/@value" />
  <xsl:param name="default" select="$var/lvmp:default" />

  <xsl:text>$contract-&gt;getValueByContext( </xsl:text>
    <!-- recursive process contexts -->
    <xsl:apply-templates select="lvmp:value" mode="lvmp:render">
      <xsl:with-param name="scope" select="$scope" />
      <xsl:with-param name="default" select="$default" />
    </xsl:apply-templates>
  <xsl:text>, '</xsl:text>
    <!-- TODO: validate existence of key in dict -->
    <xsl:value-of select="$name" />
  <xsl:text>' </xsl:text>

  <xsl:text> )</xsl:text>
</xsl:template>


<xsl:template match="lvmp:value" mode="lvmp:render-value" priority="1">
  <xsl:param name="var" />
  <xsl:param name="scope" />
  <xsl:param name="default" />
  <xsl:param name="from" />
  <!-- optional -->
  <xsl:param name="value-node" select="." />
  <xsl:param name="context" />

  <!-- the name of the index we reference for our value defaults to our own
       name, but can be overridden in the mapping -->
  <xsl:param name="index-name">
    <xsl:choose>
      <!-- link provided; override -->
      <xsl:when test="$var/@link">
        <!-- parse the link and replace it with the var it maps to -->
        <xsl:call-template name="lvmp:var-from">
          <xsl:with-param name="name" select="$var/@link" />
          <xsl:with-param name="scope" select="$scope" />
        </xsl:call-template>
      </xsl:when>

      <!-- default to our own index -->
      <xsl:otherwise>
        <xsl:value-of select="$from" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <xsl:text>$contract-&gt;getValue( '</xsl:text>
    <xsl:value-of select="$from" />
  <xsl:text>', $contract->getValueIndex( '</xsl:text>
    <xsl:value-of select="$index-name" />
  <xsl:text>' )</xsl:text>

  <xsl:if test="$default">
    <xsl:text>, </xsl:text>
    <xsl:apply-templates select="$default" mode="lvmp:render" />
  </xsl:if>

  <xsl:text> )</xsl:text>
</xsl:template>


<!--
  Generates an error in the event that the given var name cannot be found in
  the given scope.
-->
<xsl:template name="lvmp:check-var">
  <xsl:param name="name" />
  <xsl:param name="scope" select="ancestor::lvmp:scope[1]" />

  <!-- look up variable in parent scope -->
  <xsl:variable name="var" select="$scope/lvmp:var[ @name=$name ][1]" />

  <!-- provide error if the variable could not be found in the current scope -->
  <xsl:if test="not( $var )">
    <xsl:message terminate="yes">
      <xsl:text>error: variable not within scope: </xsl:text>
      <xsl:value-of select="$scope/@id" />
      <xsl:text>/</xsl:text>
      <xsl:value-of select="$name" />
    </xsl:message>
  </xsl:if>
</xsl:template>


<!--
  Validates and then returns the source mapping of the given variable within
  the given (or current) scope
-->
<xsl:template name="lvmp:var-from">
  <xsl:param name="name" />
  <xsl:param name="scope" select="ancestor::lvmp:scope[1]" />

  <!-- undefined/scoping check -->
  <xsl:call-template name="lvmp:check-var">
    <xsl:with-param name="name" select="$name" />
    <xsl:with-param name="scope" select="$scope" />
  </xsl:call-template>

  <xsl:value-of select="$scope/lvmp:var[ @name=$name ][1]/@from" />
</xsl:template>


<xsl:template match="lvmp:translate" mode="lvmp:render" priority="5">
  <!-- no longer needed -->
</xsl:template>

<xsl:template match="lvmp:var" mode="lvmp:render" priority="5">
  <!-- no longer needed -->
</xsl:template>

<xsl:template match="lvmp:var/lvmp:default" mode="lvmp:render" priority="5">
  <!-- render contents -->
  <xsl:apply-templates mode="lvmp:render" />
</xsl:template>


<!--
  Scope boundary; no longer needed in output
-->
<xsl:template match="lvmp:scope" mode="lvmp:render" priority="2">
  <xsl:apply-templates mode="lvmp:render" />
</xsl:template>


<!--
  Conditional boundary; not needed in output
-->
<xsl:template match="lvmp:condition" mode="lvmp:render" priority="2">
  <xsl:apply-templates mode="lvmp:render" />
</xsl:template>

<xsl:template mode="lvmp:render" priority="2"
              match="lvmp:condition/lvmp:when
                     |lvmp:condition/lvmp:cmp">
  <!-- will be processed as part of sibling output -->
</xsl:template>


<!--
  Iteration boundary
-->
<xsl:template match="lvmp:for-each" mode="lvmp:render" priority="5">
  <xsl:variable name="from">
    <xsl:call-template name="lvmp:var-from">
      <xsl:with-param name="name" select="@name" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:text>$contract->iterateValues( '</xsl:text>
      <xsl:value-of select="$from" />
    <xsl:text>', </xsl:text>
    <xsl:text>function( $contract ) {</xsl:text>
      <xsl:text>  return array(</xsl:text>
      <xsl:apply-templates mode="lvmp:render" />
      <xsl:text>);</xsl:text>
    <xsl:text>}</xsl:text>
  <xsl:text>)</xsl:text>
</xsl:template>


<xsl:template match="lvmp:for-each[ lvmp:condition[ @when ] ]" mode="lvmp:render" priority="8">
  <xsl:variable name="from">
    <xsl:call-template name="lvmp:var-from">
      <xsl:with-param name="name" select="@name" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:text>$contract->iterateValues( '</xsl:text>
  <xsl:value-of select="$from" />
  <xsl:text>', </xsl:text>
  <xsl:text>function( $contract ) {</xsl:text>
  <xsl:text>  return </xsl:text>
  <xsl:apply-templates mode="lvmp:render" select="lvmp:condition" />
  <xsl:text>;</xsl:text>
  <xsl:text>}</xsl:text>
  <xsl:text>)</xsl:text>
</xsl:template>


<!--
  The C1 import system recognizes the following formats:

    '>Node'
    'N>Node'

  where N is any number; the latter is used for creating unique indexes for
  multiple siblings of the same name.
-->
<xsl:template match="lvmp:node" mode="lvmp:render" priority="5">
  <xsl:variable name="name" select="@name" />

  <xsl:text>'</xsl:text>

  <!-- generate a unique key to avoid conflicts -->
  <xsl:value-of select="generate-id(.)" />

  <xsl:text>&gt;</xsl:text>
  <xsl:value-of select="@name" />

  <xsl:text>'</xsl:text>
</xsl:template>


<!--
  Node boundary; not needed in output

  The boundary dictates the context of a conditional. This template has a lower
  priority and will be the default if there is no parent conditional.
-->
<xsl:template match="lvmp:node-boundary" mode="lvmp:render" priority="2">
  <xsl:apply-templates mode="lvmp:render" />

  <!-- unconditional delimiter; nothing funky going on -->
  <xsl:text>, </xsl:text>
</xsl:template>

<xsl:template mode="lvmp:render" priority="5" match="
  lvmp:node-boundary[
    parent::lvmp:condition
    or parent::lvmp:scope/parent::lvmp:condition
  ]
">
  <!-- select the nearest condition -->
  <xsl:variable name="cond" as="element( lvmp:condition )"
                select="ancestor::lvmp:condition[1]" />

  <xsl:text>( ( </xsl:text>
    <xsl:text>$contract->isTruthy( </xsl:text>
      <xsl:apply-templates select="$cond/lvmp:when/lvmp:*" mode="lvmp:render" />
      <xsl:if test="$cond/lvmp:cmp/text() != ''">
        <xsl:text>,</xsl:text>
        <xsl:apply-templates select="$cond/lvmp:cmp/text()" mode="lvmp:render" />
      </xsl:if>
    <xsl:text>)</xsl:text>
  <xsl:text> ) ? </xsl:text>
    <xsl:apply-templates mode="lvmp:render">
      <xsl:with-param name="no-trailing-sep" select="true()" />
    </xsl:apply-templates>
  <xsl:text> : null ), </xsl:text>
</xsl:template>



<xsl:template mode="lvmp:render" priority="8" match="lvmp:condition[ @when ]">
  <xsl:variable name="cond" select="." />

  <xsl:text>( ( </xsl:text>
  <xsl:text>$contract->isTruthy( </xsl:text>
  <xsl:apply-templates select="$cond/lvmp:when/lvmp:*" mode="lvmp:render" />
  <xsl:if test="$cond/lvmp:cmp/*">
    <xsl:text>,</xsl:text>
    <xsl:apply-templates select="$cond/lvmp:cmp/lvmp:*" mode="lvmp:render" />
  </xsl:if>
  <xsl:text>)</xsl:text>
  <xsl:text> ) ? array(</xsl:text>
  <xsl:apply-templates mode="lvmp:render">
    <xsl:with-param name="no-trailing-sep" select="true()" />
  </xsl:apply-templates>
  <xsl:text>) : null )</xsl:text>
</xsl:template>


<xsl:template match="*" mode="lvmp:render" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>[c1map] fatal: unexpected node during render: </xsl:text>
    <xsl:apply-templates select="." mode="lvmp:node-out" />
  </xsl:message>
</xsl:template>

</xsl:stylesheet>
