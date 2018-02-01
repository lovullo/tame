<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Validates a document for correctness in a manner that is beyond XSD

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

  Schematron is not used for this due to certain complexieis. Furthermore, we
  already have the data in a package structure that is easy to use and query
  against.

  Validations that can be expressed in the XSD will not be included here, unless
  there is significant overlap that would make the XSD representation
  pointlessly incomplete.

  FIXME: Needs aggresive refactoring after introduction of symbol table, for
  both performance and maintinance.
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvv="http://www.lovullo.com/rater/validate"
  xmlns:sym="http://www.lovullo.com/rater/symbol-map"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<xsl:import href="validate/domain.xsl" />

<xsl:include href="validate/param.xsl" />


<xsl:param name="prohibit-validation" select="'false'" />


<!-- FOR PERFORMANCE: constants may be used for large tables of data -->
<xsl:template match="lv:const" mode="lvv:validate" priority="9">
  <!-- nothing to be done; constants are merely data declarations -->
</xsl:template>


<!--
  Perform validations on a given rater/package

  Validations will be returned as an RTF (result tree fragment), which can be
  converted to a NodeSet using exsl:node-set(). All errors are returned in the
  following format:

  <lvv:error desc="Description of error type>Error details</lvv:error>

  @return error RTF
-->
<xsl:template match="lv:package" mode="lvv:validate" priority="9">
  <xsl:param name="symbol-map" />

  <xsl:choose>
    <xsl:when test="$prohibit-validation = 'true'">
      <xsl:message>
        <xsl:text>[validate] prohibited; skipping </xsl:text>
        <xsl:value-of select="local-name()" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>...</xsl:text>
      </xsl:message>
    </xsl:when>

    <xsl:otherwise>
      <xsl:message>
        <xsl:text>[validate] validating </xsl:text>
        <xsl:value-of select="local-name()" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>...</xsl:text>
      </xsl:message>

      <!-- validate -->
      <xsl:apply-templates mode="lvv:validate" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="preproc:*" mode="lvv:validate" priority="9">
  <!-- well that would just be silly -->
</xsl:template>


<xsl:template match="lv:package[ not( @program='true' ) ]/lv:yield" mode="lvv:validate" priority="5">
  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'lv:yield cannot appear within a non-program package'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content" select="." />
  </xsl:call-template>
</xsl:template>


<xsl:template match="*" mode="lvv:validate" priority="1">
  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<xsl:template match="lv:template" mode="lvv:validate" priority="9">
  <!-- do not validate templates; we'll only validate expansions -->
</xsl:template>


<xsl:template name="lvv:symbol-chk">
  <xsl:param name="root" />
  <xsl:param name="symbol-map" />

  <!-- build a symbol list of all used and default symbols -->
  <xsl:variable name="symlist">
    <!-- @sym attributes -->
    <xsl:for-each select="$root//lv:*[@sym]">
      <lvv:sym value="{@sym}" />
    </xsl:for-each>

    <!-- defaults from mapping document -->
    <xsl:for-each select="$symbol-map/sym:symbol[ not( ./* ) ]">
      <lvv:sym value="{.}" />
    </xsl:for-each>
  </xsl:variable>

  <!-- error for each restricted node -->
  <xsl:for-each select="
      $root//lv:*[
        @sym = $symbol-map/sym:reserved/sym:reserve/@sym
      ]
    ">

    <xsl:variable name="symbol" select="@sym" />

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Symbol is reserved and cannot be used'" />
      <xsl:with-param name="refnode" select="$root//lv:*[@sym=$symbol]" />
      <xsl:with-param name="content" select="$symbol" />
    </xsl:call-template>
  </xsl:for-each>

  <!-- error for each duplicate node -->
  <xsl:for-each select="
      $symlist/*[
        @value = following-sibling::*/@value
      ]
    ">

    <xsl:variable name="symbol" select="@value" />

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Symbol is not unique'" />
      <xsl:with-param name="refnode" select="$root//lv:*[@sym=$symbol]" />
      <xsl:with-param name="content" select="$symbol" />
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<xsl:template match="c:apply[@name]" mode="lvv:validate" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="self" select="." />
  <xsl:variable name="fsym" select="
      root(.)/preproc:symtable/preproc:sym[
        @type='func'
        and @name=$name
      ]
    " />

  <!-- ensure that a function is being applied -->
  <xsl:if test="not( $fsym )">
    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Applying non-function'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content" select="@name" />
    </xsl:call-template>
  </xsl:if>

  <!-- check that all required arguments are provided -->
  <xsl:for-each select="
      $fsym/preproc:sym-ref[
        concat( ':', $name, ':', @name ) = ancestor::preproc:symtable/preproc:sym[
          @type='lparam'
          and not( @default )
        ]
      ]
    ">

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Missing required argument'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:value-of select="@name" />
        <xsl:text> for application of </xsl:text>
        <xsl:value-of select="$name" />
        <xsl:text>()</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>

  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<xsl:template match="c:*[ @index ]/c:index" mode="lvv:validate" priority="9">
  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Ambiguous index specification'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content" select="../@name" />
  </xsl:call-template>

  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<!--
  Validate that match @on's exist
-->
<xsl:template match="lv:classify[ @as ]//lv:match" mode="lvv:validate" priority="9">
  <xsl:if test="not( @on=root(.)/preproc:symtable/preproc:sym[ @type ]/@name )">
    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Unknown match @on'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:text>`</xsl:text>
        <xsl:value-of select="@on" />
        <xsl:text>' is unknown for classification </xsl:text>

        <xsl:variable name="class"
                      select="ancestor::lv:classify" />
        <xsl:value-of select="if ( $class/@preproc:generated-from ) then
                                $class/@preproc:generated-from
                              else
                                $class/@as" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:apply-templates select="." mode="lvv:validate-match" />
</xsl:template>

<!--
  Validate that non-numeric value matches actually exist and are constants
-->
<xsl:template match="lv:match[@value]" mode="lvv:validate-match" priority="5">
  <xsl:if test="
      not( number( @value ) = @value )
      and not(
        @value=root(.)/preproc:symtable/preproc:sym[
          @type='const'
        ]/@name
      )
    ">

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Unknown match value'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:text>`</xsl:text>
        <xsl:value-of select="@value" />
        <xsl:text>' is unknown for classification </xsl:text>

        <xsl:variable name="class"
                      select="ancestor::lv:classify" />
        <xsl:value-of select="if ( $class/@preproc:generated-from ) then
                                $class/@preproc:generated-from
                              else
                                $class/@as" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:apply-templates mode="lvv:validate-match" />
</xsl:template>

<xsl:template match="lv:match" mode="lvv:validate-match" priority="2">
  <xsl:apply-templates mode="lvv:validate-match" />
</xsl:template>

<!--
  Classification match assumptions must operate only on other classifiers and
  must assume values that the referenced classifier actually matches on
-->
<xsl:template match="lv:match/lv:assuming" mode="lvv:validate-match" priority="5">
  <xsl:variable name="on" select="../@on" />
  <xsl:variable name="ref" select="root(.)//lv:classify[ @yields=$on ]" />

  <!-- assumptions must only operate on variables mentioned in the referenced
       classification -->
  <xsl:for-each select="
      .//lv:that[
        not( @name=$ref//lv:match/@on )
      ]
    ">

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Invalid classification assumption'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:value-of select="@name" />
        <xsl:text> is not used to classify </xsl:text>
        <xsl:value-of select="$on" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>

<xsl:template match="c:*" mode="lvv:validate-match" priority="2">
  <xsl:apply-templates select="." mode="lvv:validate" />
</xsl:template>

<xsl:template match="*" mode="lvv:validate-match" priority="1">
  <!-- do nothing -->
</xsl:template>


<xsl:template match="c:value" mode="lvv:validate" priority="5">
  <!-- do nothing; just prevent the below validation from occurring -->
  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>

<xsl:template match="c:let" mode="lvv:validate" priority="5">
  <!-- do not validate this node itself -->
  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>

<xsl:template match="c:*[@name or @of]" mode="lvv:validate" priority="2">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="@of">
        <xsl:value-of select="@of" />
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="@name" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- XXX: have to maintain this list! -->
  <xsl:variable name="nodes" select="
      root(.)//lv:*[
        @name=$name
        or @yields=$name
        or @as=$name
      ]
      , root(.)//c:*[
        @generates=$name
      ]
      , root(.)//c:values/c:value[ @name=$name ]
    " />

  <!-- locate function params/let vars -->
  <xsl:variable name="fname" select="
      ancestor::lv:function[
        lv:param[
          @name=$name
        ]
      ]/@name
      |ancestor::c:apply[
        c:arg[
          @name=$name
        ]
      ]/@name
      |ancestor::c:let[
        c:values/c:value[
          @name=$name
        ]
      ]/@name
    " />

  <!-- if this name references a function parameter, then it takes
       precedence (note that this consequently means that it masks any other
       names that may be globally defined) -->
  <xsl:variable name="sym" select="
    if ( $fname ) then
      root(.)/preproc:symtable/preproc:sym[
        @name=concat( ':', $fname, ':', $name )
      ]
    else
      root(.)/preproc:symtable/preproc:sym[
        @name=$name
      ]
    " />

  <xsl:variable name="type" select="$sym/@dtype" />

  <!-- all calculations must make use of numeric types -->
  <xsl:if test="
      not(
        ( $type = 'integer' )
        or ( $type = 'float' )
        or ( $type = 'boolean' )
        or ( ancestor::c:*[ @of and @index=$name ] )
      )
    ">

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Non-numeric type in calculation'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:value-of select="$name" />
        <xsl:text> is </xsl:text>

        <xsl:choose>
          <xsl:when test="not( $type ) or $type = ''">
            <xsl:text>undefined</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:text>of type '</xsl:text>
              <xsl:value-of select="$type" />
            <xsl:text>'</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:variable name="is-set" select="$sym/@dim" />

  <xsl:choose>
      <!-- furthermore, if @of is provided, then it must be a set -->
    <xsl:when test="@of">
      <xsl:if test="$is-set = 0">
        <xsl:call-template name="lvv:error">
          <xsl:with-param name="desc" select="'@of must reference a set'" />
          <xsl:with-param name="refnode" select="." />
          <xsl:with-param name="content" select="$name" />
        </xsl:call-template>
      </xsl:if>
    </xsl:when>

    <!-- otherwise, an index is required to reference an item in the set unless
         the value is being passed as an argument of the same set type, or is
         the return value of a function (we assume it to be a return value if it
         is in a tail position) -->
    <!-- TODO: re-add argument param check for sets -->
    <!-- TODO: c:values/c:value/@set check; should also only be allowed in tail -->
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="
            ( ( not( @index ) or ( @index = '' ) ) and not( ./c:index ) )
            and not( ancestor::lv:match )
            and (
              $is-set != '0'
              and not(
                ancestor::c:arg
                or (
                  ancestor::lv:function
                  and not( ./* )
                )
                or parent::c:length-of
                or ancestor::c:value[ @set ]
              )
            )
            and not( parent::c:length-of )
          ">

          <xsl:choose>
            <xsl:when test="$sym/@dim = '?'">
              <xsl:message>
                <xsl:text>internal warning: unresolved param </xsl:text>
                <xsl:text>dimension: `</xsl:text>
                <xsl:value-of select="@name" />
                <xsl:text>'</xsl:text>
              </xsl:message>
            </xsl:when>

            <xsl:otherwise>
              <xsl:call-template name="lvv:error">
                <xsl:with-param name="desc">
                  <xsl:text>Unexpected vector/matrix reference</xsl:text>
                </xsl:with-param>
                <xsl:with-param name="refnode" select="." />
                <xsl:with-param name="content">
                  <xsl:value-of select="@name" />
                  <xsl:text> (did you forget @index?)</xsl:text>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>

        <!-- TODO: generalize this! -->
        <!-- Matrix references require two indexes, unless referenced within
             contexts -->
        <xsl:when test="
            ( number( $is-set ) gt 1 )
            and not( ./c:index[ $is-set ] )
            and not( ancestor::c:arg )
            and not( ancestor::c:let )
            and not( ancestor::c:product[ @dot ] )
            and not( ancestor::c:length-of )
            and not( ancestor::c:cons )
            and not( ancestor::c:cons )
            and not(
              ancestor::lv:function
              and not( ./* )
            )
          ">
          <xsl:call-template name="lvv:error">
            <xsl:with-param name="desc" select="'Invalid matrix specification'" />
            <xsl:with-param name="refnode" select="." />
            <xsl:with-param name="content">
              <xsl:value-of select="@name" />
              <xsl:text> requires </xsl:text>
                <xsl:value-of select="$is-set" />
              <xsl:text> indexes (use c:index) in this context</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>

        <!-- ensure that we do not have too many indexes -->
        <xsl:when test="( number( $is-set ) gt 0 ) and ./c:index[ number( $is-set ) + 1 ]">
          <xsl:call-template name="lvv:error">
            <xsl:with-param name="desc" select="'Invalid vector/matrix specification'" />
            <xsl:with-param name="refnode" select="." />
            <xsl:with-param name="content">
              <xsl:value-of select="@name" />
              <xsl:text> may only have </xsl:text>
                <xsl:value-of select="$is-set" />
              <xsl:text> index(s)</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>

        <!-- if we have an index, but we're not dealing with a set, then that is
             also an issue -->
        <xsl:when test="@index and ( $is-set = '' )">
          <xsl:call-template name="lvv:error">
            <xsl:with-param name="desc" select="'Using index with non-set'" />
            <xsl:with-param name="refnode" select="." />
            <xsl:with-param name="content" select="@name" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>

  <!-- index references should be defined -->
  <xsl:if test="
      @index and not( local-name() = 'sum' or local-name() = 'product' )
    ">

    <xsl:variable name="index" select="@index" />

    <!-- search for index definition -->
    <!-- XXX: This also requires knowledge of match and require-param -->
    <xsl:if test="
        not(
          ancestor::c:*[ @index = $index ]
          or ( root(.)//lv:*[ @name = $index ]
            and (
              local-name() != 'match'
              and local-name() != 'require-param'
            )
          )
        )
      ">
      <xsl:call-template name="lvv:error">
        <xsl:with-param name="desc" select="'Undefined index'" />
        <xsl:with-param name="refnode" select="." />
        <xsl:with-param name="content" select="@index" />
      </xsl:call-template>
    </xsl:if>
  </xsl:if>

  <!-- recursively validate any nested calculations -->
  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<xsl:template match="c:apply/c:arg[@name]" mode="lvv:validate" priority="5">
  <!-- merely validate its existence -->
  <xsl:variable name="fname" select="parent::c:apply/@name" />
  <xsl:if test="not(
      concat( ':', $fname, ':', @name ) = root(.)/preproc:symtable/preproc:sym[
        @type='lparam'
        and @parent=$fname
      ]/@name
    )">

    <xsl:call-template name="lvv:error">
      <xsl:with-param name="desc" select="'Unknown argument'" />
      <xsl:with-param name="refnode" select="." />
      <xsl:with-param name="content">
        <xsl:text>Argument `</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>' is unknown for function `</xsl:text>
          <xsl:value-of select="$fname" />
        <xsl:text>'</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <!-- recursively validate any nested calculations -->
  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<xsl:template match="c:product[@dot]" mode="lvv:validate" priority="5">
  <!-- TODO -->
</xsl:template>


<xsl:template mode="lvv:validate" priority="2" match="
    lv:union/lv:typedef[
      ./lv:*[1]/@type != preceding-sibling::lv:typedef[1]/lv:*[1]/@type
    ]
  ">

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Union type mismatch'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>Expected type '</xsl:text>
      <xsl:value-of select="preceding-sibling::lv:typedef[1]/lv:*[1]/@type" />
      <xsl:text>' for </xsl:text>
      <xsl:value-of select="@name" />
      <xsl:text>, but found '</xsl:text>
      <xsl:value-of select="./lv:*[1]/@type" />
      <xsl:text>'</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<!--
  Checks for use of undefined classifications
-->
<xsl:template mode="lvv:validate" priority="2"
  match="lv:rate/lv:class[
    not( concat( ':class:', @ref ) = root(.)/preproc:symtable/preproc:sym[ @type='class' ]/@name )
  ]">

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Unknown classification'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>unknown classification '</xsl:text>
        <xsl:value-of select="@ref" />
      <xsl:text>' referenced by </xsl:text>
      <xsl:value-of select="ancestor::lv:rate/@yields" />
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<!--
  All rate blocks must have non-empty yields

  This is an awkward error, because it's not possible to identify the
  rate block by name...there is none.
-->
<xsl:template mode="lvv:validate" priority="9"
              match="lv:rate[ not( @yields ) or @yields = '' ]">
  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Unidentifiable rate block'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>missing or empty @yields; see document dump</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<!--
  lv:rate blocks have no use for @generates.  Since XSDs don't work within
  templates, let's validate that independently.  This is particularly
  important for developers unfamiliar with the distinction between lv:rate
  and lv:rate-each.
-->
<xsl:template mode="lvv:validate" priority="7"
              match="lv:rate[ @generates ]">
  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'lv:rate/@generate'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content"
                    select="concat( '`', @yields, ''': lv:rate does ',
                                    'not support @generates' )" />
  </xsl:call-template>
</xsl:template>


<!--
  Rate block cannot be nested.
-->
<xsl:template mode="lvv:validate" priority="8"
              match="lv:rate[ ancestor::lv:rate ]">
  <xsl:variable name="within" as="element( lv:rate )"
                select="ancestor::lv:rate[1]" />

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Nested rate block'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content"
                    select="concat( '`', @yields, ''' cannot be nested ',
                                    'within `', $within/@yields, '''' )" />
  </xsl:call-template>
</xsl:template>


<!--
  Throws an error if a generator is requested using unsupported data

  Specifically, a generator is intended to generate a set from an expression
  while looping over another set. If we're not looping, then we're not
  generating a set. Furthermore, if a child expression was not provided, then
  the set produced would be equivalent to @of, which is useless.
-->
<xsl:template mode="lvv:validate"
  match="c:*[ @generates and not( @of and ./c:* ) ]" priority="9">

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'Invalid generator'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>Cannot create generator '</xsl:text>
        <xsl:value-of select="@generates" />
      <xsl:text>'; generating expressions must contain both @of </xsl:text>
      <xsl:text>and a sub-expression.</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<!--
  Since @generates creates a new variable that can be referenced, it needs
  documentation! Refuse to compile if documentation is not provided. Yeah, we're
  assholes.
-->
<xsl:template mode="lvv:validate"
  match="c:*[ @generates and not( @desc ) ]" priority="9">

  <xsl:call-template name="lvv:error">
    <xsl:with-param name="desc" select="'No generator description'" />
    <xsl:with-param name="refnode" select="." />
    <xsl:with-param name="content">
      <xsl:text>@desc required when creating generator </xsl:text>
      <xsl:value-of select="@generates" />
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates mode="lvv:validate" />
</xsl:template>


<xsl:template match="ext:*" mode="lvv:get-path">
  <!-- omit from path output -->
</xsl:template>

<xsl:template match="*" mode="lvv:get-path">
  <xsl:apply-templates select="parent::*" mode="lvv:get-path" />
  <xsl:text>/</xsl:text>
  <xsl:value-of select="name()" />

  <!-- certain nodes may support path descriptions to aid in determining which
       node is being referenced -->
  <xsl:variable name="desc">
    <xsl:apply-templates select="." mode="lvv:get-path-desc" />
  </xsl:variable>

  <xsl:if test="$desc != ''">
    <xsl:text>[</xsl:text>
      <xsl:value-of select="$desc" />
    <xsl:text>]</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="lv:rate[ @yields ]" mode="lvv:get-path-desc">
  <xsl:text>@yields=</xsl:text>
  <xsl:value-of select="@yields" />
</xsl:template>

<xsl:template match="c:*[ @name ]" mode="lvv:get-path-desc" priority="5">
  <xsl:text>@name=</xsl:text>
  <xsl:value-of select="@name" />
</xsl:template>
<xsl:template match="c:*[ @label ]" mode="lvv:get-path-desc" priority="1">
  <xsl:text>@label=</xsl:text>
  <xsl:value-of select="@label" />
</xsl:template>

<xsl:template match="*" mode="lvv:get-path-desc">
  <!-- no desc by default -->
</xsl:template>


<xsl:template name="lvv:error">
  <xsl:param name="desc" />
  <xsl:param name="refnode" />
  <xsl:param name="content" />

  <xsl:variable name="path">
    <xsl:if test="$refnode">
      <xsl:apply-templates select="$refnode" mode="lvv:get-path" />
    </xsl:if>
  </xsl:variable>

  <lvv:error desc="{$desc}" path="{$path}">
    <xsl:value-of select="$content" />
  </lvv:error>
</xsl:template>

</xsl:stylesheet>
