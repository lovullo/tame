<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Validates a document for correctness in a manner that is beyond XSD

  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

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
<stylesheet version="1.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:map="http://www.w3.org/2005/xpath-functions/map"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:ext="http://www.lovullo.com/ext"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:lvv="http://www.lovullo.com/rater/validate"
            xmlns:sym="http://www.lovullo.com/rater/symbol-map"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<import href="validate/domain.xsl" />

<include href="validate/param.xsl" />


<param name="prohibit-validation" select="'false'" />


<!-- FOR PERFORMANCE: constants may be used for large tables of data -->
<template match="lv:const" mode="lvv:validate" priority="9">
  <!-- nothing to be done; constants are merely data declarations -->
</template>


<!--
  Perform validations on a given rater/package

  Validations will be returned as an RTF (result tree fragment), which can be
  converted to a NodeSet using enode-set(). All errors are returned in the
  following format:

  <lvv:error desc="Description of error type>Error details</lvv:error>

  @return error RTF
-->
<template match="lv:package" mode="lvv:validate" priority="9">
  <param name="symbol-map" />

  <variable name="symtable-map" as="map( xs:string, element( preproc:sym ) )"
            select="map:merge(
                      for $sym in preproc:symtable/preproc:sym
                        return map{ string( $sym/@name ) : $sym } )" />

  <choose>
    <when test="$prohibit-validation = 'true'">
      <message>
        <text>[validate] prohibited; skipping </text>
        <value-of select="local-name()" />
        <text> </text>
        <value-of select="@name" />
        <text>...</text>
      </message>
    </when>

    <otherwise>
      <message>
        <text>[validate] validating </text>
        <value-of select="local-name()" />
        <text> </text>
        <value-of select="@name" />
        <text>...</text>
      </message>

      <!-- validate -->
      <apply-templates mode="lvv:validate">
        <with-param name="symtable-map" select="$symtable-map"
                    tunnel="yes" />
      </apply-templates>
    </otherwise>
  </choose>
</template>


<template match="preproc:*" mode="lvv:validate" priority="9">
  <!-- well that would just be silly -->
</template>


<template match="lv:package[ not( @program='true' ) ]/lv:yield" mode="lvv:validate" priority="5">
  <call-template name="lvv:error">
    <with-param name="desc" select="'lv:yield cannot appear within a non-program package'" />
    <with-param name="refnode" select="." />
    <with-param name="content" select="." />
  </call-template>
</template>


<template match="*" mode="lvv:validate" priority="1">
  <apply-templates mode="lvv:validate" />
</template>


<template match="lv:template" mode="lvv:validate" priority="9">
  <!-- do not validate templates; we'll only validate expansions -->
</template>


<!-- XXX: Nothing is calling this! -->
<template name="lvv:symbol-chk">
  <param name="root" />
  <param name="symbol-map" />

  <!-- build a symbol list of all used and default symbols -->
  <variable name="symlist">
    <!-- @sym attributes -->
    <for-each select="$root//lv:*[@sym]">
      <lvv:sym value="{@sym}" />
    </for-each>

    <!-- defaults from mapping document -->
    <for-each select="$symbol-map/sym:symbol[ not( ./* ) ]">
      <lvv:sym value="{.}" />
    </for-each>
  </variable>

  <!-- error for each restricted node -->
  <for-each select="
      $root//lv:*[
        @sym = $symbol-map/sym:reserved/sym:reserve/@sym
      ]
    ">

    <variable name="symbol" select="@sym" />

    <call-template name="lvv:error">
      <with-param name="desc" select="'Symbol is reserved and cannot be used'" />
      <with-param name="refnode" select="$root//lv:*[@sym=$symbol]" />
      <with-param name="content" select="$symbol" />
    </call-template>
  </for-each>

  <!-- error for each duplicate node -->
  <for-each select="
      $symlist/*[
        @value = following-sibling::*/@value
      ]
    ">

    <variable name="symbol" select="@value" />

    <call-template name="lvv:error">
      <with-param name="desc" select="'Symbol is not unique'" />
      <with-param name="refnode" select="$root//lv:*[@sym=$symbol]" />
      <with-param name="content" select="$symbol" />
    </call-template>
  </for-each>
</template>


<template match="c:apply[@name]" mode="lvv:validate" priority="5">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="name" select="@name" />
  <variable name="self" select="." />

  <variable name="fsym" as="element( preproc:sym )"
            select="$symtable-map( $name )" />

  <!-- ensure that a function is being applied -->
  <if test="not( $fsym )">
    <call-template name="lvv:error">
      <with-param name="desc" select="'Applying non-function'" />
      <with-param name="refnode" select="." />
      <with-param name="content" select="@name" />
    </call-template>
  </if>

  <!-- check that all required arguments are provided -->
  <for-each select="
      $fsym/preproc:sym-ref[
        concat( ':', $name, ':', @name ) = ancestor::preproc:symtable/preproc:sym[
          @type='lparam'
          and not( @default )
        ]
      ]
    ">

    <call-template name="lvv:error">
      <with-param name="desc" select="'Missing required argument'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <value-of select="@name" />
        <text> for application of </text>
        <value-of select="$name" />
        <text>()</text>
      </with-param>
    </call-template>
  </for-each>

  <apply-templates mode="lvv:validate" />
</template>


<template match="c:*[ @index ]/c:index" mode="lvv:validate" priority="9">
  <call-template name="lvv:error">
    <with-param name="desc" select="'Ambiguous index specification'" />
    <with-param name="refnode" select="." />
    <with-param name="content" select="../@name" />
  </call-template>

  <apply-templates mode="lvv:validate" />
</template>


<!--
  Validate that match @on's exist
-->
<template match="lv:classify[ @as ]//lv:match" mode="lvv:validate" priority="9">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="sym" as="element( preproc:sym )?"
            select="$symtable-map( @on )" />

  <if test="not( $sym )">
    <call-template name="lvv:error">
      <with-param name="desc" select="'Unknown match @on'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <text>`</text>
        <value-of select="@on" />
        <text>' is unknown for classification </text>

        <variable name="class"
                      select="ancestor::lv:classify" />
        <value-of select="if ( $class/@preproc:generated-from ) then
                                $class/@preproc:generated-from
                              else
                                $class/@as" />
      </with-param>
    </call-template>
  </if>

  <apply-templates select="." mode="lvv:validate-match" />
</template>

<!--
  Validate that non-numeric value matches actually exist and are constants
-->
<template match="lv:match[@value]" mode="lvv:validate-match" priority="5">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="sym" as="element( preproc:sym )?"
            select="$symtable-map( @value )" />

  <if test="not( number( @value ) = @value )
            and not( $sym )">

    <call-template name="lvv:error">
      <with-param name="desc" select="'Unknown match value'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <text>`</text>
        <value-of select="@value" />
        <text>' is unknown for classification </text>

        <variable name="class"
                      select="ancestor::lv:classify" />
        <value-of select="if ( $class/@preproc:generated-from ) then
                                $class/@preproc:generated-from
                              else
                                $class/@as" />
      </with-param>
    </call-template>
  </if>

  <apply-templates mode="lvv:validate-match" />
</template>

<template match="lv:match" mode="lvv:validate-match" priority="2">
  <apply-templates mode="lvv:validate-match" />
</template>


<template match="c:*" mode="lvv:validate-match" priority="2">
  <apply-templates select="." mode="lvv:validate" />
</template>

<template match="*" mode="lvv:validate-match" priority="1">
  <!-- do nothing -->
</template>


<template match="c:value" mode="lvv:validate" priority="5">
  <!-- do nothing; just prevent the below validation from occurring -->
  <apply-templates mode="lvv:validate" />
</template>

<template match="c:let" mode="lvv:validate" priority="5">
  <!-- do not validate this node itself -->
  <apply-templates mode="lvv:validate" />
</template>

<template match="c:*[@name or @of]" mode="lvv:validate" priority="2">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="name">
    <choose>
      <when test="@of">
        <value-of select="@of" />
      </when>

      <otherwise>
        <value-of select="@name" />
      </otherwise>
    </choose>
  </variable>

  <!-- locate function params/let vars -->
  <variable name="fname" select="
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
  <variable name="sym" as="element( preproc:sym )?"
            select="if ( $fname ) then
                        $symtable-map( concat( ':', $fname, ':', $name ) )
                      else
                        $symtable-map( $name )" />

  <variable name="type" select="$sym/@dtype" />

  <!-- all calculations must make use of numeric types -->
  <if test="
      not(
        ( $type = 'integer' )
        or ( $type = 'float' )
        or ( $type = 'boolean' )
        or ( ancestor::c:*[ @of and @index=$name ] )
      )
    ">

    <call-template name="lvv:error">
      <with-param name="desc" select="'Non-numeric type in calculation'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <value-of select="$name" />
        <text> is </text>

        <choose>
          <when test="not( $type ) or $type = ''">
            <text>undefined</text>
          </when>

          <otherwise>
            <text>of type '</text>
              <value-of select="$type" />
            <text>'</text>
          </otherwise>
        </choose>
      </with-param>
    </call-template>
  </if>

  <variable name="is-set" select="$sym/@dim" />

  <choose>
      <!-- furthermore, if @of is provided, then it must be a set -->
    <when test="@of">
      <if test="$is-set = 0">
        <call-template name="lvv:error">
          <with-param name="desc" select="'@of must reference a set'" />
          <with-param name="refnode" select="." />
          <with-param name="content" select="$name" />
        </call-template>
      </if>
    </when>

    <!-- otherwise, an index is required to reference an item in the set unless
         the value is being passed as an argument of the same set type, or is
         the return value of a function (we assume it to be a return value if it
         is in a tail position) -->
    <!-- TODO: re-add argument param check for sets -->
    <!-- TODO: c:values/c:value/@set check; should also only be allowed in tail -->
    <otherwise>
      <choose>
        <when test="
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

          <choose>
            <when test="$sym/@dim = '?'">
              <!-- probably uses an extern; this will be taken care of by
                   the linker, or it will provide an error then -->
            </when>

            <otherwise>
              <call-template name="lvv:error">
                <with-param name="desc">
                  <text>Unexpected vector/matrix reference</text>
                </with-param>
                <with-param name="refnode" select="." />
                <with-param name="content">
                  <value-of select="@name" />
                  <text> (did you forget @index?)</text>
                </with-param>
              </call-template>
            </otherwise>
          </choose>
        </when>

        <!-- TODO: generalize this! -->
        <!-- Matrix references require two indexes, unless referenced within
             contexts -->
        <when test="
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
          <call-template name="lvv:error">
            <with-param name="desc" select="'Invalid matrix specification'" />
            <with-param name="refnode" select="." />
            <with-param name="content">
              <value-of select="@name" />
              <text> requires </text>
                <value-of select="$is-set" />
              <text> indexes (use c:index) in this context</text>
            </with-param>
          </call-template>
        </when>

        <!-- ensure that we do not have too many indexes -->
        <when test="( number( $is-set ) gt 0 ) and ./c:index[ number( $is-set ) + 1 ]">
          <call-template name="lvv:error">
            <with-param name="desc" select="'Invalid vector/matrix specification'" />
            <with-param name="refnode" select="." />
            <with-param name="content">
              <value-of select="@name" />
              <text> may only have </text>
                <value-of select="$is-set" />
              <text> index(s)</text>
            </with-param>
          </call-template>
        </when>

        <!-- if we have an index, but we're not dealing with a set, then that is
             also an issue -->
        <when test="@index and ( $is-set = '' )">
          <call-template name="lvv:error">
            <with-param name="desc" select="'Using index with non-set'" />
            <with-param name="refnode" select="." />
            <with-param name="content" select="@name" />
          </call-template>
        </when>
      </choose>
    </otherwise>
  </choose>

  <!-- index references should be defined -->
  <if test="
      @index and not( local-name() = 'sum' or local-name() = 'product' )
    ">

    <variable name="index" select="@index" />

    <!-- search for index definition -->
    <!-- XXX: This also requires knowledge of match and require-param -->
    <if test="
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
      <call-template name="lvv:error">
        <with-param name="desc" select="'Undefined index'" />
        <with-param name="refnode" select="." />
        <with-param name="content" select="@index" />
      </call-template>
    </if>
  </if>

  <!-- recursively validate any nested calculations -->
  <apply-templates mode="lvv:validate" />
</template>


<template match="c:apply/c:arg[@name]" mode="lvv:validate" priority="5">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <!-- merely validate its existence -->
  <variable name="fname" select="parent::c:apply/@name" />

  <if test="not( $symtable-map( concat( ':', $fname, ':', @name ) ) )">
    <call-template name="lvv:error">
      <with-param name="desc" select="'Unknown argument'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <text>Argument `</text>
          <value-of select="@name" />
        <text>' is unknown for function `</text>
          <value-of select="$fname" />
        <text>'</text>
      </with-param>
    </call-template>
  </if>

  <!-- recursively validate any nested calculations -->
  <apply-templates mode="lvv:validate" />
</template>


<template match="c:product[@dot]" mode="lvv:validate" priority="5">
  <!-- TODO -->
</template>


<template mode="lvv:validate" priority="2" match="
    lv:union/lv:typedef[
      ./lv:*[1]/@type != preceding-sibling::lv:typedef[1]/lv:*[1]/@type
    ]
  ">

  <call-template name="lvv:error">
    <with-param name="desc" select="'Union type mismatch'" />
    <with-param name="refnode" select="." />
    <with-param name="content">
      <text>Expected type '</text>
      <value-of select="preceding-sibling::lv:typedef[1]/lv:*[1]/@type" />
      <text>' for </text>
      <value-of select="@name" />
      <text>, but found '</text>
      <value-of select="./lv:*[1]/@type" />
      <text>'</text>
    </with-param>
  </call-template>

  <apply-templates mode="lvv:validate" />
</template>


<!--
  Checks for use of undefined classifications
-->
<template mode="lvv:validate" priority="2"
          match="lv:rate/lv:class">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <if test="not( $symtable-map( concat( ':class:', @ref ) ) )">
    <call-template name="lvv:error">
      <with-param name="desc" select="'Unknown classification'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <text>unknown classification '</text>
        <value-of select="@ref" />
        <text>' referenced by </text>
        <value-of select="ancestor::lv:rate/@yields" />
      </with-param>
    </call-template>
  </if>
</template>


<!--
  All rate blocks must have non-empty yields

  This is an awkward error, because it's not possible to identify the
  rate block by name...there is none.
-->
<template mode="lvv:validate" priority="9"
              match="lv:rate[ not( @yields ) or @yields = '' ]">
  <call-template name="lvv:error">
    <with-param name="desc" select="'Unidentifiable rate block'" />
    <with-param name="refnode" select="." />
    <with-param name="content">
      <text>missing or empty @yields; see document dump</text>
    </with-param>
  </call-template>
</template>


<!--
  lv:rate blocks have no use for @generates.  Since XSDs don't work within
  templates, let's validate that independently.  This is particularly
  important for developers unfamiliar with the distinction between lv:rate
  and lv:rate-each.
-->
<template mode="lvv:validate" priority="7"
              match="lv:rate[ @generates ]">
  <call-template name="lvv:error">
    <with-param name="desc" select="'lv:rate/@generate'" />
    <with-param name="refnode" select="." />
    <with-param name="content"
                    select="concat( '`', @yields, ''': lv:rate does ',
                                    'not support @generates' )" />
  </call-template>
</template>


<!--
  Rate block cannot be nested.
-->
<template mode="lvv:validate" priority="8"
              match="lv:rate[ ancestor::lv:rate ]">
  <variable name="within" as="element( lv:rate )"
                select="ancestor::lv:rate[1]" />

  <call-template name="lvv:error">
    <with-param name="desc" select="'Nested rate block'" />
    <with-param name="refnode" select="." />
    <with-param name="content"
                    select="concat( '`', @yields, ''' cannot be nested ',
                                    'within `', $within/@yields, '''' )" />
  </call-template>
</template>


<!--
  Throws an error if a generator is requested using unsupported data

  Specifically, a generator is intended to generate a set from an expression
  while looping over another set. If we're not looping, then we're not
  generating a set. Furthermore, if a child expression was not provided, then
  the set produced would be equivalent to @of, which is useless.
-->
<template mode="lvv:validate"
  match="c:*[ @generates and not( @of and ./c:* ) ]" priority="9">

  <call-template name="lvv:error">
    <with-param name="desc" select="'Invalid generator'" />
    <with-param name="refnode" select="." />
    <with-param name="content">
      <text>Cannot create generator '</text>
        <value-of select="@generates" />
      <text>'; generating expressions must contain both @of </text>
      <text>and a sub-expression.</text>
    </with-param>
  </call-template>

  <apply-templates mode="lvv:validate" />
</template>


<!--
  Since @generates creates a new variable that can be referenced, it needs
  documentation! Refuse to compile if documentation is not provided. Yeah, we're
  assholes.
-->
<template mode="lvv:validate"
  match="c:*[ @generates and not( @desc ) ]" priority="9">

  <call-template name="lvv:error">
    <with-param name="desc" select="'No generator description'" />
    <with-param name="refnode" select="." />
    <with-param name="content">
      <text>@desc required when creating generator </text>
      <value-of select="@generates" />
    </with-param>
  </call-template>

  <apply-templates mode="lvv:validate" />
</template>


<template match="ext:*" mode="lvv:get-path">
  <!-- omit from path output -->
</template>

<template match="*" mode="lvv:get-path">
  <apply-templates select="parent::*" mode="lvv:get-path" />
  <text>/</text>
  <value-of select="name()" />

  <!-- certain nodes may support path descriptions to aid in determining which
       node is being referenced -->
  <variable name="desc">
    <apply-templates select="." mode="lvv:get-path-desc" />
  </variable>

  <if test="$desc != ''">
    <text>[</text>
      <value-of select="$desc" />
    <text>]</text>
  </if>
</template>

<template match="lv:rate[ @yields ]" mode="lvv:get-path-desc">
  <text>@yields=</text>
  <value-of select="@yields" />
</template>

<template match="c:*[ @name ]" mode="lvv:get-path-desc" priority="5">
  <text>@name=</text>
  <value-of select="@name" />
</template>
<template match="c:*[ @label ]" mode="lvv:get-path-desc" priority="1">
  <text>@label=</text>
  <value-of select="@label" />
</template>

<template match="*" mode="lvv:get-path-desc">
  <!-- no desc by default -->
</template>


<template name="lvv:error">
  <param name="desc" />
  <param name="refnode" />
  <param name="content" />

  <variable name="path">
    <if test="$refnode">
      <apply-templates select="$refnode" mode="lvv:get-path" />
    </if>
  </variable>

  <lvv:error desc="{$desc}" path="{$path}">
    <value-of select="$content" />
  </lvv:error>
</template>

</stylesheet>
