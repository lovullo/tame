<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript (legacy classification system)

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:map="http://www.w3.org/2005/xpath-functions/map"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:lvp="http://www.lovullo.com"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:w="http://www.lovullo.com/rater/worksheet"
            xmlns:wc="http://www.lovullo.com/rater/worksheet/compiler"
            xmlns:compiler="http://www.lovullo.com/rater/compiler"
            xmlns:calc-compiler="http://www.lovullo.com/calc/compiler"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:util="http://www.lovullo.com/util"
            xmlns:ext="http://www.lovullo.com/ext">


<function name="compiler:compile-classify-legacy" as="xs:string+">
  <param name="symtable-map" as="map(*)" />
  <param name="classify" as="element( lv:classify )" />

  <apply-templates mode="compile-legacy" select="$classify">
    <with-param name="symtable-map" select="$symtable-map"
                tunnel="yes" />
  </apply-templates>
</function>


<!--
  Generate code to perform a classification

  Based on the criteria provided by the classification, generate and store the
  result of a boolean expression performing the classification using global
  arguments.

  @return generated classification expression
-->
<template match="lv:classify" mode="compile-legacy">
  <param name="symtable-map" as="map(*)" tunnel="yes" />
  <param name="noclass" />
  <param name="ignores" />

  <variable name="self" select="." />

  <value-of select="$compiler:nl" />

  <variable name="dest">
    <text>A['</text>
    <value-of select="@yields" />
    <text>']</text>
  </variable>

  <if test="not( $noclass )">
    <sequence select="concat( $dest, '=[];', $compiler:nl )" />

    <if test="@preproc:generated='true'">
      <text>g</text>
    </if>

    <text>c['</text>
      <value-of select="@as" />
    <text>'] = (function(){var result,tmp; </text>
  </if>

  <!-- locate classification predicates (since lv:any and lv:all are split
       into their own classifications, matching on any depth ensures we get
       into any preproc:* nodes as well) -->
  <variable name="criteria" as="element( lv:match )*"
            select="./lv:match[
                      not( $ignores )
                      or not( @on=$ignores/@ref ) ]" />

  <variable name="criteria-syms" as="element( preproc:sym )*"
            select="for $match in $criteria
                      return $symtable-map( $match/@on )" />

  <!-- generate boolean value from match expressions -->
  <choose>
    <!-- if classification criteria were provided, then use them -->
    <when test="$criteria">
      <variable name="op" as="xs:string"
                select="compiler:match-group-op( $self )" />

      <text></text>
      <!-- order matches from highest to lowest dimensions (required for
           the cmatch algorithm)-->
      <for-each select="reverse( xs:integer( min( $criteria-syms/@dim ) )
                          to xs:integer( max( $criteria-syms/@dim ) ) )">
        <apply-templates mode="compile-legacy"
                         select="$criteria[
                                   @on = $criteria-syms[
                                            @dim = current() ]/@name ]">
          <with-param name="ignores" select="$ignores" />
          <with-param name="operator" select="$op" />
        </apply-templates>
      </for-each>
    </when>

    <!-- if no classification criteria, then always true/false -->
    <otherwise>
      <!-- this is only useful if $noclass is *not* set -->
      <if test="not( $noclass )">
        <choose>
          <!-- universal -->
          <when test="not( @any='true' )">
            <text>tmp = true; </text>
          </when>

          <!-- existential -->
          <otherwise>
            <text>tmp = false; </text>
          </otherwise>
        </choose>
      </if>

      <!-- if @yields was provided, then store the value in a variable of their
           choice as well (since cmatch will not be done) -->
      <if test="@yields">
        <value-of select="$dest" />
        <choose>
          <!-- universal -->
          <when test="not( @any='true' )">
            <text> = 1;</text>
          </when>

          <!-- existential -->
          <otherwise>
            <text> = 0;</text>
          </otherwise>
        </choose>
      </if>
    </otherwise>
  </choose>

  <text> return tmp;})();</text>

  <!-- support termination on certain classifications (useful for eligibility
       and error conditions) -->
  <if test="@terminate = 'true'">
    <text>if ( _canterm &amp;&amp; </text>

    <if test="@preproc:generated='true'">
      <text>g</text>
    </if>
    <text>c['</text>
      <value-of select="@as" />
    <text>'] ) throw Error( '</text>
      <value-of select="replace( @desc, '''', '\\''' )" />
    <text>' );</text>

    <value-of select="$compiler:nl" />
  </if>

    <variable name="sym"
      select="$symtable-map( $self/@yields )" />

  <!-- if we are not any type of set, then yield the value of the first
         index (note the $criteria check; see above); note that we do not do
         not( @set ) here, since that may have ill effects as it implies that
         the node is not preprocessed -->
  <!-- TODO: this can be simplified, since @yields is always provided -->
  <if test="$criteria and @yields and ( $sym/@dim='0' )">
    <value-of select="$dest" />
    <text> = </text>
      <value-of select="$dest" />
    <text>[0];</text>

    <value-of select="$compiler:nl" />
  </if>
</template>


<!--
  Generate code asserting a match

  Siblings are joined by default with ampersands to denote an AND relationship,
  unless overridden.

  @return generated match code
-->
<template match="lv:match" mode="compile-legacy" priority="1">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <!-- default to all matches being required -->
  <param name="operator" select="'&amp;&amp;'" />
  <param name="yields" select="../@yields" />

  <variable name="name" select="@on" />

  <variable name="sym-on" as="element( preproc:sym )"
            select="$symtable-map( $name )" />

  <text> tmp = </text>

  <variable name="input-raw">
    <choose>
      <!-- if we have assumptions, then we'll be recalculating (rather than
           referencing) an existing classification -->
      <when test="lv:assuming">
        <text>_cassume</text>
      </when>

      <otherwise>
        <choose>
          <when test="$sym-on/@type = 'const'">
            <text>consts</text>
          </when>
          <otherwise>
            <text>A</text>
          </otherwise>
        </choose>

        <text>['</text>
          <value-of select="translate( @on, &quot;'&quot;, '' )" />
        <text>']</text>
      </otherwise>
    </choose>
  </variable>

  <!-- yields (if not set, generate one so that cmatches still works properly)
       -->
  <variable name="yieldto">
    <call-template name="compiler:gen-match-yieldto">
      <with-param name="yields" select="$yields" />
    </call-template>
  </variable>

  <!-- the input value -->
  <variable name="input">
    <choose>
      <when test="@scalar = 'true'">
        <text>stov( </text>
          <value-of select="$input-raw" />
        <text>, ( ( </text>
          <value-of select="$yieldto" />
        <!-- note that we default to 1 so that there is at least a single
             element (which will be the case of the scalar is the first match)
             in a given classification; the awkward inner [] is to protect
             against potentially undefined values and will hopefully never
             happen, and the length is checked on the inner grouping rather than
             on the outside of the entire expression to ensure that it will
             yield the intended result if yieldto.length === 0 -->
        <text> || [] ).length || 1 ) )</text>
      </when>

      <otherwise>
        <value-of select="$input-raw" />
      </otherwise>
    </choose>
  </variable>

  <if test="lv:assuming">
    <text>(function(){</text>
      <!-- initialize variable (ensuring that the closure we're about to generate
           will properly assign the value rather than encapsulate it) -->
      <text>var </text>
      <value-of select="$input-raw" />
      <text>; </text>

      <!-- generate assumptions and recursively generate the referenced
           classification -->
      <apply-templates select="." mode="compile-match-assumptions">
        <with-param name="result-var" select="$input-raw" />
      </apply-templates>
      <text>; return </text>
  </if>

  <!-- invoke the classification matcher on this input -->
  <text>anyValue( </text>
    <value-of select="$input" />
  <text>, </text>

  <!-- TODO: error if multiple; also, refactor -->
  <choose>
    <when test="@value">
      <variable name="value" select="@value" />
      <variable name="sym" as="element( preproc:sym )?"
                select="$symtable-map( $value )" />

      <choose>
        <!-- value unavailable (TODO: vector/matrix support) -->
        <when test="$sym and not( $sym/@value )">
            <message>
              <text>[jsc] !!! bad classification match: `</text>
                <value-of select="$value" />
              <text>' is not a scalar constant</text>
            </message>
        </when>

        <!-- simple constant -->
        <when test="$sym and @value">
          <value-of select="$sym/@value" />
        </when>

        <otherwise>
          <text>'</text>
            <!-- TODO: Should we disallow entirely? -->
            <message>
              <text>[jsc] warning: static classification match '</text>
                <value-of select="$value" />
              <text>' in </text>
              <value-of select="ancestor::lv:classify[1]/@as" />
              <text>; use calculation predicate or constant instead</text>
            </message>

            <value-of select="$value" />
          <text>'</text>
        </otherwise>
      </choose>
    </when>

    <when test="@pattern">
      <text>function( val ) { </text>
        <text>return /</text>
          <value-of select="@pattern" />
        <text>/.test( val );</text>
      <text> }</text>
    </when>

    <when test="./c:*">
      <text>function( val, __$$i ) { </text>
        <text>return ( </text>
          <for-each select="./c:*">
            <if test="position() > 1">
              <text disable-output-escaping="yes"> &amp;&amp; </text>
            </if>

            <text>( val </text>
              <apply-templates select="." mode="compile-calc-when" />
            <text> ) </text>
          </for-each>
        <text>);</text>
      <text>}</text>
    </when>

    <otherwise>
      <apply-templates select="." mode="compiler:match-anyof-legacy" />
    </otherwise>
  </choose>

  <text>, </text>
  <value-of select="$yieldto" />
  <text>, </text>

  <!-- if this match is part of a classification that should yield a matrix,
       then force a matrix set -->
  <choose>
    <when test="ancestor::lv:classify/@set = 'matrix'">
      <text>true</text>
    </when>
    <otherwise>
      <text>false</text>
    </otherwise>
  </choose>

  <text>, </text>
  <choose>
    <when test="parent::lv:classify/@any='true'">
      <text>false</text>
    </when>
    <otherwise>
      <text>true</text>
    </otherwise>
  </choose>

  <!-- for debugging -->
  <if test="$debug-id-on-stack">
    <text>/*!+*/,"</text>
    <value-of select="$input" />
    <text>"/*!-*/</text>
  </if>

  <!-- end of anyValue() call -->
  <text> ) </text>

  <!-- end of assuming function call -->
  <if test="lv:assuming">
    <text>})()</text>
  </if>

  <text>;</text>

  <text>/*!+*/( D['</text>
    <value-of select="@_id" />
  <text>'] || ( D['</text>
    <value-of select="@_id" />
  <text>'] = [] ) ).push( tmp );/*!-*/ </text>


  <text>result = </text>
  <choose>
    <!-- join with operator if not first in set -->
    <when test="position() > 1">
      <text>result </text>
      <value-of select="$operator" />
      <text> tmp;</text>
    </when>

    <otherwise>
      <text>tmp;</text>
    </otherwise>
  </choose>
</template>


<!--
  Handles the special "float" domain

  Rather than the impossible task of calculating all possible floats and
  asserting that the given values is within that set, the obvious task is to
  assert whether or not the value is logically capable of existing in such a
  set based on a definition of such a set.

  See also "integer"
-->
<template match="lv:match[ @anyOf='float' ]" mode="compiler:match-anyof-legacy" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function( val ) {</text>
    <text>return ( typeof +val === 'number' ) </text>
      <text disable-output-escaping="yes">&amp;&amp; </text>
      <!-- note: greater than or equal to, since we want to permit integers as
           well -->
      <text disable-output-escaping="yes">( Math.ceil( val ) >= Math.floor( val ) )</text>
    <text>;</text>
  <text>}</text>
</template>

<!--
  Handles the special "float" domain

  Rather than the impossible task of calculating all possible integers and
  asserting that the given values is within that set, the obvious task is to
  assert whether or not the value is logically capable of existing in such a
  set based on a definition of such a set.

  See also "float"
-->
<template match="lv:match[ @anyOf='integer' ]" mode="compiler:match-anyof-legacy" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function( val ) {</text>
    <text>return ( typeof +val === 'number' ) </text>
      <text disable-output-escaping="yes">&amp;&amp; </text>
      <text>( Math.floor( val ) === Math.ceil( val ) )</text>
    <text>;</text>
  <text>}</text>
</template>

<!--
  Handles matching on an empty set

  This is useful for asserting against fields that may have default values,
  since in such a case an empty value would be permitted.
-->
<template match="lv:match[ @anyOf='empty' ]" mode="compiler:match-anyof-legacy" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function( val ) {</text>
    <text>return ( val === '' ) </text>
      <text>|| ( val === undefined ) || ( val === null )</text>
    <text>;</text>
  <text>}</text>
</template>

<!--
  Uh oh. Hopefully this never happens; will throw an exception if a type is
  defined as a base type (using typedef), but is not handled by the compiler.
-->
<template match="lv:match[ @anyOf=root(.)//lv:typedef[ ./lv:base-type ]/@name ]"
  mode="compiler:match-anyof-legacy" priority="3">

  <text>function( val ) {</text>
    <text>throw Error( 'CRITICAL: Unhandled base type: </text>
      <value-of select="@anyOf" />
    <text>' );</text>
  <text>}</text>
</template>

<!--
  Used for user-defined domains
-->
<template match="lv:match[ @anyOf ]" mode="compiler:match-anyof-legacy" priority="1">
  <text>types['</text>
    <value-of select="@anyOf" />
  <text>'].values</text>
</template>


</stylesheet>
