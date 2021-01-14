<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
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


<!-- calculation compiler -->
<include href="js-calc.xsl" />

<!-- rating worksheet definition compiler -->
<include href="worksheet.xsl" />

<!-- newline -->
<variable name="compiler:nl" select="'&#10;'" />

<!-- output additional information on the stack for debugging -->
<variable name="debug-id-on-stack" select="false()" />


<!--
  Generates rater function

  The rater is a single function that may be invoked with a key-value argument
  list and will return a variety of data, including the final premium.

  @param NodeSet pkgs all packages, including rater

  @return compiled JS
-->
<template name="compiler:entry">
  <!-- enclose everything in a self-executing function to sandbox our data -->
  <text>( function() { </text>
    <!-- to store debug information for equations (we have to put this out here
         so that functions also have access to it...yes, it's stateful, yes it's
         bullshit, but oh well) -->
    <text>/**@expose*/var consts = C = {};</text>
    <text>/**@expose*/var debug = D = {};</text>
    <text>/**@expose*/var params = P = {};</text>
    <text>/**@expose*/var types = {};</text>
    <text>/**@expose*/var meta = {};</text>
</template>


<template name="compiler:entry-rater">
    <!-- the rater itself -->
    <value-of select="$compiler:nl" />
    <text>function rater( arglist, _canterm ) {</text>
      <text>_canterm = ( _canterm == undefined ) ? true : !!_canterm;</text>

      <!-- XXX: fix; clear debug from any previous runs -->
      <text>debug = D = {};</text>

      <!-- magic constants (N.B. these ones must be re-calculated with each
           call, otherwise the data may be incorrect!) -->
      <value-of select="$compiler:nl" />

      <!-- XXX: Remove this; shouldn't be magic -->
      <text>consts['__DATE_YEAR__'] = ( new Date() ).getFullYear(); </text>

      <!-- clone the object so as not to modify the one that was passed
           (ES5 feature); also adds constants -->
      <text>var args = A = Object.create( arglist );</text>

      <!-- will store the global params that we ended up requiring -->
      <text>var req_params = {};</text>

      <!-- handle defaults -->
      <text>init_defaults( args, params );</text>

      <value-of select="$compiler:nl" />
      <text>/**@expose*/var classes = {};</text>
      <text>/**@expose*/var genclasses = {};</text>

      <!-- temporaries used in computations -->
      <text>var result, tmp;</text>
</template>

<template name="compiler:classifier">
  <!-- allow classification of any arbitrary dataset -->
  <value-of select="$compiler:nl" />
  <text>rater.classify = function( args, _canterm ) {</text>
    return rater( args, _canterm ).classes;
  <text> };</text>

  <text>rater.classify.fromMap = function( args_base, _canterm ) { </text>
    <text>var ret = {}; </text>
    <text>rater.fromMap( args_base, function( args ) {</text>
      <text>var result = rater( args, _canterm ); </text>

      <text>
        for ( var c in rater.classify.classmap )
        {
          ret[ c ] = {
            /**@expose*/ is: !!result.classes[ c ],
            /**@expose*/ indexes: result.vars[ rater.classify.classmap[ c ] ]
          };
        }
      </text>
    <text>} );</text>
    <text>return ret;</text>
  <text> }; </text>
</template>

<template name="compiler:exit-rater">
  <param name="name" as="xs:string "/>
  <param name="symbols" as="element( preproc:sym )*" />
  <param name="mapfrom" as="element()*" />

      <value-of select="$compiler:nl" />
      <text>return { </text>
        <!-- round the premium (special symbol ___yield) to max of 2 decimal places -->
        <text>/**@expose*/premium: ( Math.round( args.___yield * 100 ) / 100 ), </text>
        <text>/**@expose*/classes: classes, </text>
        <text>/**@expose*/vars: args, </text>
        <text>/**@expose*/reqParams: req_params, </text>
        <text>/**@expose*/debug: debug </text>
      <text>}; </text>
    <text>}</text>

    <!-- make the name of the supplier available -->
    <text>/**@expose*/rater.supplier = '</text>
      <value-of select="substring-after( $name, '/' )" />
    <text>'; </text>

    <text>/**@expose*/rater.meta = meta;</text>
    <text>/**@expose*/rater.consts = consts;</text>

    <!-- provide classification -> yields mapping -->
    <value-of select="$compiler:nl" />
    <text>/**@expose*/rater.classify.classmap = { </text>
      <apply-templates select="." mode="compiler:classifier-yields-map">
        <with-param name="symbols" select="$symbols" />
      </apply-templates>
    <text> }; </text>

    <!-- provide classification descriptions -->
    <value-of select="$compiler:nl" />
    <text>/**@expose*/rater.classify.desc = { </text>
    <sequence select="
        compiler:class-desc(
          $symbols[ @type='class' ] )" />
    <text> }; </text>

    <!-- mapped fields (external names) -->
    <value-of select="$compiler:nl" />
    <text>/**@expose*/rater.knownFields = {</text>
      <for-each select="$mapfrom">
        <if test="position() > 1">
          <text>, </text>
        </if>

        <text>'</text>
          <value-of select="@name" />
        <text>': true</text>
      </for-each>
    <text>}; </text>

    <text>/**@expose*/rater.params = params;</text>

    <!-- the rater has been generated; return it -->
    <text>return rater;</text>
  <text>} )()</text>
</template>


<template match="lv:package" mode="compiler:classifier-yields-map">
  <param name="symbols" />

  <!-- for each cgen symbol (which is the classification @yields), map the
       classification name (the @parent) to the cgen symbol name -->
  <for-each select="$symbols[ @type='class' and not( @preproc:generated ) ]">
    <if test="position() > 1">
      <text>,</text>
    </if>

    <text>'</text>
      <value-of select="substring-after( @name, ':class:' )" />
    <text>':'</text>
      <!-- yields -->
      <value-of select="@yields" />
    <text>'</text>
  </for-each>
</template>


<function name="compiler:class-desc">
  <param name="syms" as="element( preproc:sym )*" />

  <for-each select="$syms">
    <if test="position() > 1">
      <text>,</text>
    </if>

    <text>'</text>
      <value-of select="substring-after( @name, ':class:' )" />
    <text>':'</text>
      <!-- todo: escape -->
      <value-of select="translate( normalize-space(@desc), &quot;'&quot;, '' )" />
    <text>'</text>
  </for-each>
</function>


<!--
  Compile global parameter list into an object literal

  @return key and value for the given parameter
-->
<template match="lv:param" mode="compile">
  <!-- generate key using param name -->
  <text>P['</text>
  <value-of select="@name" />
  <text>']={</text>

  <!-- param properties -->
  <text>type: '</text>
    <value-of select="@type" />
  <text>',</text>

  <text>'default': '</text>
    <value-of select="@default" />
  <text>',</text>

  <text>depth: </text>
    <!-- TODO: this logic is duplicated multiple places -->
    <choose>
      <when test="@set = 'vector'">
        <sequence select="1" />
      </when>

      <when test="@set = 'matrix'">
        <sequence select="2" />
      </when>

      <otherwise>
        <sequence select="0" />
      </otherwise>
    </choose>
  <text>,</text>

  <text>required: </text>
    <!-- param is required if the attribute is present, not non-empty -->
    <value-of select="string( not( boolean( @default ) ) )" />
  <text>};</text>
</template>


<!--
  Generate value table for unions

  The type of the table will be considered the type of the first enum and each
  enum value table will be combined.

  @return object literal properties containing union data
-->
<template match="lv:typedef/lv:union" mode="compile" priority="5">
  <!-- generate key using type name -->
  <text>types['</text>
  <value-of select="../@name" />
  <text>']={</text>

  <!-- its type will be the type of its first enum (all must share the same
       domain) -->
  <text>type: '</text>
    <value-of select=".//lv:enum[1]/@type" />
  <text>',</text>

  <!-- finally, its table of values should consist of every enum it contains -->
  <text>values: {</text>
    <for-each select="./lv:typedef/lv:enum/lv:item">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <apply-templates select="." mode="compile" />
    </for-each>
  <text>}</text>

  <text>};</text>
</template>


<!--
  Generate value table for enums

  @return object literal properties containing enum data
-->
<template match="lv:typedef/lv:enum" mode="compile" priority="5">
  <!-- generate key using type name -->
  <text>types['</text>
  <value-of select="../@name" />
  <text>']={</text>

  <!-- domain of all values -->
  <text>type: '</text>
    <value-of select="@type" />
  <text>',</text>

  <!-- table of acceptable values -->
  <text>values: {</text>
    <for-each select="./lv:item">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <apply-templates select="." mode="compile" />
    </for-each>
  <text>}</text>

  <text>};</text>
</template>


<template match="lv:typedef/lv:base-type" mode="compile" priority="5">
  <text>types['</text>
  <value-of select="../@name" />
  <text>']={</text>

  <!-- base types are their own type -->
  <text>type: '</text>
    <value-of select="../@name" />
  <text>',</text>

  <text>values:{}</text>

  <text>};</text>
</template>


<template match="lv:typedef/*" mode="compile" priority="1">
  <message terminate="yes">
    <text>[lvc] Unhandled typedef: </text>
    <value-of select="../@name" />
  </message>
</template>


<!--
  Generate enum item value

  @return property representing a specific value
-->
<template match="lv:enum/lv:item" mode="compile">
  <param name="as-const" as="xs:boolean"
         select="false()" />

  <!-- determine enumerated value -->
  <variable name="value">
    <choose>
      <when test="@value">
        <value-of select="@value" />
      </when>

      <!-- default to string value equivalent to name -->
      <otherwise>
        <value-of select="@name" />
      </otherwise>
    </choose>
  </variable>

  <!-- we are only interest in its value; its constant is an internal value -->
  <sequence select="if ( $as-const ) then
                      concat( 'C[''', @name, '''] = ', $value, ';' )
                    else
                      concat( '''', $value, ''': true' )" />
</template>


<!--
  Generate an object containing values of constant sets

  This is done instead of inlining constant values as we do with non-sets since
  the specific index can be determined at runtime.

  @return JS object assignment for constant set values
-->
<template mode="compile" priority="2"
          match="lv:const[ element() or @values ]">
  <text>C['</text>
    <value-of select="@name" />
  <text>']=[</text>

    <!-- matrices -->
    <for-each select="compiler:const-sets( . )[ not( . = '' ) ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <text>[</text>
        <for-each select="compiler:set-items( ., true() )">
          <if test="position() > 1">
            <text>, </text>
          </if>

          <value-of select="compiler:js-number( . )" />
        </for-each>
      <text>]</text>
    </for-each>

    <!-- vectors -->
    <for-each select="compiler:set-items( ., false() )">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <value-of select="compiler:js-number( . )" />
    </for-each>

  <text>];</text>
</template>


<!--
  Falls back to scalar constants
-->
<template mode="compile" priority="1"
          match="lv:const">
  <text>C['</text>
    <value-of select="@name" />
  <text>']=</text>
    <value-of select="compiler:js-number( @value )" />
  <text>;</text>
</template>


<!--
  Produce sequence of sets

  Sets are used to group together items in a matrix.  A set can be
  defined explicitly (using nodes), or via a GNU Octave or
  MATLAB-style matrix definition.
-->
<function name="compiler:const-sets" as="item()*">
  <param name="const" as="element( lv:const )" />

  <variable name="values-def" as="xs:string?"
            select="$const/@values" />

  <choose>
    <when test="$values-def and contains( $values-def, ';' )">
      <sequence select="tokenize(
                          normalize-space( $values-def ), ';' )" />
    </when>

    <otherwise>
      <sequence select="$const/lv:set" />
    </otherwise>
  </choose>
</function>


<!--
  Produce a sequence of items

  Items represent elements of a vector.  They may be specified
  explicitly using nodes, or via a comma-delimited string.
-->
<function name="compiler:set-items" as="xs:string*">
  <param name="set"          as="item()*" />
  <param name="allow-values" as="xs:boolean" />

  <choose>
    <when test="$set instance of xs:string">
      <sequence select="tokenize(
                          normalize-space( $set ), ',' )" />
    </when>

    <when test="$set/@values and $allow-values">
      <sequence select="tokenize(
                          normalize-space( $set/@values ), ',' )" />
    </when>

    <otherwise>
      <sequence select="$set/lv:item/@value" />
    </otherwise>
  </choose>
</function>


<!--
  Format JS numbers such that they won't be misinterpreted as octal (if they
  have leading zeroes)
-->
<function name="compiler:js-number" as="xs:string?">
  <param name="src" as="xs:string?" />

  <variable name="norm" as="xs:string?"
            select="normalize-space( $src )" />

  <!-- note that this will make 0 into an empty string! -->
  <variable name="stripped" as="xs:string"
            select="replace( $norm, '^0+', '' )" />

  <sequence select="if ( $stripped = '' ) then
                      $norm
                    else
                      $stripped" />
</function>


<!--
  Single-TRUE-match classifications are effectively aliases
-->
<template mode="compile" priority="7"
          match="lv:classify[ count( lv:match ) = 1
                                and lv:match/@value='TRUE' ]">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="src" as="xs:string"
            select="lv:match/@on" />
  <variable name="src-sym" as="element( preproc:sym )"
            select="$symtable-map( $src )" />

  <choose>
    <!-- we only handle aliasing of other classifications -->
    <when test="$src-sym/@type = 'cgen'">
      <sequence select="$compiler:nl" />

      <!-- simply alias the @yields -->
      <sequence select="concat( compiler:class-yields-var(., false()), '=',
                                compiler:sym-class-yields-var(
                                  $symtable-map( $src-sym/@parent ) ),
                                '; ')" />

      <variable name="class-sym" as="element( preproc:sym )"
                select="$symtable-map( $src-sym/@parent )" />

      <variable name="cdest" as="xs:string"
                select="if ( @preproc:generated = 'true' ) then
                            'genclasses'
                          else
                            'classes'" />

      <variable name="cdest-src" as="xs:string"
                select="if ( $class-sym/@preproc:generated = 'true' ) then
                            'genclasses'
                          else
                            'classes'" />

      <sequence select="concat( $cdest, '[''', @as, '''] = ',
                                  $cdest-src, '[''',
                                    $class-sym/@orig-name, '''];' )" />
    </when>

    <!-- they must otherwise undergo the usual computation -->
    <otherwise>
      <next-match />
    </otherwise>
  </choose>
</template>


<!--
  Classification with no predicates always yields true/false, depending on
  whether it's conjunctive or disjunctive
-->
<template mode="compile" priority="7"
          match="lv:classify[ empty( lv:match ) ]">
  <variable name="val" as="xs:string"
            select="if ( not( @any = 'true' ) ) then '1' else '0'" />

  <value-of select="$compiler:nl" />
  <sequence select="concat( compiler:class-var(.), '=!!', $val, ';' )" />

  <if test="@yields">
    <sequence select="concat( compiler:class-yields-var(., true()), '=',
                              $val, ';' )" />
  </if>
</template>


<!--
  JS variable to which boolean class result will be assigned
-->
<function name="compiler:class-var" as="xs:string">
  <param name="class" as="element( lv:classify )" />

  <variable name="prefix" as="xs:string"
            select="if ( $class/@preproc:generated='true' ) then
                        'gen'
                      else
                        ''" />

  <sequence select="concat( $prefix, 'classes[''', $class/@as, ''']' )" />
</function>


<!--
  JS variable to which class @yields will be assigned
-->
<function name="compiler:class-yields-var" as="xs:string">
  <param name="class" as="element( lv:classify )" />
  <param name="define" as="xs:boolean" />

  <variable name="prefix" as="xs:string"
            select="if ( $define ) then 'var ' else ''" />

  <sequence select="if ( $class/@preproc:generated='true' ) then
                        concat( $prefix, '_cgen', $class/@yields )
                      else
                        concat( 'A[''', $class/@yields, ''']' )" />
</function>

<function name="compiler:sym-class-yields-var" as="xs:string">
  <param name="sym" as="element( preproc:sym )" />

  <variable name="yields" as="xs:string"
            select="$sym/@yields" />

  <sequence select="if ( $sym/@preproc:generated = 'true' ) then
                        concat( '_cgen', $yields )
                      else
                        concat( 'A[''', $yields, ''']' )" />
</function>


<!--
  Generate code to perform a classification

  Based on the criteria provided by the classification, generate and store the
  result of a boolean expression performing the classification using global
  arguments.

  @return generated classification expression
-->
<template match="lv:classify" mode="compile" priority="5">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="self" select="." />

  <value-of select="$compiler:nl" />

  <variable name="dest" as="xs:string"
            select="compiler:class-yields-var(., true())" />

  <sequence select="concat( $dest, '=[];', $compiler:nl )" />

  <!-- locate classification predicates (since lv:any and lv:all are split
       into their own classifications, matching on any depth ensures we get
       into any preproc:* nodes as well) -->
  <variable name="criteria" as="element( lv:match )*"
            select="lv:match" />

  <variable name="criteria-syms" as="element( preproc:sym )*"
            select="for $match in $criteria
                      return $symtable-map( $match/@on )" />

  <!-- generate boolean value from match expressions -->
  <variable name="op" as="xs:string"
            select="compiler:match-group-op( $self )" />

  <!-- order matches from highest to lowest dimensions (required for
       the cmatch algorithm)-->
  <for-each select="reverse( xs:integer( min( $criteria-syms/@dim ) )
                      to xs:integer( max( $criteria-syms/@dim ) ) )">
    <apply-templates mode="compile"
                     select="$criteria[
                               @on = $criteria-syms[
                                        @dim = current() ]/@name ]">
      <with-param name="operator" select="$op" />
    </apply-templates>
  </for-each>

  <variable name="var" as="xs:string"
            select="compiler:class-var( . )" />

  <sequence select="concat( $var, '=tmp;' )" />

  <!-- support termination on certain classifications (useful for eligibility
       and error conditions) -->
  <if test="@terminate = 'true'">
    <text>if (_canterm &amp;&amp; </text>
    <value-of select="$var" />
    <text>) throw Error( '</text>
      <value-of select="replace( @desc, '''', '\\''' )" />
    <text>');</text>
    <value-of select="$compiler:nl" />
  </if>

  <variable name="sym"
    select="$symtable-map( $self/@yields )" />

  <!-- if we are not any type of set, then yield the value of the first
         index (note the $criteria check; see above); note that we do not do
         not( @set ) here, since that may have ill effects as it implies that
         the node is not preprocessed -->
  <!-- TODO: this can be simplified, since @yields is always provided -->
  <if test="@yields and ( $sym/@dim='0' )">
    <variable name="dest-ref" as="xs:string"
              select="compiler:class-yields-var(., false())" />
    <value-of select="$dest-ref" />
    <text>=</text>
      <value-of select="$dest-ref" />
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
<template match="lv:match" mode="compile" priority="1">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <!-- default to all matches being required -->
  <param name="operator" select="'&amp;&amp;'" />
  <param name="yields" select="../@yields" />

  <variable name="name" select="@on" />

  <variable name="sym-on" as="element( preproc:sym )"
            select="$symtable-map( $name )" />

  <text> tmp=</text>

  <variable name="input-raw">
    <choose>
      <when test="$sym-on/@type = 'const'">
        <text>C['</text>
          <value-of select="translate( @on, &quot;'&quot;, '' )" />
        <text>']</text>
      </when>

      <!-- lv:match generates these and so is the only thing capable of
           referencing them -->
      <when test="$sym-on/@type = 'cgen'">
        <value-of select="compiler:sym-class-yields-var(
                            $symtable-map( $sym-on/@parent ) )" />
      </when>

      <otherwise>
        <text>A['</text>
          <value-of select="translate( @on, &quot;'&quot;, '' )" />
        <text>']</text>
      </otherwise>
    </choose>

  </variable>

  <variable name="yieldto" as="xs:string"
            select="compiler:class-yields-var(
                      parent::lv:classify,
                      false() )" />

  <!-- the input value -->
  <variable name="input">
    <choose>
      <when test="@scalar = 'true'">
        <text>stov( </text>
          <value-of select="$input-raw" />
        <text>, ((</text>
          <value-of select="$yieldto" />
        <!-- note that we default to 1 so that there is at least a single
             element (which will be the case of the scalar is the first match)
             in a given classification; the awkward inner [] is to protect
             against potentially undefined values and will hopefully never
             happen, and the length is checked on the inner grouping rather than
             on the outside of the entire expression to ensure that it will
             yield the intended result if yieldto.length === 0 -->
        <text>||[]).length||1))</text>
      </when>

      <otherwise>
        <value-of select="$input-raw" />
      </otherwise>
    </choose>
  </variable>

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
      <text>function(val) {</text>
        <text>return /</text>
          <value-of select="@pattern" />
        <text>/.test(val);</text>
      <text>}</text>
    </when>

    <when test="./c:*">
      <text>function(val, __$$i) { </text>
        <text>return (</text>
          <for-each select="./c:*">
            <if test="position() > 1">
              <text disable-output-escaping="yes"> &amp;&amp; </text>
            </if>

            <text>(val </text>
              <apply-templates select="." mode="compile-calc-when" />
            <text>)</text>
          </for-each>
        <text>);</text>
      <text>}</text>
    </when>

    <otherwise>
      <apply-templates select="." mode="compiler:match-anyof" />
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
  <text>);</text>

  <text>/*!+*/(D['</text>
    <value-of select="@_id" />
  <text>']||(D['</text>
    <value-of select="@_id" />
  <text>']=[])).push(tmp);/*!-*/ </text>
</template>

<!--
  Handles the special "float" domain

  Rather than the impossible task of calculating all possible floats and
  asserting that the given values is within that set, the obvious task is to
  assert whether or not the value is logically capable of existing in such a
  set based on a definition of such a set.

  See also "integer"
-->
<template match="lv:match[ @anyOf='float' ]" mode="compiler:match-anyof" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function(val) {</text>
    <text>return (typeof +val === 'number') </text>
      <text disable-output-escaping="yes">&amp;&amp; </text>
      <!-- note: greater than or equal to, since we want to permit integers as
           well -->
      <text disable-output-escaping="yes">(Math.ceil(val) >= Math.floor(val))</text>
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
<template match="lv:match[ @anyOf='integer' ]" mode="compiler:match-anyof" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function(val) {</text>
    <text>return (typeof +val === 'number') </text>
      <text disable-output-escaping="yes">&amp;&amp; </text>
      <text>( Math.floor(val) === Math.ceil(val))</text>
    <text>;</text>
  <text>}</text>
</template>

<!--
  Handles matching on an empty set

  This is useful for asserting against fields that may have default values,
  since in such a case an empty value would be permitted.
-->
<template match="lv:match[ @anyOf='empty' ]" mode="compiler:match-anyof" priority="5">
  <!-- ceil(x) - floor(x) = [ x is not an integer ] -->
  <text>function(val) {</text>
    <text>return (val==='')</text>
      <text>||(val===undefined)||(val===null)</text>
    <text>;</text>
  <text>}</text>
</template>

<!--
  Uh oh. Hopefully this never happens; will throw an exception if a type is
  defined as a base type (using typedef), but is not handled by the compiler.
-->
<template match="lv:match[ @anyOf=root(.)//lv:typedef[ ./lv:base-type ]/@name ]"
  mode="compiler:match-anyof" priority="3">

  <text>function(val){</text>
    <text>throw Error( 'CRITICAL: Unhandled base type: </text>
      <value-of select="@anyOf" />
    <text>');</text>
  <text>}</text>
</template>

<!--
  Used for user-defined domains
-->
<template match="lv:match[ @anyOf ]" mode="compiler:match-anyof" priority="1">
  <text>types['</text>
    <value-of select="@anyOf" />
  <text>'].values</text>
</template>


<function name="compiler:match-group-op" as="xs:string">
  <param name="class" as="element( lv:classify )" />

  <sequence select="if ( $class/@any = 'true' ) then
                      '||'
                    else
                      '&amp;&amp;'" />
</function>


<!--
  Compiles a function

  Parameters will be converted into actual function parameters. The function
  will return the result of its expression (represented by a calculation in the
  XML).

  If the special param __experimental_guided_tco is defined, recursive calls
  to the same function can set it to a true value to perform tail call
  optimization (TCO).  See js-calc.xsl for more information.

  @return generated function
-->
<template match="lv:function" mode="compile">
  <value-of select="$compiler:nl" />

  <text>function </text>
    <call-template name="calc-compiler:gen-func-name">
      <with-param name="name" select="@name" />
    </call-template>
  <text>( args </text>

  <!-- add parameters -->
  <for-each select="./lv:param">
    <text>, </text>
    <value-of select="@name" />
  </for-each>

  <text>) {</text>

  <variable name="tco" as="xs:boolean"
            select="compiler:function-supports-tco( . )" />

  <if test="$tco">
    <message select="concat('warning: ', @name, ' enabled experimental guided TCO')" />
  </if>

  <!-- top of this function's trampoline, if TCO was requested -->
  <if test="$tco">
      <text>do{__experimental_guided_tco=0;</text>
  </if>

  <text>var fresult=(</text>
    <!-- begin calculation generation (there should be only one calculation node
         as a child, so only it will be considered) -->
    <apply-templates select="./c:*[1]" mode="compile" />
  <text>);</text>

  <!-- bottom of this function's trampoline, if TCO was requested; if the
       flag is set (meaning a relevant tail call was hit), jump back to
       the beginning of the function  -->
  <if test="$tco">
    <text>}while(__experimental_guided_tco);</text>
  </if>

  <text>return fresult;} </text>
</template>



<!--
  Generates a premium calculation

  The result of the generated expression, as denoted by a calculation in the
  XML, will be stored in the variable identified by @yields.

  TODO: If another calculation uses the yielded premium in the document before
  this lv:rate block, then this block needs to be compiled *before* the block
  that references is. We don't want to depend on order, as that would not be
  declarative (in this particular scenario, at least).

  @return generated self-executing premium calculation function
-->
<template match="lv:rate" mode="compile">
  <value-of select="$compiler:nl" />

  <!-- see c:ceil/c:floor precision comments in js-calc -->
  <variable name="precision">
    <choose>
      <when test="@precision">
        <value-of select="concat( '1e', @precision )" />
      </when>

      <otherwise>
        <text>1e8</text>
      </otherwise>
    </choose>
  </variable>

  <apply-templates select="." mode="compile-cmatch" />

  <variable name="predmatch">
    <apply-templates select="." mode="compile-class-condition" />
  </variable>

   <!-- destination var -->
   <variable name="store">
     <!-- TODO: escape single quotes (even though there should never be any) -->
     <text>A['</text>
       <value-of select="@yields" />
     <text>']</text>
   </variable>

  <if test="$predmatch != 'true'">
    <!-- preempt expensive logic, but still return a vector of the proper
         length -->
    <!-- TODO: when writing TAMER, note that this must be improved upon: it
         only detects iterators of immedite children -->
    <text>if(!</text>
    <value-of select="$predmatch" />
    <text>){</text>
      <for-each select="c:sum[@generates]|c:product[@generates]">
        <variable name="value">
          <apply-templates mode="js-name-ref"
                           select="." />
        </variable>

        <text>A['</text>
          <value-of select="@generates" />
        <text>']=stov(0,</text>
          <value-of select="$value" />
        <text>.length);</text>
      </for-each>

      <value-of select="$store" />
      <text>=0;</text>

    <!-- predicate matches -->
    <text>}else{</text>
  </if>

  <!-- store the premium -->
  <value-of select="$store" />
  <text>=precision(</text>
    <value-of select="$precision" />
  <!-- return the result of the calculation for this rate block -->
  <text>,+(</text>
    <!-- yield 0 if there are no calculations (rather than a syntax error!) -->
    <if test="empty( c:* )">
      <message>
        <text>[jsc] warning: empty rate block: `</text>
        <value-of select="@yields" />
        <text>'</text>
      </message>

      <text>0</text>
    </if>

    <!-- begin calculation generation (there should be only one calculation
         node as a child, so only it will be considered) -->
    <apply-templates select="./c:*[1]" mode="compile" />
  <text>));</text>

  <if test="$predmatch != 'true'">
    <text>}</text>
  </if>
</template>

<template match="lv:rate" mode="compile-class-condition">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="rate" select="." />

  <!-- Generate expression for class list (leave the @no check to the cmatch
       algorithm, since we want per-index @no's).  If @gentle-no is
       set by rate-each expansion, then we want to ignore them entirely,
       since we do not want it to clear our the final yield (generators take
       care of this using _CMATCH_). -->
  <variable name="class-set"
            select="./lv:class[
                      ( @no = 'true'
                        and not( $rate/@gentle-no = 'true' ) )
                      or not( @no = 'true' ) ]" />

  <choose>
    <when test="$class-set">
      <if test="count( $class-set ) > 1">
        <text>(</text>
      </if>

      <for-each select="$class-set">
        <!-- join class expressions with AND operator -->
        <if test="position() > 1">
          <text disable-output-escaping="yes"> &amp;&amp; </text>
        </if>

        <!-- negate if @no -->
        <if test="@no='true'">
          <text>!</text>
        </if>

        <variable name="ref" select="@ref" />

        <if test="$symtable-map( concat( ':class:', $ref ) )
                    /@preproc:generated='true'">
          <text>gen</text>
        </if>

        <text>classes['</text>
          <value-of select="@ref" />
        <text>']</text>
      </for-each>

      <if test="count( $class-set ) > 1">
        <text>)</text>
      </if>
    </when>

    <!-- well, we need to output something -->
    <otherwise>
      <text>true</text>
    </otherwise>
  </choose>
</template>


<!-- Non-predicated lv:rate -->
<template mode="compile-cmatch" priority="7"
          match="lv:rate[ count( lv:class ) = 0 ]">
  <!-- nothing (but the body must not reference it, or the body will have an
       old value!) -->
</template>


<!--
  Single-predicate lv:rate can be aliased

  @no is excluded for simplicity (and it's also generally used with a
  non-@no, and it's uncommon).
-->
<template mode="compile-cmatch" priority="7"
          match="lv:rate[ count( lv:class ) = 1
                          and not( lv:class/@no ) ]">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <text>C['_CMATCH_']=</text>
    <call-template name="compiler:get-class-yield">
      <with-param name="symtable-map" select="$symtable-map" />
      <with-param name="name" select="@ref" />
      <with-param name="search" select="root(.)" />
    </call-template>
  <text>;</text>
</template>


<template match="lv:rate" mode="compile-cmatch" priority="5">
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="root" select="root(.)" />

  <!-- set the magic _CMATCH_ var to represent a list of indexes that meet all
       the classifications (note: this has to be calculated even on a
       non-match, since it is often referenced by c:sum/c:product) -->
  <text>C['_CMATCH_']=cmatch([</text>
    <for-each select="lv:class[ not( @no='true' ) ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <call-template name="compiler:get-class-yield">
        <with-param name="symtable-map" select="$symtable-map" />
        <with-param name="name" select="@ref" />
        <with-param name="search" select="$root" />
      </call-template>
    </for-each>
  <text>], [</text>
    <for-each select="lv:class[ @no='true' ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <call-template name="compiler:get-class-yield">
        <with-param name="symtable-map" select="$symtable-map" />
        <with-param name="name" select="@ref" />
        <with-param name="search" select="$root" />
      </call-template>
    </for-each>
  <text>]);</text>
</template>


<template name="compiler:get-class-yield">
  <param name="symtable-map" as="map(*)" />
  <param name="name" />
  <param name="search" />

  <sequence select="compiler:sym-class-yields-var(
                      $symtable-map( concat( ':class:', $name ) ) )" />
</template>


<!--
  Generates calculation used to yield a final premium

  @return generated expression
-->
<template match="lv:yield" mode="compile">
  <!-- compile yield calculation -->
  <apply-templates select="./c:*[1]" mode="compile" />
</template>



<template match="lv:meta/lv:prop" mode="compile">
  <text>meta['</text>
  <value-of select="@name" />
  <text>']=</text>

  <call-template name="util:json">
    <with-param name="array">
      <!-- values -->
      <for-each select="lv:value">
        <variable name="name" select="@name" />

        <util:value>
          <!-- TODO: refactor -->
          <value-of select="
            root(.)//lv:const[ @name=$name ]/@value
            , root(.)//lv:item[ @name=$name ]/@value" />
        </util:value>
      </for-each>

      <!-- constants -->
      <for-each select="lv:const">
        <util:value>
          <value-of select="@value" />
        </util:value>
      </for-each>
    </with-param>
  </call-template>

  <text>;</text>
</template>



<template match="lvp:*" mode="compile" priority="1">
  <!-- do nothing with UI nodes -->
</template>
<template match="lvp:*" priority="1">
  <!-- do nothing with UI nodes -->
</template>

<!--
  Static code common to each rater

  This is included here because XSLT cannot, without extension, read plain-text
  files (the included file must be XML). If we separated it into another file,
  we would be doing the same thing as we are doing here.

  @return JavaScript code
-->
<template name="compiler:static">
<text>
<![CDATA[
    var domains = {
        'integer': function( value )
        {
            return ( value == +value );
        },

        'float': function( value )
        {
            return ( value == +value );
        },

        'boolean': function( value )
        {
            return ( ( +value === 1 ) || ( +value === 0 ) );
        },

        'string': function( value )
        {
            // well, everything is a string
            return true;
        }
    };


    function precision(p, x)
    {
        if (x % 1 === 0) return x;
        return Math.round(x * p) / p;
    }


    /**
     * Checks for matches against values for any param value
     *
     * A single successful match will result in a successful classification.
     *
     * For an explanation and formal definition of this algorithm, please see
     * the section entitled "Classification Match (cmatch) Algorithm" in the
     * manual.
     *
     * @param {Array|string} param    value or set of values to check
     * @param {Array|string} values   or set of values to match against
     * @param {Object}       yield_to object to yield into
     * @param {boolean}      clear    when true, AND results; otherwise, OR
     *
     * @return {boolean} true if any match is found, otherwise false
     */
    function anyValue( param, values, yield_to, ismatrix, clear, _id )
    {
        // convert everything to an array if needed (we'll assume all objects to
        // be arrays; Array.isArray() is ES5-only) to make them easier to work
        // with
        if ( !Array.isArray( param ) )
        {
            param = [ param ];
        }

        var values_orig = values;
        if ( typeof values !== 'object' )
        {
            // the || 0 here ensures that non-values are treated as 0, as
            // mentioned in the specification
            values = [ values || 0 ];
        }
        else
        {
            var tmp = [];
            for ( var v in values )
            {
                tmp.push( v );
            }

            values = tmp;
        }

        // if no yield var name was provided, we'll just be storing in a
        // temporary array which will be discarded when it goes out of scope
        // (this is the result vector in the specification)
        var store = yield_to || [];

        var i      = param.length,
            found  = false,
            scalar = ( i === 0 ),
            u      = ( store.length === 0 ) ? clear : false;

        while ( i-- )
        {
            // these var names are as they appear in the algorithm---temporary,
            // and value
            var t,
                v = returnOrReduceOr( store[ i ], u );

            // recurse on vectors
            if ( Array.isArray( param[ i ] ) || Array.isArray( store[ i ] ) )
            {
                var r = deepClone( store[ i ] || [] );
                if ( !Array.isArray( r ) )
                {
                    r = [ r ];
                }

                var rfound = !!anyValue( param[ i ], values_orig, r, false, clear, _id );
                found = ( found || rfound );

                if ( Array.isArray( store[ i ] )
                  || ( store[ i ] === undefined )
                )
                {
                    // we do not want to reduce; this is the match that we are
                    // interested in
                    store[ i ] = r;
                    continue;
                }
                else
                {
                    t = returnOrReduceOr( r, clear );
                }
            }
            else
            {
                // we have a scalar, folks!
                scalar = true;
                t = anyPredicate( values, ( param[ i ] || 0 ), i );
            }

            store[ i ] = +( ( clear )
                ? ( v && t )
                : ( v || t )
            );

            // equivalent of "Boolean Classification Match" section of manual
            found = ( found || !!store[ i ] );
        }

        if ( store.length > param.length )
        {
            var sval = ( scalar ) ? anyPredicate( values, param[0] ) : null;
            if ( typeof sval === 'function' )
            {
                // pass the scalar value to the function
                sval = values[0]( param[0] );
            }

            // XXX: review the algorithm; this is a mess
            for ( var k = param.length, l = store.length; k < l; k++ )
            {
                // note that this has the same effect as initializing (in the
                // case of a scalar) the scalar to the length of the store
                var v = +(
                  ( returnOrReduceOr( store[ k ], clear )
                    || ( !clear && ( scalar && sval ) )
                  )
                  && ( !clear || ( scalar && sval ) )
                );

                store[ k ] = ( scalar )
                  ? v
                  : [ v ];

                found = ( found || !!v );
            }
        }

        return found;
    }


    function anyPredicate( preds, value, index )
    {
        return preds.some( function( p ) {
            return (typeof p === 'function')
                ? p(value, index)
                : p == value;
        } );
    }


    function returnOrReduceOr( arr, c )
    {
        if ( arr === undefined )
        {
            return !!c;
        }
        else if ( !( arr.length ) )
        {
            return arr;
        }

        return arr.reduce( function( a, b ) {
            return a || returnOrReduceOr( b, c );
        } );
    }


    function returnOrReduceAnd( arr, c )
    {
        if ( arr === undefined )
        {
            return !!c;
        }
        else if ( !( arr.length ) )
        {
            return arr;
        }

        return arr.reduce( function( a, b ) {
            return a && returnOrReduceAnd( b, c );
        } );
    }


    function deepClone( arr )
    {
        if ( !Array.isArray( arr ) ) return arr;
        return arr.map( deepClone );
    }


    /**
     * Converts a match set to an integer
     *
     * If the given set is an array, then return a sum of each of its boolean
     * values (if any one is set, then it's 1); otherwise, cast the given
     * value to a number, just in case it's not.
     *
     * This function does not check to ensure that the given set contains valid
     * data.
     *
     * @param {*} match set to convert
     *
     * @return {number} 1 or 0
     */
    function matchSetToInt( match )
    {
        if ( Array.isArray( match ) )
        {
            return match.reduce( function( a, b ) {
                return a + b;
            }, 0);
        }

        return +match;
    }


    function cmatch( match, nomatch )
    {
        var len    = 0,
            cmatch = [];

        // the length of our set should be the length of the largest set (we
        // will not know this until runtime)
        for ( var i in match )
        {
            // note that this has the consequence of not matching on scalar-only
            // classifications...is this what we want? If not, we need to
            // document a proper solution.
            if ( ( match[ i ] || [] ).length > len )
            {
                len = match[ i ].length;
            }
        }

        for ( var i in nomatch )
        {
            if ( ( nomatch[ i ] || [] ).length > len )
            {
                len = nomatch[ i ].length;
            }
        }

        while ( len-- )
        {
            var fail = false;

            for ( var i in match )
            {
                // if we're dealing with a scalar, then it should be used for
                // every index
                var mdata = ( !Array.isArray( match[ i ] )
                  ? match[ i ]
                  : ( match[ i ] || [] )[ len ]
                );

                if ( !( matchSetToInt( mdata ) ) )
                {
                    fail = true;
                }
            }

            // XXX duplicate code
            for ( var i in nomatch )
            {
                // if we're dealing with a scalar, then it should be used for
                // every index
                var mdata = ( !Array.isArray( nomatch[ i ] )
                  ? nomatch[ i ]
                  : ( nomatch[ i ] || [] )[ len ]
                );

                if ( matchSetToInt( mdata ) !== 0 )
                {
                    fail = true;
                }
            }

            cmatch[ len ] = ( fail ) ? 0 : 1;
        }

        return cmatch;
    }


    /**
     * Return the length of the longest set
     *
     * Provide each set as its own argument.
     *
     * @return number length of longest set
     */
    function longerOf()
    {
        var i   = arguments.length,
            len = 0;
        while ( i-- )
        {
            var thislen = arguments[ i ].length;

            if ( thislen > len )
            {
                len = thislen;
            }
        }

        return len;
    }


    /* scalar to vector */
    function stov( s, n )
    {
        // already a vector
        if ( Array.isArray( s ) )
        {
            // if the length is only one, then we can pretend that it is a
            // scalar (unless the requested length is one, in which case it is
            // utterly pointless to continue)
            if ( ( n === 1 ) || ( s.length !== 1 ) )
            {
                return s;
            }

            s = s[ 0 ];
        }

        return (new Array(n)).fill(s);
    }


    function argreplace( orig, value )
    {
        if ( !( typeof orig === 'object' ) )
        {
            return value;
        }

        // we have an object; recurse
        for ( var i in orig )
        {
            return argreplace( orig[ i ], value );
        }
    }


    function init_defaults( args, params )
    {
        for ( var param in params )
        {
            var param_data = params[ param ];
            var val = param_data['default'] || 0;
            var depth = param_data.depth || 0;

            args[ param ] = set_defaults( args[ param ], val, +depth );
        }
    }


    function set_defaults( input, value, depth )
    {
        // scalar
        if ( depth === 0 )
        {
            return ( input === '' || input === undefined ) ? value : input;
        }

        input = input || [];

        // vector or matrix
        var i = input.length || 1;
        var ret = [];
        var value = ( depth === 2 ) ? [ value ] : value;

        while ( i-- ) {
            ret[i] = ( input[i] === '' || input[i] === undefined ) ? value : input[i];
        }

        return ret;
    }


    /**
     * Map each string in INPUT to uppercase
     *
     * @param {Array|string} input string
     *
     * @return {Array<number>|number} mapped value
     */
    function map_method_uppercase( input )
    {
        if ( Array.isArray( input ) )
        {
            return input.map( map_method_uppercase );
        }

        return ( ''+input ).toUpperCase();
    }


    /**
     * Map each string in INPUT to an integer
     *
     * An integer is constructed by taking the four higher-order bytes from
     * the SHA256 hash of the input (corresponding to eight hexadecimal digits).
     *
     * @param {Array|string} input preimage
     *
     * @return {Array<number>|number} mapped value
     */
    function map_method_hash( input )
    {
        if ( Array.isArray( input ) )
        {
            return input.map( map_method_hash );
        }

        const hash = sha256( ''+input ).substr( 0, 8 );
        return parseInt( hash, 16 );
    }
]]>
</text>

<sequence select="unparsed-text(
                    concat( $__path-root, '/src/js/sha256.js' ) )" />
</template>

</stylesheet>
