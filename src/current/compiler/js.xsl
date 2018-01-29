<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript

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


<!--
  Generates rater function

  The rater is a single function that may be invoked with a key-value argument
  list and will return a variety of data, including the final premium.

  @param NodeSet pkgs all packages, including rater

  @return compiled JS
-->
<template match="lv:package" mode="compiler:entry">
  <!-- enclose everything in a self-executing function to sandbox our data -->
  <text>( function() { </text>
    <!-- to store debug information for equations (we have to put this out here
         so that functions also have access to it...yes, it's stateful, yes it's
         bullshit, but oh well) -->
    <text>var debug = {};</text>
    <text>var consts = {};</text>
    <text>var params = {};</text>
    <text>var types = {};</text>
    <text>var meta = {};</text>
    <!--
    <value-of select="$compiler:nl" />
    <apply-templates
      select="root(.)//lv:const[ .//lv:item or preproc:iou ]"
      mode="compile" />
    -->

</template>


<template match="lv:package" mode="compiler:entry-rater">
    <!-- the rater itself -->
    <value-of select="$compiler:nl" />
    <text>function rater( arglist, _canterm ) {</text>
      <text>_canterm = ( _canterm == undefined ) ? true : !!_canterm;</text>

      <!-- XXX: fix; clear debug from any previous runs -->
      <text>debug = {};</text>

      <!-- magic constants (N.B. these ones must be re-calculated with each
           call, otherwise the data may be incorrect!) -->
      <value-of select="$compiler:nl" />

      <!-- XXX: Remove this; shouldn't be magic -->
      <text>consts['__DATE_YEAR__'] = ( new Date() ).getFullYear(); </text>

      <!-- clone the object so as not to modify the one that was passed
           (ES5 feature); also adds constants -->
      <text>var args = Object.create( arglist );</text>

      <!-- will store the global params that we ended up requiring -->
      <text>var req_params = {};</text>

      <!-- handle defaults -->
      <text>init_defaults( args, params );</text>

      <!-- perform classifications -->
      <value-of select="$compiler:nl" />
      <text>var classes = rater.classify( args, _canterm );</text>
      <!-- for @external generated clases -->
      <text>var genclasses = {};</text>
</template>

<template match="lv:package" mode="compiler:entry-classifier">
  <!-- allow classification of any arbitrary dataset -->
  <value-of select="$compiler:nl" />
  <text>rater.classify = function( args, _canterm ) {</text>
    <text>_canterm = ( _canterm == undefined ) ? true : !!_canterm;</text>

    <!-- XXX: Remove this; shouldn't be magic -->
    <text>consts['__DATE_YEAR__'] = ( new Date() ).getFullYear(); </text>

    <!-- object into which all classifications will be stored -->
    <text>var classes = {}, genclasses = {}; </text>

    <!-- TODO: We need to do something with this... -->
    <text>var req_params = {}; </text>
</template>

<template match="lv:package" mode="compiler:exit-classifier">
    <text>return classes;</text>
  <text> };</text>

  <!-- TODO: make sure fromMap has actually been compiled -->
  <text>rater.classify.fromMap = function( args_base, _canterm ) { </text>
    <text>var ret = {}; </text>
    <text>rater.fromMap( args_base, function( args ) {</text>
      <text>var classes = rater.classify( args, _canterm ); </text>

      <text>
        for ( var c in classes )
        {
          ret[ c ] = {
            is: !!classes[ c ],
            indexes: args[ rater.classify.classmap[ c ] ]
          };
        }
      </text>
    <text>} );</text>
    <text>return ret;</text>
  <text> }; </text>
</template>

<template match="lv:package" mode="compiler:exit-rater">
  <param name="symbols" as="element( preproc:sym )*" />

      <value-of select="$compiler:nl" />
      <text>return { </text>
        <!-- round the premium (special symbol ___yield) to max of 2 decimal places -->
        <text>premium: ( Math.round( args.___yield * 100 ) / 100 ), </text>
        <text>classes: classes, </text>
        <text>vars: args, </text>
        <text>consts: consts, </text>
        <text>reqParams: req_params, </text>
        <text>debug: debug </text>
      <text>}; </text>
    <text>}</text>

    <!-- make the name of the supplier available -->
    <text>rater.supplier = '</text>
      <value-of select="substring-after( @name, '/' )" />
    <text>'; </text>

    <text>rater.meta = meta;</text>

    <!-- provide classification -> yields mapping -->
    <value-of select="$compiler:nl" />
    <text>rater.classify.classmap = { </text>
      <apply-templates select="." mode="compiler:classifier-yields-map">
        <with-param name="symbols" select="$symbols" />
      </apply-templates>
    <text> }; </text>

    <!-- provide classification descriptions -->
    <value-of select="$compiler:nl" />
    <text>rater.classify.desc = { </text>
    <sequence select="
        compiler:class-desc(
          $symbols[ @type='class' ] )" />
    <text> }; </text>

    <variable name="mapfrom" select="
        preproc:symtable/preproc:sym[
          @type='map'
        ]/preproc:from[
          not(
            @name = parent::preproc:sym
              /preceding-sibling::preproc:sym[
                @type='map'
              ]/preproc:from/@name
          )
        ]
      " />

    <!-- mapped fields (external names) -->
    <text>rater.knownFields = {</text>
      <for-each select="$mapfrom">
        <if test="position() > 1">
          <text>, </text>
        </if>

        <text>'</text>
          <value-of select="@name" />
        <text>': true</text>
      </for-each>
    <text>}; </text>

    <!-- the rater has been generated; return it -->
    <text>return rater;</text>
  <text>} )()</text>
</template>


<template match="lv:package" mode="compiler:classifier-yields-map">
  <param name="symbols" />

  <!-- for each cgen symbol (which is the classification @yields), map the
       classification name (the @parent) to the cgen symbol name -->
  <for-each select="$symbols[ @type='cgen' ]">
    <if test="position() > 1">
      <text>,</text>
    </if>

    <text>'</text>
      <value-of select="substring-after( @parent, ':class:' )" />
    <text>':'</text>
      <value-of select="@name" />
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
      <value-of select="translate( @desc, &quot;'&quot;, '' )" />
    <text>'</text>
  </for-each>
</function>


<!--
  Compile global parameter list into an object literal

  @return key and value for the given parameter
-->
<template match="lv:param" mode="compile">
  <!-- generate key using param name -->
  <text>params['</text>
  <value-of select="@name" />
  <text>'] = {</text>

  <!-- param properties -->
  <text>type: '</text>
    <value-of select="@type" />
  <text>',</text>

  <text>'default': '</text>
    <value-of select="@default" />
  <text>',</text>

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
  <text>'] = {</text>

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
  <text>'] = {</text>

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
  <text>'] = {</text>

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
  <text>'</text>
  <value-of select="$value" />
  <text>': true</text>
</template>


<!--
  Generate an object containing values of constant sets

  This is done instead of inlining constant values as we do with non-sets since
  the specific index can be determined at runtime.

  @return JS object assignment for constant set values
-->
<template mode="compile" priority="1"
          match="lv:const[ element() or @values ]">
  <text>consts['</text>
    <value-of select="@name" />
  <text>'] = [ </text>

    <!-- matrices -->
    <for-each select="compiler:const-sets( . )[ not( . = '' ) ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <text>[ </text>
        <for-each select="compiler:set-items( ., true() )">
          <if test="position() > 1">
            <text>, </text>
          </if>

          <value-of select="." />
        </for-each>
      <text> ]</text>
    </for-each>

    <!-- vectors -->
    <for-each select="compiler:set-items( ., false() )">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <value-of select="." />
    </for-each>

  <text> ]; </text>
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
  Generate code to perform a classification

  Based on the criteria provided by the classification, generate and store the
  result of a boolean expression performing the classification using global
  arguments.

  TODO: Refactor; both @yields and $result-set checks are unneeded; they can be
  combined (@yields as the default, which may or may not exist)

  @return generated classification expression
-->
<template match="lv:classify" mode="compile">
  <param name="noclass" />
  <param name="result-set" />
  <param name="ignores" />

  <variable name="self" select="." />

  <value-of select="$compiler:nl" />

  <if test="not( $noclass )">
    <if test="@preproc:generated='true'">
      <text>gen</text>
    </if>

    <text>classes['</text>
      <value-of select="@as" />
    <text>'] = (function(){var result,tmp; </text>
  </if>

  <!-- locate classification criteria -->
  <variable name="criteria" as="element( lv:match )*"
            select="lv:match[
                      not( $ignores )
                      or not( @on=$ignores/@ref ) ]" />

  <variable name="criteria-syms" as="element( preproc:sym )*"
            select="root(.)/preproc:symtable/preproc:sym[
                      @name = $criteria/@on ]" />

  <variable name="dest">
    <choose>
      <when test="$result-set">
        <value-of select="$result-set" />
      </when>

      <otherwise>
        <text>args['</text>
          <value-of select="@yields" />
        <text>']</text>
      </otherwise>
    </choose>
  </variable>

  <!-- generate boolean value from match expressions -->
  <choose>
    <!-- if classification criteria were provided, then use them -->
    <when test="$criteria">
      <variable name="criteria-scalar" as="element( lv:match )*"
                select="$criteria[
                          @on = $criteria-syms[
                              @dim = '0' ]/@name ]" />

      <variable name="op" as="xs:string"
                select="compiler:match-group-op( $self )" />

      <text></text>
        <!-- first, non-scalar criteria -->
        <apply-templates mode="compile"
                         select="$criteria[
                                   not( @on = $criteria-scalar/@on ) ]">
          <with-param name="result-set" select="$result-set" />
          <with-param name="ignores" select="$ignores" />
          <with-param name="operator" select="$op" />
        </apply-templates>

        <!-- scalars must be matched last -->
        <apply-templates mode="compile"
                         select="$criteria-scalar">
          <with-param name="result-set" select="$result-set" />
          <with-param name="ignores" select="$ignores" />
          <with-param name="operator" select="$op" />
        </apply-templates>
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
      <if test="@yields or $result-set">
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
      <text>gen</text>
    </if>
    <text>classes['</text>
      <value-of select="@as" />
    <text>'] ) throw Error( '</text>
      <value-of select="@desc" />
    <text>' );</text>

    <value-of select="$compiler:nl" />
  </if>

    <variable name="sym"
      select="root(.)/preproc:symtable/preproc:sym[ @name=$self/@yields ]" />

  <!-- if we are not any type of set, then yield the value of the first
         index (note the $criteria check; see above); note that we do not do
         not( @set ) here, since that may have ill effects as it implies that
         the node is not preprocessed -->
  <!-- TODO: this can be simplified, since @yields is always provided -->
  <if test="$criteria and ( @yields or $result-set ) and ( $sym/@dim='0' )">
    <value-of select="$dest" />
    <text> = </text>
      <value-of select="$dest" />
    <text>[0];</text>

    <value-of select="$compiler:nl" />
  </if>
</template>


<!--
  Generate domain checks for require-param nodes

  The resulting code will cause an exception to be thrown if the domain check
  fails.

  FIXME: remove

  @return generated domain check code
-->
<template match="lv:required-param" mode="compile">
  <!--
  <variable name="name" select="@ref" />

  <text>vocalDomainCheck( '</text>
    <value-of select="$name" />
  <text>', '</text>
    <value-of select="root(.)//lv:param[ @name=$name ]/@type" />
  <text>', args['</text>
    <value-of select="$name" />
  <text>'] ); </text>
  -->

  <!-- record that this param was required -->
  <!--
  <text>req_params['</text>
    <value-of select="$name" />
  <text>'] = true; </text>
  -->
</template>


<!--
  Generate code asserting a match

  Siblings are joined by default with ampersands to denote an AND relationship,
  unless overridden.

  @return generated match code
-->
<template match="lv:match" mode="compile" priority="1">
  <!-- default to all matches being required -->
  <param name="operator" select="'&amp;&amp;'" />
  <param name="yields" select="../@yields" />
  <param name="result-set" />

  <variable name="name" select="@on" />

  <variable name="sym-on" as="element( preproc:sym )"
            select="root(.)/preproc:symtable/preproc:sym[ @name = $name ]" />

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
            <text>args</text>
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
    <choose>
      <!-- if we were given a result set to use, then use it -->
      <when test="$result-set">
        <value-of select="$result-set" />
      </when>

      <!-- store directly into the destination result set -->
      <otherwise>
        <call-template name="compiler:gen-match-yieldto">
          <with-param name="yields" select="$yields" />
        </call-template>
      </otherwise>
    </choose>
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
      <variable name="sym"
        select="root(.)/preproc:symtable/preproc:sym[ @name=$value ]" />

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
      <apply-templates select="." mode="compiler:match-anyof" />
    </otherwise>
  </choose>

  <text>, ( </text>
  <value-of select="$yieldto" />
  <text> || ( </text>
  <value-of select="$yieldto" />
  <text> = [] ) ), </text>

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
  <text>,"</text>
  <value-of select="$input" />
  <text>"</text>

  <!-- end of anyValue() call -->
  <text> ) </text>

  <!-- end of assuming function call -->
  <if test="lv:assuming">
    <text>})()</text>
  </if>

  <text>;</text>

  <text>/*!+*/( debug['</text>
    <value-of select="@_id" />
  <text>'] || ( debug['</text>
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

<template name="compiler:gen-match-yieldto">
  <param name="yields" />

  <text>args['</text>
    <choose>
      <when test="$yields">
        <value-of select="$yields" />
      </when>

      <otherwise>
        <call-template name="compiler:gen-default-yield">
          <with-param name="name" select="ancestor::lv:classify/@as" />
        </call-template>
      </otherwise>
    </choose>
  <text>']</text>
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
<template match="lv:match[ @anyOf='integer' ]" mode="compiler:match-anyof" priority="5">
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
<template match="lv:match[ @anyOf='empty' ]" mode="compiler:match-anyof" priority="5">
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
  mode="compiler:match-anyof" priority="3">

  <text>function( val ) {</text>
    <text>throw Error( 'CRITICAL: Unhandled base type: </text>
      <value-of select="@anyOf" />
    <text>' );</text>
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

  <text>return ( </text>
    <!-- begin calculation generation (there should be only one calculation node
         as a child, so only it will be considered) -->
    <apply-templates select="./c:*[1]" mode="compile" />
  <text> );</text>

  <text>} </text>
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
        <value-of select="@precision" />
      </when>

      <otherwise>
        <text>8</text>
      </otherwise>
    </choose>
  </variable>

  <variable name="store">
    <!-- TODO: escape single quotes (even though there should never be any) -->
    <text>args['</text>
      <value-of select="@yields" />
    <text>']</text>
  </variable>

  <!-- store the premium -->
  <value-of select="$store" />
  <text> = </text>

  <text>( function rate_</text>
    <!-- dashes, which may end up in generated code from templates, must be
         removed -->
    <value-of select="translate( @yields, '-', '_' )" />
  <text>() {</text>

  <text>var predmatch = ( </text>
    <apply-templates select="." mode="compile-class-condition" />
  <text> ); </text>

  <!-- set the magic _CMATCH_ var to represent a list of indexes that meet all
       the classifications -->
  <text>consts._CMATCH_ = </text>
    <apply-templates select="." mode="compile-cmatch" />
  <text>;</text>

  <!-- return the result of the calculation for this rate block -->
  <text>return (+( </text>
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
  <text> )).toFixed(</text>
    <value-of select="$precision" />
  <text>) * predmatch; } )() </text>

  <text>; </text>
</template>

<template match="lv:rate" mode="compile-class-condition">
  <!-- generate expression for class list (leave the @no check to the cmatch
       algorithm, since we want per-index @no's) -->
  <text>( </text>
    <variable name="class-set" select="./lv:class" />

    <choose>
      <when test="$class-set">
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

          <if test="
              root(.)/preproc:symtable/preproc:sym[
                @name=concat( ':class:', $ref )
              ]/@preproc:generated='true'
            ">
            <text>gen</text>
          </if>

          <text>classes['</text>
            <value-of select="@ref" />
          <text>']</text>
        </for-each>
      </when>

      <!-- well, we need to output something -->
      <otherwise>
        <text>true</text>
      </otherwise>
    </choose>
  <text> )</text>
</template>


<template match="lv:rate" mode="compile-cmatch">
  <variable name="root" select="root(.)" />

  <!-- generate cmatch call that will generate the cmatch set -->
  <text>cmatch( [</text>
    <for-each select="lv:class[ not( @no='true' ) ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <text>args['</text>
        <call-template name="compiler:get-class-yield">
          <with-param name="name" select="@ref" />
          <with-param name="search" select="$root" />
        </call-template>
      <text>']</text>
    </for-each>
  <text>], [</text>
    <for-each select="lv:class[ @no='true' ]">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <text>args['</text>
        <call-template name="compiler:get-class-yield">
          <with-param name="name" select="@ref" />
          <with-param name="search" select="$root" />
        </call-template>
      <text>']</text>
    </for-each>
  <text>] )</text>
</template>


<template name="compiler:get-class-yield">
  <param name="name" />
  <param name="search" />

  <variable name="yields">
    <value-of select="
        root(.)/preproc:symtable/preproc:sym[
          @name=concat( ':class:', $name )
        ]/@yields
      " />
  </variable>

  <choose>
    <when test="$yields != ''">
      <value-of select="$yields" />
    </when>

    <otherwise>
      <call-template name="compiler:gen-default-yield">
        <with-param name="name" select="$name" />
      </call-template>
    </otherwise>
  </choose>
</template>


<template name="compiler:gen-default-yield">
  <param name="name" />

  <!-- a random name that would be invalid to reference from the XML -->
  <text>___$$</text>
  <value-of select="$name" />
  <text>$$</text>
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
  <text>'] = </text>

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


    function argCheck( args, params )
    {
        var req = {};

        for ( var name in params )
        {
            // if the argument is not required, then we do not yet need to deal
            // with it
            if ( !( params[ name ].required ) )
            {
                continue;
            }

            // first, ensure that required arguments have been provided (note
            // the strict check for .length; this is important, since it won't
            // have a length property if it's not an array, in which case we do
            // not want to trigger an error)
            if ( !( args[ name ] ) || ( args[ name ].length === 0 ) )
            {
                throw Error( "argument required: " + name );
            }

            var value = args[ name ];

            // next, ensure that the argument is within the domain of its type
            vocalDomainCheck( name, params[ name ].type, deepClone( value ) );

            // record that we required this param
            req[ name ] = true;
        }

        return req;
    }


    function vocalDomainCheck( name, domain, value )
    {
        var default_val = ( rater.params[ name ] || {} )['default'];

        if ( !( domainCheck( domain, value, default_val ) ) )
        {
            throw Error(
                "argument '" + name + "' value outside domain of '" +
                domain + "': " + JSON.stringify( value )
            );
        }

        return true;
    }


    function domainCheck( domain, value, default_val )
    {
        var type;

        // if it's an object, then the value is assumed to be an array
        if ( typeof value === 'object' )
        {
            if ( value.length < 1 )
            {
                return true;
            }

            // clone before popping so that we don't wipe out any values that
            // will be used
            value = Array.prototype.slice.call( value );

            // check each value recursively
            return domainCheck( domain, value.pop(), default_val )
                && domainCheck( domain, value, default_val );
        }

        if ( ( ( value === undefined ) || ( value === '' ) )
          && ( default_val !== undefined )
        )
        {
            value = +default_val;
        }

        if ( domains[ domain ] )
        {
            return domains[ domain ]( value );
        }
        else if ( type = types[ domain ] ) /** XXX: types global **/
        {
            // custom type checks are two-fold: ensure that the value is within
            // the domain of its base type and that it is within its list of
            // acceptable values
            return !!( domainCheck( type.type, value )
                && type.values[ value ]
            );
        }

        // no domain found
        return false;
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
        if ( ( param === undefined ) || ( param === null ) )
        {
            // according to the specification, an undefined input vector should
            // yield an empty result set, which in turn will be interpreted as
            // false (yield_to is the result vector)
            param = [];
        }
        else if ( typeof param !== 'object' )
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
            if ( typeof param[ i ] === 'object' )
            {
                var r = deepClone( store[ i ] || [] );
                if ( typeof r !== 'object' )
                {
                    r = [ r ];
                }

                var rfound = !!anyValue( param[ i ], values_orig, r, false, clear, _id );
                found = ( found || rfound );

                if ( ( typeof store[ i ] === 'object' )
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
        for ( var i in preds )
        {
            var p = preds[ i ];

            if ( ( typeof p === 'function' )
              && p( value, index )
            )
            {
                return true;
            }
            // lazy equality intentional
            else if ( p == value )
            {
                return true;
            }
        }

        return false;
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

        return reduce( arr, function( a, b )
        {
            return returnOrReduceOr( a, c ) || returnOrReduceOr( b, c );
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

        return reduce( arr, function( a, b )
        {
            return returnOrReduceAnd( a, c ) && returnOrReduceAnd( b, c );
        } );
    }


    function deepClone( obj )
    {
        var objnew = [];

        // if we were not given an object, then do nothing
        if ( typeof obj !== 'object' )
        {
            return obj;
        }

        for ( var i in obj )
        {
            // deep-clone for matrices
            objnew[ i ] = ( typeof obj[ i ] === 'object' )
              ? deepClone( obj[ i ] )
              : obj[ i ];
        }

        return objnew;
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
            return reduce( match, function( a, b )
            {
                return a + b;
            } );
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
                var mdata = ( ( typeof match[ i ] !== 'object' )
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
                var mdata = ( ( typeof nomatch[ i ] !== 'object' )
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


    /**
     * Some browsers don't support Array.reduce(), and adding to the prototype
     * causes problems since we cannot make it non-enumerable in those browsers
     * due to broken Object.defineProperty implementations (IE8).
     */
    function reduce( arr, c )
    {
        var ret = arr[ 0 ],
            i = 0, // skip first
            l = arr.length;

        while ( ++i < l )
        {
            ret = c( ret, arr[ i ] );
        }

        // note that this will have the effet of returning the first element if
        // there are none/no more than 1
        return ret;
    }


    /* scalar to vector */
    function stov( s, n )
    {
        // already a vector
        if ( typeof s === 'object' )
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

        var v = [];
        for ( var i = 0; i < n; i++ )
        {
            v.push( s );
        }

        return v;
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
            var val = params[ param ]['default'];
            if ( !val )
            {
                continue;
            }

            args[ param ] = set_defaults( args[ param ], val );
        }
    }


    function set_defaults( input, value )
    {
        // scalar
        if ( !( typeof input === 'object' ) )
        {
            return ( input === '' || input === undefined ) ? value : input;
        }

        // otherwise, assume array
        var i = input.length;
        var ret = [];
        while ( i-- ) {
            ret[i] = ( input[i] === '' ) ? value : input[i];
        }
        return ret;
    }
]]>
</text>
</template>

</stylesheet>
