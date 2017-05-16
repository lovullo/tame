<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles calculation XML into JavaScript

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).

  It is assumed that validations are performed prior to compiling; otherwise, it
  is possible to inject JavaScript into the output; only pass correct input to
  this compiler.

  All undefined values in a set are considered to be zero, resulting in an
  infinite set; this simplifies sums and compilation.

  The generated code may not be optimal, but it may be processed by another
  system (e.g. Closure Compiler) to perform additional optimizations.
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:calc-compiler="http://www.lovullo.com/calc/compiler">


<!-- enable debugging by default -->
<xsl:param name="calcc-debug" select="'yes'" />

<!-- name to output when reporting problems -->
<xsl:variable name="calc-compiler:name" select="'js-calc-compiler'" />

<xsl:template name="calc-compiler:error">
  <xsl:param name="message" select="'unspecified error'" />

  <xsl:message terminate="yes">
    <xsl:value-of select="$calc-compiler:name" />
    <xsl:text>: </xsl:text>
    <xsl:value-of select="$message" />
  </xsl:message>
</xsl:template>


<xsl:template match="c:*" mode="compile" priority="1">
  <xsl:variable name="debugval">
    <xsl:if test="
        ( $calcc-debug = 'yes' )
        or ( //w:worksheet//w:display/@name = @generates )
        or (
          (
            local-name() = 'case'
            or ./c:index
          )
          and //w:worksheet//w:display/@name = ancestor::c:*/@generates
        )
      ">

      <xsl:text>yes</xsl:text>
    </xsl:if>
  </xsl:variable>

  <!-- should we force debugging? TODO: decouple from lv namespace -->
  <xsl:variable name="debug-force" select="
      ancestor::lv:*/@yields =
        root(.)//calc-compiler:force-debug/calc-compiler:ref/@name
    " />

  <xsl:if test="$debugval = 'yes' or $debug-force">
    <xsl:text>( function() { var result = </xsl:text>
  </xsl:if>

  <xsl:apply-templates select="." mode="compile-pre" />

  <xsl:if test="$debugval = 'yes' or $debug-force">
    <xsl:text>; </xsl:text>
    <xsl:text>/*!+*/( debug['</xsl:text>
      <xsl:value-of select="@_id" />
    <xsl:text>'] || ( debug['</xsl:text>
      <xsl:value-of select="@_id" />
    <xsl:text>'] = [] ) ).push( result );/*!-*/ </xsl:text>

    <xsl:text>return result; </xsl:text>
    <xsl:text>} )() </xsl:text>
  </xsl:if>
</xsl:template>


<!--
  Begins compilation of a calculation

  This is responsible for wrapping the value to enforce precedence rules and
  convert it to a number (to avoid string concatenation when adding), then
  delegating the compilation to the appropriate template(s).

  @return generated JS for the given calculation
-->
<xsl:template match="c:*" mode="compile-pre" priority="1">
  <!-- ensure everything is grouped (for precedence) and converted to a
       number -->
  <xsl:text>( </xsl:text>
  <xsl:apply-templates select="." mode="compile-calc" />
  <xsl:text> )</xsl:text>
</xsl:template>


<xsl:template match="c:const[ ./c:when ]|c:value-of[ ./c:when ]" mode="compile-pre" priority="5">
  <xsl:text>( </xsl:text>
    <!-- first, do what we normally would do (compile the value) -->
    <xsl:text>( </xsl:text>
      <xsl:apply-templates select="." mode="compile-calc" />
    <xsl:text> )</xsl:text>

    <!-- then multiply by the c:when result -->
    <xsl:text> * ( </xsl:text>
      <xsl:for-each select="./c:when">
        <xsl:if test="position() > 1">
          <xsl:text> * </xsl:text>
        </xsl:if>

        <xsl:apply-templates select="." mode="compile" />
      </xsl:for-each>
    <xsl:text> )</xsl:text>
  <xsl:text> )</xsl:text>
</xsl:template>



<!--
  Generates code for the sum or product of a set

  This applies only with sets provided via the @of attribute, which will be used
  to denote what set to some over. If no @index is provided, a dummy index will
  be used. Otherwise, the index provided will be used and may be referenced by
  children.

  If child nodes exist, the summation/product will be performed over their
  resulting equation. Otherwise, the summation/product will be performed over
  the set of values only.

  The generated self-executing function may be used inline with the rest of the
  expression; the computed value will be returned.

  @return generated summation/product anonymous self-executing function
-->
<xsl:template match="c:product[@of]|c:sum[@of]" mode="compile-calc" priority="1">
  <!-- see c:ceil/c:floor precision comments -->
  <xsl:variable name="precision">
    <xsl:choose>
      <xsl:when test="@precision">
        <xsl:value-of select="@precision" />
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>8</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="operator">
    <xsl:apply-templates select="." mode="compile-getop" />
  </xsl:variable>

  <xsl:variable name="index">
    <xsl:choose>
      <xsl:when test="@index">
        <xsl:value-of select="@index" />
      </xsl:when>

      <!-- if they don't provide us with an index, then we don't want them using
           it -->
      <xsl:otherwise>
        <xsl:text>_$i$_</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- introduce scope both to encapsulate values and so we can insert this as
       part of a larger expression (will return a value) -->
  <xsl:text>( function() {</xsl:text>

    <!-- will store result of the summation/product -->
    <xsl:text>var sum = 0;</xsl:text>

    <xsl:variable name="of" select="@of" />

    <xsl:variable name="func" select="ancestor::lv:function" />

    <xsl:variable name="value">
      <xsl:choose>
        <!-- is @of a function param? -->
        <xsl:when test="
            $func
            and root(.)/preproc:symtable/preproc:sym[
              @type='lparam'
              and @name=concat( ':', $func/@name, ':', $of )
            ]
          ">

          <xsl:value-of select="@of" />
        </xsl:when>

        <!-- maybe a constant? -->
        <xsl:when test="
            root(.)/preproc:symtable/preproc:sym[
              @type='const'
              and @name=$of
            ]
          ">

          <xsl:text>consts['</xsl:text>
            <xsl:value-of select="@of" />
          <xsl:text>']</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>args['</xsl:text>
            <xsl:value-of select="@of" />
          <xsl:text>']</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <!-- if we're looking to generate a set, initialize it -->
    <xsl:if test="@generates">
      <xsl:text>var G = []; </xsl:text>
    </xsl:if>

    <!-- loop through each value -->
    <xsl:text>for ( var </xsl:text>
      <xsl:value-of select="$index" />
    <xsl:text> in </xsl:text>
      <xsl:value-of select="$value" />
    <xsl:text> ) {</xsl:text>

    <xsl:text>var result = +(+( </xsl:text>
      <xsl:choose>
        <!-- if there are child nodes, use that as the summand/expression -->
        <xsl:when test="./c:*">
          <xsl:apply-templates select="." mode="compile-calc-sumprod" />
        </xsl:when>

        <!-- otherwise, sum/multiply ever value in the set identified by @of -->
        <xsl:otherwise>
          <xsl:value-of select="$value" />
          <xsl:text>[</xsl:text>
            <xsl:value-of select="$index" />
          <xsl:text>]</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    <xsl:text> )).toFixed(</xsl:text>
      <xsl:value-of select="$precision" />
    <xsl:text>);</xsl:text>

    <!-- if generating a set, store this result -->
    <xsl:if test="@generates">
      <xsl:text>G.push( result ); </xsl:text>
    </xsl:if>

    <!-- generate summand -->
    <xsl:text>sum </xsl:text>
      <xsl:value-of select="$operator" />
    <xsl:text>= +result;</xsl:text>

    <!-- end of loop -->
    <xsl:text>}</xsl:text>

    <!-- if a set has been generated, store it -->
    <xsl:if test="@generates">
      <xsl:text>args['</xsl:text>
        <xsl:value-of select="@generates" />
      <xsl:text>'] = G; </xsl:text>
    </xsl:if>

    <xsl:text>return sum;</xsl:text>
  <xsl:text>} )()</xsl:text>
</xsl:template>


<!--
  Compile dot products

  N.B. This match has a higher priority than other products.

  Dot products should operate only on vectors; if other input passes the
  validator, then the result is undefined.
-->
<xsl:template match="c:product[@dot]" mode="compile-calc" priority="5">
  <xsl:text>( function() { </xsl:text>

  <!-- we need to determine which vector is the longest to ensure that we
       properly compute every value (remember, undefined will be equivalent to
       0, so the vectors needn't be of equal length *gasp* blasphemy!) -->
  <xsl:text>var _$dlen$ = longerOf( </xsl:text>
    <xsl:for-each select=".//c:value-of">
      <xsl:if test="position() > 1">
        <xsl:text>, </xsl:text>
      </xsl:if>

      <!-- skip any wrapping; we want the direct reference (so compile-calc, not
           compile)-->
      <xsl:apply-templates select="." mode="compile-calc" />
    </xsl:for-each>
  <xsl:text> ); </xsl:text>

  <!-- will store the total sum -->
  <xsl:text>var _$dsum$ = 0;</xsl:text>

  <!-- sum the product of each -->
  <xsl:text disable-output-escaping="yes">for ( var _$d$ = 0; _$d$ &lt; _$dlen$; _$d$++ ) {</xsl:text>
    <xsl:text>_$dsum$ += </xsl:text>
      <!-- product of each -->
      <xsl:for-each select=".//c:value-of">
        <xsl:if test="position() > 1">
          <xsl:text> * </xsl:text>
        </xsl:if>

        <xsl:text>( ( </xsl:text>
          <xsl:apply-templates select="." mode="compile" />
        <xsl:text> || [] )[ _$d$ ] || 0 )</xsl:text>
      </xsl:for-each>
    <xsl:text>; </xsl:text>
  <xsl:text>}</xsl:text>

  <xsl:text>return _$dsum$;</xsl:text>

  <xsl:text> } )()</xsl:text>
</xsl:template>


<!--
  Generates a sum/product over the expression denoted by its child nodes

  When no @of set is provided, this will add or multiple each child expression.

  @return generated expression
-->
<xsl:template match="c:sum|c:product" mode="compile-calc">
  <xsl:apply-templates select="." mode="compile-calc-sumprod" />
</xsl:template>


<xsl:template match="c:sum|c:product" mode="compile-calc-sumprod">
  <xsl:variable name="operator">
    <xsl:apply-templates select="." mode="compile-getop" />
  </xsl:variable>

  <xsl:for-each select="./c:*">
    <!-- add operator between each expression -->
    <xsl:if test="position() > 1">
      <xsl:text> </xsl:text>
      <xsl:value-of select="$operator" />
      <xsl:text> </xsl:text>
    </xsl:if>

    <xsl:apply-templates select="." mode="compile" />
  </xsl:for-each>
</xsl:template>


<!--
  @return addition operator
-->
<xsl:template match="c:sum" mode="compile-getop">
  <xsl:text>+</xsl:text>
</xsl:template>

<!--
  @return multiplication operator
-->
<xsl:template match="c:product" mode="compile-getop">
  <xsl:text>*</xsl:text>
</xsl:template>


<!--
  Generate code for ceiling/floor functions

  @return ceil/floor compiled JS
-->
<xsl:template match="c:ceil|c:floor" mode="compile-calc">
  <!-- determine precision for ceil/floor (floating point precision errors can
       drastically affect the outcome) -->
  <xsl:variable name="precision">
    <xsl:choose>
      <!-- if a precision was explicitly provided, then use that -->
      <xsl:when test="@precision">
        <xsl:value-of select="@precision" />
      </xsl:when>

      <!-- ECMAScript uses a default precision of 24; by reducing the
           precision to 8 decimal places, we can drastically reduce the affect
           of precision errors on the calculations -->
      <xsl:otherwise>
        <xsl:text>8</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:text>Math.</xsl:text>
  <xsl:value-of select="local-name()" />
  <xsl:text>( +(</xsl:text>
    <xsl:apply-templates select="./c:*" mode="compile" />
  <xsl:text> ).toFixed( </xsl:text>
    <xsl:value-of select="$precision" />
  <xsl:text> ) )</xsl:text>
</xsl:template>


<!--
  Replaces a constant with its value in the compiled expression

  Constants are syntatic sugar in the language to prevent magic values; they are
  not required at runtime. As such, the value of the constant will be placed
  directly into the compiled code.

  @return quoted constant value
-->
<xsl:template match="c:const" mode="compile-calc">
  <!-- assumed to be numeric -->
  <xsl:value-of select="@value" />
</xsl:template>


<!--
  Generate JS representing the value associated with the given name

  @return generated JS representing value or 0
-->
<xsl:template match="c:value-of" mode="compile-calc" priority="1">
  <xsl:apply-templates select="." mode="compile-calc-value" />
</xsl:template>

<!-- TODO: this should really be decoupled -->
<!-- TODO: does not properly support matrices -->
<xsl:template match="c:value-of[ ancestor::lv:match ]" mode="compile-calc" priority="5">
  <xsl:variable name="name" select="@name" />

  <xsl:choose>
    <!-- scalar -->
    <xsl:when test="
        root(.)/preproc:symtable/preproc:sym[ @name=$name ]
          /@dim = '0'
      ">
      <xsl:apply-templates select="." mode="compile-calc-value" />
    </xsl:when>

    <!-- non-scalar -->
    <xsl:otherwise>
      <xsl:text>(</xsl:text>
        <xsl:apply-templates select="." mode="compile-calc-value" />
      <xsl:text>)[__$$i]</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Generate JS representing the value of a function argument

  This will match whenever value-of is used on a name that matches any function
  parameter.

  XXX: We want to remain decoupled from lv if possible.

  @return generated JS representing argument value or 0
-->
<xsl:template mode="compile-calc-value"
  match="c:*[@name=ancestor::lv:function/lv:param/@name]">

  <!-- use the argument passed to the function -->
  <xsl:apply-templates select="." mode="compile-calc-index">
    <xsl:with-param name="value" select="@name" />
  </xsl:apply-templates>

  <xsl:text> || 0</xsl:text>
</xsl:template>


<!--
  Using value from let expressions
-->
<xsl:template mode="compile-calc-value"
  match="c:*[ @name=ancestor::c:let/c:values/c:value/@name ]">

  <!-- compile the value with the index (if any) -->
  <xsl:apply-templates select="." mode="compile-calc-index">
    <xsl:with-param name="value" select="@name" />
  </xsl:apply-templates>

  <xsl:text> || 0</xsl:text>
</xsl:template>


<!--
  Generate JS representing the value of a global constant

  Since constants are intended only to prevent magic values during development
  (and are not required at runtime), the value of the constant will be placed
  directly into the compiled code. However, we will *not* do this if the
  constant is a set, since its index may be determined at runtime.

  Note that "magic" constants' values are not inlined.

  @return quoted constant value
-->
<xsl:template mode="compile-calc-value"
  match="
    c:*[
      @name=root(.)/preproc:symtable/preproc:sym[
        @type='const'
        and @dim='0'
      ]/@name
    ]
  ">

  <xsl:variable name="name" select="@name" />
  <xsl:variable name="sym"
    select="root(.)/preproc:symtable/preproc:sym[ @name=$name ]" />

  <!-- it is expected that validations prior to compiling will prevent JS
       injection here -->
  <xsl:choose>
    <!-- "magic" constants should not have their values inlined -->
    <xsl:when test="$sym/@magic='true'">
      <xsl:text>consts['</xsl:text>
        <xsl:value-of select="@name" />
      <xsl:text>']</xsl:text>
    </xsl:when>

    <!-- @value should be defined when @dim=0 -->
    <xsl:otherwise>
      <xsl:value-of select="$sym/@value" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Generates JS representing the value of a constant as part of a set

  Since the index of constant sets can be determined at runtime, we need to
  store all possible values. As such, we shouldn't repeat ourselves by inlining
  all possible values; instead, we'll reference a pre-generated set of values
  for the particular constant.

  @return generated code representing value of a variable, or 0 if undefined
-->
<xsl:template mode="compile-calc-value"
  match="
    c:*[
      @name=root(.)/preproc:symtable/preproc:sym[
        @type='const'
        and not( @dim='0' )
      ]/@name
    ]
  ">

  <xsl:variable name="value">
    <xsl:text>consts['</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>']</xsl:text>
  </xsl:variable>

  <xsl:apply-templates select="." mode="compile-calc-index">
    <xsl:with-param name="value" select="$value" />
  </xsl:apply-templates>

  <!-- undefined values in sets are considered to be 0 -->
  <xsl:text> || 0</xsl:text>
</xsl:template>


<!--
  Generate JS representing the value of a generated index

  @return generated code associated with the value of the generated index
-->
<xsl:template mode="compile-calc-value"
  match="c:*[ @name = ancestor::c:*[ @of ]/@index ]">

  <!-- depending on how the index is generated, it could be a string, so we must
       cast it -->
  <xsl:text>+</xsl:text>
  <xsl:value-of select="@name" />
</xsl:template>


<!--
  Generates JS representing the value of a variable

  If the variable is undefined, the value will be considered to be 0 (this is
  especially important for the summation of sets within this implementation).
  That is: a value will never be considered undefined.

  @return generated code representing value of a variable, or 0 if undefined
-->
<xsl:template match="c:*" mode="compile-calc-value">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:variable name="dim"
                select="$pkg/preproc:symtable/preproc:sym[ @name=$name ]/@dim" />

  <!-- retrieve the value, casting to a number (to avoid potentially nasty
       string concatenation bugs instead of integer/floating point arithmetic)
       as long as we're either not a set, or provide an index for the set -->
  <xsl:variable name="value">
    <xsl:text>args['</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>']</xsl:text>
  </xsl:variable>

  <!-- the awkward double-negatives are intentional, since @index may not
       exist; here's what we're doing:

      - If it's not a set, then indexes are irrelevant; always cast scalars
      - Otherwise
        - If an index was provided and it is not a matrix, cast
        - Otherwise
          - If two indexes were provided and it is a matrix, cast
  -->
  <!-- N.B. it is important to do this outside the value variable, otherwise the
       cast may be done at the incorrect time -->
  <xsl:if test="
      (
        $dim='0'
        or (
          (
            ( @index and not( @index = '' ) )
            or ( ./c:index )
          )
          and not( $dim='2' )
        )
        or (
          ( $dim='2' )
          and ./c:index[2]
        )
      )
      and not(
        parent::c:arg
        and not( @index )
      )
    ">

    <xsl:text>+</xsl:text>
  </xsl:if>

  <xsl:apply-templates select="." mode="compile-calc-index">
    <xsl:with-param name="value" select="$value" />
  </xsl:apply-templates>

  <!-- default to 0 if nothing is set (see notes on bottom of summary page; we
       assume all undefined values in a set to be implicitly 0, which greatly
       simplifies things) -->
  <xsl:text> || 0</xsl:text>
</xsl:template>


<!--
  Include the index if one was provided

  The index is represented as a var in the compiled JS, so treat it as an
  identifier not a string.

  An @index attribute or node may be provided. In the former case, the
  identifier simply represents a variable. In the latter, a more complicate
  expression may be used to generate the index.

  Note that the compiled variable to which the index will be applied is
  included, as a string, to this template. This allows it to be wrapped if
  necessary (using multiple indexes) so that the default value can be properly
  applied, avoiding undefined[N].

  @param string value compiled variable to which index will be applied

  @return index (including brackets), if one was provided
-->
<xsl:template match="c:*" mode="compile-calc-index">
  <xsl:param name="value" />
  <xsl:variable name="index" select="@index" />

  <xsl:choose>
    <xsl:when test="@index">
      <!-- output the value, falling back on an empty array to prevent errors
           when attempting to access undefined values -->
      <xsl:text>(</xsl:text>
        <xsl:value-of select="$value" />
      <xsl:text>||[])</xsl:text>

      <xsl:text>[</xsl:text>

      <xsl:choose>
        <!-- if this index was explicitly defined as such, just output the name
             (since it will have been defined as a variable in the compiled code)
             -->
        <xsl:when test="ancestor::c:*[ @of and @index=$index ]">
          <xsl:value-of select="@index" />
        </xsl:when>

        <!-- if the index references a parameter, simply output the identifier -->
        <xsl:when test="@index=ancestor::lv:function/lv:param/@name">
          <xsl:value-of select="@index" />
        </xsl:when>

        <!-- scalar constant -->
        <xsl:when test="@index = root(.)/preproc:symtable/preproc:sym
                          [ @type='const'
                            and @dim='0' ]
                          /@name">
          <xsl:value-of select="root(.)/preproc:symtable
                                  /preproc:sym[ @name=$index ]
                                  /@value" />
        </xsl:when>

        <!-- otherwise, it's a variable -->
        <xsl:otherwise>
          <xsl:text>args['</xsl:text>
            <xsl:value-of select="@index" />
          <xsl:text>']</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:text>]</xsl:text>
    </xsl:when>

    <!-- if index node(s) were provided, then recursively generate -->
    <xsl:when test="./c:index">
      <xsl:apply-templates select="./c:index[1]" mode="compile-calc-index">
        <xsl:with-param name="wrap" select="$value" />
      </xsl:apply-templates>
    </xsl:when>

    <!-- otherwise, we have no index, so just output the compiled variable -->
    <xsl:otherwise>
      <xsl:value-of select="$value" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="c:index" mode="compile-calc-index">
  <xsl:param name="wrap" />

  <!-- get the next index node, if available -->
  <xsl:variable name="next" select="following-sibling::c:index" />

  <xsl:variable name="value">
    <xsl:value-of select="$wrap" />

    <!-- generate index -->
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="./c:*[1]" mode="compile" />
    <xsl:text>]</xsl:text>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$next">
      <!-- recurse on any sibling indexes, wrapping with default value -->
      <xsl:apply-templates select="$next" mode="compile-calc-index">
        <xsl:with-param name="wrap">
          <!-- wrap the value in parenthesis so that we can provide a default value if
               the index lookup fails -->
          <xsl:text>(</xsl:text>

          <xsl:value-of disable-output-escaping="yes" select="$value" />

          <!-- using 0 as the default is fine, even if we have $next; accessing an index
               of 0 is perfectly fine, since it will be cast to an object; we just must
               make sure that it is not undefined -->
          <xsl:text> || 0 )</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:when>

    <!-- if there is no sibling, just output our value with the index, unwrapped
         (since the caller will handle its default value for us -->
    <xsl:otherwise>
      <xsl:value-of disable-output-escaping="yes" select="$value" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Generates a quotient between two calculations

  The result is simply division between the two nodes, the first being the
  numerator and the second being the denominator.

  @return generate quotient
-->
<xsl:template match="c:quotient" mode="compile-calc">
  <!-- we only accept a numerator and a denominator -->
  <xsl:apply-templates select="./c:*[1]" mode="compile" />
  <xsl:text> / </xsl:text>
  <xsl:apply-templates select="./c:*[2]" mode="compile" />
</xsl:template>


<xsl:template match="c:expt" mode="compile-calc">
  <!-- we only accept a numerator and a denominator -->
  <xsl:text>Math.pow(</xsl:text>
  <xsl:apply-templates select="./c:*[1]" mode="compile" />
  <xsl:text>, </xsl:text>
  <xsl:apply-templates select="./c:*[2]" mode="compile" />
  <xsl:text>)</xsl:text>
</xsl:template>


<!--
  Generates a function application (call)

  Arguments are set by name in the XML, but the generate code uses actual
  function arguments. Therefore, the generated argument list will be generated
  in the order that the function is expecting, filling in unset parameters with
  the constant 0.

  @return generated function application
-->
<xsl:template match="c:apply" mode="compile-calc">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="self" select="." />

  <xsl:call-template name="calc-compiler:gen-func-name">
    <xsl:with-param name="name" select="@name" />
  </xsl:call-template>

  <xsl:text>( args</xsl:text>

  <xsl:variable name="arg-prefix" select="concat( ':', $name, ':' )" />

  <!-- generate argument list in the order that the arguments are expected (they
       can be entered in the XML in any order) -->
  <xsl:for-each select="
      root(.)/preproc:symtable/preproc:sym[
        @type='func'
        and @name=$name
      ]/preproc:sym-ref
    ">

    <xsl:text>, </xsl:text>

    <xsl:variable name="pname" select="substring-after( @name, $arg-prefix )" />
    <xsl:variable name="arg" select="$self/c:arg[@name=$pname]" />

    <xsl:choose>
      <!-- if the call specified this argument, then use it -->
      <xsl:when test="$arg">
        <xsl:apply-templates select="$arg/c:*[1]" mode="compile" />
      </xsl:when>

      <!-- otherwise, there's no value; pass in 0 -->
      <xsl:otherwise>
        <xsl:value-of select="'0'" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>

  <xsl:text> )</xsl:text>

  <!-- if c:when was provided, compile it in such a way that we retain the
       function call (we want the result for providing debug information) -->
  <xsl:if test="./c:when">
    <xsl:text> * </xsl:text>
    <xsl:apply-templates select="./c:when" mode="compile" />
  </xsl:if>
</xsl:template>


<xsl:template match="c:when" mode="compile-calc">
  <!-- note that if we have multiple c:whens, they'll be multiplied together by
       whatever calls this, so we're probably fine -->

  <xsl:text>( function() {</xsl:text>
    <!-- return a 1 or a 0 depending on the result of the expression -->
    <xsl:text>return ( </xsl:text>
      <xsl:text>( </xsl:text>
        <!-- get the value associated with this node -->
        <xsl:apply-templates select="." mode="compile-calc-value" />
      <xsl:text> ) </xsl:text>

      <!-- generate remainder of expression -->
      <xsl:apply-templates select="./c:*[1]" mode="compile-calc-when" />
    <xsl:text>) ? 1 : 0; </xsl:text>
  <xsl:text>} )()</xsl:text>
</xsl:template>


<xsl:template match="c:eq|c:ne|c:lt|c:gt|c:lte|c:gte" mode="compile-calc-when">
  <xsl:variable name="name" select="local-name()" />

  <!-- map to LaTeX equivalent -->
  <xsl:variable name="map">
    <c id="eq">==</c>
    <c id="ne">!=</c>
    <c id="gt">&gt;</c>
    <c id="lt">&lt;</c>
    <c id="gte">&gt;=</c>
    <c id="lte">&lt;=</c>
  </xsl:variable>

  <xsl:value-of disable-output-escaping="yes" select="$map/*[ @id=$name ]" />
  <xsl:text> </xsl:text>

  <xsl:apply-templates select="./c:*[1]" mode="compile" />
</xsl:template>


<xsl:template match="c:*" mode="compile-calc-when">
  <xsl:apply-templates select="." mode="compile-pre" />
</xsl:template>


<xsl:template match="c:cases" mode="compile-calc">
  <xsl:text>( function() {</xsl:text>

    <xsl:for-each select="./c:case">
      <!-- turn "if" into an "else if" if needed -->
      <xsl:if test="position() > 1">
        <xsl:text>else </xsl:text>
      </xsl:if>

      <xsl:text>if (</xsl:text>
        <xsl:for-each select="./c:when">
          <xsl:if test="position() > 1">
            <!-- they all yield integer values, so this should work just fine -->
            <xsl:text> * </xsl:text>
          </xsl:if>

          <xsl:apply-templates select="." mode="compile" />
        </xsl:for-each>
      <xsl:text> ) { return </xsl:text>
        <!-- process on its own so that we can debug its final value -->
        <xsl:apply-templates select="." mode="compile" />
      <xsl:text>; } </xsl:text>
    </xsl:for-each>

    <!-- check for the existence of an "otherwise" clause, which should be
         trivial enough to generate -->
    <xsl:if test="c:otherwise">
      <!-- this situation may occur in templates since it's a convenient and
           easy-to-use template notation (conditional cases, none of which may
           actually match and be output) -->
      <xsl:choose>
        <xsl:when test="c:case">
          <xsl:text>else</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>if ( true )</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:text> { return </xsl:text>
        <xsl:apply-templates select="c:otherwise" mode="compile" />
      <xsl:text>; } </xsl:text>
    </xsl:if>

  <xsl:text> } )() || 0</xsl:text>
</xsl:template>

<xsl:template match="c:case" mode="compile-calc">
  <xsl:apply-templates
    select="./c:*[ not( local-name() = 'when' ) ][1]"
    mode="compile-calc-when" />
</xsl:template>

<xsl:template match="c:otherwise" mode="compile-calc">
  <xsl:apply-templates
    select="c:*[1]"
    mode="compile-calc-when" />
</xsl:template>


<xsl:template match="c:set" mode="compile-calc">
  <xsl:text>[</xsl:text>
    <xsl:for-each select="./c:*">
      <xsl:if test="position() > 1">
        <xsl:text>,</xsl:text>
      </xsl:if>

      <xsl:apply-templates select="." mode="compile" />
    </xsl:for-each>
  <xsl:text>]</xsl:text>
</xsl:template>


<!--
  Just list the Lisp cons (constructs a list)
-->
<xsl:template match="c:cons" mode="compile-calc">
  <xsl:variable name="car" select="./c:*[1]" />
  <xsl:variable name="cdr" select="./c:*[2]" />

  <xsl:text>(function(){</xsl:text>
    <!-- duplicate the array just in case...if we notice a performance impact,
         then we can determine if such a duplication is necessary -->
    <xsl:text>var cdr = Array.prototype.slice.call(</xsl:text>
      <xsl:apply-templates select="$cdr" mode="compile" />
    <xsl:text>, 0);</xsl:text>
    <xsl:text>cdr.unshift( </xsl:text>
      <xsl:apply-templates select="$car" mode="compile" />
    <xsl:text> ); </xsl:text>
    <!-- no longer the cdr -->
    <xsl:text>return cdr; </xsl:text>
  <xsl:text>})()</xsl:text>
</xsl:template>


<xsl:template match="c:car" mode="compile-calc">
  <xsl:text>(</xsl:text>
    <xsl:apply-templates select="c:*[1]" mode="compile" />
  <xsl:text>[0]||0)</xsl:text>
</xsl:template>

<xsl:template match="c:cdr" mode="compile-calc">
  <xsl:apply-templates select="c:*[1]" mode="compile" />
  <xsl:text>.slice(1)</xsl:text>
</xsl:template>


<!--
  Returns the length of any type of set (not just a vector)
-->
<xsl:template match="c:length-of" mode="compile-calc">
  <xsl:text>( </xsl:text>
  <xsl:apply-templates select="./c:*[1]" mode="compile" />
  <xsl:text>.length || 0 )</xsl:text>
</xsl:template>


<xsl:template match="c:let" mode="compile-calc">
  <!-- the first node contains all the values -->
  <xsl:variable name="values" select="./c:values/c:value" />

  <!-- if a @name was provided, then treat it like a named Scheme let
       expression in that it can be invoked with different arguments -->
  <xsl:variable name="fname">
    <xsl:if test="@name">
      <xsl:call-template name="calc-compiler:gen-func-name">
        <xsl:with-param name="name" select="@name" />
      </xsl:call-template>
    </xsl:if>
  </xsl:variable>

  <xsl:text>( </xsl:text>
    <xsl:if test="@name">
      <xsl:value-of select="$fname" />
      <xsl:text> = </xsl:text>
    </xsl:if>
  <xsl:text>function( </xsl:text>
    <!-- generate arguments -->
    <xsl:for-each select="$values">
      <xsl:if test="position() > 1">
        <xsl:text>,</xsl:text>
      </xsl:if>

      <xsl:value-of select="@name" />
    </xsl:for-each>
  <xsl:text> ) { </xsl:text>

    <!-- the second node is the body -->
    <xsl:text>return </xsl:text>
      <xsl:apply-templates select="./c:*[2]" mode="compile" />
    <xsl:text>;</xsl:text>
  <xsl:text>} )</xsl:text>

  <!-- assign the arguments according to the calculations -->
  <xsl:text>( </xsl:text>
    <xsl:for-each select="$values">
      <xsl:if test="position() > 1">
        <xsl:text>,</xsl:text>
      </xsl:if>

      <!-- compile the argument value (the parenthesis are just to make it
           easier to read the compiled code) -->
      <xsl:text>(</xsl:text>
        <xsl:apply-templates select="./c:*[1]" mode="compile" />
      <xsl:text>)</xsl:text>
    </xsl:for-each>
  <xsl:text> ) </xsl:text>
</xsl:template>


<!--
  Generate a function name for use as the name of a function within the compiled
  code

  @return generated function name
-->
<xsl:template name="calc-compiler:gen-func-name">
  <xsl:param name="name" />

  <xsl:text>func_</xsl:text>
  <xsl:value-of select="$name" />
</xsl:template>


<xsl:template match="c:debug-to-console" mode="compile-calc">
  <xsl:text>(function(){</xsl:text>
    <xsl:text>var result = </xsl:text>
      <xsl:apply-templates select="./c:*[1]" mode="compile" />
    <xsl:text>;</xsl:text>

    <!-- log the result and return it so that we do not inhibit the calculation
         (allowing it to be inlined anywhere) -->
    <xsl:text>console.log( </xsl:text>
      <xsl:if test="@label">
        <xsl:text>'</xsl:text>
          <xsl:value-of select="@label" />
        <xsl:text>', </xsl:text>
      </xsl:if>

      <xsl:text>result ); </xsl:text>
    <xsl:text>return result; </xsl:text>
  <xsl:text>})()</xsl:text>
</xsl:template>


<!--
  Catch-all for calculations

  If an unmatched calculation element is used, then the generated code will
  output an error to the console (if a console is available) and return a value
  that may be used within context of an equation; this allows it to be placed
  inline.

  @return self-executing anonymous error function
-->
<xsl:template match="c:*" mode="compile-calc">
  <xsl:text>( function () {</xsl:text>
    <xsl:text>throw Error( "Unknown calculation: </xsl:text>
      <xsl:value-of select="name()" />
    <xsl:text>" ); </xsl:text>
  <xsl:text>} )() </xsl:text>
</xsl:template>

</xsl:stylesheet>
