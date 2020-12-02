<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles calculation XML into JavaScript

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

  It is assumed that validations are performed prior to compiling; otherwise, it
  is possible to inject JavaScript into the output; only pass correct input to
  this compiler.

  All undefined values in a set are considered to be zero, resulting in an
  infinite set; this simplifies sums and compilation.

  The generated code may not be optimal, but it may be processed by another
  system (e.g. Closure Compiler) to perform additional optimizations.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:compiler="http://www.lovullo.com/rater/compiler"
            xmlns:calc-compiler="http://www.lovullo.com/calc/compiler">


<!-- enable debugging by default -->
<param name="calcc-debug" select="'yes'" />

<!-- name to output when reporting problems -->
<variable name="calc-compiler:name" select="'js-calc-compiler'" />

<template name="calc-compiler:error">
  <param name="message" select="'unspecified error'" />

  <message terminate="yes">
    <value-of select="$calc-compiler:name" />
    <text>: </text>
    <value-of select="$message" />
  </message>
</template>


<!--
  Compile calculations with debug information.  This collects the
  result of the calculation, allowing for extremely fine-grained
  recursive study of a calculation.
-->
<template mode="compile" priority="1"
          match="c:*">
  <if test="$calcc-debug = 'yes'">
    <text>( function() { var result = </text>
  </if>

  <apply-templates select="." mode="compile-pre" />

  <if test="$calcc-debug = 'yes'">
    <text>; </text>
    <text>/*!+*/( debug['</text>
      <value-of select="@_id" />
    <text>'] || ( debug['</text>
      <value-of select="@_id" />
    <text>'] = [] ) ).push( result );/*!-*/ </text>

    <text>return result; </text>
    <text>} )() </text>
  </if>
</template>


<!--
  Functions are invoked many times and providing information about
  each of their invocations is of limited use, so they should not
  collect debug information.  This saves a lot of time and memory,
  and hopefully allows JIT engines to better optimize.
-->
<template mode="compile" priority="7"
          match="c:*[ ancestor::lv:function ]">
  <apply-templates select="." mode="compile-pre" />
</template>


<!--
  Begins compilation of a calculation

  This is responsible for wrapping the value to enforce precedence rules and
  convert it to a number (to avoid string concatenation when adding), then
  delegating the compilation to the appropriate template(s).

  @return generated JS for the given calculation
-->
<template match="c:*" mode="compile-pre" priority="1">
  <!-- ensure everything is grouped (for precedence) and converted to a
       number -->
  <text>( </text>
  <apply-templates select="." mode="compile-calc" />
  <text> )</text>
</template>


<template match="c:const[ ./c:when ]|c:value-of[ ./c:when ]" mode="compile-pre" priority="5">
  <text>( </text>
    <!-- first, do what we normally would do (compile the value) -->
    <text>( </text>
      <apply-templates select="." mode="compile-calc" />
    <text> )</text>

    <!-- then multiply by the c:when result -->
    <text> * ( </text>
      <for-each select="./c:when">
        <if test="position() > 1">
          <text> * </text>
        </if>

        <apply-templates select="." mode="compile" />
      </for-each>
    <text> )</text>
  <text> )</text>
</template>



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
<template match="c:product[@of]|c:sum[@of]" mode="compile-calc" priority="1">
  <!-- see c:ceil/c:floor precision comments -->
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

  <variable name="operator">
    <apply-templates select="." mode="compile-getop" />
  </variable>

  <variable name="index">
    <choose>
      <when test="@index">
        <value-of select="@index" />
      </when>

      <!-- if they don't provide us with an index, then we don't want them using
           it -->
      <otherwise>
        <text>_$i$_</text>
      </otherwise>
    </choose>
  </variable>

  <variable name="dim" as="xs:integer"
            select="if ( @dim ) then @dim else 0" />

  <!-- introduce scope both to encapsulate values and so we can insert this as
       part of a larger expression (will return a value) -->
  <text>( function() {</text>

    <!-- will store result of the summation/product -->
    <text>var sum = 0;</text>

    <variable name="of" select="@of" />

    <variable name="func" select="ancestor::lv:function" />

    <!-- XXX: this needs to use compile-calc-value, but can't right now
         beacuse it's not a c:value-of! -->
    <variable name="value">
      <choose>
        <!-- is @of a function param? -->
        <when test="
            $func
            and root(.)/preproc:symtable/preproc:sym[
              @type='lparam'
              and @name=concat( ':', $func/@name, ':', $of )
            ]
          ">

          <value-of select="@of" />
        </when>

        <!-- let expression -->
        <when test="$of = ancestor::c:let/c:values/c:value/@name">
          <value-of select="$of" />
        </when>

        <!-- maybe a constant? -->
        <when test="
            root(.)/preproc:symtable/preproc:sym[
              @type='const'
              and @name=$of
            ]
          ">

          <text>consts['</text>
            <value-of select="@of" />
          <text>']</text>
        </when>

        <otherwise>
          <text>args['</text>
            <value-of select="@of" />
          <text>']</text>
        </otherwise>
      </choose>
    </variable>

    <!-- if we're looking to generate a set, initialize it -->
    <if test="@generates">
      <text>var G = []; </text>
    </if>

    <!-- loop through each value -->
    <text>for ( var </text>
      <value-of select="$index" />
    <text> in </text>
      <value-of select="$value" />
    <text> ) {</text>

    <text>var result = </text>
    <!-- if caller wants to yield a vector, don't cast -->
    <sequence select="if ( not( $dim gt 0 ) ) then
                          '+(+( '
                        else
                          '(( '" />
      <choose>
        <!-- if there are child nodes, use that as the summand/expression -->
        <when test="./c:*">
          <apply-templates select="." mode="compile-calc-sumprod" />
        </when>

        <!-- otherwise, sum/multiply ever value in the set identified by @of -->
        <otherwise>
          <value-of select="$value" />
          <text>[</text>
            <value-of select="$index" />
          <text>]</text>
        </otherwise>
      </choose>
    <text> ))</text>

    <!-- if caller wants to yield a vector, don't truncate -->
    <if test="not( $dim gt 0 )">
      <text>.toFixed(</text>
        <value-of select="$precision" />
      <text>)</text>
    </if>

    <text>;</text>

    <!-- if generating a set, store this result -->
    <if test="@generates">
      <text>G.push( result ); </text>
    </if>

    <!-- generate summand -->
    <text>sum </text>
      <value-of select="$operator" />
    <text>= +result;</text>

    <!-- end of loop -->
    <text>}</text>

    <!-- if a set has been generated, store it -->
    <if test="@generates">
      <text>args['</text>
        <value-of select="@generates" />
      <text>'] = G; </text>
    </if>

    <text>return sum;</text>
  <text>} )()</text>
</template>


<!--
  Compile dot products

  N.B. This match has a higher priority than other products.

  Dot products should operate only on vectors; if other input passes the
  validator, then the result is undefined.
-->
<template match="c:product[@dot]" mode="compile-calc" priority="5">
  <text>( function() { </text>

  <!-- we need to determine which vector is the longest to ensure that we
       properly compute every value (remember, undefined will be equivalent to
       0, so the vectors needn't be of equal length *gasp* blasphemy!) -->
  <text>var _$dlen$ = longerOf( </text>
    <for-each select=".//c:value-of">
      <if test="position() > 1">
        <text>, </text>
      </if>

      <!-- skip any wrapping; we want the direct reference (so compile-calc, not
           compile)-->
      <apply-templates select="." mode="compile-calc" />
    </for-each>
  <text> ); </text>

  <!-- will store the total sum -->
  <text>var _$dsum$ = 0;</text>

  <!-- sum the product of each -->
  <text disable-output-escaping="yes">for ( var _$d$ = 0; _$d$ &lt; _$dlen$; _$d$++ ) {</text>
    <text>_$dsum$ += </text>
      <!-- product of each -->
      <for-each select=".//c:value-of">
        <if test="position() > 1">
          <text> * </text>
        </if>

        <text>( ( </text>
          <apply-templates select="." mode="compile" />
        <text> || [] )[ _$d$ ] || 0 )</text>
      </for-each>
    <text>; </text>
  <text>}</text>

  <text>return _$dsum$;</text>

  <text> } )()</text>
</template>


<!--
  Generates a sum/product over the expression denoted by its child nodes

  When no @of set is provided, this will add or multiple each child expression.

  @return generated expression
-->
<template match="c:sum|c:product" mode="compile-calc">
  <apply-templates select="." mode="compile-calc-sumprod" />
</template>


<template match="c:sum|c:product" mode="compile-calc-sumprod">
  <variable name="operator">
    <apply-templates select="." mode="compile-getop" />
  </variable>

  <for-each select="./c:*">
    <!-- add operator between each expression -->
    <if test="position() > 1">
      <text> </text>
      <value-of select="$operator" />
      <text> </text>
    </if>

    <apply-templates select="." mode="compile" />
  </for-each>
</template>


<!--
  @return addition operator
-->
<template match="c:sum" mode="compile-getop">
  <text>+</text>
</template>

<!--
  @return multiplication operator
-->
<template match="c:product" mode="compile-getop">
  <text>*</text>
</template>


<!--
  Generate code for ceiling/floor functions

  @return ceil/floor compiled JS
-->
<template match="c:ceil|c:floor" mode="compile-calc">
  <!-- determine precision for ceil/floor (floating point precision errors can
       drastically affect the outcome) -->
  <variable name="precision">
    <choose>
      <!-- if a precision was explicitly provided, then use that -->
      <when test="@precision">
        <value-of select="@precision" />
      </when>

      <!-- ECMAScript uses a default precision of 24; by reducing the
           precision to 8 decimal places, we can drastically reduce the affect
           of precision errors on the calculations -->
      <otherwise>
        <text>8</text>
      </otherwise>
    </choose>
  </variable>

  <text>Math.</text>
  <value-of select="local-name()" />
  <text>( +(</text>
    <apply-templates select="./c:*" mode="compile" />
  <text> ).toFixed( </text>
    <value-of select="$precision" />
  <text> ) )</text>
</template>


<!--
  Replaces a constant with its value in the compiled expression

  Constants are syntatic sugar in the language to prevent magic values; they are
  not required at runtime. As such, the value of the constant will be placed
  directly into the compiled code.

  @return quoted constant value
-->
<template match="c:const" mode="compile-calc">
  <!-- assumed to be numeric -->
  <sequence select="compiler:js-number( @value )" />
</template>


<!--
  Generate JS representing the value associated with the given name

  @return generated JS representing value or 0
-->
<template match="c:value-of" mode="compile-calc" priority="1">
  <apply-templates select="." mode="compile-calc-value" />
</template>

<!-- TODO: this should really be decoupled -->
<!-- TODO: does not properly support matrices -->
<template match="c:value-of[ ancestor::lv:match ]" mode="compile-calc" priority="5">
  <variable name="name" select="@name" />

  <choose>
    <!-- scalar -->
    <when test="
        root(.)/preproc:symtable/preproc:sym[ @name=$name ]
          /@dim = '0'
      ">
      <apply-templates select="." mode="compile-calc-value" />
    </when>

    <!-- non-scalar -->
    <otherwise>
      <text>(</text>
        <apply-templates select="." mode="compile-calc-value" />
      <text>)[__$$i]</text>
    </otherwise>
  </choose>
</template>


<!--
  Generate JS representing the value of a function argument

  This will match whenever value-of is used on a name that matches any function
  parameter.

  XXX: We want to remain decoupled from lv if possible.

  @return generated JS representing argument value or 0
-->
<template mode="compile-calc-value"
  match="c:*[@name=ancestor::lv:function/lv:param/@name]">

  <!-- use the argument passed to the function -->
  <apply-templates select="." mode="compile-calc-index">
    <with-param name="value" select="@name" />
  </apply-templates>

  <text> || 0</text>
</template>


<!--
  Using value from let expressions
-->
<template mode="compile-calc-value"
  match="c:*[ @name=ancestor::c:let/c:values/c:value/@name ]">

  <!-- compile the value with the index (if any) -->
  <apply-templates select="." mode="compile-calc-index">
    <with-param name="value" select="@name" />
  </apply-templates>

  <text> || 0</text>
</template>


<!--
  Generate JS representing the value of a global constant

  Since constants are intended only to prevent magic values during development
  (and are not required at runtime), the value of the constant will be placed
  directly into the compiled code. However, we will *not* do this if the
  constant is a set, since its index may be determined at runtime.

  Note that "magic" constants' values are not inlined.

  @return quoted constant value
-->
<template mode="compile-calc-value"
  match="
    c:*[
      @name=root(.)/preproc:symtable/preproc:sym[
        @type='const'
        and @dim='0'
      ]/@name
    ]
  ">

  <variable name="name" select="@name" />
  <variable name="sym"
    select="root(.)/preproc:symtable/preproc:sym[ @name=$name ]" />

  <!-- it is expected that validations prior to compiling will prevent JS
       injection here -->
  <choose>
    <!-- "magic" constants should not have their values inlined -->
    <when test="$sym/@magic='true'">
      <text>consts['</text>
        <value-of select="@name" />
      <text>']</text>
    </when>

    <!-- @value should be defined when @dim=0 -->
    <otherwise>
      <value-of select="$sym/@value" />
    </otherwise>
  </choose>
</template>


<!--
  Generates JS representing the value of a constant as part of a set

  Since the index of constant sets can be determined at runtime, we need to
  store all possible values. As such, we shouldn't repeat ourselves by inlining
  all possible values; instead, we'll reference a pre-generated set of values
  for the particular constant.

  @return generated code representing value of a variable, or 0 if undefined
-->
<template mode="compile-calc-value"
  match="
    c:*[
      @name=root(.)/preproc:symtable/preproc:sym[
        @type='const'
        and not( @dim='0' )
      ]/@name
    ]
  ">

  <variable name="value">
    <text>consts['</text>
      <value-of select="@name" />
    <text>']</text>
  </variable>

  <apply-templates select="." mode="compile-calc-index">
    <with-param name="value" select="$value" />
  </apply-templates>

  <!-- undefined values in sets are considered to be 0 -->
  <text> || 0</text>
</template>


<!--
  Generate JS representing the value of a generated index

  @return generated code associated with the value of the generated index
-->
<template mode="compile-calc-value"
  match="c:*[ @name = ancestor::c:*[ @of ]/@index ]">

  <!-- depending on how the index is generated, it could be a string, so we must
       cast it -->
  <text>+</text>
  <value-of select="@name" />
</template>


<!--
  Generates JS representing the value of a variable

  If the variable is undefined, the value will be considered to be 0 (this is
  especially important for the summation of sets within this implementation).
  That is: a value will never be considered undefined.

  @return generated code representing value of a variable, or 0 if undefined
-->
<template match="c:*" mode="compile-calc-value">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <variable name="dim"
                select="$pkg/preproc:symtable/preproc:sym[ @name=$name ]/@dim" />

  <!-- retrieve the value, casting to a number (to avoid potentially nasty
       string concatenation bugs instead of integer/floating point arithmetic)
       as long as we're either not a set, or provide an index for the set -->
  <variable name="value">
    <text>args['</text>
      <value-of select="@name" />
    <text>']</text>
  </variable>

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
  <if test="
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

    <text>+</text>
  </if>

  <apply-templates select="." mode="compile-calc-index">
    <with-param name="value" select="$value" />
  </apply-templates>

  <!-- default to 0 if nothing is set (see notes on bottom of summary page; we
       assume all undefined values in a set to be implicitly 0, which greatly
       simplifies things) -->
  <text> || 0</text>
</template>


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
<template match="c:*" mode="compile-calc-index">
  <param name="value" />
  <variable name="index" select="@index" />

  <choose>
    <when test="@index">
      <!-- output the value, falling back on an empty array to prevent errors
           when attempting to access undefined values -->
      <text>(</text>
        <value-of select="$value" />
      <text>||[])</text>

      <text>[</text>

      <choose>
        <!-- if this index was explicitly defined as such, just output the name
             (since it will have been defined as a variable in the compiled code)
             -->
        <when test="ancestor::c:*[ @of and @index=$index ]">
          <value-of select="@index" />
        </when>

        <!-- if the index references a parameter, simply output the identifier -->
        <when test="@index=ancestor::lv:function/lv:param/@name">
          <value-of select="@index" />
        </when>

        <!-- scalar constant -->
        <when test="@index = root(.)/preproc:symtable/preproc:sym
                          [ @type='const'
                            and @dim='0' ]
                          /@name">
          <value-of select="root(.)/preproc:symtable
                                  /preproc:sym[ @name=$index ]
                                  /@value" />
        </when>

        <!-- otherwise, it's a variable -->
        <otherwise>
          <text>args['</text>
            <value-of select="@index" />
          <text>']</text>
        </otherwise>
      </choose>

      <text>]</text>
    </when>

    <!-- if index node(s) were provided, then recursively generate -->
    <when test="./c:index">
      <apply-templates select="./c:index[1]" mode="compile-calc-index">
        <with-param name="wrap" select="$value" />
      </apply-templates>
    </when>

    <!-- otherwise, we have no index, so just output the compiled variable -->
    <otherwise>
      <value-of select="$value" />
    </otherwise>
  </choose>
</template>

<template match="c:index" mode="compile-calc-index">
  <param name="wrap" />

  <!-- get the next index node, if available -->
  <variable name="next" select="following-sibling::c:index" />

  <variable name="value">
    <value-of select="$wrap" />

    <!-- generate index -->
    <text>[</text>
    <apply-templates select="./c:*[1]" mode="compile" />
    <text>]</text>
  </variable>

  <choose>
    <when test="$next">
      <!-- recurse on any sibling indexes, wrapping with default value -->
      <apply-templates select="$next" mode="compile-calc-index">
        <with-param name="wrap">
          <!-- wrap the value in parenthesis so that we can provide a default value if
               the index lookup fails -->
          <text>(</text>

          <value-of disable-output-escaping="yes" select="$value" />

          <!-- using 0 as the default is fine, even if we have $next; accessing an index
               of 0 is perfectly fine, since it will be cast to an object; we just must
               make sure that it is not undefined -->
          <text> || 0 )</text>
        </with-param>
      </apply-templates>
    </when>

    <!-- if there is no sibling, just output our value with the index, unwrapped
         (since the caller will handle its default value for us -->
    <otherwise>
      <value-of disable-output-escaping="yes" select="$value" />
    </otherwise>
  </choose>
</template>


<!--
  Generates a quotient between two calculations

  The result is simply division between the two nodes, the first being the
  numerator and the second being the denominator.

  @return generate quotient
-->
<template match="c:quotient" mode="compile-calc">
  <!-- we only accept a numerator and a denominator -->
  <apply-templates select="./c:*[1]" mode="compile" />
  <text> / </text>
  <apply-templates select="./c:*[2]" mode="compile" />
</template>


<template match="c:expt" mode="compile-calc">
  <!-- we only accept a numerator and a denominator -->
  <text>Math.pow(</text>
  <apply-templates select="./c:*[1]" mode="compile" />
  <text>, </text>
  <apply-templates select="./c:*[2]" mode="compile" />
  <text>)</text>
</template>


<!--
  Generates a function application (call)

  Arguments are set by name in the XML, but the generate code uses actual
  function arguments. Therefore, the generated argument list will be generated
  in the order that the function is expecting, filling in unset parameters with
  the constant 0.

  @return generated function application
-->
<template match="c:apply" mode="compile-calc" priority="5">
  <variable name="name" select="@name" />
  <variable name="self" select="." />

  <call-template name="calc-compiler:gen-func-name">
    <with-param name="name" select="@name" />
  </call-template>

  <text>( args</text>

  <variable name="arg-prefix" select="concat( ':', $name, ':' )" />

  <!-- generate argument list in the order that the arguments are expected (they
       can be entered in the XML in any order) -->
  <for-each select="
      root(.)/preproc:symtable/preproc:sym[
        @type='func'
        and @name=$name
      ]/preproc:sym-ref
    ">

    <text>, </text>

    <variable name="pname" select="substring-after( @name, $arg-prefix )" />
    <variable name="arg" select="$self/c:arg[@name=$pname]" />

    <choose>
      <!-- if the call specified this argument, then use it -->
      <when test="$arg">
        <apply-templates select="$arg/c:*[1]" mode="compile" />
      </when>

      <!-- otherwise, there's no value; pass in 0 -->
      <otherwise>
        <value-of select="'0'" />
      </otherwise>
    </choose>
  </for-each>

  <text> )</text>

  <!-- if c:when was provided, compile it in such a way that we retain the
       function call (we want the result for providing debug information) -->
  <if test="./c:when">
    <text> * </text>
    <apply-templates select="./c:when" mode="compile" />
  </if>
</template>


<!--
  Whether the given function supports tail call optimizations (TCO)

  This is an experimental feature that must be explicitly requested.
-->
<function name="compiler:function-supports-tco" as="xs:boolean">
  <param name="func" as="element( lv:function )" />

  <sequence select="exists( $func/lv:param[
                              @name='__experimental_guided_tco' ] )" />
</function>


<!--
  Whether a recursive function application is marked as being in tail
  position within a function supporting TCO

  A human must determined if a recursive call is in tail position, and
  hopefully the human is not wrong.
-->
<function name="compiler:apply-uses-tco" as="xs:boolean">
  <param name="apply" as="element( c:apply )" />

  <variable name="ancestor-func" as="element( lv:function )?"
            select="$apply/ancestor::lv:function" />

  <sequence select="exists( $apply/c:arg[ @name='__experimental_guided_tco' ] )
                   and $apply/@name = $ancestor-func/@name
                   and compiler:function-supports-tco( $ancestor-func ) " />
</function>


<!--
  Experimental guided tail call optimization (TCO)

  When the special param `__experimental_guided_tco' is defined and set to a
  true value, the recursive call instead overwrites the original function
  arguments and returns a dummy value.  The function's trampoline is then
  responsible for re-invoking the function's body.

  Note that this only applies to self-recursive functions; mutual recursion
  is not recognized.

  By forcing a human to specify whether a recursive call is in tail
  position, we free ourselves from having to track tail position within this
  nightmare of a compiler; we can figure this out properly in TAMER.
-->
<template mode="compile-calc" priority="7"
          match="c:apply[ compiler:apply-uses-tco( . ) ]">
  <variable name="name" select="@name" />
  <variable name="self" select="." />

  <message select="concat('warning: ', $name, ' recursing with experimental guided TCO')" />

  <variable name="arg-prefix" select="concat( ':', $name, ':' )" />

  <text>(/*TCO*/function(){</text>

  <!-- reassign function arguments -->
  <variable name="args" as="element(c:arg)*">
    <for-each select="
        root(.)/preproc:symtable/preproc:sym[
          @type='func'
          and @name=$name
        ]/preproc:sym-ref
      ">

      <variable name="pname" select="substring-after( @name, $arg-prefix )" />
      <variable name="arg" select="$self/c:arg[@name=$pname]" />

      <!-- if the call specified this argument, then use it -->
      <sequence select="$arg" />
    </for-each>
  </variable>

  <!-- store reassignments first in a temporary variable, since the
       expressions may reference the original arguments and we do not want
       to overwrite yet -->
  <for-each select="$args">
    <sequence select="concat( 'const __tco_', @name, '=' )" />
    <apply-templates select="c:*[1]" mode="compile" />
    <text>;</text>
  </for-each>

  <!-- perform final reassignments, now that expressions no longer need the
       original values  -->
  <for-each select="$args">
    <sequence select="concat( @name, '=__tco_', @name, ';' )" />
  </for-each>

  <!-- return value, which doesn't matter since it won't be used -->
  <text>return 0;})()</text>

  <!-- don't support c:when here; not worth the effort -->
  <if test="./c:when">
    <message terminate="yes"
             select="'c:when unsupported on TCO c:apply: ', ." />
  </if>
</template>


<template match="c:when" mode="compile-calc">
  <!-- note that if we have multiple c:whens, they'll be multiplied together by
       whatever calls this, so we're probably fine -->

  <text>( function() {</text>
    <!-- return a 1 or a 0 depending on the result of the expression -->
    <text>return ( </text>
      <text>( </text>
        <!-- get the value associated with this node -->
        <apply-templates select="." mode="compile-calc-value" />
      <text> ) </text>

      <!-- generate remainder of expression -->
      <apply-templates select="./c:*[1]" mode="compile-calc-when" />
    <text>) ? 1 : 0; </text>
  <text>} )()</text>
</template>


<template match="c:eq|c:ne|c:lt|c:gt|c:lte|c:gte" mode="compile-calc-when">
  <variable name="name" select="local-name()" />

  <!-- map to LaTeX equivalent -->
  <variable name="map">
    <calc-compiler:c id="eq">==</calc-compiler:c>
    <calc-compiler:c id="ne">!=</calc-compiler:c>
    <calc-compiler:c id="gt">&gt;</calc-compiler:c>
    <calc-compiler:c id="lt">&lt;</calc-compiler:c>
    <calc-compiler:c id="gte">&gt;=</calc-compiler:c>
    <calc-compiler:c id="lte">&lt;=</calc-compiler:c>
  </variable>

  <value-of disable-output-escaping="yes" select="$map/*[ @id=$name ]" />
  <text> </text>

  <apply-templates select="./c:*[1]" mode="compile" />
</template>


<template match="c:*" mode="compile-calc-when">
  <apply-templates select="." mode="compile-pre" />
</template>


<template match="c:cases" mode="compile-calc">
  <text>( function() {</text>

    <for-each select="./c:case">
      <!-- turn "if" into an "else if" if needed -->
      <if test="position() > 1">
        <text>else </text>
      </if>

      <text>if (</text>
        <for-each select="./c:when">
          <if test="position() > 1">
            <!-- they all yield integer values, so this should work just fine -->
            <text> * </text>
          </if>

          <apply-templates select="." mode="compile" />
        </for-each>
      <text> ) { return </text>
        <!-- process on its own so that we can debug its final value -->
        <apply-templates select="." mode="compile" />
      <text>; } </text>
    </for-each>

    <!-- check for the existence of an "otherwise" clause, which should be
         trivial enough to generate -->
    <if test="c:otherwise">
      <!-- this situation may occur in templates since it's a convenient and
           easy-to-use template notation (conditional cases, none of which may
           actually match and be output) -->
      <choose>
        <when test="c:case">
          <text>else</text>
        </when>

        <otherwise>
          <text>if ( true )</text>
        </otherwise>
      </choose>

      <text> { return </text>
        <apply-templates select="c:otherwise" mode="compile" />
      <text>; } </text>
    </if>

  <text> } )() || 0</text>
</template>

<template match="c:case" mode="compile-calc">
  <apply-templates
    select="./c:*[ not( local-name() = 'when' ) ][1]"
    mode="compile-calc-when" />
</template>

<template match="c:otherwise" mode="compile-calc">
  <apply-templates
    select="c:*[1]"
    mode="compile-calc-when" />
</template>


<!--
  TODO: Remove c:set in the future.
-->
<template match="c:set" mode="compile-calc" priority="9">
  <message select="'warning: c:set is deprecated; use c:vector instead'" />
  <next-match />
</template>


<template match="c:set|c:vector" mode="compile-calc" priority="5">
  <text>[</text>
    <for-each select="./c:*">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <apply-templates select="." mode="compile" />
    </for-each>
  <text>]</text>
</template>


<!--
  Just list the Lisp cons (constructs a list)
-->
<template match="c:cons" mode="compile-calc">
  <variable name="car" select="./c:*[1]" />
  <variable name="cdr" select="./c:*[2]" />

  <text>(function(){</text>
    <!-- duplicate the array just in case...if we notice a performance impact,
         then we can determine if such a duplication is necessary -->
    <text>var cdr = Array.prototype.slice.call(</text>
      <apply-templates select="$cdr" mode="compile" />
    <text>, 0);</text>
    <text>cdr.unshift( </text>
      <apply-templates select="$car" mode="compile" />
    <text> ); </text>
    <!-- no longer the cdr -->
    <text>return cdr; </text>
  <text>})()</text>
</template>


<template match="c:car" mode="compile-calc">
  <text>(</text>
    <apply-templates select="c:*[1]" mode="compile" />
  <text>[0]||0)</text>
</template>

<template match="c:cdr" mode="compile-calc">
  <apply-templates select="c:*[1]" mode="compile" />
  <text>.slice(1)</text>
</template>


<!--
  Returns the length of any type of set (not just a vector)
-->
<template match="c:length-of" mode="compile-calc">
  <text>( </text>
  <apply-templates select="./c:*[1]" mode="compile" />
  <text>.length || 0 )</text>
</template>


<template match="c:let" mode="compile-calc">
  <!-- the first node contains all the values -->
  <variable name="values" select="./c:values/c:value" />

  <!-- if a @name was provided, then treat it like a named Scheme let
       expression in that it can be invoked with different arguments -->
  <variable name="fname">
    <if test="@name">
      <call-template name="calc-compiler:gen-func-name">
        <with-param name="name" select="@name" />
      </call-template>
    </if>
  </variable>

  <text>function </text>
    <value-of select="$fname" />
  <text>( </text>
    <!-- generate arguments -->
    <for-each select="$values">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <value-of select="@name" />
    </for-each>
  <text> ) { </text>

    <!-- the second node is the body -->
    <text>return </text>
      <apply-templates select="./c:*[2]" mode="compile" />
    <text>;</text>
  <text>}</text>

  <!-- assign the arguments according to the calculations -->
  <text>( </text>
    <for-each select="$values">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <!-- compile the argument value (the parenthesis are just to make it
           easier to read the compiled code) -->
      <text>(</text>
        <apply-templates select="./c:*[1]" mode="compile" />
      <text>)</text>
    </for-each>
  <text> ) </text>
</template>


<!--
  Generate a function name for use as the name of a function within the compiled
  code

  @return generated function name
-->
<template name="calc-compiler:gen-func-name">
  <param name="name" />

  <text>func_</text>
  <value-of select="$name" />
</template>


<template match="c:debug-to-console" mode="compile-calc">
  <text>(function(){</text>
    <text>var result = </text>
      <apply-templates select="./c:*[1]" mode="compile" />
    <text>;</text>

    <!-- log the result and return it so that we do not inhibit the calculation
         (allowing it to be inlined anywhere) -->
    <text>console.log( </text>
      <if test="@label">
        <text>'</text>
          <value-of select="@label" />
        <text>', </text>
      </if>

      <text>result ); </text>
    <text>return result; </text>
  <text>})()</text>
</template>


<!--
  Catch-all for calculations

  If an unmatched calculation element is used, then the generated code will
  output an error to the console (if a console is available) and return a value
  that may be used within context of an equation; this allows it to be placed
  inline.

  @return self-executing anonymous error function
-->
<template match="c:*" mode="compile-calc">
  <text>( function () {</text>
    <text>throw Error( "Unknown calculation: </text>
      <value-of select="name()" />
    <text>" ); </text>
  <text>} )() </text>
</template>

</stylesheet>
