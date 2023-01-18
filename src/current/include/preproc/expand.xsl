<?xml version="1.0" encoding="utf-8"?>
<!--
  Handles node expansion

  Copyright (C) 2014-2023 Ryan Specialty, LLC.

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

  This process is responsible for expanding shorthand and various other data
  into a consistent format for the compiler and other processes.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:t="http://www.lovullo.com/rater/apply-template"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:w="http://www.lovullo.com/rater/worksheet">


<include href="domain.xsl" />


<template match="lv:package[ not( @preproc:name ) ]"
              mode="preproc:expand" priority="5"
              as="element( lv:package )">
  <copy>
    <sequence select="@*" />

    <!-- generate name from source package identifier -->
    <attribute name="name" select="$__srcpkg" />

    <!-- relative path to root src directory (for resolving absolute include
         paths) -->
    <attribute name="__rootpath" select="$__relroot" />

    <!-- TODO: temporary; remove -->
    <attribute name="preproc:name" select="$__srcpkg" />

    <apply-templates mode="preproc:expand" />
  </copy>
</template>



<template match="*" mode="preproc:expand" priority="1">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<!-- imports relative to project root -->
<template match="lv:import[ starts-with( @package, '/' ) ]" mode="preproc:expand" priority="5">
  <copy>
    <sequence select="@*" />

    <!-- resolve path into path relative to project root -->
    <attribute name="package">
      <call-template name="__apply-relroot">
        <with-param name="path" select="@package" />
      </call-template>
    </attribute>
  </copy>
</template>


<!--
  Domain data are extracted from typedefs

  Eventually, the typedefs will be converted into templates and removed entirely.
-->
<template match="lv:typedef"
  mode="preproc:expand" priority="5">

  <apply-templates select="." mode="preproc:mkdomain" />

  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<!--
  Infer primitive type if not provided.
-->
<template mode="preproc:expand"
              match="c:const[ not( @type ) ]
                     |lv:const[ not( @type ) ]"
              priority="5">
  <copy>
    <sequence select="@*" />

    <attribute name="type"
                   select="if ( substring-before( @value, '.' ) ) then
                             'float'
                           else
                             'integer'" />

    <sequence select="*" />
  </copy>
</template>


<!--
  Translate dimension aliases (e.g. scaler, vector, matrix) into respective
  numeric representations.
-->
<template mode="preproc:expand" priority="8"
              match="c:*[ @dim
                          and not( string( @dim ) castable as xs:integer ) ]">
  <copy>
    <sequence select="@*" />

    <!-- replace dim with numeric -->
    <attribute name="dim">
      <choose>
        <when test="@dim = 'scaler' or @dim = ''">
          <sequence select="0" />
        </when>

        <when test="@dim = 'vector'">
          <sequence select="1" />
        </when>

        <when test="@dim = 'matrix'">
          <sequence select="2" />
        </when>

        <otherwise>
          <message terminate="yes"
                       select="concat(
                                 '!!! [preproc] error: ',
                                 'unknown dimension alias ''',
                                 @dim, '''' )" />
        </otherwise>
      </choose>
    </attribute>

    <apply-templates select="node()" mode="preproc:expand" />
  </copy>
</template>


<!--
  Give let's a name so that they may be easily referenced uniquely
-->
<template match="c:let[ not( @name ) ]" mode="preproc:expand" priority="5">
  <copy>
    <sequence select="@*" />
    <attribute name="name" select="generate-id(.)" />

    <apply-templates select="*" mode="preproc:expand" />
  </copy>
</template>


<!--
  Default label of c:let value expressions to description if no label is provided

  This is useful for breakdown display.

  TODO: play well with others; if we change the priority back to 5, we introduce
  match ambiguities
-->
<template match="c:let/c:values/c:value/c:*[1][ not( @label ) ]" mode="preproc:expand" priority="4">
  <copy>
    <sequence select="@*" />

    <!-- default the label to the description of the parent c:value -->
    <attribute name="label" select="../@desc" />

    <apply-templates select="*" mode="preproc:expand" />
  </copy>
</template>


<!--
  c:when with no children is shorthand for > 0

  Note: we check for any children because we may have things like
  template applications that we do not want wiped out.
-->
<template match="c:when[ not( * ) ]" mode="preproc:expand" priority="5">
  <copy>
    <sequence select="@*" />

    <c:gt>
      <c:value-of name="FALSE" />
    </c:gt>
  </copy>
</template>


<!--
  c:when with multiple children
-->
<template match="c:when[ count( c:* ) gt 1 ]" mode="preproc:expand" priority="5">
  <variable name="when" select="." />

  <!-- expand into adjacent c:when's -->
  <for-each select="c:*">
    <c:when>
      <sequence select="$when/@*" />
      <apply-templates select="." mode="preproc:expand" />
    </c:when>
  </for-each>
</template>


<!--
  Recursion shorthand

  This exists simply because function application is so verbose and, when
  recursing, generally only a small fraction of the arguments actually change.
-->
<template match="c:recurse" mode="preproc:expand" priority="5">
  <variable name="self" select="." />
  <variable name="fname" select="ancestor::lv:function/@name" />
  <variable name="overrides" select="./c:arg" />

  <c:apply name="{$fname}">
    <!-- every non-@name attribute should be converted into an argument -->
    <call-template name="preproc:arg-short-expand" />

    <!-- include all non-overridden args -->
    <for-each select="
        ancestor::lv:function/lv:param[
          not(
            @name=$overrides/@name
            or @name=$self/@*/local-name()
            or starts-with( @name, '__experimental_' )
          )
        ]
      ">

      <!-- copy the arg value -->
      <c:arg name="{@name}">
        <c:value-of name="{@name}" />
      </c:arg>
    </for-each>

    <!-- copy in the overrides -->
    <apply-templates select="$overrides" mode="preproc:expand" />
  </c:apply>
</template>


<!-- metadata constants have different semantics -->
<!-- TODO: maybe ignore single-quoted? -->
<template mode="preproc:expand" priority="6"
              match="lv:meta/lv:prop/lv:const">
  <sequence select="." />
</template>


<!-- anything with a '@' is likely a template variable reference, so we
     should not attempt to perform constant expansion  -->
<template mode="preproc:expand" priority="7"
    match="c:const[ substring-after( @value, '@' ) ]
           |lv:const[ substring-after( @value, '@' ) ]">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<!-- constants that contain 'e' (scientific notation) should be expanded; allows
     for avoiding constants with many zeroes, which is hard to read -->
<template mode="preproc:expand" priority="6"
    match="c:const[ substring-before( @value, 'e' ) ]
           |lv:const[ substring-before( @value, 'e' ) ]">
  <copy>
    <sequence select="@*" />

    <attribute name="value">
      <call-template name="preproc:expand-e">
        <with-param name="number" select="@value" />
      </call-template>
    </attribute>

    <apply-templates mode="preproc:expand" />
  </copy>
</template>

<template mode="preproc:expand" priority="6"
    match="c:const[ substring-before( @value, 'm' ) ]
           |lv:const[ substring-before( @value, 'm' ) ]">
  <copy>
    <sequence select="@*" />

    <attribute name="value">
      <call-template name="preproc:expand-e">
        <with-param name="number"
          select="concat( substring-before( @value, 'm' ), 'e6' )" />
      </call-template>
    </attribute>

    <apply-templates mode="preproc:expand" />
  </copy>
</template>

<template mode="preproc:expand" priority="6"
    match="c:const[ substring-before( @value, 'k' ) ]
           |lv:const[ substring-before( @value, 'k' ) ]">
  <copy>
    <sequence select="@*" />

    <attribute name="value">
      <call-template name="preproc:expand-e">
        <with-param name="number"
          select="concat( substring-before( @value, 'k' ), 'e3' )" />
      </call-template>
    </attribute>

    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<!-- expand scientific notation -->
<!-- XXX: negatives not currently supported -->
<template name="preproc:expand-e">
  <param name="number" />
  <param name="whole" select="substring-before( $number, '.' )" />
  <param name="dec"   select="substring-before( substring-after( $number, '.' ), 'e' )" />
  <param name="count" as="xs:double"
             select="number( substring-after( $number, 'e' ) )" />

  <!-- output the whole number portion -->
  <choose>
    <when test="$whole and not( $whole = '' )">
      <value-of select="$whole" />
    </when>

    <!-- if no decimal was provided, then use the entire number before 'e' -->
    <when test="$number and not( $number = '' ) and ( $whole = '' )">
      <value-of select="substring-before( $number, 'e' )" />
    </when>
  </choose>

  <choose>
    <when test="$count > 0">
      <choose>
        <!-- if we have a decimal, then use the first digit (as if we moved one
             place to the right) -->
        <when test="$dec and not( $dec = '' )">
          <value-of select="substring( $dec, 1, 1 )" />
        </when>

        <!-- no decimal portion remaining; fill with 0 -->
        <otherwise>
          <text>0</text>
        </otherwise>
      </choose>

      <!-- recursively expand -->
      <call-template name="preproc:expand-e">
        <!-- already processed the whole -->
        <with-param name="whole" select="''" />

        <with-param name="dec">
          <!-- move to the right one decimal place; otherwise, no decimal -->
          <if test="$dec">
            <value-of select="substring( $dec, 2 )" />
          </if>
        </with-param>

        <with-param name="count" select="$count - 1" />
      </call-template>
    </when>

    <!-- output the remaining decimal, if any -->
    <otherwise>
      <if test="$dec and not( $dec = '' )">
        <text>.</text>
        <value-of select="$dec" />
      </if>
    </otherwise>
  </choose>
</template>


<!--
  Optimize away c:cases if they contain only c:otherwise

  This is useful primarily for templates that may create a case statement for
  conditional operations (using lv:if/lv:unless) and ensures that there is no
  penalty for doing so if none of the template conditions result in a c:case.

  Note that we should *not* perform these optimizations if there are templates
  awaiting application or any other lv:* nodes that have not been expanded.
-->
<template mode="preproc:expand" priority="5" match="
    c:cases[
      not( lv:* or t:* )
      and c:otherwise[
        not( preceding-sibling::c:* or following-sibling::c:* )
      ]
    ]
  ">

  <!-- just replace with the content of the otherwise block (do not explicitly
       process c:*, since there may be templates) -->
  <apply-templates select="c:otherwise/*" mode="preproc:expand" />
</template>


<!--
  Optimize away c:sum/c:product blocks that contain one or zero elements, so
  long as they do not contain a generator (since that would remove a ref) or @of
  (since that will actually loop through multiple).

  Note that we should *not* perform these optimizations if there are templates
  awaiting application or any other lv:* nodes that have not been expanded.
-->
<template match="c:sum[ lv:*[ not( @preproc:*) ] or t:* ]
                     |c:product[ lv:*[ not( @preproc:* ) ] or t:* ]"
              mode="preproc:expand" priority="7">
  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:expand" />
  </copy>
</template>

<template match="c:sum[ not( @of or @generates ) and count( c:* ) &lt; 2 ]" mode="preproc:expand" priority="5">
  <apply-templates select="c:*" mode="preproc:expand" />
</template>
<template match="c:product[ not( @of or @generates ) and count( c:* ) &lt; 2 ]" mode="preproc:expand" priority="5">
  <apply-templates select="c:*" mode="preproc:expand" />
</template>


<!-- TODO: We could add shorthand for indexes too, e.g. name[i] or name[0] -->
<template match="
    c:apply[
      @*[
        not(
          local-name() = 'name'
          or local-name() = 'label'
        )
      ]
    ]
  "
  mode="preproc:expand" priority="5">

  <copy>
    <!-- keep the name attribute, which specifies what function to apply -->
    <sequence select="@name, @label" />

    <!-- every other attribute should be converted into an argument -->
    <call-template name="preproc:arg-short-expand" />

    <apply-templates select="c:arg" mode="preproc:expand" />
  </copy>
</template>


<template name="preproc:arg-short-expand">
  <for-each select="@*[
      not(
        local-name() = 'name'
        or local-name() = 'label'
      )
    ]">

    <c:arg name="{local-name()}">
      <c:value-of name="{.}" />
    </c:arg>
  </for-each>
</template>


<template match="lv:rate[ lv:class ]|lv:function[ lv:class ]|lv:yield[ lv:class ]"
  mode="preproc:expand" priority="9">
  <!-- already processed -->
  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:expand" />
  </copy>
</template>

<!--
  Add lv:class nodes containing the values of each individual class

  This eliminates the need to tokenize later and drastically simplifies xpath
  queries.
-->
<template match="lv:rate|lv:function|lv:yield" mode="preproc:expand" priority="5">
  <variable name="self" select="." />

  <variable name="classes" select="tokenize( @class, ' ' )" />
  <variable name="no-classes" select="tokenize( @no, ' ' )" />

  <copy>
    <sequence select="@*" />

    <!-- convert classes into nodes to make life easier down the road (if any) -->
    <for-each select="$classes">
      <if test=".">
        <lv:class ref="{.}" no="false" />
      </if>
    </for-each>

    <for-each select="$no-classes">
      <if test=".">
        <lv:class ref="{.}" no="true" />
      </if>
    </for-each>

    <apply-templates mode="preproc:expand" />
  </copy>
</template>



<!--
  To make life a bit easier, calculate the set type of a classification @yields
  and add it to the node as a @set attribute
-->
<template match="lv:classify" mode="preproc:expand" priority="5">
  <variable name="self" select="." />

  <copy>
    <!-- if there is no @yields attribute, then generate one -->
    <if test="not( @yields )">
      <attribute name="yields">
        <text>__is</text>
        <!-- certain characters are not valid for @yields -->
        <value-of select="translate( @as, '-', '' )" />
      </attribute>

      <attribute name="preproc:yields-generated"
                     select="'true'" />
    </if>

    <apply-templates mode="preproc:expand"
                         select="@*" />

    <!-- copy everything else -->
    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<!--
  Normalize whitespace for class descriptions
-->
<template mode="preproc:expand" priority="5"
              match="lv:classify/@desc">
  <attribute name="desc"
                 select="normalize-space( . )" />
</template>

<!--
  All other class attributes are copied verbatim
-->
<template mode="preproc:expand" priority="1"
              match="lv:classify/@*">
  <sequence select="." />
</template>


<!-- default lv:match/@on short-hand to assert on a value of TRUE -->
<template match="lv:match[ not( @value
                                    or @anyOf
                                    or @pattern
                                    or * ) ]"
  mode="preproc:expand" priority="7">

  <copy>
    <copy-of select="@*" />
    <attribute name="value"
                   select="'TRUE'" />
  </copy>
</template>


<!-- expand lv:match/@value into a c:* expression to simplify static
     analysis -->
<template match="lv:match[ @value ]"
  mode="preproc:expand" priority="7">

  <copy>
    <copy-of select="@*[ not( local-name() = 'value' ) ]" />

    <c:eq>
      <c:value-of name="{@value}" />
    </c:eq>
  </copy>
</template>


<!-- enums have implicit values (as they are, well, enumerated; @value overrides) -->
<!-- TODO: should @value set the next implicit index? -->
<template match="lv:item[ not( @value ) ]" mode="preproc:expand" priority="5">
  <copy>
    <sequence select="@*" />
    <attribute name="value" select="count( preceding-sibling::* )" />
    <apply-templates mode="preproc:expand" />
  </copy>
</template>


<template match="w:display[ @prefix ]" mode="preproc:expand" priority="5">
  <variable name="prefix" select="@prefix" />
  <variable name="children" select="w:*" />

  <for-each select="root(.)//lv:rate[ starts-with( @yields, $prefix ) ]">
    <w:display name="{@yields}">
      <sequence select="$children" />
    </w:display>
  </for-each>
</template>


<!-- remove templates that have been copied from an external source for
     processing -->
<template match="lv:template[
                       @name=root()
                         /preproc:symtable/preproc:sym[ @src ]/@name ]"
  mode="preproc:expand" priority="5">
</template>

<!-- IMPORTANT: do not process unexpanded templates -->
<template match="lv:template" mode="preproc:expand" priority="4">
  <sequence select="." />
</template>


<template match="preproc:symtable" mode="preproc:expand" priority="5">
  <!-- ignore -->
  <sequence select="." />
</template>


<template match="lv:__external-data" mode="preproc:expand" priority="5">
  <!-- intended for use by code generators; data is not retained in object file
       unless some other process overrides this template -->
</template>

</stylesheet>
