<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Handles node expansion

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

  This process is responsible for expanding shorthand and various other data
  into a consistent format for the compiler and other processes.
-->
<xsl:stylesheet
    version="2.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:preproc="http://www.lovullo.com/rater/preproc"
    xmlns:lv="http://www.lovullo.com/rater"
    xmlns:t="http://www.lovullo.com/rater/apply-template"
    xmlns:c="http://www.lovullo.com/calc"
    xmlns:w="http://www.lovullo.com/rater/worksheet">


<xsl:include href="domain.xsl" />


<xsl:template match="lv:package[ not( @preproc:name ) ]"
              mode="preproc:expand" priority="5"
              as="element( lv:package )">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <!-- generate name from source package identifier -->
    <xsl:attribute name="name" select="$__srcpkg" />

    <!-- relative path to root src directory (for resolving absolute include
         paths) -->
    <xsl:attribute name="__rootpath" select="$__relroot" />

    <!-- TODO: temporary; remove -->
    <xsl:attribute name="preproc:name" select="$__srcpkg" />

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>



<xsl:template match="*" mode="preproc:expand" priority="1">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!-- imports relative to project root -->
<xsl:template match="lv:import[ starts-with( @package, '/' ) ]" mode="preproc:expand" priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <!-- resolve path into path relative to project root -->
    <xsl:attribute name="package">
      <xsl:call-template name="__apply-relroot">
        <xsl:with-param name="path" select="@package" />
      </xsl:call-template>
    </xsl:attribute>
  </xsl:copy>
</xsl:template>


<!--
  Domain data are extracted from typedefs

  Eventually, the typedefs will be converted into templates and removed entirely.
-->
<xsl:template match="lv:typedef"
  mode="preproc:expand" priority="5">

  <xsl:apply-templates select="." mode="preproc:mkdomain" />

  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!--
  Infer primitive type if not provided.
-->
<xsl:template mode="preproc:expand"
              match="c:const[ not( @type ) ]
                     |lv:const[ not( @type ) ]"
              priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="type"
                   select="if ( substring-before( @value, '.' ) ) then
                             'float'
                           else
                             'integer'" />

    <xsl:sequence select="*" />
  </xsl:copy>
</xsl:template>


<!--
  Translate dimension aliases (e.g. scaler, vector, matrix) into respective
  numeric representations.
-->
<xsl:template mode="preproc:expand" priority="8"
              match="c:*[ @dim
                          and not( string( @dim ) castable as xs:integer ) ]">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <!-- replace dim with numeric -->
    <xsl:attribute name="dim">
      <xsl:choose>
        <xsl:when test="@dim = 'scaler' or @dim = ''">
          <xsl:sequence select="0" />
        </xsl:when>

        <xsl:when test="@dim = 'vector'">
          <xsl:sequence select="1" />
        </xsl:when>

        <xsl:when test="@dim = 'matrix'">
          <xsl:sequence select="2" />
        </xsl:when>

        <xsl:otherwise>
          <xsl:message terminate="yes"
                       select="concat(
                                 '!!! [preproc] error: ',
                                 'unknown dimension alias ''',
                                 @dim, '''' )" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>

    <xsl:apply-templates select="node()" mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!--
  Give let's a name so that they may be easily referenced uniquely
-->
<xsl:template match="c:let[ not( @name ) ]" mode="preproc:expand" priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:attribute name="name" select="generate-id(.)" />

    <xsl:apply-templates select="*" mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!--
  Default label of c:let value expressions to description if no label is provided

  This is useful for breakdown display.

  TODO: play well with others; if we change the priority back to 5, we introduce
  match ambiguities
-->
<xsl:template match="c:let/c:values/c:value/c:*[1][ not( @label ) ]" mode="preproc:expand" priority="4">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <!-- default the label to the description of the parent c:value -->
    <xsl:attribute name="label" select="../@desc" />

    <xsl:apply-templates select="*" mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!--
  c:when with no children is shorthand for > 0

  Note: we check for any children because we may have things like
  template applications that we do not want wiped out.
-->
<xsl:template match="c:when[ not( * ) ]" mode="preproc:expand" priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <c:gt>
      <c:value-of name="FALSE" />
    </c:gt>
  </xsl:copy>
</xsl:template>


<!--
  c:when with multiple children
-->
<xsl:template match="c:when[ count( c:* ) gt 1 ]" mode="preproc:expand" priority="5">
  <xsl:variable name="when" select="." />

  <!-- expand into adjacent c:when's -->
  <xsl:for-each select="c:*">
    <c:when>
      <xsl:sequence select="$when/@*" />
      <xsl:apply-templates select="." mode="preproc:expand" />
    </c:when>
  </xsl:for-each>
</xsl:template>


<!--
  Recursion shorthand

  This exists simply because function application is so verbose and, when
  recursing, generally only a small fraction of the arguments actually change.
-->
<xsl:template match="c:recurse" mode="preproc:expand" priority="5">
  <xsl:variable name="self" select="." />
  <xsl:variable name="fname" select="ancestor::lv:function/@name" />
  <xsl:variable name="overrides" select="./c:arg" />

  <c:apply name="{$fname}">
    <!-- every non-@name attribute should be converted into an argument -->
    <xsl:call-template name="preproc:arg-short-expand" />

    <!-- include all non-overridden args -->
    <xsl:for-each select="
        ancestor::lv:function/lv:param[
          not(
            @name=$overrides/@name
            or @name=$self/@*/local-name()
          )
        ]
      ">

      <!-- copy the arg value -->
      <c:arg name="{@name}">
        <c:value-of name="{@name}" />
      </c:arg>
    </xsl:for-each>

    <!-- copy in the overrides -->
    <xsl:apply-templates select="$overrides" mode="preproc:expand" />
  </c:apply>
</xsl:template>


<!-- metadata constants have different semantics -->
<!-- TODO: maybe ignore single-quoted? -->
<xsl:template mode="preproc:expand" priority="6"
              match="lv:meta/lv:prop/lv:const">
  <xsl:sequence select="." />
</xsl:template>


<!-- constants that contain 'e' (scientific notation) should be expanded; allows
     for avoiding constants with many zeroes, which is hard to read -->
<xsl:template mode="preproc:expand" priority="5"
    match="c:const[ substring-before( @value, 'e' ) ]
           |lv:const[ substring-before( @value, 'e' ) ]">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="value">
      <xsl:call-template name="preproc:expand-e">
        <xsl:with-param name="number" select="@value" />
      </xsl:call-template>
    </xsl:attribute>

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>

<xsl:template mode="preproc:expand" priority="5"
    match="c:const[ substring-before( @value, 'm' ) ]
           |lv:const[ substring-before( @value, 'm' ) ]">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="value">
      <xsl:call-template name="preproc:expand-e">
        <xsl:with-param name="number"
          select="concat( substring-before( @value, 'm' ), 'e6' )" />
      </xsl:call-template>
    </xsl:attribute>

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>

<xsl:template mode="preproc:expand" priority="5"
    match="c:const[ substring-before( @value, 'k' ) ]
           |lv:const[ substring-before( @value, 'k' ) ]">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="value">
      <xsl:call-template name="preproc:expand-e">
        <xsl:with-param name="number"
          select="concat( substring-before( @value, 'k' ), 'e3' )" />
      </xsl:call-template>
    </xsl:attribute>

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!-- expand scientific notation -->
<!-- XXX: negatives not currently supported -->
<xsl:template name="preproc:expand-e">
  <xsl:param name="number" />
  <xsl:param name="whole" select="substring-before( $number, '.' )" />
  <xsl:param name="dec"   select="substring-before( substring-after( $number, '.' ), 'e' )" />
  <xsl:param name="count" as="xs:double"
             select="number( substring-after( $number, 'e' ) )" />

  <!-- output the whole number portion -->
  <xsl:choose>
    <xsl:when test="$whole and not( $whole = '' )">
      <xsl:value-of select="$whole" />
    </xsl:when>

    <!-- if no decimal was provided, then use the entire number before 'e' -->
    <xsl:when test="$number and not( $number = '' ) and ( $whole = '' )">
      <xsl:value-of select="substring-before( $number, 'e' )" />
    </xsl:when>
  </xsl:choose>

  <xsl:choose>
    <xsl:when test="$count > 0">
      <xsl:choose>
        <!-- if we have a decimal, then use the first digit (as if we moved one
             place to the right) -->
        <xsl:when test="$dec and not( $dec = '' )">
          <xsl:value-of select="substring( $dec, 1, 1 )" />
        </xsl:when>

        <!-- no decimal portion remaining; fill with 0 -->
        <xsl:otherwise>
          <xsl:text>0</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <!-- recursively expand -->
      <xsl:call-template name="preproc:expand-e">
        <!-- already processed the whole -->
        <xsl:with-param name="whole" select="''" />

        <xsl:with-param name="dec">
          <!-- move to the right one decimal place; otherwise, no decimal -->
          <xsl:if test="$dec">
            <xsl:value-of select="substring( $dec, 2 )" />
          </xsl:if>
        </xsl:with-param>

        <xsl:with-param name="count" select="$count - 1" />
      </xsl:call-template>
    </xsl:when>

    <!-- output the remaining decimal, if any -->
    <xsl:otherwise>
      <xsl:if test="$dec and not( $dec = '' )">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$dec" />
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Optimize away c:cases if they contain only c:otherwise

  This is useful primarily for templates that may create a case statement for
  conditional operations (using lv:if/lv:unless) and ensures that there is no
  penalty for doing so if none of the template conditions result in a c:case.

  Note that we should *not* perform these optimizations if there are templates
  awaiting application or any other lv:* nodes that have not been expanded.
-->
<xsl:template mode="preproc:expand" priority="5" match="
    c:cases[
      not( lv:* or t:* )
      and c:otherwise[
        not( preceding-sibling::c:* or following-sibling::c:* )
      ]
    ]
  ">

  <!-- just replace with the content of the otherwise block (do not explicitly
       process c:*, since there may be templates) -->
  <xsl:apply-templates select="c:otherwise/*" mode="preproc:expand" />
</xsl:template>


<!--
  Optimize away c:sum/c:product blocks that contain one or zero elements, so
  long as they do not contain a generator (since that would remove a ref) or @of
  (since that will actually loop through multiple).

  Note that we should *not* perform these optimizations if there are templates
  awaiting application or any other lv:* nodes that have not been expanded.
-->
<xsl:template match="c:sum[ lv:*[ not( @preproc:*) ] or t:* ]
                     |c:product[ lv:*[ not( @preproc:* ) ] or t:* ]"
              mode="preproc:expand" priority="7">
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>

<xsl:template match="c:sum[ not( @of or @generates ) and count( c:* ) &lt; 2 ]" mode="preproc:expand" priority="5">
  <xsl:apply-templates select="c:*" mode="preproc:expand" />
</xsl:template>
<xsl:template match="c:product[ not( @of or @generates ) and count( c:* ) &lt; 2 ]" mode="preproc:expand" priority="5">
  <xsl:apply-templates select="c:*" mode="preproc:expand" />
</xsl:template>


<!-- TODO: We could add shorthand for indexes too, e.g. name[i] or name[0] -->
<xsl:template match="
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

  <xsl:copy>
    <!-- keep the name attribute, which specifies what function to apply -->
    <xsl:sequence select="@name, @label" />

    <!-- every other attribute should be converted into an argument -->
    <xsl:call-template name="preproc:arg-short-expand" />

    <xsl:apply-templates select="c:arg" mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<xsl:template name="preproc:arg-short-expand">
  <xsl:for-each select="@*[
      not(
        local-name() = 'name'
        or local-name() = 'label'
      )
    ]">

    <c:arg name="{local-name()}">
      <c:value-of name="{.}" />
    </c:arg>
  </xsl:for-each>
</xsl:template>


<xsl:template match="lv:rate[ lv:class ]|lv:function[ lv:class ]|lv:yield[ lv:class ]"
  mode="preproc:expand" priority="9">
  <!-- already processed -->
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>

<!--
  Add lv:class nodes containing the values of each individual class

  This eliminates the need to tokenize later and drastically simplifies xpath
  queries.
-->
<xsl:template match="lv:rate|lv:function|lv:yield" mode="preproc:expand" priority="5">
  <xsl:variable name="self" select="." />

  <xsl:variable name="classes" select="tokenize( @class, ' ' )" />
  <xsl:variable name="no-classes" select="tokenize( @no, ' ' )" />

  <xsl:copy>
    <xsl:sequence select="@*" />

    <!-- convert classes into nodes to make life easier down the road (if any) -->
    <xsl:for-each select="$classes">
      <xsl:if test=".">
        <lv:class ref="{.}" no="false" />
      </xsl:if>
    </xsl:for-each>

    <xsl:for-each select="$no-classes">
      <xsl:if test=".">
        <lv:class ref="{.}" no="true" />
      </xsl:if>
    </xsl:for-each>

    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>



<!--
  To make life a bit easier, calculate the set type of a classification @yields
  and add it to the node as a @set attribute
-->
<xsl:template match="lv:classify" mode="preproc:expand" priority="5">
  <xsl:variable name="self" select="." />

  <xsl:copy>
    <!-- if there is no @yields attribute, then generate one -->
    <xsl:if test="not( @yields )">
      <xsl:attribute name="yields">
        <xsl:text>__is</xsl:text>
        <!-- certain characters are not valid for @yields -->
        <xsl:value-of select="translate( @as, '-', '' )" />
      </xsl:attribute>
    </xsl:if>

    <xsl:apply-templates mode="preproc:expand"
                         select="@*" />

    <!-- copy everything else -->
    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<!--
  Normalize whitespace for class descriptions
-->
<xsl:template mode="preproc:expand" priority="5"
              match="lv:classify/@desc">
  <xsl:attribute name="desc"
                 select="normalize-space( . )" />
</xsl:template>

<!--
  All other class attributes are copied verbatim
-->
<xsl:template mode="preproc:expand" priority="1"
              match="lv:classify/@*">
  <xsl:sequence select="." />
</xsl:template>


<!-- default lv:match/@on short-hand to assert on a value of TRUE -->
<xsl:template match="lv:match[ not( @value
                                    or @anyOf
                                    or @pattern
                                    or * ) ]"
  mode="preproc:expand" priority="7">

  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:attribute name="value"
                   select="'TRUE'" />
  </xsl:copy>
</xsl:template>


<xsl:template mode="preproc:expand"
              match="lv:join[ @all='true' ]"
              priority="8">
  <xsl:call-template name="preproc:mk-class-join-contents" />
</xsl:template>


<xsl:template mode="preproc:expand"
              match="lv:join"
              priority="7">
  <lv:any>
    <xsl:call-template name="preproc:mk-class-join-contents" />
  </lv:any>
</xsl:template>


<xsl:template name="preproc:mk-class-join-contents">
  <xsl:variable name="prefix" select="@prefix" />

  <!-- TODO: remove lv:template nodes in a pass before this so that this
       check is not necessary -->
  <xsl:for-each select="root(.)/lv:classify[
                        starts-with( @as, $prefix )
                        and not( ancestor::lv:template )
                        ]">
    <lv:match value="TRUE">
      <xsl:attribute name="on">
        <xsl:choose>
          <xsl:when test="@yields">
            <xsl:value-of select="@yields" />
          </xsl:when>

          <xsl:otherwise>
            <xsl:text>__is</xsl:text>
            <xsl:value-of select="@as" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
    </lv:match>
  </xsl:for-each>
</xsl:template>


<!-- enums have implicit values (as they are, well, enumerated; @value overrides) -->
<!-- TODO: should @value set the next implicit index? -->
<xsl:template match="lv:item[ not( @value ) ]" mode="preproc:expand" priority="5">
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:attribute name="value" select="count( preceding-sibling::* )" />
    <xsl:apply-templates mode="preproc:expand" />
  </xsl:copy>
</xsl:template>


<xsl:template match="w:display[ @prefix ]" mode="preproc:expand" priority="5">
  <xsl:variable name="prefix" select="@prefix" />
  <xsl:variable name="children" select="w:*" />

  <xsl:for-each select="root(.)//lv:rate[ starts-with( @yields, $prefix ) ]">
    <w:display name="{@yields}">
      <xsl:sequence select="$children" />
    </w:display>
  </xsl:for-each>
</xsl:template>


<!-- remove templates that have been copied from an external source for
     processing -->
<xsl:template match="lv:template[
                       @name=root()
                         /preproc:symtable/preproc:sym[ @src ]/@name ]"
  mode="preproc:expand" priority="5">
</xsl:template>

<!-- IMPORTANT: do not process unexpanded templates -->
<xsl:template match="lv:template" mode="preproc:expand" priority="4">
  <xsl:sequence select="." />
</xsl:template>


<xsl:template match="preproc:symtable" mode="preproc:expand" priority="5">
  <!-- ignore -->
  <xsl:sequence select="." />
</xsl:template>


<xsl:template match="lv:__external-data" mode="preproc:expand" priority="5">
  <!-- intended for use by code generators; data is not retained in object file
       unless some other process overrides this template -->
</xsl:template>

</xsl:stylesheet>

<!--
Footnotes

What!? We need footnotes for this project!?

[0] This was a complicated issue to begin dealing with due to the information we
    need and the information that is available. In particular, at this point, we
    would like to exclude any non-external objects from appearing in the
    rate-only output, but we cannot do that, since the preprocessor has not yet
    reached that block! We cannot reorder, because the class order block depends
    on the rate block!

    (See the commit that introduced this footnote: In the past, the template at
    this position passed an `external-only' flag to the template.)

    Originally, the method was to ``guess'' based on the how the system was
    currently being used (dangerous, but was meant to be temporary...though we
    know how that goes...): implicit externals were ignored, meaning it may not
    function as we would like. Now, this could be resolved in the short-term by
    actually addinging explicit external attributes to everything that needed it
    so that this code would run smoothly. Worked great! Well, so I had thought.

    Then it came down to a certain classification that used the `frame'
    classification. This classification was only used by an external
    classification in scottsdale, but other companies used it for rating, so the
    @external classifier was inappropriate. Of course, the system could easily
    figure out if it was to be marked as external or not (it already does), but
    we do not yet have access to that information. Therefore, what ended up
    happening, was the frame classification was excluded from the classifier,
    excluded from one of the iterations that this footnote references (because
    it was not explicitly external) and included elsewhere where we didn't care
    if it was external or not. When the dependency tree was flattened to
    determine compilation order, the classification that uses `frame' was
    compiled *before* the `frame' classification itself, due to that exclusion!
    Since `frame' was excluded from the classifier, therefore, it was always
    false! That is the situation I was trying to avoid with the explicit
    @external attributes, but here, that solution would not work.

    Therefore, checking for external-only here will not work; we must output
    everything and work on post-processing the data once everything is said and
    done, to remove the duplicates that are also present in the classifier.
-->
