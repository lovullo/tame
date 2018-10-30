<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Handles calculation output in LaTeX format for styling by Mathjax

  Copyright (C) 2016, 2017 R-T Specialty, LLC.

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
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Recursively apply any child equations and then do the same in calc-after mode

  @return LaTeX equation
-->
<xsl:template match="c:*" mode="calc-recurse">
  <xsl:apply-templates select="./c:*" />

  <!-- invoke `after' templates, which allows inserting data for display after
       the initial equation -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<!--
  Style sum of values as a LaTeX equation

  Note that this does not deal with the summation of a series; that's left to
  the handling of the @of attribute.

  @return LaTeX equation
-->
<xsl:template match="c:sum">
  <xsl:apply-templates select="." mode="sum-body" />
</xsl:template>

<xsl:template match="c:sum" mode="sum-body">
  <xsl:for-each select="./c:*">
    <!-- get value to display -->
    <xsl:variable name="display">
      <xsl:apply-templates select="." />
    </xsl:variable>

    <!-- delimit with +s if not first; if we're adding a negative, omit the
         addition symbol as well (unless we're displaying a product, since
         multiplying by a negative would otherwise appear to be subtraction) -->
    <xsl:if test="
        ( position() > 1 )
        and (
          not( substring( $display, 1, 1 ) = '-' )
          or ( local-name() = 'product' )
        )
      ">

      <xsl:text> + </xsl:text>
    </xsl:if>

    <!-- the only reason we would have a sum within a sum (that doesn't use
         sigma-notation) is for grouping -->
    <xsl:if test="( local-name() = 'sum' ) and not( @of )">
      <xsl:text>\left(</xsl:text>
    </xsl:if>

      <xsl:copy-of select="$display" />

    <xsl:if test="( local-name() = 'sum' ) and not( @of )">
      <xsl:text>\right)</xsl:text>
    </xsl:if>
  </xsl:for-each>

  <!-- since we looped manually, we must also invoke `after' templates
       manually -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<!--
  Style summation of a set as a LaTeX equation

  Note that @of deals witht summation of sets only (rather, using the index of a
  set to sum over the provided calculation). See the other template(s) for
  summing values without a set.

  An index may optionally be provided via an @index attribute; otherwise, one
  will be chosen for you. The index is used for the lower limit; the upper limit
  is omitted. The child nodes are then used to generate the equation to be
  applied by the summation.

  If no child nodes are provided, then the summation is meant to imply that each
  value in the set should be summed. Adding child nodes overrides this behavior.

  @return LaTeX equation
-->
<xsl:template match="c:sum[@of]">
  <xsl:variable name="of" select="@of" />

  <!-- if no index is provided, simply use its symbol to indicate all values
       within its domain -->
  <xsl:variable name="index">
    <xsl:choose>
      <xsl:when test="@index">
        <xsl:value-of select="preproc:tex-index( @index )" />
      </xsl:when>

      <xsl:otherwise>
        <!-- TODO: Determine an index that is not in use -->
        <xsl:text>k</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="ref">
    <xsl:value-of select="@of" />
  </xsl:variable>

  <!-- retrieve the symbol associated with this value (no index) -->
  <xsl:variable name="symbol">
    <xsl:call-template name="get-symbol">
      <xsl:with-param name="name" select="$ref" />
      <xsl:with-param name="search" select="/" />
    </xsl:call-template>
  </xsl:variable>

  <!-- also retrieve the symbol without its index -->

  <!-- if an index was provided, set the lower limit to 0 (we do this in a
       separate variable so that we can display the symbol on its own elsewhere)
       -->
  <xsl:variable name="index-limit">
    <xsl:value-of select="$index" />

    <!-- we only need the explicit notation if we are summing more than the
         set -->
    <xsl:if test="./c:*">
      <xsl:text>=0</xsl:text>
    </xsl:if>
  </xsl:variable>

  <xsl:text>\sum \limits_{</xsl:text>
  <xsl:value-of select="$index-limit" />
  <xsl:text>}</xsl:text>

  <!-- upper limit is only necessary for clarification if they have provided a
       more complex expression; if we're only summing over a single set, then
       the extra notation is unnecessary and will just clutter -->
  <xsl:if test="./c:*">
    <!-- the upper limit of the summation will be denoted by #S, where S is the
         symbol for a given set -->
    <xsl:text>^{\left|</xsl:text>
      <xsl:copy-of select="$symbol" />
    <xsl:text>\right|-1}</xsl:text>
  </xsl:if>

  <!-- if no children are provided, just sum @of -->
  <xsl:if test="not(./c:*)">
    <!-- output the symbol followed by its index, only if an index was provided
         (and is therefore necessary) -->
    <xsl:call-template name="get-symbol">
      <xsl:with-param name="name" select="$ref" />
      <xsl:with-param name="index-symbol" select="$index" />
      <xsl:with-param name="search" select="/" />
    </xsl:call-template>
  </xsl:if>

  <!-- output any additional expressions, if any -->
  <xsl:apply-templates select="." mode="sum-body" />
</xsl:template>


<xsl:function name="preproc:tex-index" as="xs:string?">
  <xsl:param name="index" as="xs:string?" />

  <!-- TODO: there are potential display conflicts with the below
       replacement -->

  <!-- underscores are frequently used for internal names, but the display
       is lousy, so just remove this. -->
  <xsl:sequence select="if ( $index ) then
                            replace( $index, '_', '' )
                          else
                            ()" />
</xsl:function>


<!--
  Style product of values as a LaTeX equation

  Note that this does not deal with the product of a series; that's left to
  the handling of the @of attribute (TODO: @of not yet implemented for
  products).

  @return LaTeX equation
-->
<xsl:template match="c:product">
  <xsl:variable name="enclose" select="
      @dot='true'
      and (
        preceding-sibling::c:*
        or following-sibling::c:*
      )
    " />

  <xsl:if test="$enclose">
    <xsl:text>(</xsl:text>
  </xsl:if>

  <xsl:for-each select="./c:*">
    <!-- Function symbols can have multiple chars, so we'll need to add the
         multiplication symbol. Adjacent constants should also be separated by a
         dot, otherwise it'll look like one giant number. -->
    <xsl:if test="
        (
          local-name() = 'apply'
          or ../@dot = 'true'
          or (
            ( local-name() = 'const' )
            and ( local-name( preceding-sibling::*[1] ) = 'const' )
          )
        )
        and ( position() > 1 )
      ">
      <xsl:text> \,\cdot\, </xsl:text>
    </xsl:if>

    <!-- if precedence of this operation is lower, we will need to include
         parenthesis -->
    <!-- XXX: Relies on hard-coded precedence rules in multiple locations;
         refactor! -->
    <xsl:if test="
        ( local-name() = 'sum' )
        or ( ( local-name() = 'product' ) and not( @of ) )
      ">
      <xsl:text>\left(</xsl:text>
    </xsl:if>

    <xsl:apply-templates select="." />

    <!-- close parenthesis -->
    <xsl:if test="
        ( local-name() = 'sum' )
        or ( ( local-name() = 'product' ) and not( @of ) )
      ">
      <xsl:text>\right)</xsl:text>
    </xsl:if>
  </xsl:for-each>

  <xsl:if test="$enclose">
    <xsl:text>)</xsl:text>
  </xsl:if>

  <!-- since we looped manually, we must also invoke `after' templates
       manually -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<!--
  Style quotient of two values as a LaTeX equation

  This is used to divide two values and will be styled as a fraction. The
  numerator should be the first calculation node and the denominator the second;
  there should be no additional nodes.

  @return LaTeX equation
-->
<xsl:template match="c:quotient">
  <!-- numerator (first child) -->
  <xsl:text>\frac{</xsl:text>
  <xsl:apply-templates select="./c:*[1]" />

  <!-- denominator (second child) -->
  <xsl:text>}{</xsl:text>
  <xsl:apply-templates select="./c:*[2]" />
  <xsl:text>}</xsl:text>

  <!-- since we processed manually, we must also invoke `after' templates
       manually -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<xsl:template match="c:let">
  <!-- process the body of the let expression (the variables should have been
       processed separately) -->
  <xsl:apply-templates select="c:*" />
</xsl:template>


<!--
  Style a value for display within a LaTeX equation

  Forwards to calc-get-value template.
-->
<xsl:template match="c:value-of">
  <xsl:apply-templates select="." mode="calc-get-value" />
</xsl:template>


<!--
  Values from a c:let must have their names altered before looking up the symbol
-->
<xsl:template match="c:*[ @name=ancestor::c:let/c:values/c:value/@name ]" mode="calc-get-value">
  <xsl:call-template name="calc-get-value">
    <!-- :<let-name>:<our-name> -->
    <xsl:with-param name="name" select="
        concat( ':', ancestor::c:let[1]/@name, ':', @name )
      " />
  </xsl:call-template>
</xsl:template>


<!--
  Style a value for display within a LaTeX equation

  By default, the symbol for the given value (variable) will be rendered in
  place of this node.

  This element is not expected to have any children.

  XXX: Refactor me; there are more clear and less convoluted ways to accomplish
  this.

  @return LaTeX equation
-->
<xsl:template name="calc-get-value" match="c:*" mode="calc-get-value">
  <xsl:param name="name" select="@name" />

  <xsl:variable name="index-symbol">
    <xsl:if test="./c:index">
      <xsl:for-each select="./c:index">
        <!-- separate multiple indexes with commas -->
        <xsl:if test="position() > 1">
          <xsl:text>,</xsl:text>
        </xsl:if>

        <xsl:apply-templates select="./c:*[1]" />
      </xsl:for-each>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="sym" select="
      /lv:*/preproc:symtable/preproc:sym[
        @name=$name
      ]
    " />


  <xsl:variable name="value">
    <xsl:choose>
      <!-- for scalar constants that do not have a symbol, simply inline their
           value (if they have a symbol, then it is assumed that their symbolic
           meaning is more meaningful than its value) -->
      <xsl:when test="
          $sym[
            @type='const'
            and @dim='0'
            and ( not( @text ) or @tex='' )
          ]
        ">
        <xsl:value-of select="$sym/@value" />
      </xsl:when>

      <!-- local index (generated with @of) -->
      <xsl:when test="ancestor::c:*[ @of and @index=$name ]">
        <xsl:value-of select="$name" />
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="get-symbol">
          <xsl:with-param name="name" select="$name" />
          <xsl:with-param name="index" select="preproc:tex-index( @index )" />
          <xsl:with-param name="index-symbol" select="$index-symbol" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:copy-of select="$value" />

  <!-- yes, a value still may have things to appear after it -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<!--
  Style a constant value for use in a LaTeX equation

  Constant values are not treated as variables; instead, their value (rather
  than their symbol) is immediately rendered.

  Use this only if the value itself makes more sense (and is more clear) than a
  variable.

  This element is not expected to have any children.

  @return LaTeX equation
-->
<xsl:template match="c:const">
  <!-- a constant value of 1 with Iverson's brackets is redundant -->
  <xsl:if test="not( ( @value = '1' ) and ./c:when )">
    <!-- display constant value -->
    <xsl:value-of select="@value" />
  </xsl:if>

  <!-- a constant may still have things to appear after it -->
  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<xsl:template match="c:ceil|c:floor">
  <xsl:text>\left\l</xsl:text>
  <xsl:value-of select="local-name()" />
  <xsl:text> </xsl:text>
    <xsl:apply-templates select="." mode="calc-recurse" />
  <xsl:text>\right\r</xsl:text>
  <xsl:value-of select="local-name()" />
</xsl:template>


<!--
  Styles a function application for display in a LaTeX equation

  Indicates a function application. A call to the function, with each of its
  arguments in parenthesis, will be rendered.

  @return LaTeX equation
-->
<xsl:template match="c:apply">
  <xsl:variable name="self" select="." />
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="fsym" select="//lv:function[@name=$name]/@sym" />

  <xsl:call-template name="get-symbol">
    <xsl:with-param name="name" select="@name" />
    <xsl:with-param name="search" select="/" />
  </xsl:call-template>

  <!-- if a symbol is provided, then omit the parenthesis -->
  <xsl:if test="not( $fsym )">
    <xsl:text>\left(</xsl:text>
  </xsl:if>

  <!-- output the symbol associated with the value of each argument -->
  <xsl:for-each select="./c:arg">
    <!-- delimit params with a comma -->
    <xsl:if test="position() > 1">
      <xsl:text>,</xsl:text>
    </xsl:if>

    <xsl:variable name="valname" select="./c:*[1]/@name" />

    <xsl:choose>
      <!-- if this variable has been defined as an index, then simply output
           it -->
      <xsl:when test="ancestor::c:*[ @of and @index=$valname ]">
        <xsl:value-of select="$valname" />
      </xsl:when>

      <!-- display the value of constants -->
      <xsl:when test="local-name( ./c:*[1] ) = 'const'">
        <xsl:value-of select="./c:*[1]/@value" />
      </xsl:when>

      <!-- otherwise, it's some other variable and we must look up its
           symbol -->
      <xsl:otherwise>
        <xsl:apply-templates select="./c:*[1]" />
      </xsl:otherwise>
    </xsl:choose>

    <!-- we may have c:when, etc (args are their own sub-equations) -->
    <xsl:apply-templates select="./c:*[1]/c:*" mode="calc-after" />
  </xsl:for-each>

  <xsl:if test="not( $fsym )">
    <xsl:text>\right)</xsl:text>
  </xsl:if>

  <xsl:apply-templates select="./c:*" mode="calc-after" />
</xsl:template>


<!--
  Outputs Iverson's brackets only if forced; see calc-after mode

  This is hidden by default until calc-after to ensure that this is display
  *after* the equation is output.

  @param boolean force-show optionally force the display of the notation

  @return LaTeX equation
-->
<xsl:template match="c:when">
  <xsl:param name="force-show" select="false()" />
  <xsl:param name="standalone" select="false()" />

  <!-- by default, we want to defer showing this until after the equation has
       been output; however, the caller can force it to be displayed if needed -->
  <xsl:if test="$force-show = true()">
    <xsl:apply-templates select="." mode="calc-after">
      <xsl:with-param name="standalone" select="$standalone" />
    </xsl:apply-templates>
  </xsl:if>
</xsl:template>


<!--
  Outputs Iverson's brackets

  This is called *after* the equation is output

  @return LaTeX equation
-->
<xsl:template match="c:when" mode="calc-after" priority="5">
  <xsl:param name="brackets" select="true()" />
  <xsl:param name="standalone" select="false()" />

  <xsl:variable name="preceding" select="preceding-sibling::c:when" />

  <!-- output bracket only if (a) requested and (b) first in set -->
  <xsl:if test="$brackets and ( $standalone or not( $preceding ) )">
    <xsl:text>\left[</xsl:text>
  </xsl:if>

  <!-- if we do have a preceding sibling, prefix with "and" -->
  <xsl:if test="not( $standalone ) and $preceding">
    <xsl:text>\text{ and }</xsl:text>
  </xsl:if>

    <!-- output the symbol for the variable we are comparing against -->
    <xsl:apply-templates select="." mode="calc-get-value" />

    <xsl:text> </xsl:text>
    <xsl:apply-templates select="./c:*" mode="calc-iversons" />

  <!-- output bracket only if (a) requested and (b) last in set -->
  <xsl:if test="
      $brackets and ( $standalone or not( following-sibling::c:when ) )
    ">

    <xsl:text>\right]</xsl:text>
  </xsl:if>
</xsl:template>


<xsl:template match="c:cases">
  <xsl:text>\begin{cases}</xsl:text>
    <xsl:apply-templates select="./c:case|./c:otherwise" />
  <xsl:text>\end{cases}</xsl:text>

  <!-- if any equations immediately follow, add some extra space so as not to
       confuse the reader -->
  <xsl:if test="following-sibling::c:*">
    <xsl:text>\;\;\;</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  Generate a case

  When $force-show is provided, as it is when displaying only small portions of
  an equation, it will display the line using Iverson's brackets.
-->
<xsl:template match="c:cases/c:case|c:cases/c:otherwise">
  <xsl:param name="force-show" select="false()" />

  <!-- generate value -->
  <xsl:apply-templates select="./c:*[ not( local-name() = 'when' ) ][1]" />

  <xsl:choose>
    <xsl:when test="not( $force-show )">
      <xsl:text> &amp; </xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <xsl:text> [</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:choose>
    <xsl:when test="local-name() != 'otherwise'">
      <xsl:if test="not( $force-show )">
        <xsl:text>\text{if } </xsl:text>
      </xsl:if>

      <!-- generate condition under which this value will apply -->
      <xsl:apply-templates select="./c:when" mode="calc-after">
        <xsl:with-param name="brackets" select="false()" />
      </xsl:apply-templates>
    </xsl:when>

    <xsl:otherwise>
      <xsl:text>\text{otherwise}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

  <!-- determine how we end the line (more cases or end?) -->
  <xsl:choose>
    <xsl:when test="$force-show">
      <xsl:text>]</xsl:text>
    </xsl:when>

    <xsl:when test="following-sibling::c:case|following-sibling::c:otherwise">
      <xsl:text>; \\</xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <xsl:text>.</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Do nothing with calc-after for any unmatched calculations
-->
<xsl:template match="c:*" mode="calc-after" priority="1">
  <!-- make sure nothing is done for all other nodes with calc-after -->
</xsl:template>


<!--
  Display a notation intended for use within Iverson's brackets

  @return LaTeX equation
-->
<xsl:template match="c:eq|c:ne|c:gt|c:lt|c:gte|c:lte" mode="calc-iversons">
  <xsl:param name="recurse" select="true()" />

  <xsl:variable name="name" select="local-name()" />

  <!-- map to LaTeX equivalent -->
  <xsl:variable name="map">
    <c id="eq">=</c>
    <c id="ne">\not=\;</c>
    <c id="gt">\gt</c>
    <c id="lt">\lt</c>
    <c id="gte">\geq</c>
    <c id="lte">\leq</c>
  </xsl:variable>

  <xsl:value-of select="$map/*[ @id=$name ]" />
  <xsl:text> </xsl:text>

  <xsl:if test="$recurse">
    <xsl:apply-templates select="." mode="calc-recurse" />
  </xsl:if>
</xsl:template>


<!--
  Style vectors
-->
<xsl:template match="c:set|c:vector" priority="1">
  <xsl:text>\left[</xsl:text>
    <xsl:for-each select="./c:*">
      <xsl:if test="position() > 1">
        <xsl:text>,</xsl:text>
      </xsl:if>

      <xsl:apply-templates select="." />
    </xsl:for-each>
  <xsl:text>\right]^T</xsl:text>
</xsl:template>


<!--
  Style vector of vector as matrix
-->
<xsl:template priority="5"
              match="c:set[ c:set ]
                     |c:vector[ c:vector ]">
  <xsl:text>\left[\begin{array}\\</xsl:text>

  <xsl:for-each select="c:set|c:vector">
    <xsl:if test="position() > 1">
      <xsl:text>\\</xsl:text>
    </xsl:if>

    <xsl:for-each select="./c:*">
      <xsl:if test="position() > 1">
        <xsl:text disable-output-escaping="yes"> &amp; </xsl:text>
      </xsl:if>

      <xsl:text>{</xsl:text>
        <xsl:apply-templates select="." />
      <xsl:text>}</xsl:text>
    </xsl:for-each>
  </xsl:for-each>

  <xsl:text>\end{array}\right]</xsl:text>
</xsl:template>


<xsl:template match="c:length-of">
  <xsl:text>\left|\left(</xsl:text>
    <xsl:apply-templates select="./c:*[1]" />
  <xsl:text>\right)\right|</xsl:text>
</xsl:template>

<xsl:template match="c:cons">
  <xsl:text>\textrm{cons}\left(</xsl:text>
    <xsl:apply-templates select="./c:*[1]" />
  <xsl:text>,</xsl:text>
    <xsl:apply-templates select="./c:*[2]" />
  <xsl:text>\right)</xsl:text>
</xsl:template>

<xsl:template match="c:car">
  <xsl:text>\left(</xsl:text>
  <xsl:apply-templates select="./c:*[1]" />
  <xsl:text>\right)_0</xsl:text>
</xsl:template>

<xsl:template match="c:cdr">
  <xsl:text>\textrm{cdr}\left(</xsl:text>
  <xsl:apply-templates select="./c:*[1]" />
  <xsl:text>\right)</xsl:text>
</xsl:template>

</xsl:stylesheet>
