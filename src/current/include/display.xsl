<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Display-related tasks

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
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:sym="http://www.lovullo.com/rater/symbol-map"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:summary="http://www.lovullo.com/rater/summary"

  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl">


<!-- maps certain elements to their default symbols -->
<xsl:variable name="symbol-map" select="document( 'symbol-map.xml' )/sym:symbol-map/*" />

<!-- easy-to-reference linked dependency list -->
<xsl:variable name="edeps" select="/lv:*/preproc:deps/preproc:sym" />

<xsl:template name="get-symbol-map">
  <xsl:copy-of select="$symbol-map" />
</xsl:template>


<xsl:template match="preproc:sym[ @type='rate' ]" mode="summary:desc" priority="5">
  <span class="letlist-{@name}">
    <a href="#{@name}">
      <xsl:value-of select="@name" />
    </a>
    <xsl:text> scalar</xsl:text>
  </span>
</xsl:template>


<xsl:template match="preproc:sym[ @type='gen' ]" mode="summary:desc" priority="5">
  <span class="letlist-{@parent}">
    <a href="#{@parent}">
      <xsl:value-of select="@name" />
    </a>
    <xsl:text> generator; vector</xsl:text>

    <span class="param">
      <xsl:text> (</xsl:text>
        <a href="#{@parent}">
          <xsl:value-of select="@parent" />
        </a>
      <xsl:text>)</xsl:text>
    </span>
  </span>
</xsl:template>


<xsl:template match="preproc:sym[ @type='cgen' ]" mode="summary:desc" priority="5">
  <xsl:variable name="parent" select="@parent" />
  <xsl:variable name="sym" select="
      ancestor::preproc:symtable/preproc:sym[ @name=$parent ]
    " />

  <xsl:apply-templates select="$sym" mode="summary:desc" />
</xsl:template>


<xsl:template match="preproc:sym[ @type='class' ]" mode="summary:desc" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="document" select="
      if ( @src ) then
        document( concat( @src, '.xmlo' ), . )/lv:*
      else
        /lv:*
    " />
  <xsl:variable name="class" select="
      $document/lv:classify[
        @as=substring-after( $name, ':class:' )
      ]
    " />

  <span class="letlist-{$class/@as}">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="$class/@desc" />
    <xsl:text>"</xsl:text>
    <xsl:text> classification </xsl:text>

    <xsl:choose>
      <xsl:when test="@dim = '0'">
        <xsl:text>scalar</xsl:text>
      </xsl:when>

      <xsl:when test="@dim = '1'">
        <xsl:text>vector</xsl:text>
      </xsl:when>

      <xsl:when test="@dim = '2'">
        <xsl:text>matrix</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text> [dim </xsl:text>
          <xsl:value-of select="@dim" />
        <xsl:text>]</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

    <!-- TODO: use generator in letlist-* -->
    <span class="param">
      <xsl:text> (</xsl:text>
      <a href="#:class:{$class/@as}">
        <xsl:value-of select="$class/@as" />
      </a>
      <xsl:text>)</xsl:text>
    </span>
  </span>
</xsl:template>


<xsl:template match="preproc:sym[ @type='const' ]" mode="summary:desc" priority="5">
  <xsl:value-of select="@name" />
</xsl:template>


<xsl:template match="preproc:sym[ @type='param' ]" mode="summary:desc" priority="5">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="document" select="
      if ( @src ) then
        document( concat( @src, '.xmlo' ), . )/lv:*
      else
        /lv:*
    " />
  <xsl:variable name="param" select="
      $document/lv:param[
        @name=$name
      ]
    " />

  <xsl:value-of select="$param/@desc" />

  <span class="param letlist-{$param/@name}">
    <xsl:text> (</xsl:text>
    <a href="#{$param/@name}">
      <xsl:value-of select="$param/@name" />
    </a>
    <xsl:text>)</xsl:text>
  </span>
</xsl:template>


<xsl:template match="preproc:sym" mode="summary:desc" priority="1">
  <xsl:value-of select="@name" />
  <xsl:text> (!)</xsl:text>
</xsl:template>


<xsl:template name="get-symbol">
  <xsl:param name="name" select="@name" />
  <xsl:param name="index" />
  <xsl:param name="index-symbol" />
  <xsl:param name="default" />

  <preproc:sym-ref name="{$name}">
    <!-- might be an empty string (if provided) -->
    <xsl:if test="$default">
      <xsl:attribute name="default" select="$default" />
    </xsl:if>
  </preproc:sym-ref>

  <xsl:choose>
    <xsl:when test="$index-symbol != ''">
      <xsl:text>_{</xsl:text>
        <xsl:value-of select="$index-symbol" />
      <xsl:text>}</xsl:text>
    </xsl:when>

    <xsl:when test="$index">
      <xsl:text>_{</xsl:text>
        <preproc:sym-ref name="{$index}" default="{$index}" />
      <xsl:text>}</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<xsl:template name="_get-index-symbol">
  <xsl:param name="element" />
  <xsl:param name="index" />
  <xsl:param name="search" />

  <xsl:call-template name="get-symbol">
    <xsl:with-param name="name" select="$index" />
    <xsl:with-param name="search" select="$search" />
    <xsl:with-param name="default" select="$index" />
  </xsl:call-template>
</xsl:template>


<!--
  Retrieve the default symbol for the given type (in LaTeX)

  If the type is "function", the given name will be used for its default symbol.

  @param Node    element node to retrieve symbol for
  @param NodeSet search  all document nodes

  @return default symbol (LaTeX)
-->
<xsl:template name="_get-default-symbol">
  <xsl:param name="element" />
  <xsl:param name="name" />
  <xsl:param name="index" />
  <xsl:param name="search" />

  <xsl:variable name="type">
    <xsl:choose>
      <xsl:when test="
        ( local-name( $element ) = 'param' )
        and ( local-name( $element/.. ) = 'function' )">

        <!-- this is a function parameter; make a distinction between a global
             parameter -->
        <xsl:text>fparam</xsl:text>
      </xsl:when>

      <!-- if matching lv:classify/@as, then it represents an accumulator -->
      <xsl:when test="
          ( local-name( $element ) = 'classify' )
          and ( $element/@as = $name )
        ">

        <xsl:text>class</xsl:text>
      </xsl:when>

      <xsl:when test="$element/@generates = $name">
        <xsl:text>generator</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="local-name( $element )" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="symbol" select="$symbol-map[@type=$type]" />

  <!-- output the symbol default -->
  <xsl:choose>
    <!-- certain types use their own name for a default (e.g. functions) -->
    <xsl:when test="$symbol/sym:name">
      <xsl:text>\textrm{</xsl:text>
      <xsl:value-of select="$name" />
      <xsl:text>}</xsl:text>
    </xsl:when>

    <xsl:when test="$symbol/sym:nothing">
      <!-- do nothing; no symbol is desired -->
    </xsl:when>

    <xsl:otherwise>
      <xsl:if test="$index and ( $index != '' )">
        <xsl:text>(</xsl:text>
      </xsl:if>

      <xsl:value-of select="$symbol" />

      <!-- determine if our default index should be subscript or superscript -->
      <xsl:variable name="subsup">
        <xsl:choose>
          <xsl:when test="$symbol/@index-pos">
            <xsl:value-of select="$symbol/@index-pos" />
          </xsl:when>

          <!-- default to subscript -->
          <xsl:otherwise>
            <xsl:text>_</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <!-- in addition to the symbol itself, which alone is not likely to be
           unique, we will add a subscript to uniquely identify it by number -->
      <xsl:if test="$search">
        <xsl:value-of select="$subsup" />
        <xsl:text>{</xsl:text>

        <xsl:call-template name="_get-name-index">
          <xsl:with-param name="element" select="$element" />
          <xsl:with-param name="name" select="$name" />
          <xsl:with-param name="search" select="$search" />
        </xsl:call-template>

        <xsl:text>}</xsl:text>
      </xsl:if>

      <xsl:if test="$index and ( $index != '' )">
        <xsl:text>)</xsl:text>
      </xsl:if>

      <!-- if an index was given, and our default index was *not* a subscript,
           then we can dedicate the subscript to the index -->
      <xsl:if test="$index and ( $index != '' )">
        <xsl:text>_{</xsl:text>
        <xsl:value-of select="$index" />
        <xsl:text>}</xsl:text>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Retrieve index of the element associated with the given name across all named
  elements of the same type and parent type in all of $search

  TODO: surely there's a more performant manner...not that speed is an issue
  right now

  @param Node    element node to retrieve symbol for
  @param NodeSet search  all document nodes

  @return index
-->
<xsl:template name="_get-name-index">
  <xsl:param name="element" />
  <xsl:param name="name" />
  <xsl:param name="search" />

  <xsl:choose>
    <!-- functions are handled slightly differently, as they introduce scope -->
    <xsl:when test="local-name( $element/.. ) = 'function'">
      <xsl:for-each select="$element/../lv:param">
        <xsl:if test="@name = $name">
          <xsl:value-of select="position()" />
        </xsl:if>
      </xsl:for-each>
    </xsl:when>

    <!-- non-function -->
    <xsl:otherwise>
      <xsl:value-of select="
          $search//summary:default-indexes/summary:index[ @name=$name ]/@value"
        />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Retrieve description for the given element by name

  $vardesc, for those who support it, is useful if the description describes the
  node, not a variable generated from it. For example, lv:classify's description
  is a short description of the classification, but if documenting @yields, we
  want to describe what it is yielding, which would not be immediately clear
  from the description.

  @param string  name    name of element
  @param NodeSet search  all documents to search

  @return element description
-->
<xsl:template name="get-desc">
  <xsl:param name="name" />
  <xsl:param name="search" />

  <!-- XXX: Have to maintain this list! -->
  <xsl:variable name="desc"
    select="$search//summary:descs/summary:desc[ @name=$name ]/@desc" />

  <xsl:choose>
    <xsl:when test="$desc">
      <xsl:copy-of select="$desc" />
    </xsl:when>

    <!-- if we cannot find the element, then display an error -->
    <xsl:otherwise>
      <span class="error">
        <xsl:text>Unknown @name reference: </xsl:text>
        <xsl:value-of select="$name" />
      </span>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>





<!--
  Retrieve processed name for the given element by name

  @param string  name   name of element
  @param NodeSet search all documents to search

  @return element description
-->
<xsl:template name="get-name">
  <xsl:param name="name" />
  <xsl:param name="search" />

  <xsl:value-of select="
      $search//summary:descs/summary:desc[ @name=$name ]/@display
    " />
</xsl:template>


<xsl:template match="lv:rate" mode="gen-let-list" priority="5">
  <xsl:param name="deps" />
  <xsl:param name="context" />

  <xsl:call-template name="do-gen-let-list">
    <xsl:with-param name="symname" select="@yields" />
    <xsl:with-param name="context" select="$context" />
  </xsl:call-template>
</xsl:template>


<xsl:template match="lv:function" mode="gen-let-list" priority="5">
  <xsl:param name="deps" />

  <xsl:call-template name="do-gen-let-list">
    <xsl:with-param name="symname" select="@name" />
  </xsl:call-template>
</xsl:template>


<xsl:template match="*" mode="gen-let-list" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>[summary] !!! unknown let-list type </xsl:text>
    <xsl:value-of select="name()" />
  </xsl:message>
</xsl:template>


<!--
  Generate list of let statements describing each variable in the given node set

  Variables come from various sources depending on the operation being
  performed.
-->
<xsl:template name="do-gen-let-list">
  <xsl:param name="context" />
  <xsl:param name="symname" />

  <xsl:variable name="deps" select="
      /lv:*/preproc:sym-deps/preproc:sym-dep[
        @name=$symname
      ]
    " />

  <ul class="let">
    <!-- output a description for each dependency -->
    <xsl:variable name="result">
      <xsl:for-each select="
          /lv:*/preproc:symtable/preproc:sym[
            not( @type='lparam' )
            and @name=$deps/preproc:sym-ref/@name
          ]
        ">

        <xsl:call-template name="_gen-let-list-item">
          <xsl:with-param name="context" select="$context" />
        </xsl:call-template>
      </xsl:for-each>


      <!-- handle c:let formatting separately -->
      <xsl:for-each select="
          /lv:*/preproc:symtable/preproc:sym[
            @type='lparam'
            and @name=$deps/preproc:sym-ref/@name
          ]
        ">

        <xsl:call-template name="_gen-let-list-item">
          <xsl:with-param name="context" select="$context" />
          <xsl:with-param name="class" select="'letequ'" />
        </xsl:call-template>
      </xsl:for-each>
    </xsl:variable>

    <xsl:apply-templates select="$result" mode="typeset-final">
      <xsl:with-param name="deps" select="$deps" />
    </xsl:apply-templates>
  </ul>
</xsl:template>


<xsl:template name="_gen-let-list-item">
  <xsl:param name="context" />
  <xsl:param name="class" />

  <li>
    <xsl:if test="$class">
      <xsl:attribute name="class" select="$class" />
    </xsl:if>

    <xsl:choose>
      <xsl:when test="@type='lparam' and $context">
        <xsl:text>\(</xsl:text>
          <preproc:sym-ref name="{@name}" />
          <xsl:text> = </xsl:text>

          <xsl:variable name="varname" select="@varname" />

          <xsl:apply-templates select="
              $context//c:let/c:values/c:value[
                @name=$varname
              ]/c:*
            " />
        <xsl:text>\) </xsl:text>

        <span class="letdesc">
          <xsl:text>(</xsl:text>
            <xsl:value-of select="@desc" />
          <xsl:text>)</xsl:text>
        </span>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>let \(</xsl:text>
          <preproc:sym-ref name="{@name}" />
        <xsl:text>\) = </xsl:text>

        <xsl:apply-templates select="." mode="summary:desc" />
      </xsl:otherwise>
    </xsl:choose>

    <!--
    <xsl:variable name="param-name">
      <xsl:call-template name="get-name">
        <xsl:with-param name="name" select="$param" />
        <xsl:with-param name="search" select="/" />
      </xsl:call-template>
    </xsl:variable>

    <xsl:if test="$param-name != ''">
      <span class="param letlist-{$param-name}">
        <xsl:text> (</xsl:text>
        <a href="#{$param-name}">
          <xsl:value-of select="$param-name" />
        </a>
        <xsl:text>)</xsl:text>
      </span>
    </xsl:if>
    -->
  </li>
</xsl:template>

</xsl:stylesheet>
