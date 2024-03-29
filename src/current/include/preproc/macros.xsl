<?xml version="1.0" encoding="utf-8"?>
<!--
  Handles macro preprocessing

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:t="http://www.lovullo.com/rater/apply-template"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:compiler="http://www.lovullo.com/rater/compiler"
            xmlns:eseq="http://www.lovullo.com/tame/preproc/expand/eseq"
            xmlns:ext="http://www.lovullo.com/ext">


<include href="template.xsl" />
<include href="eligclass.xsl" />


<!--
  Perform a macro expansion pass

  This will continue to recurse until no preproc:repass nodes are found; this
  allos macros to expand into macros for further processing.
-->
<template match="*" mode="preproc:macropass" priority="1"
              as="node()+">
  <variable name="result" as="node()+">
    <apply-templates select="." mode="preproc:macros" />
  </variable>

  <variable name="nodeset" select="$result" />

  <variable name="repass"
                select="$nodeset//preproc:repass" />

  <!-- halt if we are in error -->
  <for-each select="$nodeset//preproc:error">
    <message terminate="yes">
      <text>!!! [preproc] error: </text>
      <value-of select="." />
    </message>
  </for-each>

  <choose>
    <!-- if it was indicated that we must do so, recurse -->
    <when test="$repass and not( $repass[ @need-sym ] )">

      <!-- record the repass to keep a count -->
      <!-- TODO: reintroduce
      <preproc:repass-record />
      -->

      <!-- output is slow and this happens a lot
      <message>[preproc] *REPASS*</message>
      -->

      <!-- perform the repass -->
      <apply-templates select="$nodeset" mode="preproc:macropass">
        <with-param name="clear-tpl-step"
                        tunnel="yes"
                        select="false()" />
      </apply-templates>
    </when>

    <!-- no more passes needed; strip any cruft and we're done -->
    <otherwise>
      <apply-templates mode="preproc:strip-tpl-cruft"
                           select="$nodeset" />
    </otherwise>
  </choose>
</template>


<template match="*" mode="preproc:macros" priority="1">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:macros" />
  </copy>
</template>


<!--
  Remove repass nodes left over from the previous pass

  Otherwise, we would recurse indefinately.
-->
<template match="preproc:repass" mode="preproc:macros" priority="5">
  <!-- remove; no longer needed -->
</template>


<template match="preproc:tpl-step" mode="preproc:macros" priority="5">
  <param name="clear-tpl-step"
             tunnel="yes"
             select="true()" />

  <choose>
    <when test="$clear-tpl-step">
      <!-- strip -->
    </when>

    <otherwise>
      <copy-of select="." />
    </otherwise>
  </choose>
</template>


<!--
  lv:rater is just a special type of package
-->
<template match="lv:rater" mode="preproc:macros" priority="9">
  <lv:package program="true">
    <sequence select="@*, *" />
  </lv:package>

  <preproc:repass src="lv:rater" />
</template>



<!--
  FOR PERFORMANCE ONLY:

  These nodes (usually) contain nothing that can be processed on the macro pass,
  so recursion is unnecessary; note the low priority.
-->
<template match="lv:typedef"
              mode="preproc:macros" priority="2">

  <sequence select="." />
</template>


<!--
  Expand values beginning with `#' into constants

  It is a nuisance to have separate params (e.g. templates) for constants
  and values.
-->
<template mode="preproc:macros"
              match="c:value-of[ starts-with( @name, '#' ) ]"
              priority="7">
  <variable name="desc" as="xs:string"
                select="if ( @label ) then
                            @label
                          else
                            'Generated short-hand constant'" />

  <c:const value="{substring-after( @name, '#' )}"
           type="float"
           desc="{$desc}" />
</template>


<!--
  Expand index values beginning with `#' into constants

  It is a nuisance to have separate params (e.g. templates) for constants
  and values.
-->
<template mode="preproc:macros"
              match="c:value-of[ starts-with( @index, '#' ) ]"
              priority="7">
  <copy>
    <copy-of select="@*[ not( name() = 'index' ) ]" />

    <c:index>
      <c:const value="{substring-after( @index, '#' )}"
               type="float"
               desc="Generated short-hand constant" />
    </c:index>
  </copy>
</template>


<!--
  It does not make sense to try to take an index of a scalar
-->
<template mode="preproc:macros"
              match="c:value-of[
                       @index
                       and starts-with( @name, '#' ) ]"
              priority="9">
  <preproc:error>
    <text>Cannot take index of scalar value: </text>
    <value-of select="@name" />
  </preproc:error>
</template>


<!--
  We can't accurately determine how to rewrite classifications if tempaltes
  have not yet been expanded.
-->
<template mode="preproc:macros" priority="9"
              match="lv:classify[ t:* ]">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:macros"
                         select="*" />
  </copy>

  <preproc:repass src="lv:classify wait on template expansion" />
</template>


<!--
  Classifications containing only an lv:any child node can be converted into
  existential classifications
-->
<template mode="preproc:macros" priority="8"
              match="lv:classify[
                       lv:any
                       and count( lv:* ) = 1
                       and not( preproc:tpl-barrier/lv:* ) ]">
  <copy>
    <sequence select="@*" />
    <attribute name="any" select="'true'" />

    <sequence select="lv:any/*" />
  </copy>

  <preproc:repass src="lv:classify any" />
</template>


<!--
  Strip template barriers after expansion is complete.

  These barriers really frustrate static analysis, and serve no use after
  templates have been expanded, aside from indicating _what_ templates were
  expanded.  The benefits there do not outweigh the optimization
  opportunities.
-->
<template mode="preproc:macros" priority="8"
          match="lv:classify[
                   .//preproc:tpl-barrier
                     and not( eseq:is-expandable(.) ) ]">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:strip-tpl-barrier" />
    <preproc:repass src="lv:classify tpl barrier strip" />
  </copy>
</template>


<!-- strip preproc:* nodes -->
<template mode="preproc:strip-tpl-barrier" priority="5"
          match="preproc:*">
  <apply-templates mode="preproc:strip-tpl-barrier" />
</template>

<template mode="preproc:strip-tpl-barrier" priority="1"
          match="element()">
  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:strip-tpl-barrier" />
  </copy>
</template>


<template mode="preproc:macros" priority="6"
          match="lv:classify[
                   ( lv:any | lv:all )
                     and not( eseq:is-expandable(.) ) ]">
  <variable name="result">
    <apply-templates select="." mode="preproc:class-groupgen">
      <with-param name="legacy-classify"
                  select="compiler:use-legacy-classify()"
                  tunnel="yes" />
    </apply-templates>
  </variable>

  <apply-templates select="$result/lv:classify" mode="preproc:class-extract" />

  <preproc:repass src="lv:classify any|all" />
</template>


<template match="lv:classify" mode="preproc:class-groupgen" priority="5">
  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:class-groupgen" />
  </copy>
</template>


<!--
  Not only should we not generate a group for single-predicate any/all, but
  we should remove it entirely.
-->
<template mode="preproc:class-groupgen" priority="7"
    match="(lv:any|lv:all)[ count( lv:* ) lt 2 ]">
  <apply-templates mode="preproc:class-groupgen" />
</template>


<!--
  A very specific optimization targeting a common scenario from template
  expansions.

  If there is an <any> expression of this form:

    <any>
      <all>
        <match on="foo" />
        <match on="bar" value="BAZ" />
      </all>
      <all>
        <match on="foo" />
        <match on="bar" value="QUUX" />
      </all>
    </any>

 Then the common "foo" will be hoisted out and the expression will become:

   <match on="foo" />
   <any>
     <match on="bar" value="BAZ" />
     <match on="bar" value="QUUX" />
   </any>

 The goal of this optimization is primarily to significantly reduce the
 number of wasteful intermediate classifications that are generated.
-->
<template mode="preproc:class-groupgen" priority="6"
    match="(lv:classify[ @any='true' ] | lv:any)
             [ count( lv:all[ count(lv:*) = 2 ] ) = count(lv:*) ]">

  <!-- TODO: missing @value may not have been expanded yet...! -->
  <variable name="ons" as="element( lv:match )*"
            select="lv:all/lv:match[
                      @value = 'TRUE'
                      or (
                        not( @value )
                        and not( @anyOf )
                        and not( c:* ) ) ]" />

  <variable name="distinct-ons" as="xs:string*"
            select="distinct-values( $ons/@on )" />

  <variable name="nall" as="xs:integer"
            select="count( lv:all )" />

  <choose>
    <when test="count( $distinct-ons ) = 1
                  and count( $ons ) = $nall
                  and count( $ons/parent::lv:all ) = $nall">
      <!-- then replace the <alls> with their remaining predicate (which is
           either before or after) -->
      <choose>
        <when test="@any = 'true'">
          <copy>
            <sequence select="@*[ not( local-name() = 'any' ) ]" />

            <!-- they're all the same, so hoist the first one out of the <any>,
                 which is now in a universal context because we omitted @any
                 above -->
            <sequence select="$ons[1]" />

            <lv:any>
              <apply-templates mode="preproc:class-groupgen"
                               select="$ons/preceding-sibling::lv:match
                                       | $ons/following-sibling::lv:match"/>
            </lv:any>
          </copy>
        </when>

        <otherwise>
          <!-- they're all the same, so hoist the first one out of the <any>,
               which must be in a universal context -->
          <sequence select="$ons[1]" />

          <copy>
            <apply-templates mode="preproc:class-groupgen"
                             select="$ons/preceding-sibling::lv:match
                                     | $ons/following-sibling::lv:match"/>
          </copy>
        </otherwise>
      </choose>
    </when>

    <!-- none, or more than one -->
    <otherwise>
      <next-match />
    </otherwise>
  </choose>
</template>


<template match="lv:any|lv:all" mode="preproc:class-groupgen" priority="5">
  <param name="legacy-classify" as="xs:boolean" tunnel="yes" />

  <!-- this needs to be unique enough that there is unlikely to be a conflict
       between generated ids in various packages; generate-id is not enough for
       cross-package guarantees (indeed, I did witness conflicts), so there is
       a random seed passed into the stylesheet externally -->
  <variable name="id" select="concat( $__rseed, generate-id(.) )" />

  <variable name="parent-name" select="ancestor::lv:classify/@as" />
  <variable name="yields" select="concat( 'is', $id )" />

  <!-- this will be raised outside of the parent classification during
       post-processing -->
  <lv:classify as="{$id}" yields="{$yields}"
               preproc:generated="true"
               preproc:generated-from="{$parent-name}"
               desc="(generated from predicate group of {$parent-name}">
    <if test="local-name() = 'any'">
      <attribute name="any" select="'true'" />
    </if>

    <if test="not( $legacy-classify )">
      <attribute name="preproc:inline" select="'true'" />
    </if>

    <apply-templates mode="preproc:class-groupgen" />
  </lv:classify>

  <!-- this will remain in its place -->
  <lv:match on="{$yields}" value="TRUE"
            preproc:generated="true">
    <if test="not( $legacy-classify )">
      <attribute name="preproc:inline" select="'true'" />
    </if>
  </lv:match>
</template>


<!-- traverse through preproc nodes (e.g. preproc:tpl-barrier) -->
<template match="preproc:*" mode="preproc:class-groupgen" priority="2">
  <copy>
    <sequence select="@*" />
    <apply-templates select="node()" mode="preproc:class-groupgen" />
  </copy>
</template>


<!-- retain everything else -->
<template match="*" mode="preproc:class-groupgen" priority="1">
  <sequence select="." />
</template>


<template match="lv:classify" mode="preproc:class-extract" priority="5">
  <apply-templates select="lv:classify" mode="preproc:class-extract" />

  <copy>
    <sequence select="@*" />
    <apply-templates mode="preproc:class-filter" />
  </copy>
</template>


<template match="*" mode="preproc:class-extract" priority="1">
  <!-- ignore non-class -->
</template>


<template match="lv:classify" mode="preproc:class-filter" priority="5">
  <!-- remove -->
</template>


<template match="*" mode="preproc:class-filter" priority="1">
  <sequence select="." />
</template>


<!--
  Sections exist purely for organization and documentation.  Move all
  nodes out of it, so that we do not complicate parsing.
-->
<template mode="preproc:macros" priority="2"
              match="lv:section">
  <apply-templates select="*" mode="preproc:macros" />
</template>


<!--
  wrapper around `<t:yield />`
-->
<template match="lv:yield" mode="preproc:macros" priority="5">
  <t:yield>
    <copy-of select="@*" />

    <apply-templates mode="preproc:expand" />
  </t:yield>
</template>

<!--
  wrapper around `<t:rate-each />`
-->
<template match="lv:rate-each" mode="preproc:macros" priority="5">
  <t:rate-each>
    <copy-of select="@*" />
    <apply-templates mode="preproc:expand"
                     select="*[ not( local-name() = 'class' ) ]" />
  </t:rate-each>
</template>

</stylesheet>
