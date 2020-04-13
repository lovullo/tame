<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Handles macro preprocessing

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:t="http://www.lovullo.com/rater/apply-template"
            xmlns:c="http://www.lovullo.com/calc"
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

      <message>[preproc] *REPASS*</message>

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


<template match="lv:classify[ .//lv:any|.//lv:all ]" mode="preproc:macros" priority="6">
  <variable name="result">
    <apply-templates select="." mode="preproc:class-groupgen" />
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


<template mode="preproc:class-groupgen" priority="9"
    match="lv:any[ not( element() ) ]
           |lv:all[ not( element() ) ]">
  <!-- useless; remove -->
</template>


<template match="lv:any|lv:all" mode="preproc:class-groupgen" priority="5">
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

    <apply-templates mode="preproc:class-groupgen" />
  </lv:classify>

  <!-- this will remain in its place -->
  <lv:match on="{$yields}" value="TRUE" preproc:generated="true" />
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
