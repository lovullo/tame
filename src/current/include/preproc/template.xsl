<?xml version="1.0" encoding="utf-8"?>
<!--
  Performs template processing and expansion

  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.

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
            xmlns:ext="http://www.lovullo.com/ext"
            xmlns:util="http://www.lovullo.com/util"
            xmlns:eseq="http://www.lovullo.com/tame/preproc/expand/eseq"
            exclude-result-prefixes="xs ext eseq lv util">

<import href="../../tame/src/preproc/expand/expand-sequence.xsl" />


<!--
  Macro that abstracts away the boilerplate template application code when
  dealing with a single root template

  This macro will cause another macro expansion pass.
-->
<template match="lv:rate-each-template" mode="preproc:macros" priority="5">
  <!-- TODO: debug flag
  <message>
    <text>[preproc] expanding rate-each-template </text>
    <value-of select="@yields" />
    <text>...</text>
  </message>
  -->

  <lv:rate-each>
    <!-- copy all attributes except for those we will explicitly handle below -->
    <sequence select="@*[
        not( local-name() = 'name' )
      ]" />

    <!-- since the entire body is composed from a template, we do not need to
         accept an index from the user; traditionally, we use 'k' to leave
         indexes like 'i' and 'j' open -->
    <variable name="index">
      <text>k</text>
    </variable>

    <attribute name="index">
      <value-of select="$index" />
    </attribute>

    <!-- any lv:* nodes should be placed at the top -->
    <sequence select="./lv:class" />

    <!-- the template is expected to have an @index@ parameter -->
    <lv:apply-template name="{@name}">
      <lv:with-param name="@index@" value="{$index}" />

      <!-- copy any user-provided params into the template application -->
      <sequence select="./lv:with-param" />
    </lv:apply-template>
  </lv:rate-each>

  <!-- since we generated another preprocessor macro (lv:rate-each), notify the
       system that we will need to make another pass -->
  <preproc:repass tpl="{@name}" />
</template>


<!--
  If the symbol table is not yet available, defer short-hand
  applications for now

  It is far less costly to leave the node alone than it is to process
  and copy potentially massive trees only to have to repass anyway.
-->
<template mode="preproc:macros" priority="9"
              match="t:*[ not( root(.)/preproc:symtable ) ]">
  <sequence select="." />
  <preproc:repass need-sym="true" />
</template>


<!--
  Converts shorthand template applications into full template expansions

  TODO: This causes an extra pass; we'd like to avoid having to do that.
-->
<template match="t:*" mode="preproc:macros" priority="5">
  <!-- TODO: debug flag
  <message>
    <text>[preproc] expanding template shorthand for </text>
    <value-of select="local-name()" />
    <text>...</text>
  </message>
  -->

  <variable name="name" as="xs:string"
                select="concat( '_', local-name(), '_' )" />

  <variable name="params" as="element( lv:with-param )*">
    <for-each select="@*">
      <lv:with-param name="@{local-name()}@" value="{.}" />
    </for-each>

    <!-- there may be existing lv:with-param nodes -->
    <sequence select="./lv:with-param" />

    <variable name="paramnodes"
      select="*[ not( local-name() = 'with-param' ) ]" />

    <!-- if sub-nodes were provided, pass it as the "@values@" param -->
    <if test="$paramnodes">
      <lv:with-param name="@values@">
        <sequence select="$paramnodes" />
      </lv:with-param>
    </if>
  </variable>

  <!-- XXX: a large chunk of this is duplicate code; factor out -->
  <variable name="tpl" as="element( lv:template )?"
                select="preproc:locate-template( $name, root( . ) )" />

  <variable name="src-root" as="element( lv:package )"
                select="if ( root(.)/lv:package ) then
                          root(.)/lv:package
                        else
                          root(.)" />

  <choose>
    <when test="$tpl">
      <!-- avoid a costly repass; apply immediately -->
      <sequence select="preproc:expand-template(
                            $tpl, $src-root, $params, . )" />
    </when>

    <otherwise>
      <preproc:error>Undefined template <value-of select="$name" /></preproc:error>
    </otherwise>
  </choose>
</template>


<function name="preproc:locate-template"
              as="element( lv:template )?">
  <param name="name" as="xs:string" />
  <param name="root" as="element( lv:package )" />

  <variable name="sym" as="element( preproc:sym )?"
                select="$root/preproc:symtable/preproc:sym[
                          @name = $name ]" />

  <variable name="package" as="element( lv:package )?"
                select="if ( $sym/@src ) then
                          document( concat( $sym/@src, '.xmlo' ), $__entry-root )
                            /lv:package
                        else
                          $root/lv:package" />

  <!-- legacy lookup for inline-templates; needs to regenerate symbol
       list, then this can be removed -->
  <variable name="inline-match" as="element( lv:template )?"
                select="$root//lv:template[ @name = $name ]" />

  <sequence select="if ( $inline-match ) then
                          $inline-match
                        else
                          $package//lv:template[ @name=$name ]" />
</function>



<!--
  Applies a template in place as if it were pasted directly into the XML in that
  location

  This works much like a C macro and is intended to be a very simple means of
  code re-use that permits free construction of lv:rate blocks. See XSD
  documentation.

  It is *very important* that these macros be expanded before the templates
  themselves are removed from the XML by further processing.

  Note that if the attribute shorthand is used for params, one extra expansion
  will have to occur, which is an additional performance hit.
-->
<template match="lv:apply-template[
                       @name=root(.)/preproc:symtable/preproc:sym/@name ]
                     |lv:apply-template[ @name=root(.)//lv:template/@name ]"
              mode="preproc:macros" priority="6">

  <variable name="name" select="@name" />
  <variable name="attrparams" select="@*[ not( local-name() = 'name' ) ]" />

  <!-- used for type checking -->
  <variable name="root" as="element( lv:package )"
                select="if ( root( . ) instance of document-node() ) then
                          root( . )/lv:package
                        else
                          root( . )" />

  <variable name="src-root" as="element( lv:package )"
                select="if ( $root/lv:package ) then
                          $root/lv:package
                        else
                          $root" />

  <variable name="tpl" as="element( lv:template )?"
                select="preproc:locate-template( $name, $root )" />

  <choose>
    <when test="exists( $tpl ) and $attrparams">
      <message>
        <text>[preproc] expanding template parameters for </text>
        <value-of select="@name" />
        <text>...</text>
      </message>

      <variable name="params" as="element( lv:with-param )*">
        <for-each select="$attrparams">
          <lv:with-param name="@{local-name()}@" value="{.}" />
        </for-each>

        <!-- there may also be non-shorthand params -->
        <sequence select="lv:with-param" />
      </variable>

      <!-- immediately apply without a wasteful repass -->
      <sequence select="preproc:expand-template(
                              $tpl, $src-root, $params, . )" />
    </when>

    <when test="$tpl">
      <sequence select="preproc:expand-template(
                              $tpl, $src-root, lv:with-param, . )" />
    </when>

    <otherwise>
      <preproc:error>Undefined template <value-of select="$name" /></preproc:error>
    </otherwise>
  </choose>
</template>


<function name="preproc:expand-template">
  <param name="tpl"     as="element( lv:template )" />
  <param name="src-root" />
  <param name="params"  as="element( lv:with-param )*" />
  <param name="context" as="node()" />

  <variable name="name" as="xs:string"
                select="$tpl/@name" />

  <!-- TODO: debug flag
  <message>
    <text>[preproc] applying template </text>
    <value-of select="@name" />
    <text>...</text>
  </message>
  -->

  <!-- check to ensure that each given argument is defined, unless @strict
       is false  -->
  <for-each select="
      $params[
        not( @name=$tpl/lv:param/@name )
        and not( @strict='false' )
      ]
    ">
    <preproc:error>
      <text>undefined template param </text>
      <value-of select="$name" />/<value-of select="@name" />
      <text>; available params are </text>

      <for-each select="$tpl/lv:param/@name">
        <if test="position() > 1">
          <text>; </text>
        </if>

        <value-of select="." />
      </for-each>
    </preproc:error>
  </for-each>

  <!-- replace this node with a copy of all the child nodes of the given
       template; this inlines it as if it were copied and pasted directly
       into the XML, much like a C macro -->
  <variable name="apply-result">
    <preproc:tpl-barrier>
      <apply-templates
          select="$tpl[ 1 ]/*"
          mode="preproc:apply-template">

        <with-param name="apply" select="$context"
                        tunnel="yes" />
        <with-param name="tpl" select="$tpl[ 1 ]"
                        tunnel="yes" />
        <with-param name="apply-tpl-name" select="$name"
                        tunnel="yes" />
        <with-param name="params" select="$params"
                        tunnel="yes" />

        <with-param name="first-child" select="true()" />
        <with-param name="src-root" select="$src-root"
                        tunnel="yes" />
      </apply-templates>
    </preproc:tpl-barrier>
  </variable>

  <apply-templates mode="preproc:mark-tpl-expansion"
                       select="$apply-result">
    <with-param name="tpl-name" select="$name"
                    tunnel="yes" />
  </apply-templates>

  <!-- since templates can include anything, we should perform another pass
       to be sure that all macros that this template may contain/generate
       are expanded -->
  <preproc:repass tpl="{$name}" />
  <preproc:tpl-step name="{$name}" type="apply-template" />
</function>


<!--
  Strip tpl barriers, which serve to scope metadata.
-->
<template mode="preproc:strip-tpl-cruft" priority="5"
              match="preproc:tpl-barrier">
  <apply-templates mode="preproc:strip-tpl-cruft"
                       select="node()" />
</template>

<!--
  Everything else stays.
-->
<template mode="preproc:strip-tpl-cruft" priority="3"
              match="node()">
  <copy>
    <sequence select="@*" />

    <apply-templates mode="preproc:strip-tpl-cruft"
                         select="node()" />
  </copy>
</template>



<!--
  Add nodes describing where the parent node came from.

  This helps in debugging and understanding code created by templates.
  This isn't the best way, but e.g. enclosing all expanded nodes in a parent
  tag causes problems with the rest of the system.  Adding an attribute to
  the node is an option, but this implementation allows a node to be marked
  by multiple templates, should such a thing ever occur.
-->
<template mode="preproc:mark-tpl-expansion" priority="5"
              match="element()">
  <param name="tpl-name" tunnel="yes" />

  <copy>
    <sequence select="@*" />

    <preproc:from-template name="{$tpl-name}" />

    <sequence select="node()" />
  </copy>
</template>

<template mode="preproc:mark-tpl-expansion" priority="3"
              match="node()|attribute()">
  <sequence select="." />
</template>


<!--
  Inline templates depending on the symbol table must not be expanded
  until the symbol table is actually available
-->
<template mode="preproc:macros" priority="6"
              match="lv:inline-template[ lv:for-each/lv:sym-set
                                         and not( root(.)/preproc:symtable ) ]">
  <lv:expand-sequence>
    <sequence select="." />
  </lv:expand-sequence>

  <message select="'[preproc] deferring inline-template waiting for symbol table'" />
</template>


<!--
  An inline template implicitly defines and then immediately applies a template
  with an optional looping construct
-->
<template match="lv:inline-template" mode="preproc:macros" priority="5">
  <variable name="name" select="concat( '___i', generate-id(.), '___' )" />
  <variable name="inline" select="." />

  <!-- generate template -->
  <lv:template name="{$name}" desc="Inline template"
               preproc:from-inline="{$name}"
               preproc:generated="true">
    <!-- generate params (from both our own attrs and any for-each sets) -->
    <variable name="params">
      <preproc:params>
        <for-each select="@*, lv:for-each/lv:set/@*">
          <!-- manual lv:param's will override default param generation -->
          <if test="not( local-name() = $inline/lv:param/@name )">
            <lv:param name="@{local-name()}@" desc="Generated param" />
          </if>
        </for-each>

        <if test="lv:for-each/lv:sym-set">
          <lv:param name="@sym_name@"     desc="Symbol name" />
          <lv:param name="@sym_type@"     desc="Symbol type" />
          <lv:param name="@sym_dim@"      desc="Symbol degree (dimensions)" />
          <lv:param name="@sym_desc@"     desc="Symbol description (if any)" />
          <lv:param name="@sym_yields@"   desc="Symbol yield name (if any)" />
          <lv:param name="@sym_parent@"   desc="Symbol parent (if any)" />
          <lv:param name="@sym_external@" desc="Symbol external to classifier" />
        </if>
      </preproc:params>
    </variable>

    <!-- use only unique params from each attribute source -->
    <sequence select="
        $params//lv:param[
          not( @name=preceding-sibling::lv:param/@name )
        ]
      " />

    <!-- include any params priovided by the user (they may be used to
         generate content) -->
    <sequence select="lv:param" />

    <!-- copy template body -->
    <!-- FIXME: do not use local-name here; use instance of -->
    <sequence select="*[ not( local-name() = 'for-each' ) ]" />
  </lv:template>

  <choose>
    <when test="lv:for-each">
      <apply-templates select="lv:for-each/lv:*" mode="preproc:inline-apply">
        <with-param name="name" select="$name" />
      </apply-templates>
    </when>

    <otherwise>
      <apply-templates select="." mode="preproc:inline-apply">
        <with-param name="name" select="$name" />
      </apply-templates>
    </otherwise>
  </choose>

  <!-- the next pass will perform the template application -->
  <preproc:repass tpl="{$name}" />
</template>


<template match="lv:inline-template|lv:for-each/lv:set" mode="preproc:inline-apply">
  <param name="name" />

  <!-- immediately apply the template -->
  <lv:apply-template name="{$name}">
    <!-- each attribute will be considered to be a template param (and any
         parent lv:inline-template attributes will be added to each and every
         set) -->
    <for-each select="@*|ancestor::lv:inline-template/@*">
      <lv:with-param name="@{local-name()}@" value="{.}" />
    </for-each>
  </lv:apply-template>
</template>


<template mode="preproc:inline-apply"
              match="lv:for-each/lv:sym-set">
  <param name="name" />

  <param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <!-- TODO: moar predicates! -->
  <variable name="syms" as="element( preproc:sym )*"
                select="$src-root/preproc:symtable/preproc:sym[
                          ( not( current()/@type )
                            or @type = current()/@type )
                          and ( not( current()/@yields )
                                or @yields = current()/@yields )
                          and ( not( current()/@parent )
                                or @parent = current()/@parent )
                          and ( not( current()/@imports = 'true' )
                                or not( @src ) )
                          and ( not( current()/@name-prefix )
                                or substring( @name,
                                              1,
                                              string-length(current()/@name-prefix ) )
                                     = current()/@name-prefix
                                or substring( @orig-name,
                                              1,
                                              string-length(current()/@name-prefix ) )
                                     = current()/@name-prefix ) ]" />

  <for-each select="$syms">
    <!-- some symbols are modified to include a prefix; these internal
         details should not be exposed to the user, since they are
         useless within the DSL itself -->
    <variable name="name-strip"
                  select="substring-after(
                          substring-after( @name, ':' ),
                          ':' )" />

    <variable name="sym-name"
                  select="if ( $name-strip ) then
                          $name-strip
                          else
                          @name" />

    <lv:apply-template name="{$name}">
      <lv:with-param name="@sym_name@"     value="{$sym-name}" />
      <lv:with-param name="@sym_type@"     value="{@type}" />
      <lv:with-param name="@sym_dim@"      value="{@dim}" />
      <lv:with-param name="@sym_desc@"     value="{@desc}" />
      <lv:with-param name="@sym_yields@"   value="{@yields}" />
      <lv:with-param name="@sym_parent@"   value="{@parent}" />
    </lv:apply-template>
  </for-each>
</template>


<!--
  This block is used when we attempt to apply a template that has not been
  defined
-->
<template match="lv:apply-template" mode="preproc:macros" priority="5">
  <!-- keep this application around for later -->
  <sequence select="." />

  <!-- nothing we can do yet -->
  <message>
    <text>[preproc] deferring application of unknown template </text>
    <value-of select="@name" />
  </message>

  <preproc:repass need-sym="{@name}" />
</template>


<template match="lv:template/lv:param" mode="preproc:apply-template" priority="5">
  <param name="first-child" select="false()" />

  <!-- do not copy the template params for the template being applied; that
       could result in some potentially inteesting bugs -->
  <if test="not( $first-child )">
    <sequence select="." />
  </if>
</template>


<!--
  Halt expansion at this point and remove the barrier for future expansions
-->
<template match="lv:expand-barrier" mode="preproc:apply-template" priority="5">
  <sequence select="node()" />
</template>


<!--
  Leave immediate children alone but continue expanding grandchildren
-->
<template match="lv:skip-child-expansion" mode="preproc:apply-template" priority="5">
  <for-each select="element()">
    <copy>
      <copy-of select="@*" />
      <apply-templates mode="preproc:apply-template" />
    </copy>
  </for-each>
</template>


<!--
  Preprocesses template nodes

  Currently, only attributes will be processed. Otherwise, the template ends up
  recursing on all children to process all attributes.
-->
<template match="*" mode="preproc:apply-template" priority="1">
  <copy>
    <apply-templates select="@*|node()"
                         mode="preproc:apply-template" />
  </copy>
</template>


<!--
  Generate template metadata from lv:param-meta nodes.

  Metadata contained within an lv:param-copy has its scope limited to
  children only.  Standalone lv:param-meta nodes are "hoisted" and are
  available to siblings.
-->
<template match="lv:param-meta" mode="preproc:apply-template" priority="5">
  <param name="apply" as="node()"
             tunnel="yes" />

  <variable name="name">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@name" />
    </call-template>
  </variable>

  <variable name="value">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@value" />
    </call-template>
  </variable>

  <preproc:tpl-meta name="{$name}" value="{$value}">
    <if test="not( parent::lv:param-copy )">
      <attribute name="hoist" select="'true'" />
    </if>
  </preproc:tpl-meta>
</template>


<!--
  Copies child nodes associated with the param during application into the
  replacement

  This effectively allows the definition of custom node types.
-->
<template match="lv:param-copy" mode="preproc:apply-template" priority="5">
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <!-- if any metadata has been defined, retain it so that it may be queried by
       future templates -->
  <apply-templates mode="preproc:apply-template"
                       select="lv:param-meta" />

  <variable name="varname" select="@name" />

  <!-- determine the nodes to copy -->
  <variable name="copy"
    select="$params[ @name=$varname ]/*" />

  <choose>
    <!-- if the @expand flag is set, then immediately begin expanding any
         template params that it may have (that we know of), in effect making
         the body of the template application *part* of the template itself -->
    <when test="@expand='true'">
      <message>[preproc] expanding param-copy...</message>
      <apply-templates select="$copy" mode="preproc:apply-template">
      </apply-templates>
    </when>

    <!-- by default, expansion is not requested, meaning that no template param
         expansion will take place (on this pass) for the copied nodes; if any
         template params exist, they will be handled on the next pass -->
    <otherwise>
      <sequence select="$copy" />
    </otherwise>
  </choose>
</template>


<template match="lv:dyn-node" mode="preproc:apply-template" priority="5">
  <variable name="dyn-name">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@name" />
    </call-template>
  </variable>

  <element name="{$dyn-name}">
    <apply-templates mode="preproc:apply-template" />
  </element>
</template>


<!--
  Display an arbitrary error to the user during compilation

  This produces a compile-time error and immediately halts
  compilation.
-->
<template mode="preproc:apply-template" priority="5"
              match="lv:error">
  <param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <variable name="message">
    <apply-templates mode="preproc:gen-param-value" />
  </variable>

  <preproc:error>
    <value-of select="$apply-tpl-name" />
    <text>: </text>
    <value-of select="normalize-space( $message )" />
  </preproc:error>
</template>


<!--
  Display an arbitrary warning to the user during compilation

  This produces a compile-time warning that is purely informational
  and does not halt compilation.  It is intended to point out
  undesirable conditions.
-->
<template mode="preproc:apply-template" priority="5"
              match="lv:warning">
  <param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <variable name="message">
    <apply-templates mode="preproc:gen-param-value" />
  </variable>

  <message>
    <text>warning: </text>
    <value-of select="$apply-tpl-name" />
    <text>: </text>
    <value-of select="normalize-space( $message )" />
  </message>
</template>


<template mode="preproc:apply-template" priority="5"
              match="text()">
  <value-of select="." />
</template>


<!--
  Basic conditional support within templates

  Simple concept, powerful consequences.

  ***No recursion limit check...TODO
-->
<template match="lv:if|lv:unless" mode="preproc:apply-template" priority="5">
  <param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <choose>
    <!-- if the param is unknown, retain; it may be intended for a generated
         template -->
    <when test="
        not(
          @name=ancestor::lv:template[
            @name=$apply-tpl-name
          ]/lv:param/@name
        )
      ">
      <sequence select="." />
    </when>

    <otherwise>
      <!-- calculate the param value -->
      <variable name="param-value">
        <call-template name="preproc:template-param-value">
          <with-param name="name" select="@name" />
        </call-template>
      </variable>

      <!-- @value value -->
      <variable name="cmp-value">
        <call-template name="preproc:template-param-value">
          <with-param name="name" select="@*[ not( local-name() = 'name' ) ]" />
        </call-template>
      </variable>

      <apply-templates select="." mode="preproc:apply-template-cmp">
        <with-param name="param-value" select="$param-value" />
        <with-param name="cmp-value" select="$cmp-value" />

        <with-param name="negate">
          <choose>
            <when test="local-name() = 'if'">
              <value-of select="false()" />
            </when>

            <otherwise>
              <value-of select="true()" />
            </otherwise>
          </choose>
        </with-param>
      </apply-templates>
    </otherwise>
  </choose>
</template>


<template match="lv:*[@eq]" mode="preproc:apply-template-cmp" priority="5">
  <param name="negate" select="false()" />
  <param name="param-value" />
  <param name="cmp-value" />

  <if test="
      ( ( $negate = 'true' ) and not( $param-value = $cmp-value ) )
      or ( ( $negate = 'false' ) and ( $param-value = $cmp-value ) )
    ">

    <apply-templates select="*" mode="preproc:apply-template" />
  </if>
</template>


<template mode="preproc:apply-template-cmp" priority="5"
              match="lv:*[ @prefix ]">
  <param name="negate" select="false()" />
  <param name="param-value" />
  <param name="cmp-value" />

  <variable name="has-prefix" as="xs:boolean"
                select="starts-with( $param-value, $cmp-value )" />

  <if test="( $negate and not( $has-prefix ) )
                or ( not( $negate ) and $has-prefix )">
    <apply-templates select="*" mode="preproc:apply-template" />
  </if>
</template>


<template mode="preproc:apply-template-cmp" priority="5"
              match="lv:*[ @suffix ]">
  <param name="negate" select="false()" />
  <param name="param-value" />
  <param name="cmp-value" />

  <variable name="has-suffix" as="xs:boolean"
                select="ends-with( $param-value, $cmp-value )" />

  <if test="( $negate and not( $has-suffix ) )
                or ( not( $negate ) and $has-suffix )">
    <apply-templates select="*" mode="preproc:apply-template" />
  </if>
</template>


<template match="lv:*[not(@*[not(local-name()='name')])]" mode="preproc:apply-template-cmp" priority="2">
  <param name="negate" select="'false'" />
  <param name="param-value" />
  <param name="cmp-value" />

  <!-- TODO: This is convoluted...we "know" it's empty because it'll return its
       own name as a value by default... -->
  <if test="
      ( ( $negate = 'true' ) and ( $param-value = @name ) )
      or ( ( $negate = 'false' ) and not( $param-value = @name ) )
    ">

    <apply-templates select="*" mode="preproc:apply-template" />
  </if>
</template>

<template match="lv:*" mode="preproc:apply-template-cmp" priority="1">
  <preproc:error>
    <text>Unknown template comparison attribute for </text>
    <value-of select="name()" />
    <text> in </text>
    <value-of select="ancestor::lv:template/@name" />
    <text>:</text>

    <for-each select="@*">
      <text> </text>
      <value-of select="local-name()" />
      <text>="</text>
      <value-of select="." />
      <text>"</text>
    </for-each>
  </preproc:error>
</template>


<template mode="preproc:gen-param-value" priority="5"
              match="lv:param-add">
  <!-- calculate the param value -->
  <variable name="name-value">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@name" />
    </call-template>
  </variable>

  <variable name="value-value">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@value" />
    </call-template>
  </variable>

  <if test="not( $name-value castable as xs:float )">
    <message terminate="yes"
                 select="concat( 'error: non-numeric lv:param-add/@name: ',
                                 $name-value )" />
  </if>

  <if test="not( $value-value castable as xs:float )">
    <message terminate="yes"
                 select="concat( 'error: non-numeric lv:param-add/@value: ',
                                 $name-value )" />
  </if>

  <value-of select="number( $name-value ) + number( $value-value )" />
</template>


<!--
  Process template attributes

  Provides primitive parameter support. If a parameter is found (convention is
  that it must begin and end with '@', but that convention is not enforced
  here), it is replaced by the value given by the calling lv:apply-template.

  Note that we currently do not perform any string replacements. As such, we
  only support full replacement of an attribute (that is, the attribute must
  contain only a parameter, not a parameter as part of a larger string); it is
  for this reason that the convention of beginning and ending with '@' arose: it
  allows us to perform substring replacements later on with clear delimiters.

  TODO: substring replacement for added flexibility
-->
<template match="@*" mode="preproc:apply-template" priority="5">
  <param name="tpl" tunnel="yes" />

  <variable name="name" select="local-name()" />
  <variable name="varname" select="string(.)" />

  <!-- compile param value -->
  <variable name="value">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="$varname" />
    </call-template>
  </variable>

  <choose>
    <!-- if the template being applied does not itself define this
         parameter, and we're performing a full var replacement, keep the
         name verbatim for later expansion -->
    <when test="starts-with( $varname, '@' )
                      and not( $tpl/lv:param[ @name = $varname ] )">
      <attribute name="{$name}" select="$varname" />
    </when>

    <!-- if the result is an empty string, then do not output the attribute (this
         allows for conditional attributes -->
    <when test="not( $value = '' )">
      <attribute name="{$name}" select="$value" />
    </when>
  </choose>
</template>


<template match="lv:with-param/@name" mode="preproc:apply-template" priority="9">
  <!-- do not do any sort of expansion of recursive template param names -->
  <copy />
</template>


<template name="preproc:template-param-value">
  <param name="name" />
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <variable name="after" select="substring-after( $name, '@' )" />

  <!-- TODO: support multiple @vars@ per attribute -->
  <choose>
    <when test="$name = ''">
      <!-- we're done; nothing more to do -->
    </when>

    <!-- no vars; just output and abort -->
    <when test="not( $after )">
      <value-of select="$name" />
    </when>

    <!-- if the variable is inline, output the value up until the inline
         delimiter (curly brace); this supports inline vars at any point in the
         string -->
    <when test="not( starts-with( $name, '@' ) )">
      <!-- this might be nothing (if we start with a curly brace) -->
      <value-of select="substring-before( $name, '{' )" />

      <!-- get the param between {} -->
      <variable name="param"
        select="substring-before( substring-after( $name, '{' ), '}' )" />

      <!-- if a param was found, process it -->
      <if test="$param">
        <variable name="result">
          <call-template name="preproc:template-param-value">
            <with-param name="name" select="$param" />
          </call-template>
        </variable>

        <!-- if the result is the same as the param name, then it was not found;
             re-enclose in braces so that it can be processed later (where we
             may find a value) -->
        <choose>
          <when test="$result = $param">
            <text>{</text>
              <value-of select="$result" />
            <text>}</text>
          </when>

          <otherwise>
            <value-of select="$result" />
          </otherwise>
        </choose>
      </if>

      <!-- recurse to generate the remainder of the string -->
      <call-template name="preproc:template-param-value">
        <with-param name="name" select="substring-after( $name, '}' )" />
      </call-template>
    </when>

    <!-- starts with a macro param -->
    <otherwise>
      <!-- get the param, since we may have additional text following it -->

      <!-- get the value of this parameter, if it exists, from the caller (note
           below that we will consider a value of the same name as the param to
           be undefined, allowing defaults to be generated; this is useful when
           proxying params between templates)-->
      <variable name="val"
        select="$params[ @name=$name ]/@value" />

      <!-- the param node itself -->
      <variable name="param"
        select="ancestor::lv:template/lv:param[ @name=$name ]" />


      <choose>
        <!-- this test is structured to fail if the param has not been defined
             on the templates parameter list; this is a restriction imposed
             purely for documentation; the preprocessor can do just fine without
             -->
        <when test="$val and not( $val = $name ) and $param">
          <value-of select="$val" />
        </when>

        <!-- otherwise, if we just have the param and it has child nodes,
             generate a value (in essence: generate a default value if one is
             not provided) -->
        <when test="$param and $param/lv:*">
          <apply-templates select="$param/lv:*"
                               mode="preproc:gen-param-value" />
        </when>

        <otherwise>
          <value-of select="$name" />
        </otherwise>
      </choose>
    </otherwise>
  </choose>
</template>


<template mode="preproc:gen-param-value" priority="6"
              match="lv:param/lv:text[ @unique='true' ]">
  <param name="apply" as="node()"
             tunnel="yes" />

  <value-of select="." />
  <value-of select="generate-id( $apply )" />
</template>


<template match="lv:param/lv:text" mode="preproc:gen-param-value"
              priority="4">
  <value-of select="." />
</template>


<template mode="preproc:gen-param-value" priority="5"
              match="lv:param/lv:param-inherit[@meta]">
  <param name="apply" as="node()"
             tunnel="yes" />

  <variable name="name">
    <call-template name="preproc:template-param-value">
      <with-param name="name" select="@meta" />
    </call-template>
  </variable>

  <!-- find the metadata -->
  <variable name="values"
       select="$apply/ancestor::*/preceding-sibling::preproc:tpl-meta[ @name=$name ]/@value
               , $apply/ancestor::*/preproc:tpl-barrier
                   /preproc:tpl-meta[ @hoist = 'true' and @name=$name ]/@value
               , $apply/preceding-sibling::preproc:tpl-meta[ @name=$name ]/@value" />

  <!-- take the last one (precedence) -->
  <value-of select="$values[ count( $values ) ]" />
</template>


<template mode="preproc:gen-param-value" priority="4"
            match="lv:param-value">
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <!-- name of the parameter we're referencing -->
  <variable name="pname" as="xs:string"
                select="@name" />

  <!-- the original string -->
  <variable name="str" as="xs:string?"
    select="$params[ @name=$pname ]/@value" />

  <!-- apply @snake if need be -->
  <variable name="processed">
    <choose>
      <!-- snakeify -->
      <when test="@snake">
        <value-of select="translate( $str, '-', '_' )" />
      </when>

      <!-- convert spaces and underscores to dashes -->
      <when test="@dash">
        <value-of select="translate( $str, '_ ', '--' )" />
      </when>

      <when test="@rmdash">
        <value-of select="translate( $str, '-', '' )" />
      </when>

      <when test="@rmunderscore">
        <value-of select="translate( $str, '_', '' )" />
      </when>

      <!-- do nothing -->
      <otherwise>
        <value-of select="$str" />
      </otherwise>
    </choose>
  </variable>

  <!-- perform any additional processing -->
  <apply-templates select="." mode="preproc:gen-param-value-style">
    <with-param name="str" select="$processed" />
  </apply-templates>
</template>

<template match="lv:param-value[@ucfirst]" mode="preproc:gen-param-value-style" priority="5">
  <param name="str" />

  <call-template name="util:ucfirst">
    <with-param name="str" select="$str" />
  </call-template>
</template>

<!-- slightly higher priority than @ucfirst, since this obviously should
     override -->
<template match="lv:param-value[@upper]" mode="preproc:gen-param-value-style" priority="6">
  <param name="str" />

  <call-template name="util:uppercase">
    <with-param name="str" select="$str" />
  </call-template>
</template>

<template match="lv:param-value[@lower]" mode="preproc:gen-param-value-style" priority="6">
  <param name="str" />

  <call-template name="util:lowercase">
    <with-param name="str" select="$str" />
  </call-template>
</template>

<!-- convert into valid identifier name -->
<template match="lv:param-value[@identifier]"
              mode="preproc:gen-param-value-style" priority="6">
  <param name="str" />

  <variable name="norm" as="xs:string"
                select="normalize-unicode( $str, 'NFC' )" />

  <variable name="pre" as="xs:string">
    <choose >
      <when test="@identifier = 'class'">
        <sequence select="replace( lower-case( $norm ), '[_ ]', '-' )" />
      </when>

      <when test="@identifier = 'param'">
        <sequence select="replace( lower-case( $norm ), '[- ]', '_' )" />
      </when>

      <when test="@identifier = 'const'">
        <sequence select="replace( upper-case( $norm ), '[- ]', '_' )" />
      </when>

      <!-- TODO: camelCase -->
      <when test="@identifier = 'rate'">
        <sequence select="replace( $norm, '[-_ ]', '' )" />
      </when>

      <otherwise>
        <sequence select="$norm" />
      </otherwise>
    </choose>
  </variable>

  <!-- everything else gets removed -->
  <sequence select="replace(
                          $pre,
                          '[^a-zA-Z0-9_-]',
                          '' )" />
</template>

<!-- no other styling -->
<template match="lv:param-value" mode="preproc:gen-param-value-style" priority="1">
  <param name="str" />
  <value-of select="$str" />
</template>


<!--
  Converts class name to its @yields variable name
-->
<template mode="preproc:gen-param-value" priority="5"
              match="lv:param-class-to-yields">
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <!-- get the class name from the param -->
  <variable name="pname" select="@name" />
  <variable name="as" select="$params[ @name=$pname ]/@value" />

  <!-- get @yields from class -->
  <variable name="yields" select="
      $src-root/preproc:symtable/preproc:sym[
        @name=concat( ':class:', $as )
      ]/@yields
    " />

  <choose>
    <when test="not( $yields ) or $yields=''">
      <preproc:error>
        <text>error: unable to determine @yields for class `</text>
        <value-of select="$as" />
        <text>' (has the class been imported?)</text>
      </preproc:error>
    </when>

    <otherwise>
      <value-of select="$yields" />
    </otherwise>
  </choose>
</template>


<!--
  Retrieve symbol metadata.
-->
<template mode="preproc:gen-param-value" priority="5"
              match="lv:param-sym-value">
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <variable name="pname" as="xs:string" select="@name" />
  <variable name="value" as="xs:string" select="@value" />

  <variable name="name" as="xs:string"
                select="$params[ @name = $pname ]/@value" />

  <variable name="sym-name" as="xs:string"
                select="concat( @prefix, $name )" />

  <!-- get @yields from class -->
  <variable name="sym-value" as="xs:string?" select="
      $src-root/preproc:symtable/preproc:sym[
        @name = $sym-name ]
          /@*[ local-name() = $value ]" />

  <choose>
    <when test="not( $sym-value ) or $sym-value = ''">
      <!-- error out only if lookup failures aren't explicitly suppressed -->
      <if test="not( @ignore-missing = 'true' )">
        <message>
          <text>warning: unable to find `@</text>
          <value-of select="$value" />
          <text>' for symbol `</text>
          <value-of select="$sym-name" />
          <text>' (does the symbol support `@</text>
          <value-of select="$value" />
          <text>' and has it been imported?)</text>
        </message>

        <!-- just use the name if nothing is available -->
        <value-of select="$name" />
      </if>
    </when>

    <otherwise>
      <value-of select="$sym-value" />
    </otherwise>
  </choose>
</template>


<!--
  Look up the name of a constant within a typedef by its value.

  This exists because sym-set is far too slow because of the inline
  templates; maybe it can be deprecated in the future or replaced with a
  more general primitive.

  The intent of this is to allow code generators to simply output integers
  and let Tame figure out what constant to use, thus avoiding a map to
  constant names in the code generator.  By using constant names, the values
  acquire meaning that is persisted onto the dependency graph and can be
  used for reporting and static analysis.
-->
<template mode="preproc:gen-param-value" priority="5"
          match="lv:param-typedef-lookup">
  <param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <variable name="tname" as="xs:string" select="@name" />
  <variable name="value" as="xs:string" select="@value" />

  <variable name="typedef-name" as="xs:string"
                select="if ( starts-with( $tname, '@' ) ) then
                            $params[ @name = $tname ]/@value
                          else
                            $tname" />
  <variable name="query-value" as="xs:string"
                select="$params[ @name = $value ]/@value" />

  <variable name="typedef-sym" as="element( preproc:sym )"
            select="$src-root/preproc:symtable/preproc:sym[
                      @name = $typedef-name
                      and @type = 'type' ]" />

  <variable name="typedef-pkg" as="element( lv:package )"
            select="if ( $typedef-sym/@src ) then
                          document( concat( $typedef-sym/@src, '.xmlo' ), $__entry-root )
                            /lv:package
                        else
                          $src-root/lv:package" />

  <variable name="typedef" as="element( lv:typedef )"
            select="$typedef-pkg//lv:typedef[
                      @name = $typedef-name ]" />

  <!-- get @yields from class -->
  <variable name="const-name" as="xs:string*"
            select="$typedef//lv:item[ @value = $query-value ]/@name" />

  <choose>
    <when test="count( $const-name ) gt 1">
      <message terminate="yes"
               select="concat( 'error: the value ''', $query-value, ''' ',
                               'is associated with more than one item in ''',
                               $tname, ''': ',
                               string-join( $const-name, ', ' ) )" />
    </when>

    <when test="not( $const-name )">
      <!-- error out only if lookup failures aren't explicitly suppressed -->
      <if test="not( @ignore-missing = 'true' )">
        <message terminate="yes">
          <text>error: the value `</text>
          <value-of select="$query-value" />
          <text>' does not exist in the domain of `</text>
          <value-of select="$tname" />
          <text>'</text>
        </message>
      </if>
    </when>

    <otherwise>
      <value-of select="$const-name" />
    </otherwise>
  </choose>
</template>


<template mode="preproc:gen-param-value" priority="5"
              match="text()">
  <value-of select="." />
</template>


<template mode="preproc:gen-param-value" priority="1"
              match="node()">
  <message terminate="yes">
    <text>error: unknown param node content: </text>
    <copy-of select="." />
  </message>
</template>


<!-- Templates should never be expanded until they are actually replaced by
     macro expansion using apply-templates; just keep ignoring this until it's
     eventually removed by the expand phase -->
<template match="lv:template" mode="preproc:macros" priority="5">
  <sequence select="." />
</template>


<!--
  expand-sequence

  TODO: move to a better place
-->
<template mode="preproc:macros" priority="5"
              match="lv:expand-sequence">
  <sequence select="eseq:expand-step( . )" />
</template>


<!--
  expand-group is our means of grouping together expressions to be
  expanded as a group; this is far more efficient than expanding each
  one individually, when that is unneeded.
-->
<template mode="preproc:macros" priority="5"
              match="lv:expand-group">
  <!-- strip expand-group -->
  <apply-templates mode="preproc:macros" />
</template>



<function name="eseq:is-expandable" as="xs:boolean">
  <param name="node" as="node()" />

  <!-- TODO: what a mess; clean me up by changing the point at which
       this is processed, which will expand all of these into
       lv:apply-template -->
  <sequence select="$node instance of element()
                        and not( $node instance of
                          element( lv:template ) )
                        and (
                          $node instance of
                            element( lv:apply-template )
                          or $node instance of
                            element( lv:inline-template )
                          or namespace-uri( $node )
                            = namespace-uri-for-prefix( 't', $node )
                          or (
                            $node//lv:inline-template,
                            $node//lv:apply-template,
                            $node//t:* )
                              [ not(
                                  ancestor::lv:template
                                  or ancestor::lv:apply-template ) ] )" />
</function>



<function name="eseq:expand-node" as="node()*">
  <param name="node" as="node()" />

  <apply-templates mode="preproc:macros"
                       select="$node" />
</function>



<template mode="preproc:macros" priority="9"
              match="node()[ not( . instance of element() ) ]">
  <sequence select="." />
</template>

</stylesheet>
