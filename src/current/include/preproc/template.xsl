<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Performs template processing and expansion

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
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:util="http://www.lovullo.com/util"
  xmlns:eseq="http://www.lovullo.com/tame/preproc/expand/eseq"
  exclude-result-prefixes="xs ext eseq lv util">

<xsl:import href="../../tame/src/preproc/expand/expand-sequence.xsl" />


<!--
  Macro that abstracts away the boilerplate template application code when
  dealing with a single root template

  This macro will cause another macro expansion pass.
-->
<xsl:template match="lv:rate-each-template" mode="preproc:macros" priority="5">
  <!-- TODO: debug flag
  <xsl:message>
    <xsl:text>[preproc] expanding rate-each-template </xsl:text>
    <xsl:value-of select="@yields" />
    <xsl:text>...</xsl:text>
  </xsl:message>
  -->

  <lv:rate-each>
    <!-- copy all attributes except for those we will explicitly handle below -->
    <xsl:sequence select="@*[
        not( local-name() = 'name' )
      ]" />

    <!-- since the entire body is composed from a template, we do not need to
         accept an index from the user; traditionally, we use 'k' to leave
         indexes like 'i' and 'j' open -->
    <xsl:variable name="index">
      <xsl:text>k</xsl:text>
    </xsl:variable>

    <xsl:attribute name="index">
      <xsl:value-of select="$index" />
    </xsl:attribute>

    <!-- any lv:* nodes should be placed at the top -->
    <xsl:sequence select="./lv:class" />

    <!-- the template is expected to have an @index@ parameter -->
    <lv:apply-template name="{@name}">
      <lv:with-param name="@index@" value="{$index}" />

      <!-- copy any user-provided params into the template application -->
      <xsl:sequence select="./lv:with-param" />
    </lv:apply-template>
  </lv:rate-each>

  <!-- since we generated another preprocessor macro (lv:rate-each), notify the
       system that we will need to make another pass -->
  <preproc:repass tpl="{@name}" />
</xsl:template>


<!--
  If the symbol table is not yet available, defer short-hand
  applications for now

  It is far less costly to leave the node alone than it is to process
  and copy potentially massive trees only to have to repass anyway.
-->
<xsl:template mode="preproc:macros" priority="9"
              match="t:*[ not( root(.)/preproc:symtable ) ]">
  <xsl:sequence select="." />
  <preproc:repass need-sym="true" />
</xsl:template>


<!--
  Converts shorthand template applications into full template expansions

  TODO: This causes an extra pass; we'd like to avoid having to do that.
-->
<xsl:template match="t:*" mode="preproc:macros" priority="5">
  <!-- TODO: debug flag
  <xsl:message>
    <xsl:text>[preproc] expanding template shorthand for </xsl:text>
    <xsl:value-of select="local-name()" />
    <xsl:text>...</xsl:text>
  </xsl:message>
  -->

  <xsl:variable name="name" as="xs:string"
                select="concat( '_', local-name(), '_' )" />

  <xsl:variable name="params" as="element( lv:with-param )*">
    <xsl:for-each select="@*">
      <lv:with-param name="@{local-name()}@" value="{.}" />
    </xsl:for-each>

    <!-- there may be existing lv:with-param nodes -->
    <xsl:sequence select="./lv:with-param" />

    <xsl:variable name="paramnodes"
      select="*[ not( local-name() = 'with-param' ) ]" />

    <!-- if sub-nodes were provided, pass it as the "@values@" param -->
    <xsl:if test="$paramnodes">
      <lv:with-param name="@values@">
        <xsl:sequence select="$paramnodes" />
      </lv:with-param>
    </xsl:if>
  </xsl:variable>

  <!-- XXX: a large chunk of this is duplicate code; factor out -->
  <xsl:variable name="tpl" as="element( lv:template )?"
                select="preproc:locate-template( $name, root( . ) )" />

  <xsl:variable name="src-root" as="element( lv:package )"
                select="if ( root(.)/lv:package ) then
                          root(.)/lv:package
                        else
                          root(.)" />

  <xsl:choose>
    <xsl:when test="$tpl">
      <!-- avoid a costly repass; apply immediately -->
      <xsl:sequence select="preproc:expand-template(
                            $tpl, $src-root, $params, . )" />
    </xsl:when>

    <xsl:otherwise>
      <preproc:error>Undefined template <xsl:value-of select="$name" /></preproc:error>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:function name="preproc:locate-template"
              as="element( lv:template )?">
  <xsl:param name="name" as="xs:string" />
  <xsl:param name="root" as="element( lv:package )" />

  <xsl:variable name="sym" as="element( preproc:sym )?"
                select="$root/preproc:symtable/preproc:sym[
                          @name = $name ]" />

  <xsl:variable name="package" as="element( lv:package )?"
                select="if ( $sym/@src ) then
                          document( concat( $sym/@src, '.xmlo' ), $__entry-root )
                            /lv:package
                        else
                          $root/lv:package" />

  <!-- legacy lookup for inline-templates; needs to regenerate symbol
       list, then this can be removed -->
  <xsl:variable name="inline-match" as="element( lv:template )?"
                select="$root//lv:template[ @name = $name ]" />

  <xsl:sequence select="if ( $inline-match ) then
                          $inline-match
                        else
                          $package//lv:template[ @name=$name ]" />
</xsl:function>



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
<xsl:template match="lv:apply-template[
                       @name=root(.)/preproc:symtable/preproc:sym/@name ]
                     |lv:apply-template[ @name=root(.)//lv:template/@name ]"
              mode="preproc:macros" priority="6">

  <xsl:variable name="name" select="@name" />
  <xsl:variable name="attrparams" select="@*[ not( local-name() = 'name' ) ]" />

  <!-- used for type checking -->
  <xsl:variable name="root" as="element( lv:package )"
                select="if ( root( . ) instance of document-node() ) then
                          root( . )/lv:package
                        else
                          root( . )" />

  <xsl:variable name="src-root" as="element( lv:package )"
                select="if ( $root/lv:package ) then
                          $root/lv:package
                        else
                          $root" />

  <xsl:variable name="tpl" as="element( lv:template )?"
                select="preproc:locate-template( $name, $root )" />

  <xsl:choose>
    <xsl:when test="exists( $tpl ) and $attrparams">
      <xsl:message>
        <xsl:text>[preproc] expanding template parameters for </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>...</xsl:text>
      </xsl:message>

      <xsl:variable name="params" as="element( lv:with-param )*">
        <xsl:for-each select="$attrparams">
          <lv:with-param name="@{local-name()}@" value="{.}" />
        </xsl:for-each>

        <!-- there may also be non-shorthand params -->
        <xsl:sequence select="lv:with-param" />
      </xsl:variable>

      <!-- immediately apply without a wasteful repass -->
      <xsl:sequence select="preproc:expand-template(
                              $tpl, $src-root, $params, . )" />
    </xsl:when>

    <xsl:when test="$tpl">
      <xsl:sequence select="preproc:expand-template(
                              $tpl, $src-root, lv:with-param, . )" />
    </xsl:when>

    <xsl:otherwise>
      <preproc:error>Undefined template <xsl:value-of select="$name" /></preproc:error>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:function name="preproc:expand-template">
  <xsl:param name="tpl"     as="element( lv:template )" />
  <xsl:param name="src-root" />
  <xsl:param name="params"  as="element( lv:with-param )*" />
  <xsl:param name="context" as="node()" />

  <xsl:variable name="name" as="xs:string"
                select="$tpl/@name" />

  <!-- TODO: debug flag
  <xsl:message>
    <xsl:text>[preproc] applying template </xsl:text>
    <xsl:value-of select="@name" />
    <xsl:text>...</xsl:text>
  </xsl:message>
  -->

  <!-- check to ensure that each given argument is defined, unless @strict
       is false  -->
  <xsl:for-each select="
      $params[
        not( @name=$tpl/lv:param/@name )
        and not( @strict='false' )
      ]
    ">
    <preproc:error>
      <xsl:text>undefined template param </xsl:text>
      <xsl:value-of select="$name" />/<xsl:value-of select="@name" />
      <xsl:text>; available params are </xsl:text>

      <xsl:for-each select="$tpl/lv:param/@name">
        <xsl:if test="position() > 1">
          <xsl:text>; </xsl:text>
        </xsl:if>

        <xsl:value-of select="." />
      </xsl:for-each>
    </preproc:error>
  </xsl:for-each>

  <!-- replace this node with a copy of all the child nodes of the given
       template; this inlines it as if it were copied and pasted directly
       into the XML, much like a C macro -->
  <xsl:variable name="apply-result">
    <preproc:tpl-barrier>
      <xsl:apply-templates
          select="$tpl[ 1 ]/*"
          mode="preproc:apply-template">

        <xsl:with-param name="apply" select="$context"
                        tunnel="yes" />
        <xsl:with-param name="tpl" select="$tpl[ 1 ]"
                        tunnel="yes" />
        <xsl:with-param name="apply-tpl-name" select="$name"
                        tunnel="yes" />
        <xsl:with-param name="params" select="$params"
                        tunnel="yes" />

        <xsl:with-param name="first-child" select="true()" />
        <xsl:with-param name="src-root" select="$src-root"
                        tunnel="yes" />
      </xsl:apply-templates>
    </preproc:tpl-barrier>
  </xsl:variable>

  <xsl:apply-templates mode="preproc:mark-tpl-expansion"
                       select="$apply-result">
    <xsl:with-param name="tpl-name" select="$name"
                    tunnel="yes" />
  </xsl:apply-templates>

  <!-- since templates can include anything, we should perform another pass
       to be sure that all macros that this template may contain/generate
       are expanded -->
  <preproc:repass tpl="{$name}" />
  <preproc:tpl-step name="{$name}" type="apply-template" />
</xsl:function>


<!--
  Strip tpl barriers, which serve to scope metadata.
-->
<xsl:template mode="preproc:strip-tpl-cruft" priority="5"
              match="preproc:tpl-barrier">
  <xsl:apply-templates mode="preproc:strip-tpl-cruft"
                       select="node()" />
</xsl:template>

<!--
  Everything else stays.
-->
<xsl:template mode="preproc:strip-tpl-cruft" priority="3"
              match="node()">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:apply-templates mode="preproc:strip-tpl-cruft"
                         select="node()" />
  </xsl:copy>
</xsl:template>



<!--
  Add nodes describing where the parent node came from.

  This helps in debugging and understanding code created by templates.
  This isn't the best way, but e.g. enclosing all expanded nodes in a parent
  tag causes problems with the rest of the system.  Adding an attribute to
  the node is an option, but this implementation allows a node to be marked
  by multiple templates, should such a thing ever occur.
-->
<xsl:template mode="preproc:mark-tpl-expansion" priority="5"
              match="element()">
  <xsl:param name="tpl-name" tunnel="yes" />

  <xsl:copy>
    <xsl:sequence select="@*" />

    <preproc:from-template name="{$tpl-name}" />

    <xsl:sequence select="node()" />
  </xsl:copy>
</xsl:template>

<xsl:template mode="preproc:mark-tpl-expansion" priority="3"
              match="node()|attribute()">
  <xsl:sequence select="." />
</xsl:template>


<!--
  Inline templates depending on the symbol table must not be expanded
  until the symbol table is actually available
-->
<xsl:template mode="preproc:macros" priority="6"
              match="lv:inline-template[ lv:for-each/lv:sym-set
                                         and not( root(.)/preproc:symtable ) ]">
  <lv:expand-sequence>
    <xsl:sequence select="." />
  </lv:expand-sequence>

  <xsl:message select="'[preproc] deferring inline-template waiting for symbol table'" />
</xsl:template>


<!--
  An inline template implicitly defines and then immediately applies a template
  with an optional looping construct
-->
<xsl:template match="lv:inline-template" mode="preproc:macros" priority="5">
  <xsl:variable name="name" select="concat( '___i', generate-id(.), '___' )" />
  <xsl:variable name="inline" select="." />

  <xsl:message>
    <xsl:text>[preproc] preparing inline template </xsl:text>
    <xsl:value-of select="$name" />
  </xsl:message>

  <!-- generate template -->
  <lv:template name="{$name}" desc="Inline template"
               preproc:from-inline="{$name}"
               preproc:generated="true">
    <!-- generate params (from both our own attrs and any for-each sets) -->
    <xsl:variable name="params">
      <params>
        <xsl:for-each select="@*, lv:for-each/lv:set/@*">
          <!-- manual lv:param's will override default param generation -->
          <xsl:if test="not( local-name() = $inline/lv:param/@name )">
            <lv:param name="@{local-name()}@" desc="Generated param" />
          </xsl:if>
        </xsl:for-each>

        <xsl:if test="lv:for-each/lv:sym-set">
          <lv:param name="@sym_name@"     desc="Symbol name" />
          <lv:param name="@sym_type@"     desc="Symbol type" />
          <lv:param name="@sym_dim@"      desc="Symbol degree (dimensions)" />
          <lv:param name="@sym_desc@"     desc="Symbol description (if any)" />
          <lv:param name="@sym_yields@"   desc="Symbol yield name (if any)" />
          <lv:param name="@sym_parent@"   desc="Symbol parent (if any)" />
          <lv:param name="@sym_external@" desc="Symbol external to classifier" />
        </xsl:if>
      </params>
    </xsl:variable>

    <!-- use only unique params from each attribute source -->
    <xsl:sequence select="
        $params//lv:param[
          not( @name=preceding-sibling::lv:param/@name )
        ]
      " />

    <!-- include any params priovided by the user (they may be used to
         generate content) -->
    <xsl:sequence select="lv:param" />

    <!-- copy template body -->
    <!-- FIXME: do not use local-name here; use instance of -->
    <xsl:sequence select="*[ not( local-name() = 'for-each' ) ]" />
  </lv:template>

  <xsl:choose>
    <xsl:when test="lv:for-each">
      <xsl:apply-templates select="lv:for-each/lv:*" mode="preproc:inline-apply">
        <xsl:with-param name="name" select="$name" />
      </xsl:apply-templates>
    </xsl:when>

    <xsl:otherwise>
      <xsl:apply-templates select="." mode="preproc:inline-apply">
        <xsl:with-param name="name" select="$name" />
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>

  <!-- the next pass will perform the template application -->
  <preproc:repass tpl="{$name}" />
</xsl:template>


<xsl:template match="lv:inline-template|lv:for-each/lv:set" mode="preproc:inline-apply">
  <xsl:param name="name" />

  <!-- immediately apply the template -->
  <lv:apply-template name="{$name}">
    <!-- each attribute will be considered to be a template param (and any
         parent lv:inline-template attributes will be added to each and every
         set) -->
    <xsl:for-each select="@*|ancestor::lv:inline-template/@*">
      <lv:with-param name="@{local-name()}@" value="{.}" />
    </xsl:for-each>
  </lv:apply-template>
</xsl:template>


<xsl:template mode="preproc:inline-apply"
              match="lv:for-each/lv:sym-set">
  <xsl:param name="name" />

  <xsl:param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <!-- TODO: moar predicates! -->
  <xsl:variable name="syms" as="element( preproc:sym )*"
                select="$src-root/preproc:symtable/preproc:sym[
                          ( not( current()/@type )
                            or @type = current()/@type )
                          and ( not( current()/@yields )
                                or @yields = current()/@yields )
                          and ( not( current()/@parent )
                                or @parent = current()/@parent )
                          and ( not( current()/@external )
                                or @external = current()/@external )
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

  <xsl:for-each select="$syms">
    <!-- some symbols are modified to include a prefix; these internal
         details should not be exposed to the user, since they are
         useless within the DSL itself -->
    <xsl:variable name="name-strip"
                  select="substring-after(
                          substring-after( @name, ':' ),
                          ':' )" />

    <xsl:variable name="sym-name"
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
      <lv:with-param name="@sym_external@" value="{@external}" />
    </lv:apply-template>
  </xsl:for-each>
</xsl:template>


<!--
  This block is used when we attempt to apply a template that has not been
  defined
-->
<xsl:template match="lv:apply-template" mode="preproc:macros" priority="5">
  <!-- keep this application around for later -->
  <xsl:sequence select="." />

  <!-- nothing we can do yet -->
  <xsl:message>
    <xsl:text>[preproc] deferring application of unknown template </xsl:text>
    <xsl:value-of select="@name" />
  </xsl:message>

  <preproc:repass need-sym="{@name}" />
</xsl:template>


<xsl:template match="lv:template/lv:param" mode="preproc:apply-template" priority="5">
  <xsl:param name="first-child" select="false()" />

  <!-- do not copy the template params for the template being applied; that
       could result in some potentially inteesting bugs -->
  <xsl:if test="not( $first-child )">
    <xsl:sequence select="." />
  </xsl:if>
</xsl:template>


<!--
  Preprocesses template nodes

  Currently, only attributes will be processed. Otherwise, the template ends up
  recursing on all children to process all attributes.
-->
<xsl:template match="*" mode="preproc:apply-template" priority="1">
  <xsl:copy>
    <xsl:apply-templates select="@*|node()"
                         mode="preproc:apply-template" />
  </xsl:copy>
</xsl:template>


<!--
  Generate template metadata from lv:param-meta nodes.

  Metadata contained within an lv:param-copy has its scope limited to
  children only.  Standalone lv:param-meta nodes are "hoisted" and are
  available to siblings.
-->
<xsl:template match="lv:param-meta" mode="preproc:apply-template" priority="5">
  <xsl:param name="apply" as="node()"
             tunnel="yes" />

  <!-- determine the value -->
  <xsl:variable name="value">
    <xsl:call-template name="preproc:template-param-value">
      <xsl:with-param name="name" select="@value" />
    </xsl:call-template>
  </xsl:variable>

  <preproc:tpl-meta name="{@name}" value="{$value}">
    <xsl:if test="not( parent::lv:param-copy )">
      <xsl:attribute name="hoist" select="'true'" />
    </xsl:if>
  </preproc:tpl-meta>
</xsl:template>


<!--
  Copies child nodes associated with the param during application into the
  replacement

  This effectively allows the definition of custom node types.
-->
<xsl:template match="lv:param-copy" mode="preproc:apply-template" priority="5">
  <xsl:param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <!-- if any metadata has been defined, retain it so that it may be queried by
       future templates -->
  <xsl:apply-templates mode="preproc:apply-template"
                       select="lv:param-meta" />

  <xsl:variable name="varname" select="@name" />

  <!-- determine the nodes to copy -->
  <xsl:variable name="copy"
    select="$params[ @name=$varname ]/*" />

  <xsl:choose>
    <!-- if the @expand flag is set, then immediately begin expanding any
         template params that it may have (that we know of), in effect making
         the body of the template application *part* of the template itself -->
    <xsl:when test="@expand='true'">
      <xsl:message>[preproc] expanding param-copy...</xsl:message>
      <xsl:apply-templates select="$copy" mode="preproc:apply-template">
      </xsl:apply-templates>
    </xsl:when>

    <!-- by default, expansion is not requested, meaning that no template param
         expansion will take place (on this pass) for the copied nodes; if any
         template params exist, they will be handled on the next pass -->
    <xsl:otherwise>
      <xsl:sequence select="$copy" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="lv:dyn-node" mode="preproc:apply-template" priority="5">
  <xsl:variable name="dyn-name">
    <xsl:call-template name="preproc:template-param-value">
      <xsl:with-param name="name" select="@name" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:element name="{$dyn-name}">
    <xsl:apply-templates mode="preproc:apply-template" />
  </xsl:element>
</xsl:template>


<!--
  Display an arbitrary error to the user during compilation

  This produces a compile-time error and immediately halts
  compilation.
-->
<xsl:template mode="preproc:apply-template" priority="5"
              match="lv:error">
  <xsl:param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <xsl:variable name="message">
    <xsl:apply-templates mode="preproc:gen-param-value" />
  </xsl:variable>

  <preproc:error>
    <xsl:value-of select="$apply-tpl-name" />
    <xsl:text>: </xsl:text>
    <xsl:value-of select="normalize-space( $message )" />
  </preproc:error>
</xsl:template>


<!--
  Display an arbitrary warning to the user during compilation

  This produces a compile-time warning that is purely informational
  and does not halt compilation.  It is intended to point out
  undesirable conditions.
-->
<xsl:template mode="preproc:apply-template" priority="5"
              match="lv:warning">
  <xsl:param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <xsl:variable name="message">
    <xsl:apply-templates mode="preproc:gen-param-value" />
  </xsl:variable>

  <xsl:message>
    <xsl:text>warning: </xsl:text>
    <xsl:value-of select="$apply-tpl-name" />
    <xsl:text>: </xsl:text>
    <xsl:value-of select="normalize-space( $message )" />
  </xsl:message>
</xsl:template>


<xsl:template mode="preproc:apply-template" priority="5"
              match="text()">
  <xsl:value-of select="." />
</xsl:template>


<!--
  Basic conditional support within templates

  Simple concept, powerful consequences.

  ***No recursion limit check...TODO
-->
<xsl:template match="lv:if|lv:unless" mode="preproc:apply-template" priority="5">
  <xsl:param name="apply-tpl-name" as="xs:string"
             tunnel="yes" />

  <xsl:choose>
    <!-- if the param is unknown, retain; it may be intended for a generated
         template -->
    <xsl:when test="
        not(
          @name=ancestor::lv:template[
            @name=$apply-tpl-name
          ]/lv:param/@name
        )
      ">
      <xsl:sequence select="." />
    </xsl:when>

    <xsl:otherwise>
      <!-- calculate the param value -->
      <xsl:variable name="param-value">
        <xsl:call-template name="preproc:template-param-value">
          <xsl:with-param name="name" select="@name" />
        </xsl:call-template>
      </xsl:variable>

      <!-- @value value -->
      <xsl:variable name="cmp-value">
        <xsl:call-template name="preproc:template-param-value">
          <xsl:with-param name="name" select="@*[ not( local-name() = 'name' ) ]" />
        </xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates select="." mode="preproc:apply-template-cmp">
        <xsl:with-param name="param-value" select="$param-value" />
        <xsl:with-param name="cmp-value" select="$cmp-value" />

        <xsl:with-param name="negate">
          <xsl:choose>
            <xsl:when test="local-name() = 'if'">
              <xsl:value-of select="false()" />
            </xsl:when>

            <xsl:otherwise>
              <xsl:value-of select="true()" />
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="lv:*[@eq]" mode="preproc:apply-template-cmp" priority="5">
  <xsl:param name="negate" select="false()" />
  <xsl:param name="param-value" />
  <xsl:param name="cmp-value" />

  <xsl:if test="
      ( ( $negate = 'true' ) and not( $param-value = $cmp-value ) )
      or ( ( $negate = 'false' ) and ( $param-value = $cmp-value ) )
    ">

    <xsl:apply-templates select="*" mode="preproc:apply-template" />
  </xsl:if>
</xsl:template>


<xsl:template mode="preproc:apply-template-cmp" priority="5"
              match="lv:*[ @prefix ]">
  <xsl:param name="negate" select="false()" />
  <xsl:param name="param-value" />
  <xsl:param name="cmp-value" />

  <xsl:variable name="has-prefix" as="xs:boolean"
                select="starts-with( $param-value, $cmp-value )" />

  <xsl:if test="( $negate and not( $has-prefix ) )
                or ( not( $negate ) and $has-prefix )">
    <xsl:apply-templates select="*" mode="preproc:apply-template" />
  </xsl:if>
</xsl:template>


<xsl:template mode="preproc:apply-template-cmp" priority="5"
              match="lv:*[ @suffix ]">
  <xsl:param name="negate" select="false()" />
  <xsl:param name="param-value" />
  <xsl:param name="cmp-value" />

  <xsl:variable name="has-suffix" as="xs:boolean"
                select="ends-with( $param-value, $cmp-value )" />

  <xsl:if test="( $negate and not( $has-suffix ) )
                or ( not( $negate ) and $has-suffix )">
    <xsl:apply-templates select="*" mode="preproc:apply-template" />
  </xsl:if>
</xsl:template>


<xsl:template match="lv:*[not(@*[not(local-name()='name')])]" mode="preproc:apply-template-cmp" priority="2">
  <xsl:param name="negate" select="'false'" />
  <xsl:param name="param-value" />
  <xsl:param name="cmp-value" />

  <!-- TODO: This is convoluted...we "know" it's empty because it'll return its
       own name as a value by default... -->
  <xsl:if test="
      ( ( $negate = 'true' ) and ( $param-value = @name ) )
      or ( ( $negate = 'false' ) and not( $param-value = @name ) )
    ">

    <xsl:apply-templates select="*" mode="preproc:apply-template" />
  </xsl:if>
</xsl:template>

<xsl:template match="lv:*" mode="preproc:apply-template-cmp" priority="1">
  <preproc:error>
    <xsl:text>Unknown template comparison attribute for </xsl:text>
    <xsl:value-of select="name()" />
    <xsl:text> in </xsl:text>
    <xsl:value-of select="ancestor::lv:template/@name" />
    <xsl:text>:</xsl:text>

    <xsl:for-each select="@*">
      <xsl:text> </xsl:text>
      <xsl:value-of select="local-name()" />
      <xsl:text>="</xsl:text>
      <xsl:value-of select="." />
      <xsl:text>"</xsl:text>
    </xsl:for-each>
  </preproc:error>
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="5"
              match="lv:param-add">
  <!-- calculate the param value -->
  <xsl:variable name="name-value">
    <xsl:call-template name="preproc:template-param-value">
      <xsl:with-param name="name" select="@name" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="value-value">
    <xsl:call-template name="preproc:template-param-value">
      <xsl:with-param name="name" select="@value" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="not( $name-value castable as xs:float )">
    <xsl:message terminate="yes"
                 select="concat( 'error: non-numeric lv:param-add/@name: ',
                                 $name-value )" />
  </xsl:if>

  <xsl:if test="not( $value-value castable as xs:float )">
    <xsl:message terminate="yes"
                 select="concat( 'error: non-numeric lv:param-add/@value: ',
                                 $name-value )" />
  </xsl:if>

  <xsl:value-of select="number( $name-value ) + number( $value-value )" />
</xsl:template>


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
<xsl:template match="@*" mode="preproc:apply-template" priority="5">
  <xsl:param name="tpl" tunnel="yes" />

  <xsl:variable name="name" select="local-name()" />
  <xsl:variable name="varname" select="string(.)" />

  <!-- compile param value -->
  <xsl:variable name="value">
    <xsl:call-template name="preproc:template-param-value">
      <xsl:with-param name="name" select="$varname" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:choose>
    <!-- if the template being applied does not itself define this
         parameter, and we're performing a full var replacement, keep the
         name verbatim for later expansion -->
    <xsl:when test="starts-with( $varname, '@' )
                      and not( $tpl/lv:param[ @name = $varname ] )">
      <xsl:attribute name="{$name}" select="$varname" />
    </xsl:when>

    <!-- if the result is an empty string, then do not output the attribute (this
         allows for conditional attributes -->
    <xsl:when test="not( $value = '' )">
      <xsl:attribute name="{$name}" select="$value" />
    </xsl:when>
  </xsl:choose>
</xsl:template>


<xsl:template match="lv:with-param/@name" mode="preproc:apply-template" priority="9">
  <!-- do not do any sort of expansion of recursive template param names -->
  <xsl:copy />
</xsl:template>


<xsl:template name="preproc:template-param-value">
  <xsl:param name="name" />
  <xsl:param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <xsl:variable name="after" select="substring-after( $name, '@' )" />

  <!-- TODO: support multiple @vars@ per attribute -->
  <xsl:choose>
    <xsl:when test="$name = ''">
      <!-- we're done; nothing more to do -->
    </xsl:when>

    <!-- no vars; just output and abort -->
    <xsl:when test="not( $after )">
      <xsl:value-of select="$name" />
    </xsl:when>

    <!-- if the variable is inline, output the value up until the inline
         delimiter (curly brace); this supports inline vars at any point in the
         string -->
    <xsl:when test="not( starts-with( $name, '@' ) )">
      <!-- this might be nothing (if we start with a curly brace) -->
      <xsl:value-of select="substring-before( $name, '{' )" />

      <!-- get the param between {} -->
      <xsl:variable name="param"
        select="substring-before( substring-after( $name, '{' ), '}' )" />

      <!-- if a param was found, process it -->
      <xsl:if test="$param">
        <xsl:variable name="result">
          <xsl:call-template name="preproc:template-param-value">
            <xsl:with-param name="name" select="$param" />
          </xsl:call-template>
        </xsl:variable>

        <!-- if the result is the same as the param name, then it was not found;
             re-enclose in braces so that it can be processed later (where we
             may find a value) -->
        <xsl:choose>
          <xsl:when test="$result = $param">
            <xsl:text>{</xsl:text>
              <xsl:value-of select="$result" />
            <xsl:text>}</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="$result" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>

      <!-- recurse to generate the remainder of the string -->
      <xsl:call-template name="preproc:template-param-value">
        <xsl:with-param name="name" select="substring-after( $name, '}' )" />
      </xsl:call-template>
    </xsl:when>

    <!-- starts with a macro param -->
    <xsl:otherwise>
      <!-- get the param, since we may have additional text following it -->

      <!-- get the value of this parameter, if it exists, from the caller (note
           below that we will consider a value of the same name as the param to
           be undefined, allowing defaults to be generated; this is useful when
           proxying params between templates)-->
      <xsl:variable name="val"
        select="$params[ @name=$name ]/@value" />

      <!-- the param node itself -->
      <xsl:variable name="param"
        select="ancestor::lv:template/lv:param[ @name=$name ]" />


      <xsl:choose>
        <!-- this test is structured to fail if the param has not been defined
             on the templates parameter list; this is a restriction imposed
             purely for documentation; the preprocessor can do just fine without
             -->
        <xsl:when test="$val and not( $val = $name ) and $param">
          <xsl:value-of select="$val" />
        </xsl:when>

        <!-- otherwise, if we just have the param and it has child nodes,
             generate a value (in essence: generate a default value if one is
             not provided) -->
        <xsl:when test="$param and $param/lv:*">
          <xsl:apply-templates select="$param/lv:*"
                               mode="preproc:gen-param-value" />
        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="$name" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="6"
              match="lv:param/lv:text[ @unique='true' ]">
  <xsl:param name="apply" as="node()"
             tunnel="yes" />

  <xsl:value-of select="." />
  <xsl:value-of select="generate-id( $apply )" />
</xsl:template>


<xsl:template match="lv:param/lv:text" mode="preproc:gen-param-value"
              priority="4">
  <xsl:value-of select="." />
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="5"
              match="lv:param/lv:param-inherit[@meta]">
  <xsl:param name="apply" as="node()"
             tunnel="yes" />

  <xsl:variable name="name" select="@meta" />

  <!-- find the metadata -->
  <xsl:variable name="values"
       select="$apply/ancestor::*/preceding-sibling::preproc:tpl-meta[ @name=$name ]/@value
               , $apply/ancestor::*/preproc:tpl-barrier
                   /preproc:tpl-meta[ @hoist = 'true' and @name=$name ]/@value
               , $apply/preceding-sibling::preproc:tpl-meta[ @name=$name ]/@value" />

  <!-- take the last one (precedence) -->
  <xsl:value-of select="$values[ count( $values ) ]" />
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="4"
            match="lv:param-value">
  <xsl:param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <!-- name of the parameter we're referencing -->
  <xsl:variable name="pname" as="xs:string"
                select="@name" />

  <!-- the original string -->
  <xsl:variable name="str" as="xs:string?"
    select="$params[ @name=$pname ]/@value" />

  <!-- apply @snake if need be -->
  <xsl:variable name="processed">
    <xsl:choose>
      <!-- snakeify -->
      <xsl:when test="@snake">
        <xsl:value-of select="translate( $str, '-', '_' )" />
      </xsl:when>

      <!-- convert spaces and underscores to dashes -->
      <xsl:when test="@dash">
        <xsl:value-of select="translate( $str, '_ ', '--' )" />
      </xsl:when>

      <xsl:when test="@rmdash">
        <xsl:value-of select="translate( $str, '-', '' )" />
      </xsl:when>

      <!-- do nothing -->
      <xsl:otherwise>
        <xsl:value-of select="$str" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- perform any additional processing -->
  <xsl:apply-templates select="." mode="preproc:gen-param-value-style">
    <xsl:with-param name="str" select="$processed" />
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="lv:param-value[@ucfirst]" mode="preproc:gen-param-value-style" priority="5">
  <xsl:param name="str" />

  <xsl:call-template name="util:ucfirst">
    <xsl:with-param name="str" select="$str" />
  </xsl:call-template>
</xsl:template>

<!-- slightly higher priority than @ucfirst, since this obviously should
     override -->
<xsl:template match="lv:param-value[@upper]" mode="preproc:gen-param-value-style" priority="6">
  <xsl:param name="str" />

  <xsl:call-template name="util:uppercase">
    <xsl:with-param name="str" select="$str" />
  </xsl:call-template>
</xsl:template>

<xsl:template match="lv:param-value[@lower]" mode="preproc:gen-param-value-style" priority="6">
  <xsl:param name="str" />

  <xsl:call-template name="util:lowercase">
    <xsl:with-param name="str" select="$str" />
  </xsl:call-template>
</xsl:template>

<!-- convert into valid identifier name -->
<xsl:template match="lv:param-value[@identifier]"
              mode="preproc:gen-param-value-style" priority="6">
  <xsl:param name="str" />

  <xsl:variable name="norm" as="xs:string"
                select="normalize-unicode( $str, 'NFC' )" />

  <xsl:variable name="pre" as="xs:string">
    <xsl:choose >
      <xsl:when test="@identifier = 'class'">
        <xsl:sequence select="replace( lower-case( $norm ), '[_ ]', '-' )" />
      </xsl:when>

      <xsl:when test="@identifier = 'param'">
        <xsl:sequence select="replace( lower-case( $norm ), '[- ]', '_' )" />
      </xsl:when>

      <xsl:when test="@identifier = 'const'">
        <xsl:sequence select="replace( upper-case( $norm ), '[- ]', '_' )" />
      </xsl:when>

      <!-- TODO: camelCase -->
      <xsl:when test="@identifier = 'rate'">
        <xsl:sequence select="replace( $norm, '[-_ ]', '' )" />
      </xsl:when>

      <xsl:otherwise>
        <xsl:sequence select="$norm" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- everything else gets removed -->
  <xsl:sequence select="replace(
                          $pre,
                          '[^a-zA-Z0-9_-]',
                          '' )" />
</xsl:template>

<!-- no other styling -->
<xsl:template match="lv:param-value" mode="preproc:gen-param-value-style" priority="1">
  <xsl:param name="str" />
  <xsl:value-of select="$str" />
</xsl:template>


<!--
  Converts class name to its @yields variable name
-->
<xsl:template mode="preproc:gen-param-value" priority="5"
              match="lv:param-class-to-yields">
  <xsl:param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <xsl:param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <!-- get the class name from the param -->
  <xsl:variable name="pname" select="@name" />
  <xsl:variable name="as" select="$params[ @name=$pname ]/@value" />

  <!-- get @yields from class -->
  <xsl:variable name="yields" select="
      $src-root/preproc:symtable/preproc:sym[
        @name=concat( ':class:', $as )
      ]/@yields
    " />

  <xsl:choose>
    <xsl:when test="not( $yields ) or $yields=''">
      <xsl:message>
        <xsl:text>error: unable to determine @yields for class `</xsl:text>
        <xsl:value-of select="$as" />
        <xsl:text>' (has the class been imported?)</xsl:text>
      </xsl:message>

      <!-- just retain the name; it'll be used for an error message,
           since it won't be foudn -->
      <!-- TODO: this is dangerous; find a way to propagate the error -->
      <xsl:value-of select="$as" />
    </xsl:when>

    <xsl:otherwise>
      <xsl:value-of select="$yields" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Retrieve symbol metadata.
-->
<xsl:template mode="preproc:gen-param-value" priority="5"
              match="lv:param-sym-value">
  <xsl:param name="params" as="element( lv:with-param )*"
             tunnel="yes" />

  <xsl:param name="src-root" as="element( lv:package )"
             select="root(.)"
             tunnel="yes" />

  <xsl:variable name="pname" as="xs:string" select="@name" />
  <xsl:variable name="value" as="xs:string" select="@value" />

  <xsl:variable name="name" as="xs:string"
                select="$params[ @name = $pname ]/@value" />

  <xsl:variable name="sym-name" as="xs:string"
                select="concat( @prefix, $name )" />

  <!-- get @yields from class -->
  <xsl:variable name="sym-value" as="xs:string?" select="
      $src-root/preproc:symtable/preproc:sym[
        @name = $sym-name ]
          /@*[ local-name() = $value ]" />

  <xsl:choose>
    <xsl:when test="not( $sym-value ) or $sym-value = ''">
      <!-- error out only if lookup failures aren't explicitly suppressed -->
      <xsl:if test="not( @ignore-missing = 'true' )">
        <xsl:message>
          <xsl:text>warning: unable to find `@</xsl:text>
          <xsl:value-of select="$value" />
          <xsl:text>' for symbol `</xsl:text>
          <xsl:value-of select="$sym-name" />
          <xsl:text>' (does the symbol support `@</xsl:text>
          <xsl:value-of select="$value" />
          <xsl:text>' and has it been imported?)</xsl:text>
        </xsl:message>

        <!-- just use the name if nothing is available -->
        <xsl:value-of select="$name" />
      </xsl:if>
    </xsl:when>

    <xsl:otherwise>
      <xsl:value-of select="$sym-value" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="5"
              match="text()">
  <xsl:value-of select="." />
</xsl:template>


<xsl:template mode="preproc:gen-param-value" priority="1"
              match="node()">
  <xsl:message terminate="yes">
    <xsl:text>error: unknown param node content: </xsl:text>
    <xsl:copy-of select="." />
  </xsl:message>
</xsl:template>


<!-- Templates should never be expanded until they are actually replaced by
     macro expansion using apply-templates; just keep ignoring this until it's
     eventually removed by the expand phase -->
<xsl:template match="lv:template" mode="preproc:macros" priority="5">
  <xsl:sequence select="." />
</xsl:template>


<!--
  expand-sequence

  TODO: move to a better place
-->
<xsl:template mode="preproc:macros" priority="5"
              match="lv:expand-sequence">
  <xsl:sequence select="eseq:expand-step( . )" />
</xsl:template>


<!--
  expand-group is our means of grouping together expressions to be
  expanded as a group; this is far more efficient than expanding each
  one individually, when that is unneeded.
-->
<xsl:template mode="preproc:macros" priority="5"
              match="lv:expand-group">
  <!-- strip expand-group -->
  <xsl:apply-templates mode="preproc:macros" />
</xsl:template>



<xsl:function name="eseq:is-expandable" as="xs:boolean"
              override="yes">
  <xsl:param name="node" as="node()" />

  <!-- TODO: what a mess; clean me up by changing the point at which
       this is processed, which will expand all of these into
       lv:apply-template -->
  <xsl:sequence select="$node instance of element()
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
</xsl:function>



<xsl:function name="eseq:expand-node" as="node()*"
              override="yes">
  <xsl:param name="node" as="node()" />

  <xsl:apply-templates mode="preproc:macros"
                       select="$node" />
</xsl:function>



<xsl:template mode="preproc:macros" priority="9"
              match="node()[ not( . instance of element() ) ]">
  <xsl:sequence select="." />
</xsl:template>

</xsl:stylesheet>
