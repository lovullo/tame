<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Dependency generation

  Copyright (C) 2016, 2018, 2019 R-T Specialty, LLC.

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

  TODO: we can combine this dependency discovery with the symbol table
  generation, eliminating extra passes

  TODO: dependency symbols should not duplicate metadata
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:map="http://www.w3.org/2005/xpath-functions/map"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:util="http://www.lovullo.com/util"
  exclude-result-prefixes="ext util xs">


<xsl:variable name="tex-defaults">
  <preproc:syms>
    <preproc:sym value="\alpha" vec="A" />
    <preproc:sym value="\beta" vec="B" />
    <preproc:sym value="\gamma" vec="\Gamma" />
    <preproc:sym value="x" vec="X" />
    <preproc:sym value="y" vec="Y" />
    <preproc:sym value="z" vec="Z" />
  </preproc:syms>
</xsl:variable>


<!-- simply allows invoking the template with dynamic input -->
<xsl:template name="preproc:gen-deps">
  <xsl:param name="pkg" as="element( lv:package )" />
  <xsl:apply-templates select="$pkg" mode="preproc:gen-deps" />
</xsl:template>


<xsl:template match="*" mode="preproc:gen-deps">
  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:message>
      <xsl:text>[depgen] *determining symbol dependencies...</xsl:text>
    </xsl:message>

    <xsl:apply-templates select="preproc:symtable" mode="preproc:depgen" />

    <xsl:sequence select="*" />
  </xsl:copy>
</xsl:template>


<xsl:template match="preproc:symtable" mode="preproc:depgen" priority="9">
  <xsl:variable name="symtable" select="." />

  <xsl:variable name="symtable-map" as="map( xs:string, element( preproc:sym ) )"
                select="map:merge( for $sym in $symtable/preproc:sym
                          return map{ string( $sym/@name ) : $sym } )" />

  <preproc:sym-deps>
    <xsl:variable name="deps" as="element( preproc:sym-dep )*">
      <xsl:apply-templates mode="preproc:depgen"
                           select="root(.)/lv:*,
                                   root(.)/lvm:*">
        <xsl:with-param name="symtable-map" select="$symtable-map"
                        tunnel="yes" />
      </xsl:apply-templates>
    </xsl:variable>

    <!-- preproc:sym-deps may be nested -->
    <xsl:for-each select="$deps, $deps//preproc:sym-dep">
      <xsl:variable name="sym-name" as="xs:string"
                    select="@name" />

      <xsl:variable name="cursym" as="element( preproc:sym )?"
                    select="$symtable-map( $sym-name )" />

      <xsl:if test="not( $cursym )">
        <xsl:message select="." />
        <xsl:message terminate="yes"
                     select="concat( 'internal error: ',
                                     'cannot find symbol in symbol table: ',
                                     '`', $sym-name, '''' )" />
      </xsl:if>

      <!-- do not output duplicates (we used to not output references
           to ourselves, but we are now retaining them, since those
           data are useful) -->
      <xsl:variable name="uniq" select="
          preproc:sym-ref[
            not( @name=preceding-sibling::preproc:sym-ref/@name )
          ]
        " />

      <!-- symbols must not have themselves as their own dependency -->
      <xsl:if test="$uniq[ not( $cursym/@allow-circular = 'true' )
                           and ( @name = $cursym/@name
                                 or @parent = $cursym/@name ) ]">
        <xsl:message terminate="yes"
                     select="concat( '[preproc] !!! fatal: symbol ',
                                     $cursym/@name,
                                     ' references itself ',
                                     '(circular dependency)' )" />
      </xsl:if>

      <!-- grab the original source symbol for these references and augment them
           with any additional dependency metadata -->
      <xsl:variable name="syms-rtf">
        <xsl:for-each select="$uniq">
          <xsl:variable name="name" select="@name" />
          <xsl:variable name="sym" as="element( preproc:sym )?"
                        select="$symtable-map( $name )" />

          <!-- we should never have this problem. -->
          <xsl:if test="not( $sym ) and not( @lax='true' )">
            <xsl:message terminate="yes">
              <xsl:text>[depgen] internal error: </xsl:text>
              <xsl:text>could not locate dependency symbol `</xsl:text>
              <xsl:value-of select="@name" />
              <xsl:text>' in local symbol table; needed by </xsl:text>
              <xsl:value-of select="$cursym/@name" />
            </xsl:message>
          </xsl:if>

          <!-- copy and augment (we set @name because $sym/@name may not exist
               if @lax) -->
          <preproc:sym name="{@name}">
            <xsl:if test="$sym">
              <xsl:sequence select="$sym/@*" />
            </xsl:if>

            <preproc:meta>
              <!-- retain type -->
              <xsl:sequence select="$sym/@type" />
              <xsl:sequence select="$sym/@dim" />

              <!-- copy any additional metadata -->
              <xsl:sequence select="@*[ not( local-name() = 'name' ) ]" />
            </preproc:meta>
          </preproc:sym>
        </xsl:for-each>
      </xsl:variable>

      <xsl:variable name="syms" select="$syms-rtf/preproc:sym" />

      <!-- only applicable if the symbol is @lax and the symbol was not
           found in the local symbol table -->
      <xsl:variable name="lax" select="
          $uniq[
            @lax='true'
            and not( @name=$syms/@name )
          ]
        " />

      <preproc:sym-dep name="{@name}">
        <!-- process symbols that have not been found in the local symbol
             table (only applicable when cursym is @lax) -->
        <xsl:for-each select="$lax">
          <!-- the @lax flag here is simply to denote that this symbol may not
               actually exist and that ignoring the check was explicitly
               requested (and not a bug in the depgen process) -->
          <preproc:sym-ref name="{@name}" lax="true">
            <xsl:sequence select="preproc:meta/@*" />
          </preproc:sym-ref>
        </xsl:for-each>

        <!-- @tex provided an non-empty, or function -->
        <xsl:for-each select="
          $syms[
            ( @tex and not( @tex='' ) )
            or @type='func'
          ]">

          <xsl:choose>
            <!-- even if function, @tex overrides symbol -->
            <xsl:when test="@tex and not( @tex='' )">
              <preproc:sym-ref tex="{@tex}">
                <xsl:sequence select="@*" />
                <xsl:sequence select="preproc:meta/@*" />
              </preproc:sym-ref>
            </xsl:when>

            <!-- must be a function; use its name -->
            <xsl:otherwise>
              <preproc:sym-ref>
                <xsl:sequence select="@*" />
                <xsl:sequence select="preproc:meta/@*" />

                <xsl:attribute name="tex">
                  <xsl:text>\textrm{</xsl:text>
                    <xsl:value-of select="@name" />
                  <xsl:text>}</xsl:text>
                </xsl:attribute>
              </preproc:sym-ref>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>

        <!-- no @tex, @tex empty, no function -->
        <xsl:for-each select="
          $syms[
            ( not( @tex ) or @tex='' )
            and not( @type='func' )
          ]">

          <xsl:variable name="name" select="@name" />
          <xsl:variable name="sym" select="." />

          <preproc:sym-ref>
            <!-- minimal attribute copy (avoid data duplication as much as
                 possible to reduce modification headaches later on) -->
            <xsl:sequence select="@name, @parent" />
            <xsl:sequence select="preproc:meta/@*" />

            <!-- assign a symbol -->
            <xsl:variable name="pos" select="position()" />
            <xsl:attribute name="tex">
              <xsl:variable name="texsym" select="
                  $tex-defaults/preproc:syms/preproc:sym[
                    position() = $pos
                  ]
                " />

              <xsl:choose>
                <xsl:when test="$sym/@tex and not( $sym/@tex='' )">
                  <xsl:value-of select="$sym/@tex" />
                </xsl:when>

                <!-- scalar/vector default -->
                <xsl:when test="$texsym and number( $sym/@dim ) lt 2">
                  <xsl:value-of select="$texsym/@value" />
                </xsl:when>

                <!-- matrix default -->
                <xsl:when test="$texsym">
                  <xsl:value-of select="$texsym/@vec" />
                </xsl:when>

                <!-- no default available; generate one -->
                <xsl:otherwise>
                  <xsl:value-of select="
                      if ( number( $sym/@dim ) lt 2 ) then '\theta'
                      else '\Theta'
                    " />
                  <xsl:text>_{</xsl:text>
                    <xsl:value-of select="$pos" />
                  <xsl:text>}</xsl:text>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </preproc:sym-ref>
        </xsl:for-each>
      </preproc:sym-dep>
    </xsl:for-each>
  </preproc:sym-deps>
</xsl:template>


<xsl:template mode="preproc:depgen" priority="7"
              match="lv:rate">
  <preproc:sym-dep name="{@yields}">
    <xsl:apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</xsl:template>


<xsl:template mode="preproc:depgen" priority="7"
              match="c:sum[@generates]
                     |c:product[@generates]">
  <preproc:sym-dep name="{@generates}">
    <preproc:sym-ref name="{ancestor::lv:rate[1]/@yields}" />
  </preproc:sym-dep>

  <!-- this may have other rules; see below -->
  <xsl:next-match />
</xsl:template>


<xsl:template mode="preproc:depgen" priority="7"
              match="lv:classify">
  <preproc:sym-dep name=":class:{@as}">
    <xsl:apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>

  <xsl:if test="@yields">
    <preproc:sym-dep name="{@yields}">
      <preproc:sym-ref name=":class:{@as}" />
    </preproc:sym-dep>
  </xsl:if>
</xsl:template>


<xsl:template mode="preproc:depgen" priority="8"
              match="lv:function/lv:param">
  <xsl:variable name="fname" as="xs:string"
                select="parent::lv:function/@name" />

  <preproc:sym-dep name=":{$fname}:{@name}">
    <preproc:sym-ref name="{$fname}" />
  </preproc:sym-dep>
</xsl:template>

<xsl:template mode="preproc:depgen" priority="7"
              match="lv:param">
  <preproc:sym-dep name="{@name}">
    <preproc:sym-ref name="{@type}" />
    <xsl:apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</xsl:template>


<xsl:template mode="preproc:depgen" priority="7"
              match="lv:function">
  <preproc:sym-dep name="{@name}">
    <xsl:apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</xsl:template>


<xsl:template mode="preproc:depgen" priority="7"
              match="lv:typedef">
  <preproc:sym-dep name="{@name}">
    <xsl:apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</xsl:template>


<xsl:template match="lv:template"
              mode="preproc:depgen" priority="8">
  <!-- ignore symbols within templates -->
</xsl:template>



<xsl:template name="preproc:depgen-c-normal" match="c:value-of|c:when" mode="preproc:depgen" priority="5">
  <xsl:param name="name" select="@name" />
  <xsl:param name="symtable-map" as="map(*)" tunnel="yes" />

  <xsl:variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <xsl:variable name="sym" as="element( preproc:sym )?"
                select="$symtable-map( $name )" />

  <!-- see if there is a c:let associated with this name -->
  <xsl:variable name="let" select="
      ancestor::c:let[ c:values/c:value/@name=$name ]
    " />

  <xsl:choose>
    <!-- c:let reference -->
    <xsl:when test="$let">
      <preproc:sym-ref name=":{$let/@name}:{$name}" />
    </xsl:when>

    <!-- scalar constant -->
    <xsl:when test="( $sym/@type='const' ) and ( $sym/@dim='0' )">
      <!-- while these are optimized away, they are still useful for evaluating
           dependency trees and generating code -->
      <preproc:sym-ref name="{$sym/@name}" />
    </xsl:when>

    <!-- function param reference -->
    <xsl:when test="$name=ancestor::lv:function/lv:param/@name">
      <xsl:variable name="fname" as="xs:string"
                    select="ancestor::lv:function/@name" />

      <preproc:sym-ref name=":{$fname}:{$name}"
                       varname="{$name}"/>
    </xsl:when>

    <!-- index reference -->
    <xsl:when test="$name=ancestor::c:*[ @of ]/@index" />

    <!-- unknown symbol (it is important to do this after the above checks) -->
    <xsl:when test="not( $sym )">
      <!-- do not terminate; validator can provide additional information -->
      <xsl:message>
        <xsl:text>[depgen] warning: unknown symbol `</xsl:text>
          <xsl:value-of select="$name" />
        <xsl:text>'</xsl:text>
      </xsl:message>
    </xsl:when>

    <xsl:when test="$sym/@parent">
      <preproc:sym-ref name="{$sym/@name}" parent="{$sym/@parent}" />
    </xsl:when>

    <!-- just an average 'ol symbol -->
    <xsl:otherwise>
      <preproc:sym-ref name="{$name}" />
    </xsl:otherwise>
  </xsl:choose>

  <xsl:apply-templates mode="preproc:depgen" />
</xsl:template>


<xsl:template match="c:sum[@of]|c:product[@of]" mode="preproc:depgen" priority="5">
  <!-- process using @of -->
  <xsl:call-template name="preproc:depgen-c-normal">
    <xsl:with-param name="name" select="@of" />
  </xsl:call-template>
</xsl:template>


<xsl:template match="c:apply" mode="preproc:depgen" priority="5">
  <!-- no special treatment yet -->
  <xsl:call-template name="preproc:depgen-c-normal" />
</xsl:template>

<xsl:template match="c:apply/c:arg" mode="preproc:depgen" priority="5">
  <!-- arguments may have calculations, so we must recurse -->
  <xsl:apply-templates mode="preproc:depgen" />
</xsl:template>


<xsl:template name="preproc:depgen-match">
  <xsl:param name="on" select="@on" />
  <xsl:param name="symtable-map" as="map(*)" tunnel="yes" />

  <xsl:variable name="class" select="ancestor::lv:classify" />
  <xsl:variable name="sym" as="element( preproc:sym )"
                select="$symtable-map( $on )" />

  <!-- are we depending on another classification? -->
  <xsl:if test="$sym/@type='cgen'">
    <xsl:variable name="cname" select="substring-after( $sym/@parent, ':class:' )" />

    <!-- check if one of our dependencies wants to be external to the classifier,
         but we're trying to pull them in...tug-of-war -->
    <xsl:if test="$sym/@extclass='true' and not( $class/@external='true' )">
      <xsl:message terminate="yes">
        <xsl:text>[preproc] !!! fatal: internal classification `</xsl:text>
          <xsl:value-of select="$class/@as" />
        <xsl:text>' cannot pull in external classification `</xsl:text>
          <xsl:value-of select="$cname" />
        <xsl:text>'</xsl:text>
      </xsl:message>
    </xsl:if>
  </xsl:if>

  <!-- process the @on -->
  <xsl:call-template name="preproc:depgen-c-normal">
    <xsl:with-param name="name" select="$on" />
  </xsl:call-template>
</xsl:template>


<xsl:template match="lv:match[ @value ]" mode="preproc:depgen" priority="5">
  <!-- process the @value -->
  <xsl:call-template name="preproc:depgen-c-normal">
    <xsl:with-param name="name" select="@value" />
  </xsl:call-template>

  <xsl:call-template name="preproc:depgen-match" />
</xsl:template>


<xsl:template match="lv:match[ @anyOf ]" mode="preproc:depgen" priority="6">
  <!-- process the "normal" match -->
  <xsl:call-template name="preproc:depgen-match" />

  <!-- we depend on the type -->
  <preproc:sym-ref name="{@anyOf}" />
  <xsl:call-template name="preproc:depgen-match" />
</xsl:template>


<xsl:template match="lv:match[ @pattern ]" mode="preproc:depgen" priority="5">
  <!-- there are no pattern dependencies; process @on -->
  <xsl:call-template name="preproc:depgen-match" />
</xsl:template>


<!-- match on calculated value -->
<xsl:template match="lv:match[ c:* ]" mode="preproc:depgen" priority="6">
  <!-- process the "normal" match -->
  <xsl:call-template name="preproc:depgen-match" />

  <!-- process the calculation dependencies -->
  <xsl:apply-templates select="c:*" mode="preproc:depgen" />
</xsl:template>


<xsl:template match="lv:template" mode="preproc:depgen" priority="9">
  <!-- don't generate dependencies for templates (which may have been
       generated inline in unexpected places) -->
</xsl:template>


<xsl:template match="lv:union" mode="preproc:depgen" priority="5">
  <xsl:for-each select="lv:typedef">
    <preproc:sym-ref name="{@name}" />
  </xsl:for-each>

  <!-- we still need to process the typedefs independently -->
  <xsl:apply-templates mode="preproc:depgen" />
</xsl:template>


<xsl:template match="lv:enum/lv:item" mode="preproc:depgen" priority="5">
  <preproc:sym-ref name="{@name}" />
</xsl:template>


<!-- @class deps -->
<xsl:template match="lv:class" mode="preproc:depgen" priority="5">
  <preproc:sym-ref name=":class:{@ref}" class-no="{@no}" />
</xsl:template>


<xsl:template match="c:*|lv:*" mode="preproc:depgen" priority="3">
  <!-- ignore -->
  <xsl:apply-templates mode="preproc:depgen" />
</xsl:template>

<xsl:template match="node()" mode="preproc:depgen" priority="1">
  <!-- skip -->
</xsl:template>

</xsl:stylesheet>
