<?xml version="1.0" encoding="utf-8"?>
<!--
  Dependency generation

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

  TODO: we can combine this dependency discovery with the symbol table
  generation, eliminating extra passes

  TODO: dependency symbols should not duplicate metadata
-->
<stylesheet version="1.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
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


<variable name="tex-defaults">
  <preproc:syms>
    <preproc:sym value="\alpha" vec="A" />
    <preproc:sym value="\beta" vec="B" />
    <preproc:sym value="\gamma" vec="\Gamma" />
    <preproc:sym value="x" vec="X" />
    <preproc:sym value="y" vec="Y" />
    <preproc:sym value="z" vec="Z" />
  </preproc:syms>
</variable>


<!-- simply allows invoking the template with dynamic input -->
<template name="preproc:gen-deps">
  <param name="pkg" as="element( lv:package )" />
  <apply-templates select="$pkg" mode="preproc:gen-deps" />
</template>


<template match="*" mode="preproc:gen-deps">
  <copy>
    <sequence select="@*" />

    <message>
      <text>[depgen] *determining symbol dependencies...</text>
    </message>

    <apply-templates select="*" mode="preproc:depgen-root" />
  </copy>
</template>


<template mode="preproc:depgen-root" priority="1"
          match="node()">
  <sequence select="." />
</template>


<template mode="preproc:depgen-root" priority="5"
          match="preproc:symtable">
  <!-- Place symbol table _before_ dependencies.  This simplifies
       streaming processing in TAMER. -->
  <sequence select="." />

  <apply-templates select="." mode="preproc:depgen" />
</template>


<template match="preproc:symtable" mode="preproc:depgen" priority="9">
  <variable name="symtable" select="." as="element( preproc:symtable )" />

  <variable name="symtable-map" as="map( xs:string, element( preproc:sym ) )"
                select="map:merge( for $sym in $symtable/preproc:sym
                          return map{ string( $sym/@name ) : $sym } )" />

  <preproc:sym-deps>
    <variable name="deps" as="element( preproc:sym-dep )*">
      <apply-templates mode="preproc:depgen"
                           select="root(.)/lv:*,
                                   root(.)/lvm:*">
        <with-param name="symtable-map" select="$symtable-map"
                        tunnel="yes" />
      </apply-templates>
    </variable>

    <!-- preproc:sym-deps may be nested -->
    <for-each select="$deps, $deps//preproc:sym-dep">
      <variable name="sym-name" as="xs:string"
                    select="@name" />

      <variable name="cursym" as="element( preproc:sym )?"
                    select="$symtable-map( $sym-name )" />

      <if test="not( $cursym )">
        <message select="." />
        <message terminate="yes"
                     select="concat( 'internal error: ',
                                     'cannot find symbol in symbol table: ',
                                     '`', $sym-name, '''' )" />
      </if>

      <!-- do not output duplicates (we used to not output references
           to ourselves, but we are now retaining them, since those
           data are useful) -->
      <variable name="uniq" select="
          preproc:sym-ref[
            not( @name=preceding-sibling::preproc:sym-ref/@name )
          ]
        " />

      <!-- symbols must not have themselves as their own dependency -->
      <if test="$uniq[ not( $cursym/@allow-circular = 'true' )
                           and ( @name = $cursym/@name
                                 or @parent = $cursym/@name ) ]">
        <message terminate="yes"
                     select="concat( '[preproc] !!! fatal: symbol ',
                                     $cursym/@name,
                                     ' references itself ',
                                     '(circular dependency)' )" />
      </if>

      <!-- grab the original source symbol for these references and augment them
           with any additional dependency metadata -->
      <variable name="syms-rtf">
        <for-each select="$uniq">
          <variable name="name" select="@name" />
          <variable name="sym" as="element( preproc:sym )?"
                        select="$symtable-map( $name )" />

          <!-- we should never have this problem. -->
          <if test="not( $sym ) and not( @lax='true' )">
            <message terminate="yes">
              <text>[depgen] internal error: </text>
              <text>could not locate dependency symbol `</text>
              <value-of select="@name" />
              <text>' in local symbol table; needed by </text>
              <value-of select="$cursym/@name" />
            </message>
          </if>

          <!-- copy and augment (we set @name because $sym/@name may not exist
               if @lax) -->
          <preproc:sym name="{@name}">
            <if test="$sym">
              <sequence select="$sym/@*" />
            </if>

            <preproc:meta>
              <!-- retain type -->
              <sequence select="$sym/@type" />
              <sequence select="$sym/@dim" />

              <!-- copy any additional metadata -->
              <sequence select="@*[ not( local-name() = 'name' ) ]" />
            </preproc:meta>
          </preproc:sym>
        </for-each>
      </variable>

      <variable name="syms" select="$syms-rtf/preproc:sym" />

      <!-- only applicable if the symbol is @lax and the symbol was not
           found in the local symbol table -->
      <variable name="lax" select="
          $uniq[
            @lax='true'
            and not( @name=$syms/@name )
          ]
        " />

      <preproc:sym-dep name="{@name}">
        <!-- process symbols that have not been found in the local symbol
             table (only applicable when cursym is @lax) -->
        <for-each select="$lax">
          <!-- the @lax flag here is simply to denote that this symbol may not
               actually exist and that ignoring the check was explicitly
               requested (and not a bug in the depgen process) -->
          <preproc:sym-ref name="{@name}" lax="true">
            <sequence select="preproc:meta/@*" />
          </preproc:sym-ref>
        </for-each>

        <!-- @tex provided an non-empty, or function -->
        <for-each select="
          $syms[
            ( @tex and not( @tex='' ) )
            or @type='func'
          ]">

          <choose>
            <!-- even if function, @tex overrides symbol -->
            <when test="@tex and not( @tex='' )">
              <preproc:sym-ref tex="{@tex}">
                <sequence select="@*" />
                <sequence select="preproc:meta/@*" />
              </preproc:sym-ref>
            </when>

            <!-- must be a function; use its name -->
            <otherwise>
              <preproc:sym-ref>
                <sequence select="@*" />
                <sequence select="preproc:meta/@*" />

                <attribute name="tex">
                  <text>\textrm{</text>
                    <value-of select="@name" />
                  <text>}</text>
                </attribute>
              </preproc:sym-ref>
            </otherwise>
          </choose>
        </for-each>

        <!-- no @tex, @tex empty, no function -->
        <for-each select="
          $syms[
            ( not( @tex ) or @tex='' )
            and not( @type='func' )
          ]">

          <variable name="name" select="@name" />
          <variable name="sym" select="." />

          <preproc:sym-ref>
            <!-- minimal attribute copy (avoid data duplication as much as
                 possible to reduce modification headaches later on) -->
            <sequence select="@name, @parent" />
            <sequence select="preproc:meta/@*" />

            <!-- assign a symbol -->
            <variable name="pos" select="position()" />
            <attribute name="tex">
              <variable name="texsym" select="
                  $tex-defaults/preproc:syms/preproc:sym[
                    position() = $pos
                  ]
                " />

              <choose>
                <when test="$sym/@tex and not( $sym/@tex='' )">
                  <value-of select="$sym/@tex" />
                </when>

                <!-- scalar/vector default -->
                <when test="$texsym and number( $sym/@dim ) lt 2">
                  <value-of select="$texsym/@value" />
                </when>

                <!-- matrix default -->
                <when test="$texsym">
                  <value-of select="$texsym/@vec" />
                </when>

                <!-- no default available; generate one -->
                <otherwise>
                  <value-of select="
                      if ( number( $sym/@dim ) lt 2 ) then '\theta'
                      else '\Theta'
                    " />
                  <text>_{</text>
                    <value-of select="$pos" />
                  <text>}</text>
                </otherwise>
              </choose>
            </attribute>
          </preproc:sym-ref>
        </for-each>
      </preproc:sym-dep>
    </for-each>
  </preproc:sym-deps>
</template>


<template mode="preproc:depgen" priority="7"
              match="lv:rate">
  <preproc:sym-dep name="{@yields}">
    <apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</template>


<template mode="preproc:depgen" priority="7"
              match="c:sum[@generates]
                     |c:product[@generates]">
  <preproc:sym-dep name="{@generates}">
    <preproc:sym-ref name="{ancestor::lv:rate[1]/@yields}" />
  </preproc:sym-dep>

  <!-- this may have other rules; see below -->
  <next-match />
</template>

<template mode="preproc:depgen" priority="9"
              match="lv:classify[ preproc:inline='true' ]">
  <!-- ignore; dependencies will be inlined -->
</template>

<template mode="preproc:depgen" priority="7"
              match="lv:classify">
  <preproc:sym-dep name=":class:{@as}">
    <apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>

  <if test="@yields">
    <preproc:sym-dep name="{@yields}">
      <preproc:sym-ref name=":class:{@as}" />
    </preproc:sym-dep>
  </if>
</template>


<template mode="preproc:depgen" priority="8"
              match="lv:function/lv:param">
  <variable name="fname" as="xs:string"
                select="parent::lv:function/@name" />

  <preproc:sym-dep name=":{$fname}:{@name}" />
</template>

<template mode="preproc:depgen" priority="7"
              match="lv:param">
  <preproc:sym-dep name="{@name}">
    <preproc:sym-ref name="{@type}" />
    <apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</template>


<template mode="preproc:depgen" priority="7"
              match="lv:function">
  <preproc:sym-dep name="{@name}">
    <apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</template>


<template mode="preproc:depgen" priority="7"
              match="lv:typedef">
  <preproc:sym-dep name="{@name}">
    <apply-templates mode="preproc:depgen" />
  </preproc:sym-dep>
</template>


<template match="lv:template"
              mode="preproc:depgen" priority="8">
  <!-- ignore symbols within templates -->
</template>



<template name="preproc:depgen-c-normal" match="c:value-of|c:when" mode="preproc:depgen" priority="5">
  <param name="name" select="@name" />
  <param name="symtable-map" as="map(*)" tunnel="yes" />

  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <variable name="sym" as="element( preproc:sym )?"
                select="$symtable-map( $name )" />

  <!-- see if there is a c:let associated with this name -->
  <variable name="let" select="
      ancestor::c:let[ c:values/c:value/@name=$name ]
    " />

  <choose>
    <!-- c:let reference -->
    <when test="$let">
      <preproc:sym-ref name=":{$let/@name}:{$name}" />
    </when>

    <!-- scalar constant -->
    <when test="( $sym/@type='const' ) and ( $sym/@dim='0' )">
      <!-- while these are optimized away, they are still useful for evaluating
           dependency trees and generating code -->
      <preproc:sym-ref name="{$sym/@name}" />
    </when>

    <!-- function param reference -->
    <when test="$name=ancestor::lv:function/lv:param/@name">
      <variable name="fname" as="xs:string"
                    select="ancestor::lv:function/@name" />

      <preproc:sym-ref name=":{$fname}:{$name}"
                       varname="{$name}"/>
    </when>

    <!-- index reference -->
    <when test="$name=ancestor::c:*[ @of ]/@index" />

    <!-- unknown symbol (it is important to do this after the above checks) -->
    <when test="not( $sym )">
      <!-- do not terminate; validator can provide additional information -->
      <message>
        <text>[depgen] warning: unknown symbol `</text>
          <value-of select="$name" />
        <text>'</text>
      </message>
    </when>

    <when test="$sym/@parent">
      <preproc:sym-ref name="{$sym/@name}" parent="{$sym/@parent}" />
    </when>

    <!-- just an average 'ol symbol -->
    <otherwise>
      <preproc:sym-ref name="{$name}" />
    </otherwise>
  </choose>

  <apply-templates mode="preproc:depgen" />
</template>


<template match="c:sum[@of]|c:product[@of]" mode="preproc:depgen" priority="5">
  <!-- process using @of -->
  <call-template name="preproc:depgen-c-normal">
    <with-param name="name" select="@of" />
  </call-template>
</template>


<template match="c:apply" mode="preproc:depgen" priority="5">
  <!-- no special treatment yet -->
  <call-template name="preproc:depgen-c-normal" />
</template>

<template match="c:apply/c:arg" mode="preproc:depgen" priority="5">
  <!-- arguments may have calculations, so we must recurse -->
  <apply-templates mode="preproc:depgen" />
</template>


<template name="preproc:depgen-match">
  <param name="on" select="@on" />

  <!-- process the @on -->
  <call-template name="preproc:depgen-c-normal">
    <with-param name="name" select="$on" />
  </call-template>
</template>


<!--
  Inlined matches will not be counted as dependencies themselves, but their
  dependencies are our own
-->
<template match="lv:match[ @preproc:inline='true' ]"
          mode="preproc:depgen" priority="7">
  <variable name="self" as="element( lv:match )" select="." />

  <variable name="classify" as="element( lv:classify )?"
            select="( parent::lv:classify
                      /preceding-sibling::lv:classify[ @yields=$self/@on ] )[1]" />

  <if test="empty( $classify )">
    <message terminate="yes"
             select="concat( 'internal error: inline depgen: ',
                             'cannot locate class `', @on, '''' )" />
  </if>

  <apply-templates mode="preproc:depgen"
                   select="$classify/element()" />
</template>


<template match="lv:match[ @value ]" mode="preproc:depgen" priority="5">
  <!-- process the @value -->
  <call-template name="preproc:depgen-c-normal">
    <with-param name="name" select="@value" />
  </call-template>

  <call-template name="preproc:depgen-match" />
</template>


<template match="lv:match[ @anyOf ]" mode="preproc:depgen" priority="6">
  <!-- process the "normal" match -->
  <call-template name="preproc:depgen-match" />

  <!-- we depend on the type -->
  <preproc:sym-ref name="{@anyOf}" />
  <call-template name="preproc:depgen-match" />
</template>


<template match="lv:match[ @pattern ]" mode="preproc:depgen" priority="5">
  <!-- there are no pattern dependencies; process @on -->
  <call-template name="preproc:depgen-match" />
</template>


<!-- match on calculated value -->
<template match="lv:match[ c:* ]" mode="preproc:depgen" priority="6">
  <!-- process the "normal" match -->
  <call-template name="preproc:depgen-match" />

  <!-- process the calculation dependencies -->
  <apply-templates select="c:*" mode="preproc:depgen" />
</template>


<template match="lv:template" mode="preproc:depgen" priority="9">
  <!-- don't generate dependencies for templates (which may have been
       generated inline in unexpected places) -->
</template>


<template match="lv:union" mode="preproc:depgen" priority="5">
  <for-each select="lv:typedef">
    <preproc:sym-ref name="{@name}" />
  </for-each>

  <!-- we still need to process the typedefs independently -->
  <apply-templates mode="preproc:depgen" />
</template>


<template match="lv:enum/lv:item" mode="preproc:depgen" priority="5">
  <preproc:sym-ref name="{@name}" />
</template>


<!-- @class deps -->
<template match="lv:class" mode="preproc:depgen" priority="5">
  <preproc:sym-ref name=":class:{@ref}" class-no="{@no}" />
</template>


<template match="c:*|lv:*" mode="preproc:depgen" priority="3">
  <!-- ignore -->
  <apply-templates mode="preproc:depgen" />
</template>

<template match="node()" mode="preproc:depgen" priority="1">
  <!-- skip -->
</template>

</stylesheet>
