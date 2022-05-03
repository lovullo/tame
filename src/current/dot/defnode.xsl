<?xml version="1.0" encoding="utf-8"?>
<!--
  Graph node definitions

  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.

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

  Nodes do not need to be defined (DOT will generate them upon first reference);
  this defines nodes that require additional data associated with them.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:dot="http://www.lovullo.com/calc/dot"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<import href="./defnode-attr.xsl" />


<!--
  Do not declare constants or generated symbols

  XXX: Duplicated logic from smy-ref!
-->
<template mode="dot:defnode" priority="9" match="
    preproc:sym[
      @type='const'
      or @type='map' or @type='retmap'
      or @type='map:head' or @type='map:tail'
      or @type='retmap:head' or @type='retmap:tail'
      or (
        @type='type'
        and (
          @name='integer'
          or @name='float'
          or @name='boolean'
        )
      )
      or @parent
      or @preproc:generated='true'
    ]
  ">
</template>


<!--
  Process parent symbol in place of current symbol

  Symbols with defined parents are generated as part of that parent and will
  therefore be treated as a single unit.
-->
<template match="preproc:sym[ @parent ]" mode="dot:defnode" priority="7">
  <variable name="parent" select="@parent" />

  <apply-templates select="
      parent::preproc:symtable/preproc:sym[ @name=$parent ]
    " />
</template>


<!--
  Default node definition

  If no attributes are generated, then the node will be entirely omitted (it'll
  be created automatically by DOT when referenced).
-->
<template match="preproc:sym" mode="dot:defnode" priority="1">
  <variable name="attr">
    <call-template name="dot:render-attr-list">
      <with-param name="attr-list">
        <!-- this kluge exists because of XSLT limitiations and the confusion
             that would result (in this particular situation) from
             apply-imports
             -->
        <apply-templates select="." mode="dot:defnode-attr" />
        <apply-templates select="." mode="dot:attr-extern" />
        <apply-templates select="." mode="dot:attr-color" />
        <apply-templates select="." mode="dot:attr-shape" />
      </with-param>
    </call-template>
  </variable>

  <if test="not( $attr = '' )">
    <text>"</text>
      <value-of select="@name" />
    <text>" [</text>
      <value-of select="$attr" />

      <if test="@src and not( @src='' )">
        <if test="$attr">
          <text>,</text>
        </if>

        <!-- link to other packages in the graph for navigation -->
        <text>href="</text>
          <value-of select="concat( @src, '.svg' )" />
        <text>"</text>
      </if>
    <text>];</text>

    <value-of select="$dot:nl" />
  </if>
</template>


</stylesheet>

