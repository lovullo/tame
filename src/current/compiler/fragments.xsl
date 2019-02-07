<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript

  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<template match="lv:package" mode="preproc:compile-fragments">
  <copy>
    <sequence select="@*, *" />

    <!-- compile each fragment in the symbol table -->
    <preproc:fragments>
      <for-each select="preproc:symtable/preproc:sym">
        <variable name="result">
          <apply-templates select="." mode="preproc:compile-fragments" />
        </variable>

        <if test="$result != ''">
          <preproc:fragment id="{@name}">
            <value-of select="$result" />
          </preproc:fragment>
        </if>
      </for-each>
    </preproc:fragments>
  </copy>
</template>


<template match="preproc:sym[ @src ]" mode="preproc:compile-fragments" priority="9">
  <!-- do not compile external symbols -->
</template>

<template match="preproc:sym" mode="preproc:compile-fragments" priority="1">
  <message terminate="yes">
    <text>[jsc] fatal: unknown symbol type for `</text>
      <value-of select="@name" />
    <text>': </text>
    <value-of select="@type" />
  </message>
</template>


<template match="preproc:sym[ @type='rate' ]" mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <!-- could be one of two places -->
  <apply-templates mode="compile" select="
    $pkg/lv:rate[ @yields=$name ]
    , $pkg/lv:rate-group/lv:rate[ @yields=$name ]
   " />
</template>
<template match="preproc:sym[ @type='gen' ]" mode="preproc:compile-fragments" priority="5">
  <!-- compiled by above -->
</template>

<template match="preproc:sym[ @type='class' ]" mode="preproc:compile-fragments" priority="5">
  <!-- name is prefixed with :class: -->
  <variable name="as" select="substring-after( @name, ':class:' )" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <apply-templates select="$pkg/lv:classify[ @as=$as ]" mode="compile" />
</template>
<template match="preproc:sym[ @type='cgen' ]" mode="preproc:compile-fragments" priority="5">
  <!-- compiled by above -->
</template>

<template match="preproc:sym[ @type='func' ]" mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <apply-templates select="$pkg/lv:function[ @name=$name ]" mode="compile" />
</template>

<template match="preproc:sym[ @type='param' ]" mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <apply-templates select="$pkg/lv:param[ @name=$name ]" mode="compile" />
</template>

<template match="preproc:sym[ @type='type' ]" mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <!-- a typedef can stand on its own or exist within another typedef -->
  <apply-templates mode="compile" select="
      $pkg/lv:typedef[ @name=$name ]
      , $pkg//lv:typedef//lv:typedef[ @name=$name ]
    " />
</template>

<template match="preproc:sym[ @type='const' ]" mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <apply-templates mode="compile"
                       select="$pkg/lv:const[ @name=$name ],
                               $pkg/lv:typedef//lv:item[ @name=$name ]">
    <with-param name="as-const" select="true()" />
  </apply-templates>
</template>

<template match="preproc:sym[ @type='tpl' ]" mode="preproc:compile-fragments" priority="5">
  <!-- templates are for the preprocessor only -->
</template>

<template match="preproc:sym[ @type='lparam' ]" mode="preproc:compile-fragments" priority="5">
  <!-- they're local and therefore compiled as part of the containing block -->
</template>

<template match="preproc:sym[ @type='meta' ]"
              mode="preproc:compile-fragments" priority="5">
  <variable name="name" select="substring-after( @name, ':meta:' )" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <variable name="node" as="element( lv:prop )"
                select="$pkg/lv:meta/lv:prop[ @name=$name ]" />
  <apply-templates mode="compile"
                       select="$node" />
</template>

</stylesheet>
