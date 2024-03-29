<?xml version="1.0" encoding="utf-8"?>
<!--
  Processes executable file dependency graph

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
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:l="http://www.lovullo.com/rater/linker"
            xmlns:dot="http://www.lovullo.com/calc/dot"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Entry point for linked executable (.xmle) DOT generation

  We wish to generate a dependency graph for an entire program. This approach is
  a little bit different than the approach to processing object files, because
  we know that the linker's symbol table contains *only* those symbols that are
  used (or kept). We further know that each symbol (unless there's a bug in the
  linker) is referenced only a single time in the symbol table.

  This makes our job easy: simply walk the symbol table, look up the
  preproc:sym-dep in the source package, and render as we normally would for an
  object file.

  Lord Jesus it's a fire.
-->
<template match="lv:package[ l:dep ]" priority="9">
  <apply-templates select="." mode="dot:head" />

  <!-- we know that all symbols in the linker symbol table are used, so we can
       immediately generate the node definitions -->
  <apply-templates mode="dot:defnode"
    select="l:dep/preproc:sym" />

  <!-- outputting the dependencies of those symbols is more involved and
       requires processing data from each object file -->
  <apply-templates select="l:dep/preproc:sym" mode="dot:ldep-sym-deps">
    <with-param name="exec-name" select="concat( @__rootpath, @name )" />
  </apply-templates>

  <apply-templates select="." mode="dot:tail" />
</template>


<!--
  Omit symbols with parent references

  Symbols with parents are generated from that parent and will be considered to
  be a single unit. Since the parent will also be in the symbol table (it is,
  after all, a dependency), we don't have to worry about these at all.
-->
<template match="preproc:sym[ @parent ]" mode="dot:ldep-sym-deps" priority="5">
  <!-- ignore -->
</template>


<!--
  Process dependencies for each symbol

  The linker symbol table only stores a flattened symbol list; to get the
  symbol's dependencies, we must consult the source object file.
-->
<template match="preproc:sym" mode="dot:ldep-sym-deps" priority="1">
  <param name="exec-name" />

  <variable name="name" select="@name" />

  <!-- empty @src implies program package -->
  <variable name="pkg" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), / )/lv:package
      else
        document( concat( $exec-name, '.xmlo' ), / )/lv:package
    " />

  <variable name="sym-dep" select="
      $pkg/preproc:sym-deps/preproc:sym-dep[
        @name=$name
      ]
    " />

  <if test="not( $sym-dep )">
    <message terminate="yes">
      <text>error: cannot locate symbol dependencies for `</text>
        <value-of select="concat( @src, '/', @name )" />
      <text>'</text>
    </message>
  </if>

  <apply-templates select="$sym-dep" mode="dot:depout" />
</template>


</stylesheet>

