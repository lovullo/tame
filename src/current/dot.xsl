<?xml version="1.0" encoding="utf-8"?>
<!--
  Outputs graph visualization of dependencies in DOT format

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
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:dot="http://www.lovullo.com/calc/dot">

<import href="dot/depout.xsl" />
<import href="dot/defnode.xsl" />

<!-- supported sources (entry points) -->
<include href="dot/pkg-obj.xsl" />
<include href="dot/pkg-exec.xsl" />

<output method="text" />


<!--
  Newline character
-->
<variable name="dot:nl" select="'&#10;'" />


<!--
  Immediately fails on unrecognized source type
-->
<template match="lv:package" priority="1">
  <message terminate="yes">
    <text>[dot] fatal: this is not an object/executable file: </text>
    <text>no symbol dependencies found</text>
  </message>
</template>


<!--
  Beginning of a DOT document
-->
<template match="lv:package" mode="dot:head">
  <text>/* dependency graph of </text>
    <value-of select="@name" />
  <text> */</text>

  <text>digraph "</text>
    <value-of select="@name" />
    <text>" { </text>

    <text>graph [rankdir="LR", ranksep="2"]; </text>
</template>


<!--
  End of a DOT document
-->
<template match="lv:package" mode="dot:tail">
  <text>}</text>
</template>


</stylesheet>

