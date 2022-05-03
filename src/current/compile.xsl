<?xml version="1.0" encoding="utf-8"?>
<!--
  Entry point for compilation

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
  for each task that requires such output.

  Also performs validation.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:lvm="http://www.lovullo.com/rater/map"
            xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:w="http://www.lovullo.com/rater/worksheet"
            xmlns:util="http://www.lovullo.com/util"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:compiler="http://www.lovullo.com/rater/compiler"
            xmlns:calc-compiler="http://www.lovullo.com/calc/compiler"
            xmlns:ext="http://www.lovullo.com/ext">

<output
  indent="yes"
  omit-xml-declaration="yes"
  />

<!-- processing utilities -->
<include href="include/util.xsl" />

<!-- compiler -> JS -->
<include href="include/preprocess.xsl" />
<include href="compiler/validate.xsl" />
<include href="compiler/js.xsl" />
<include href="compiler/map.xsl" />

<!-- TODO: move into compiler/ -->
<include href="c1map.xsl" />

<!-- contains get-symbol-map -->
<include href="include/display.xsl" />

<!-- allows disabling of time-consuming validation -->
<param name="preproc-cache-validate" select="'true'" />


<!--
  Simply copy the preprocessor output; the caller is responsible for outputting
  this to a document.
-->
<template match="/lv:rater|/lv:package" priority="5">
  <!-- XXX: Duplicate code; see summary -->
  <variable name="processed">
    <apply-templates select="." mode="preproc:compile" />
  </variable>

  <!-- fail on preprocessor errors -->
  <call-template name="preproc:err-chk">
    <with-param name="processed" select="$processed" />
  </call-template>

  <!-- validation must have passed; output the nodes -->
  <copy-of select="$processed" />
</template>


<template name="preproc:err-chk">
  <param name="processed" />

  <for-each select="$processed//preproc:error">
    <message terminate="yes">
      <text>!!! preprocessor error: </text>
      <value-of select="." />
    </message>
  </for-each>
</template>


<template match="*" mode="preproc:handle-errors" priority="1">
  <!-- do nothing -->
</template>


<template match="lvm:program-map|lvm:return-map" priority="5">
  <apply-templates select="." mode="lvmc:compile" />
</template>


<template match="w:worksheet" priority="5">
  <apply-templates select="." mode="w:compile" />
</template>


<!-- any unhandled nodes should be an error -->
<template match="*" priority="1">
  <message terminate="yes">
    <text>fatal: source file is invalid: unexpected node </text>
    <value-of select="name()" />
  </message>
</template>

</stylesheet>
