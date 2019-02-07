<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Entry point for preprocessor

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

  This progress is aggressive; the resulting tree will follow the structure of
  the original XML, but will be heavily augmented and some parts rewritten.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:t="http://www.lovullo.com/rater/apply-template"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:w="http://www.lovullo.com/rater/worksheet"
            xmlns:ext="http://www.lovullo.com/ext"
            xmlns:util="http://www.lovullo.com/util">


<include href="depgen.xsl" />
<include href="preproc/package.xsl" />


<!--
  Raters themselves get special treatment
-->
<template match="lv:rater" mode="preproc:compile" priority="9">
  <param name="orig-root" select="." />
  <param name="stopshort" />

  <message>
    <text>[preproc] [rater]</text>
  </message>

  <!-- handle package preprocessing -->
  <variable name="pkg-result">
    <call-template name="preproc:pkg-compile">
      <with-param name="orig-root" select="$orig-root" />
      <with-param name="stopshort" select="$stopshort" />
    </call-template>
  </variable>

  <copy-of select="$pkg-result" />
</template>

</stylesheet>
