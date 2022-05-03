<?xml version="1.0" encoding="utf-8"?>
<!--
  Parameter validations

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:lvv="http://www.lovullo.com/rater/validate"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Param type must be known

  TODO: Doesn't the symbol table lookup process handle this?
-->
<template match="
  lv:param[
    not(
      @type=root(.)/preproc:symtable/preproc:sym[
        @type
      ]/@name
    )
  ]"
  mode="lvv:validate" priority="5">

  <call-template name="lvv:error">
    <with-param name="desc" select="'Unknown param type'" />
    <with-param name="refnode" select="." />
    <with-param name="content">
      <text>'</text>
      <value-of select="@type" />
      <text>' is undefined for param </text>
      <value-of select="@name" />
    </with-param>
  </call-template>
</template>


<!--
  Default must be within the domain of the param

  Note that this template priority is less than the template that checks to
  ensure that the param type exists in the first place.
-->
<template match="lv:param[ @default ]"
  mode="lvv:validate" priority="4">

  <variable name="type" select="@type" />

  <!-- default must be within its domain -->
  <variable name="result">
    <call-template name="lvv:domain-check">
      <with-param name="value" select="@default" />
      <with-param name="sym-domain" select="
        root(.)/preproc:symtable/preproc:sym[
          @name = $type
        ]" />
    </call-template>
  </variable>

  <if test="not( $result/lvv:ok )">
    <variable name="fail" select="$result/lvv:fail/lvv:chk" />

    <!-- if we didn't succeed, but we didn't fail, then we did something we
         weren't supposed to -->
    <if test="not( $fail )">
      <message terminate="yes">
        <text>internal error: in limbo processing param `</text>
          <value-of select="@name" />
        <text>' @default</text>
      </message>
    </if>

    <call-template name="lvv:error">
      <with-param name="desc" select="'param @default domain violation'" />
      <with-param name="refnode" select="." />
      <with-param name="content">
        <text>param `</text>
          <value-of select="@name" />
        <text>' @default of `</text>
          <value-of select="$fail/@value" />
        <text>' is not within its domain of </text>
        <value-of select="$fail/preproc:sym/@src" />
        <text>/</text>
        <value-of select="$fail/preproc:sym/@name" />
      </with-param>
    </call-template>
  </if>
</template>


<!--
  Fallback for no validation issues
-->
<template match="lv:param" mode="lvv:validate" priority="2">
</template>

</stylesheet>

