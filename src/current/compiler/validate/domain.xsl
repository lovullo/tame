<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Domain validations

  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

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

  TODO: For core domains, validate src package path as well. (Right now,
  param types are polluting, and so this is not a problem.)
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:lvv="http://www.lovullo.com/rater/validate"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Assert that VALUE falls within the provided domain

  SYM-DOMAIN should be a symbol resolving to the domain definition.
-->
<template name="lvv:domain-check">
  <param name="value" />
  <param name="sym-domain" />

  <if test="not( $sym-domain )">
    <message terminate="yes">
      <text>internal error: no domain symbol provided; </text>
      <text>caller: </text>
      <copy-of select="." />
    </message>
  </if>

  <!-- generate node to simplify xpath expressions -->
  <variable name="sym-validate">
    <lvv:chk value="{$value}">
      <copy-of select="$sym-domain" />
    </lvv:chk>
  </variable>

  <apply-templates mode="lvv:domain-check"
    select="$sym-validate/lvv:chk">

    <with-param name="self-pkg" select="
      $sym-domain/ancestor::lv:package" />
  </apply-templates>
</template>


<!--
  Core type checks

  - Integers must simply match when rounded;
  - Floats must be any type of number; and
  - Booleans may be only 1 or 0.
-->
<template match="
  lvv:chk[
    preproc:sym/@type = 'type'
    and (
      (
        preproc:sym/@name = 'integer'
        and not(
          @value = floor( @value )
        )
      )
      or (
        preproc:sym/@name = 'float'
        and not(
          @value = number( @value )
        )
      )
      or (
        preproc:sym/@name = 'boolean'
        and not(
          number( @value ) = 0
          or number( @value ) = 1
        )
      )
    )
  ]"
  mode="lvv:domain-check" priority="5">

  <call-template name="lvv:domain-fail" />
</template>


<!--
  Domain assertions on user-defined types
-->
<template match="
    lvv:chk[
      preproc:sym/@type='type'
      and not (
        preproc:sym/@name = 'integer'
        or preproc:sym/@name = 'float'
        or preproc:sym/@name = 'boolean'
      )
    ]
  "
  mode="lvv:domain-check" priority="5">

  <param name="self-pkg" />

  <variable name="chkval" select="@value" />

  <variable name="domain">
    <call-template name="lvv:get-domain-by-sym">
      <with-param name="sym" select="preproc:sym" />
      <with-param name="self-pkg" select="$self-pkg" />
    </call-template>
  </variable>

  <choose>
    <when test="$domain/lv:domain/lv:element[ @value = $chkval ]">
      <lvv:ok type="domain-check" />
    </when>

    <otherwise>
      <call-template name="lvv:domain-fail" />
    </otherwise>
  </choose>
</template>


<!--
  No validation failure
-->
<template match="lvv:chk"
  mode="lvv:domain-check" priority="2">

  <lvv:ok type="domain-check" />
</template>


<!--
  We passed ourselves something unexpected
-->
<template match="*"
  mode="lvv:domain-chk" priority="1">

  <message terminate="yes">
    <text>internal error: unexpected node for lvv:domain-chk: </text>
    <copy-of select="." />
  </message>
</template>


<!--
  Mark validation as a failure, outputting the assertion

  TODO: Once domains are used as the primary source instead of typedefs,
  check to ensure that the symbol is an actual domain symbol.
-->
<template name="lvv:domain-fail">
  <lvv:fail type="domain-check">
    <copy-of select="." />
  </lvv:fail>
</template>


<template name="lvv:get-domain-by-sym">
  <param name="sym" />
  <param name="self-pkg" select="ancestor::lv:package" />

  <!-- package containing symbol -->
  <variable name="pkg" select="
      if ( $sym/@src and not( $sym/@src='' ) ) then
        document( concat( $sym/@src, '.xmlo' ), $__entry-root )
          /lv:package
      else
        $self-pkg
    " />

  <!-- attempt to locate domain of the given name -->
  <variable name="domain" select="
    $pkg/lv:domain[ @name = $sym/@name ]" />

  <if test="not( $domain )">
    <message terminate="yes">
      <text>error: no domain found for </text>
      <value-of select="$sym/@src" />
      <text>/</text>
      <value-of select="$sym/@name" />
    </message>
  </if>

  <copy-of select="$domain" />
</template>

</stylesheet>

