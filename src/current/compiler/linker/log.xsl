<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Logging functions

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:log="http://www.lovullo.com/logger">

<template name="log:info">
  <param name="name" />
  <param name="msg" />

  <message>
    <if test="$name">
      <text>[</text>
        <value-of select="$name" />
      <text>] </text>
    </if>

    <value-of select="$msg" />
  </message>
</template>

<template name="log:debug">
  <param name="name" />
  <param name="msg" />

  <message>
    <if test="$name">
      <text>[</text>
        <value-of select="$name" />
      <text>] </text>
    </if>

    <value-of select="$msg" />
  </message>
</template>

<template name="log:warn">
  <param name="name" />
  <param name="msg" />

  <message>
    <if test="$name">
      <text>[</text>
        <value-of select="$name" />
      <text>] warning: </text>
    </if>

    <value-of select="$msg" />
  </message>
</template>

<template name="log:error">
  <param name="name" />
  <param name="msg" />
  <param name="terminate" select="'yes'" />

  <message terminate="{$terminate}">
    <if test="$msg">
      <if test="$name">
        <text>[</text>
          <value-of select="$name" />
        <text>] error: </text>
      </if>

      <value-of select="$msg" />
    </if>
  </message>
</template>

<template name="log:internal-error">
  <param name="name" />
  <param name="msg" />
  <param name="terminate" select="'yes'" />

  <message terminate="{$terminate}">
    <if test="$name">
      <text>[</text>
        <value-of select="$name" />
      <text>] internal error: </text>
    </if>

    <value-of select="$msg" />
  </message>
</template>

</stylesheet>

