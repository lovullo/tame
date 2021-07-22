<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Utility templates

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
            xmlns:util="http://www.lovullo.com/util"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:lvm="http://www.lovullo.com/rater/map"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:ext="http://www.lovullo.com/ext">


<variable name="_chrlower" select="'abcdefghijklmnopqrstuvwxyz'" />
<variable name="_chrupper" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

<!--
  Path to map directory
-->
<param name="map-root" select="'../map/'" />
<param name="retmap-root" select="concat( $map-root, 'return/' )" />


<template name="util:load-package">
  <param name="package" />
  <param name="self" />

  <variable name="path" select="concat($package, '.xml')" />
  <variable name="pkg" select="document( $path, $self )/lv:package" />

  <ext:package name="{$pkg/@name}">
    <!-- path, including extension -->
    <attribute name="path">
      <value-of select="$path" />
    </attribute>

    <!-- path, excluding extension (as it appears in @package) -->
    <attribute name="import-path">
      <value-of select="@package" />
    </attribute>

    <copy-of select="$pkg" />
  </ext:package>
</template>


<template match="lvm:*" mode="util:map-expand" priority="1">
  <param name="path" select="'.'" />

  <copy>
    <copy-of select="@*" />
    <apply-templates mode="util:map-expand">
      <with-param name="path" select="$path" />
    </apply-templates>
  </copy>
</template>

<!-- recursively inline imports -->
<template match="lvm:import" mode="util:map-expand" priority="5">
  <param name="path" select="'.'" />

  <apply-templates
    select="document( concat( @path, '.xml' ), . )/lvm:*/*"
    mode="util:map-expand">

    <with-param name="path" select="concat( $path, '/', @path )" />
  </apply-templates>
</template>

<!-- must be lower priority than lv:import -->
<template match="/lvm:*/lvm:*" mode="util:map-expand" priority="3">
  <param name="path" select="'.'" />

  <copy>
    <copy-of select="@*" />
    <attribute name="__src" select="$path" />

    <apply-templates mode="util:map-expand">
      <with-param name="path" select="$path" />
    </apply-templates>
  </copy>
</template>


<!--
  Converts first character to uppercase

  Functions like ucfirst in PHP

  @param string str string to ucfirst

  @return string provided string with first character converted to uppercase
-->
<template name="util:ucfirst">
  <param name="str" />

  <!-- convert first char to uppercase -->
  <value-of
    select="translate( substring( $str, 1, 1 ), $_chrlower, $_chrupper )" />

  <!-- remainder of string as it was provided -->
  <value-of select="substring( $str, 2 )" />
</template>


<!--
  Converts a string to uppercase
-->
<template name="util:uppercase">
  <param name="str" />
  <value-of select="translate( $str, $_chrlower, $_chrupper )" />
</template>

<!--
  Converts a string to lowercase
-->
<template name="util:lowercase">
  <param name="str" />
  <value-of select="translate( $str, $_chrupper, $_chrlower )" />
</template>


<template name="util:json">
  <param name="id" />
  <param name="value" />
  <param name="obj" />
  <param name="array" />

  <if test="$id">
    <text>"</text>
      <call-template name="util:json-escape">
        <with-param name="string" select="$id" />
      </call-template>
    <text>":</text>
  </if>

  <choose>
    <when test="$array">
      <text>[</text>
        <for-each select="$array/*">
          <if test="position() > 1">
            <text>,</text>
          </if>

          <value-of select="." />
        </for-each>
      <text>]</text>
    </when>

    <when test="$obj">
      <text>{</text>
        <for-each select="$obj/*">
          <if test="position() > 1">
            <text>,</text>
          </if>

          <value-of select="." />
        </for-each>
      <text>}</text>
    </when>

    <when test="$value">
      <text>"</text>
        <call-template name="util:json-escape">
          <with-param name="string" select="$value" />
        </call-template>
      <text>"</text>
    </when>

    <otherwise>
      <message terminate="yes">[util] !!! invalid util:json</message>
    </otherwise>
  </choose>
</template>


<template name="util:json-escape">
  <param name="string" />

  <value-of select="$string" />
</template>

</stylesheet>
