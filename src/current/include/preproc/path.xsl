<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Operations on paths

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
-->
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<template name="preproc:get-path">
  <param name="path" />
  <param name="prev" select="''" />

  <variable name="first" select="substring-before( $path, '/' )" />
  <variable name="rest" select="substring-after( $path, '/' )" />

  <choose>
    <!-- if there's no $first, then there is no path separator, in which case
         we're done; if there's no rest, then there is a path separator, but it
         resulted in an empty string, meanaing that it ends in a path
         separator, in which case we are also done -->
    <when test="not( $first ) or not( $rest )">
      <!-- no more path separators; we're done -->
      <value-of select="$prev" />
    </when>

    <!-- keep recursing -->
    <otherwise>
      <call-template name="preproc:get-path">
        <with-param name="path" select="$rest" />
        <with-param name="prev">
          <if test="not( $prev = '' )">
            <value-of select="concat( $prev, '/' )" />
          </if>

          <value-of select="$first" />
        </with-param>
      </call-template>
    </otherwise>
  </choose>
</template>


<function name="preproc:get-path" as="xs:string">
  <param name="path" />

  <call-template name="preproc:get-path">
    <with-param name="path" select="$path" />
  </call-template>
</function>


<!-- FIXME: duplicate code with above -->
<template name="preproc:get-basename">
  <param name="path" />
  <param name="prev" select="''" />

  <variable name="first" select="substring-before( $path, '/' )" />
  <variable name="rest" select="substring-after( $path, '/' )" />

  <choose>
    <!-- if there's no $first, then there is no path separator, in which case
         we're done; if there's no rest, then there is a path separator, but it
         resulted in an empty string, meanaing that it ends in a path
         separator, in which case we are also done -->
    <when test="not( $first ) or not( $rest )">
      <!-- no more path separators; we're done -->
      <value-of select="$path" />
    </when>

    <!-- keep recursing -->
    <otherwise>
      <call-template name="preproc:get-basename">
        <with-param name="path" select="$rest" />
        <with-param name="prev">
          <if test="not( $prev = '' )">
            <value-of select="concat( $prev, '/' )" />
          </if>

          <value-of select="$first" />
        </with-param>
      </call-template>
    </otherwise>
  </choose>
</template>


<!-- TODO: rename to preproc:resolve-path -->
<template name="preproc:resolv-path">
  <param name="path" />

  <!-- in order: strip //, process ../, strip ./ -->
  <call-template name="preproc:strip-sdot-path">
    <with-param name="path">
      <call-template name="preproc:resolv-rel-path">
        <with-param name="path">
          <call-template name="preproc:strip-extra-path">
            <with-param name="path" select="$path" />
          </call-template>
        </with-param>
      </call-template>
    </with-param>
  </call-template>
</template>


<function name="preproc:resolv-path" as="xs:string">
  <param name="path" />

  <variable name="result" as="xs:string*">
    <call-template name="preproc:resolv-path">
      <with-param name="path" select="$path" />
    </call-template>
  </variable>

  <sequence select="string-join( $result, '' )" />
</function>

<!-- alias to the above -->
<function name="preproc:resolve-path" as="xs:string">
  <param name="path" />

  <sequence select="preproc:resolv-path( $path )" />
</function>


<!-- XXX: warning, this won't like 'foo../' -->
<template name="preproc:resolv-rel-path">
  <param name="path" />

  <!-- relative paths -->
  <variable name="before" select="substring-before( $path, '../' )" />
  <variable name="after" select="substring-after( $path, '../' )" />

  <choose>
    <when test="$before">
      <call-template name="preproc:resolv-rel-path">
        <with-param name="path">
          <!-- remove the last directory before the ../ -->
          <variable name="before-path">
            <call-template name="preproc:get-path">
              <with-param name="path" select="$before" />
            </call-template>
          </variable>

          <value-of select="$before-path" />

          <!-- the above get-path call will strip the trailing slash -->
          <if test="not( $before-path = '' ) and not( $after = '' )">
            <text>/</text>
          </if>

          <value-of select="$after" />
        </with-param>
      </call-template>
    </when>


    <!-- if there's no $before but there is an $after, then we must begin with
         '../', which we can do nothing with; output it and continue processing
         the remainder of the path -->
    <when test="$after">
      <text>../</text>

      <call-template name="preproc:resolv-rel-path">
        <with-param name="path" select="$after" />
      </call-template>
    </when>


    <!-- no relative paths remaining -->
    <otherwise>
      <value-of select="$path" />
    </otherwise>
  </choose>
</template>


<template name="preproc:strip-sdot-path">
  <param name="path" />

  <choose>
    <!-- the only time this should be called with an unresolved relative path
         is if it begins with one, in which case we'll simply output it and
         continue processing without it -->
    <when test="starts-with( $path, '../' )">
      <text>../</text>

      <!-- continue processing without it -->
      <call-template name="preproc:strip-sdot-path">
        <with-param name="path" select="substring-after( $path, '../' )" />
      </call-template>
    </when>


    <!-- path is safe for processing -->
    <otherwise>
      <variable name="a" select="substring-before( $path, './' )" />
      <variable name="b" select="substring-after( $path, './' )" />


      <choose>
        <!-- if we found one, recurse -->
        <when test="$a or $b">
          <call-template name="preproc:strip-sdot-path">
            <with-param name="path">
              <value-of select="$a" />

              <if test="$a and $b">
                <text>/</text>
              </if>

              <value-of select="$b" />
            </with-param>
          </call-template>
        </when>


        <!-- done -->
        <otherwise>
          <value-of select="$path" />
        </otherwise>
      </choose>
    </otherwise>
  </choose>
</template>


<template name="preproc:strip-extra-path">
  <param name="path" />

  <variable name="a" select="substring-before( $path, '//' )" />
  <variable name="b" select="substring-after( $path, '//' )" />


  <choose>
    <!-- if we found one, recurse -->
    <when test="$a or $b">
      <call-template name="preproc:strip-extra-path">
        <with-param name="path">
          <value-of select="$a" />

          <if test="$a and $b">
            <text>/</text>
          </if>

          <value-of select="$b" />
        </with-param>
      </call-template>
    </when>

    <!-- we're done! -->
    <otherwise>
      <value-of select="$path" />
    </otherwise>
  </choose>
</template>


<!--
  Resolve relative package imports

  This situation arises when a source package (src) imports another
  package (src-import) that has a reference to an external symbol
  (sub-import).

  See /doc/notes/path-processing for an illustration.
-->
<function name="preproc:resolve-relative-import" as="xs:string">
  <param name="src-root"   as="xs:string" />
  <param name="src-name"   as="xs:string" />
  <param name="src-import" as="xs:string" />
  <param name="sub-import" as="xs:string" />

  <variable name="src-prefix" as="xs:string"
            select="concat( $src-root, '/', $src-name )" />

  <!-- Step 1: resolve relative paths from sub-import -->
  <variable name="src-import-dir" as="xs:string"
            select="preproc:get-path( $src-import )" />
  <variable name="sub-concat" as="xs:string"
            select="if ( $src-import-dir ) then
                        preproc:resolve-path(
                          concat( $src-import-dir, '/', $sub-import ) )
                      else
                        $sub-import" />

  <!-- Step 2: remove package name prefix, if present -->
  <variable name="src-name-base" as="xs:string"
            select="preproc:get-path( $src-name )" />
  <variable name="src-prefix" as="xs:string"
            select="concat( $src-root, $src-name-base, '/' )" />
  <variable name="suffix" as="xs:string"
            select="substring-after( $sub-concat, $src-prefix )" />

  <sequence select="if ( $suffix ) then $suffix else $sub-concat" />
</function>

</stylesheet>
