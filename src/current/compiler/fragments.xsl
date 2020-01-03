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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:map="http://www.w3.org/2005/xpath-functions/map"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">


<template mode="preproc:compile-fragments" priority="9"
          match="lv:package">
  <copy>
    <sequence select="@*" />

    <apply-templates select="*" mode="preproc:compile-fragments-root" />
  </copy>
</template>


<template mode="preproc:compile-fragments-root" priority="1"
          match="node()">
  <sequence select="." />
</template>


<!-- Position fragments directly after dependencies.  This allows TAMER to
    halt processing early on, rather than having to read the rest of the
    file (fragments used to be placed at the end). -->
<template mode="preproc:compile-fragments-root" priority="5"
          match="preproc:sym-deps">
  <sequence select="." />

  <variable name="package" as="element( lv:package )"
            select="parent::lv:package" />

  <variable name="symtable-map" as="map( xs:string, element( preproc:sym ) )"
            select="map:merge(
                      for $sym in $package/preproc:symtable/preproc:sym
                        return map{ string( $sym/@name ) : $sym } )" />

  <preproc:fragments>
    <apply-templates select="$package/*"
                     mode="preproc:compile-fragments">
      <with-param name="symtable-map" select="$symtable-map"
                  tunnel="yes" />
    </apply-templates>
  </preproc:fragments>
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:rate">
  <preproc:fragment id="{@yields}">
    <apply-templates mode="compile" select="." />
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:classify">
  <preproc:fragment id=":class:{@as}">
    <apply-templates select="." mode="compile" />
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:function">
  <preproc:fragment id="{@name}">
    <apply-templates select="." mode="compile" />
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>

<template mode="preproc:compile-fragments" priority="7"
          match="lv:function/lv:param">
  <!-- ignore -->
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:param">
  <variable name="name" select="@name" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <preproc:fragment id="{@name}">
    <apply-templates select="." mode="compile" />
  </preproc:fragment>
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:typedef">
  <preproc:fragment id="{@name}">
    <apply-templates mode="compile" select="." />
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:const|lv:item">
  <preproc:fragment id="{@name}">
    <apply-templates mode="compile" select=".">
      <with-param name="as-const" select="true()" />
    </apply-templates>
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>


<template mode="preproc:compile-fragments" priority="5"
          match="lv:meta/lv:prop">
  <preproc:fragment id=":meta:{@name}">
    <apply-templates mode="compile" select="." />
  </preproc:fragment>

  <apply-templates mode="preproc:compile-fragments" />
</template>


<template mode="preproc:compile-fragments" priority="7"
          match="lv:template">
  <!-- don't process template bodies, since they are not yet expanded -->
</template>


<template mode="preproc:compile-fragments" priority="7"
          match="preproc:*">
  <!-- we don't compile this stuff -->
</template>


<template mode="preproc:compile-fragments" priority="1"
          match="node()">
  <apply-templates mode="preproc:compile-fragments" />
</template>

</stylesheet>
