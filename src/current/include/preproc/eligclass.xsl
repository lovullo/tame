<?xml version="1.0" encoding="utf-8"?>
<!--
  Package eligibility class generation

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

  Here, the term "eligibility" means whether the package is eligible to be used
  in a result set basead on the values of its params within their respective
  domains and other factors such as the results of terminating classifications
  and the eligibility of imported packages.

  The goal of the eligibility classification is to create a cascading failure in
  the event of bad data.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc">



<!--
  Trigger eligibility class generation
-->
<template match="lv:package[ not( @preproc:elig-class-yields ) ]"
              as="element( lv:package )"
              priority="5"
              mode="preproc:expand-elig-class">
  <param name="orig-root" as="element( lv:package )" />

  <variable name="elig-class" as="element( lv:classify )">
    <apply-templates select="." mode="preproc:gen-elig-class">
      <with-param name="orig-root" select="$orig-root" />
    </apply-templates>
  </variable>

  <copy>
    <sequence select="@*" />

    <attribute name="preproc:elig-class"
      select="$elig-class/@as" />

    <attribute name="preproc:elig-class-yields"
      select="$elig-class/@yields" />

    <sequence select="$elig-class" />
    <apply-templates mode="preproc:macros" />
  </copy>
</template>


<template match="lv:package" as="element( lv:package )"
              priority="1"
  mode="preproc:expand-elig-class">

  <!-- already processed -->
  <sequence select="." />
</template>



<!--
  Generate eligibility classification asserting all data integrity aspects of a
  package

  The eligibility classification will yield a scalar.
-->
<template match="lv:package" as="element( lv:classify )"
              mode="preproc:gen-elig-class">
  <param name="orig-root" as="element( lv:package )" />

  <message>[preproc/eligclass] generating eligibility class</message>


  <!-- class-ify name -->
  <variable name="as" as="xs:string"
                select="preproc:gen-elig-class-name( @name )" />
  <variable name="yields" as="xs:string"
                select="preproc:gen-elig-class-yields( @name )" />


  <lv:classify as="{$as}" yields="{$yields}"
    desc="{@name} package is eligible">

    <!-- each of our imported packages' elig classes must be truthful -->
    <apply-templates mode="preproc:gen-elig-class-matches"
      select="lv:import">

      <with-param name="orig-root" select="$orig-root" />
    </apply-templates>

    <!-- param values must be within their domain -->
    <!-- XXX: does not work when param is undefined due to no mapping
    <apply-templates mode="preproc:gen-elig-param-class"
      select="lv:param" />
    -->

    <!-- terminating classifications must not have matched -->
    <apply-templates mode="preproc:gen-elig-term-class"
      select="preproc:symtable/preproc:sym[ @type='class' ]" />
  </lv:classify>
</template>


<!--
  Generate eligibility classification name for package
-->
<function name="preproc:gen-elig-class-name"
              as="xs:string">
  <param name="name" />

  <sequence select="
      concat( '--elig-',
        translate(
          translate( $name, '.', '' ),
          '/', '-'
        )
      )
      " />
</function>


<!--
  Generate eligibility result scalar name for package
-->
<function name="preproc:gen-elig-class-yields"
              as="xs:string">
  <param name="name" />

  <sequence select="
      concat(
        'isElig',
        translate(
          translate( $name, '.', '' ),
          '/-', '' ) )" />
</function>


<!--
  Generate matches on eligibility of imported packages

  For each imported package, its eligibility classification must be true.
-->
<template match="lv:import[ @package ]"
              as="element( lv:match )?"
              priority="5"
              mode="preproc:gen-elig-class-matches">
  <param name="orig-root" as="element( lv:package )" />

  <!-- FIXME: path may not yet be resolved due to preprocessing order -->
  <variable name="pkg-path" as="xs:string">
    <call-template name="__apply-relroot">
      <with-param name="path" select="@package" />
    </call-template>
  </variable>

  <variable name="pkg" as="element( lv:package )"
                select="document( concat( $pkg-path, '.xmlo' ),
                                  $__entry-root )
                          /lv:package" />

  <if test="not( $pkg )">
    <message terminate="yes">
      <text>[preproc/eligclass] error: could not load `</text>
        <value-of select="$pkg-path" />
      <text>' object file</text>
    </message>
  </if>

  <variable name="chk" as="xs:string?"
                select="$pkg/@preproc:elig-class-yields" />

  <choose>
    <when test="not( $chk ) or ( $chk = '' )">
      <!-- TODO: make this an error once we make maps part of the
           conventional build process -->
      <message>
        <text>[preproc/eligclass] internal: empty eligibility </text>
        <text>class for `</text>
          <value-of select="$pkg/@name" />
        <text>'; skipping</text>
      </message>
    </when>

    <otherwise>
      <!-- use eligibility class as stated by the package -->
      <lv:match on="{$chk}" value="TRUE" />
    </otherwise>
  </choose>
</template>


<template match="lv:import" priority="1"
  mode="preproc:gen-elig-class-matches">

  <!-- do nothing -->
</template>


<!--
  Param values must be within their domain

  This is a trivial operation.
-->
<template match="lv:param"
              as="element( lv:any )"
              mode="preproc:gen-elig-param-class" priority="5">

  <lv:any>
    <lv:match on="{@name}" anyOf="{@type}" />

    <!-- TODO: defaults should always be within the domain! -->
    <if test="@default">
      <lv:match on="{@name}" anyOf="empty" />
    </if>
  </lv:any>
</template>



<!--
  Terminating classification dependencies

  All terminiating classifications defined in the package must yield false
  for the package to be eligible.
-->
<template match="preproc:sym[
                       not( @src )
                       and not( @pollute='true' )
                       and @type='class'
                       and @terminate='true'
                     ]"
              as="element( lv:match )"
              priority="5"
              mode="preproc:gen-elig-term-class">

  <lv:match on="{@yields}" value="FALSE" />
</template>


<template match="preproc:sym" priority="1"
  mode="preproc:gen-elig-term-class">

  <!-- do nothing -->
</template>

</stylesheet>

