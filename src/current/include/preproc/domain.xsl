<?xml version="1.0" encoding="utf-8"?>
<!--
  Compiles domains from typedefs

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

  The ultime goal is to implement typedefs as macros and move to a generic
  domain system that is much more powerful.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc">


<!--
  Base typedefs (internal) are declarations only

  TODO: We still want a domain generated.
-->
<template match="lv:typedef[ lv:base-type ]"
  mode="preproc:mkdomain" priority="9">
</template>


<!--
  Union typedefs

  This generates not only the contained typedefs, but also a denormalized
  domain containing the union of all the subdomain elements. This is
  intended to improve lookup performance and reduce algorithmic complexity.
-->
<template match="
    lv:typedef[
      lv:union
    ]
  "
  mode="preproc:mkdomain" priority="5">

  <!-- generate all contained domains -->
  <variable name="subdomains">
    <variable name="union-types" select="
      lv:union/lv:typedef" />

    <!-- if a union is empty, then somebody probably made an oopsie or wrote
    a defective/deficient template -->
    <if test="count( $union-types ) = 0">
      <message>
        <text>warning: union `</text>
          <value-of select="@name" />
        <text>' has no subdomains; something is probably wrong</text>
      </message>
    </if>

    <apply-templates mode="preproc:mkdomain"
      select="$union-types" />
  </variable>

  <!-- provide a denormalized domain for performance and to reduce
       algorithmic complexity-->
  <call-template name="preproc:mkdomain-union">
    <with-param name="name" select="@name" />
    <with-param name="subdomains" select="$subdomains" />
  </call-template>

  <copy-of select="$subdomains" />
</template>


<!--
  Enumerated typedefs
-->
<template match="
    lv:typedef[
      lv:enum
    ]
  "
  mode="preproc:mkdomain" priority="5">

  <lv:domain name="{@name}">
    <variable name="items" select="lv:enum/lv:item" />

    <!-- if a typedef is empty, then somebody probably made an oopsie or
         wrote a defective/deficient template -->
    <if test="count( $items ) = 0">
      <message>
        <text>warning: typedef `</text>
          <value-of select="@name" />
        <text>' is empty; something is probably wrong</text>
      </message>
    </if>

    <apply-templates mode="preproc:mkdomain"
      select="$items" />
  </lv:domain>
</template>


<!--
  Prohibit mixing explicit and auto-generated values

  For the time being at least.
-->
<template match="
  lv:typedef[
    lv:enum/lv:item[ @value ]
    and lv:enum/lv:item[ not( @value ) ]
  ]"
  mode="preproc:mkdomain" priority="2">

  <message terminate="yes">
    <text>error: typedef `</text>
      <value-of select="@name" />
    <text>' must not contain both @value and non-@value items</text>
  </message>
</template>


<!--
  Unsupported typedef

  Well, we know that it is a typedef, but its format is unknown. This
  wouldn't be surprising, since it is presently very limited.
-->
<template match="lv:typedef"
  mode="preproc:mkdomain" priority="2">

  <message terminate="yes">
    <text>error: malformed typedef `</text>
      <value-of select="@name" />
    <text>'</text>
  </message>
</template>



<!--
  Generate a denormalized domain consisting of the union of its subdomains'
  elements

  As this is a union, duplicate elements will be removed; the user will not
  be notified of this fact, as this allows domains to overlap in order to
  interpret the same data in different manners.
-->
<template name="preproc:mkdomain-union">
  <param name="name" />
  <param name="subdomains" />

  <variable name="union">
    <preproc:elements>
      <copy-of select="$subdomains/lv:domain/lv:element" />
    </preproc:elements>
  </variable>

  <!-- remove duplicate values (yes, this will take the first description if
       there are duplicates; whatever, for now) -->
  <lv:domain name="{@name}">
    <copy-of select="
      $union/preproc:elements/lv:element[
        not( @value = preceding-sibling::lv:element/@value )
      ]" />
  </lv:domain>
</template>


<!--
  Enumerated items without values require calculation

  Note the above validation that ensures that our value generation is
  sufficient.
-->
<template match="lv:enum/lv:item[ not( @value ) ]"
  mode="preproc:mkdomain" priority="5">

  <variable name="augmented">
    <copy>
      <attribute name="value" select="position()" />
    </copy>
  </variable>

  <!-- re-process using an augmented item with the value calculated -->
  <apply-templates mode="preproc:mkdomain"
    select="$augmented/lv:item" />
</template>


<!--
  Convert typedef item into a domain element

  This is a straightforward rename with sanity checking. Note that the
  element may have an empty description.

  We do not care about the name, since we use that to generate constants
  elsewhere.
-->
<template match="lv:item"
  mode="preproc:mkdomain" priority="4">

  <!-- previous templates should have prevented this, but just in case -->
  <if test="not( @value )">
    <message terminate="yes">
      <text>internal error: preproc:mkdomain on non-value item: </text>
      <copy-of select="." />
    </message>
  </if>

  <lv:element value="{@value}" desc="{@desc}" />
</template>


<!--
  Unexpected node; terminate
-->
<template match="*"
  mode="preproc:mkdomain" priority="1">

  <message terminate="yes">
    <text>internal error: unknown domain source: </text>
    <copy-of select="." />
  </message>
</template>

</stylesheet>

