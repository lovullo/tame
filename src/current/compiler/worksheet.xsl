<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles JSON worksheet representation

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:_w="http://www.lovullo.com/rater/worksheet/_priv"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
  xmlns:wc="http://www.lovullo.com/rater/worksheet/compiler"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:util="http://www.lovullo.com/util">

<variable name="wc:lc" select="'abcdefghijklmnopqrstuvwxyz'" />
<variable name="wc:uc" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

<!-- lexemes to be converted to a human-readable format -->
<!-- TODO: move this into an external file so it may be easily configurable -->
<variable name="wc:hlex">
  <!-- prefix to suffix -->
  <wc:lex prefix="prem" to="Premium" />
  <wc:lex prefix="rate" to="Rate" />
  <wc:lex prefix="credit" to="Credit" />
  <wc:lex prefix="surcharge" to="Surcharge" />

  <wc:lex str="gl" to="GL" />
  <wc:lex str="prop" to="Property" />
  <wc:lex str="equip" to="Equipment" />
  <wc:lex str="adjust" to="Adjustment" />
  <wc:lex str="adj" to="Adjustment" />
  <wc:lex str="ded" to="Deductible" />
  <wc:lex str="dw" to="Dwelling" />
  <wc:lex str="fam" to="Family" />
  <wc:lex str="tiv" to="TIV" />

  <!-- *Each is used for generators -->
  <wc:lex str="each" to="" />
</variable>


<!-- we expect that the worksheet will have been preprocessed into the rater
     document -->
<template match="w:worksheet" mode="w:compile" priority="10">
  <param name="corder" />

  <variable name="displays" as="element( w:display )*"
                select="w:display" />

  <variable name="package" as="element( lv:package )"
                select="_w:load-package( @package, . )" />

  <variable name="syms" as="element( preproc:sym )*"
                select="_w:filter-needed-symbols(
                          _w:load-symbols( $package ),
                          $displays )" />

  <lv:package __rootpath="{$__relroot}"
              lvmc:type="worksheet">
    <!-- we provide one special symbol -->
    <preproc:symtable>
      <preproc:sym name="___worksheet"
                   type="worksheet" />
    </preproc:symtable>

    <!-- TODO -->
    <preproc:sym-deps>
      <preproc:sym-dep name="___worksheet" />
    </preproc:sym-deps>

    <copy-of select="node()" />

    <preproc:fragments>
      <preproc:fragment id="___worksheet">
        <text>rater.worksheet = </text>

        <call-template name="util:json">
          <with-param name="obj">
            <for-each select="$displays">
              <sequence select="_w:compile-display( ., $syms )" />
            </for-each>

            <variable name="yield" as="element( lv:rate )?"
                          select="$package/lv:rate[ @yields = '___yield' ]" />

            <!-- always include yield -->
            <if test="$yield">
              <util:value>
                <call-template name="util:json">
                  <with-param name="id" select="'yield'" />
                  <with-param name="array">
                    <util:value>
                      <call-template name="util:json">
                        <with-param name="value" select="'Yields'" />
                      </call-template>
                    </util:value>

                    <util:value>
                      <apply-templates mode="wc:compile"
                                           select="$yield/c:*" />
                    </util:value>
                  </with-param>
                </call-template>
              </util:value>
            </if>
          </with-param>
        </call-template>

        <text>;</text>
      </preproc:fragment>
    </preproc:fragments>
  </lv:package>
</template>


<function name="_w:compile-display" as="element( util:value )">
  <param name="display" as="element( w:display )" />
  <param name="syms"    as="element( preproc:sym )*" />

  <variable name="name" as="xs:string"
                select="$display/@name" />

  <variable name="sym" as="element( preproc:sym )?"
                select="$syms[ @name = $name ]" />

  <!-- terminate on unknown references -->
  <if test="empty( $sym )">
    <message terminate="yes"
                 select="'Reference to unknown symbol:', $name" />
  </if>

  <util:value>
    <call-template name="util:json">
      <with-param name="id" select="$name" />

      <with-param name="array">
        <util:value>
          <call-template name="util:json">
            <with-param name="value">
              <call-template name="wc:var-to-hstr">
                <with-param name="var" select="$name" />
              </call-template>
            </with-param>
          </call-template>
        </util:value>

        <util:value>
          <choose>
            <when test="$display/@collapse = 'true'">
              <sequence select="''" />
            </when>

            <otherwise>
              <variable name="rate-block" as="element( lv:rate )"
                        select="_w:get-src-node( $sym )" />

              <apply-templates mode="wc:compile"
                               select="$rate-block/c:*">
                <with-param name="display" select="$display"
                            tunnel="yes" />
              </apply-templates>
            </otherwise>
          </choose>
        </util:value>

        <util:value>
          <call-template name="util:json">
            <with-param name="value">
              <value-of select="$display/@always" />
            </with-param>
          </call-template>
        </util:value>
      </with-param>
    </call-template>
  </util:value>
</function>


<function name="_w:load-package" as="element( lv:package )">
  <param name="path"    as="xs:string" />
  <param name="context" as="node()" />

  <!-- TODO: function to provide xmlo extension -->
  <variable name="package-uri" as="xs:anyURI"
                select="resolve-uri(
                          concat( $path, '.xmlo' ),
                          base-uri( $context ) )" />

  <if test="not( doc-available( $package-uri ) )">
    <message terminate="yes"
                 select="concat( 'fatal: package ',
                                 $path,
                                 ' not found' )" />
  </if>

  <sequence select="doc( $package-uri )/lv:package" />
</function>


<!-- TODO: some of this logic can be factored out into a common
     library -->
<function name="_w:load-symbols" as="element( preproc:sym )+">
  <param name="package" as="element( lv:package )" />

  <sequence select="$package/preproc:symtable/preproc:sym" />
</function>



<function name="_w:filter-needed-symbols" as="element( preproc:sym )*">
  <param name="syms"     as="element( preproc:sym )*" />
  <param name="displays" as="element( w:display )*" />

  <sequence select="$syms[ @name = $displays/@name ]" />
</function>


<function name="_w:get-src-package" as="element( lv:package )">
  <param name="sym" as="element( preproc:sym )" />

  <variable name="sym-path" as="xs:string?"
                select="$sym/@src" />

  <variable name="context-uri" as="xs:anyURI"
                select="base-uri( $sym )" />

  <!-- TODO: function to provide xmlo extension -->
  <variable name="src-uri" as="xs:anyURI"
                select="if ( $sym-path and not( $sym-path = '' ) ) then
                          resolve-uri(
                            concat( $sym-path, '.xmlo' ),
                            $context-uri )
                        else
                          $context-uri" />

  <if test="not( doc-available( $src-uri ) )">
    <message terminate="yes"
                 select="concat( 'fatal: package ',
                                 $sym-path,
                                 ' not found; required by symbol ',
                                 $sym/@name )" />
  </if>

  <sequence select="doc( $src-uri )/lv:package" />
</function>



<function name="_w:get-src-node" as="element( lv:rate )">
  <param name="sym"  as="element( preproc:sym )" />

  <variable name="package" as="element( lv:package )"
                select="_w:get-src-package( $sym )" />

  <variable name="rate-name" as="xs:string"
                select="if ( $sym/@parent ) then
                          $sym/@parent
                        else
                          $sym/@name" />

  <sequence select="$package/lv:rate[ @yields = $rate-name ]" />
</function>


<template match="c:sum[@of='_CMATCH_']" mode="wc:compile" priority="9">
  <apply-templates select="./c:*" mode="wc:compile" />
</template>

<template match="c:sum[@of='_CMATCH_']/c:product[c:value-of[@name='_CMATCH_']]"
  mode="wc:compile" priority="9">

  <!-- ignore the product and continue with the 2nd node -->
  <apply-templates select="./c:*[2]" mode="wc:compile" />
</template>


<template match="c:apply" priority="7" mode="wc:compile">
  <param name="display" as="element( w:display )"
         tunnel="yes" />

  <choose>
    <!-- do not expand -->
    <when test="
        not(
          @name = $display/ancestor::w:worksheet
            /w:expand-function/@name
          or (
            @name = $display/w:expand-function/@name
          )
        )
      ">

      <call-template name="wc:compile-calc">
        <with-param name="nochildren" select="true()" />
        <with-param name="runtime" select="true()" />
      </call-template>
    </when>

    <!-- expand -->
    <otherwise>
      <call-template name="wc:compile-calc" />
    </otherwise>
  </choose>
</template>


<template match="c:value-of[c:index]" mode="wc:compile" priority="5">
  <call-template name="wc:compile-calc">
    <with-param name="nochildren" select="true()" />
    <with-param name="runtime" select="true()" />
  </call-template>
</template>

<!-- we need to take into account constants that are compiled in place (so we
     cannot determine their value by name at runtime) -->
<template match="c:value-of[ @name=//lv:const[ not( * ) ]/@name ]" mode="wc:compile" priority="5">
  <variable name="name" select="@name" />

  <call-template name="wc:compile-calc">
    <with-param name="include-value">
      <value-of select="//lv:const[ @name=$name ]/@value" />
    </with-param>
  </call-template>
</template>


<!--
  Will output JSON of the following structure:

  [ "type", {desc}, [subnodes] ]

  The subnodes are recursively generated in the same format as above.
-->
<template name="wc:compile-calc" match="c:*" mode="wc:compile" priority="4">
  <param name="nochildren" as="xs:boolean" select="false()" />
  <param name="runtime" select="false()" />
  <param name="include-value" />

  <call-template name="util:json">
    <with-param name="array">
      <!-- output node type -->
      <util:value>
        <call-template name="util:json">
          <with-param name="value" select="local-name()" />
        </call-template>
      </util:value>

      <!-- description -->
      <util:value>
        <call-template name="util:json">
          <with-param name="obj">

            <!-- build each attribute into the description -->
            <for-each select="@*">
              <util:value>
                <call-template name="util:json">
                  <with-param name="id" select="local-name()" />
                  <with-param name="value" select="." />
                </call-template>
              </util:value>
            </for-each>

            <!-- certain values should be calculated at runtime -->
            <if test="$runtime = true()">
              <util:value>
                <call-template name="util:json">
                  <with-param name="id"    select="'runtime'" />
                  <with-param name="value" select="'true'" />
                </call-template>
              </util:value>
            </if>

          </with-param>
        </call-template>
      </util:value>

      <!-- children -->
      <util:value>
        <call-template name="util:json">
          <with-param name="array">

            <if test="not( $nochildren = true() )">
              <!-- sub-nodes (recursive) -->
              <for-each select="c:*">
                <util:value>
                  <apply-templates select="." mode="wc:compile" />
                </util:value>
              </for-each>
            </if>

          </with-param>
        </call-template>
      </util:value>

      <!-- optional value (if we can determine compile-time) -->
      <if test="$include-value">
        <util:value>
          <call-template name="util:json">
            <with-param name="value" select="$include-value" />
          </call-template>
        </util:value>
      </if>
    </with-param>
  </call-template>
</template>


<template name="wc:var-to-hstr">
  <param name="var" />

  <!-- string separators (TODO: make configurable) -->
  <variable name="pre" select="substring-before( $var, '4' )" />

  <choose>
    <when test="not( $pre = '' )">
      <!-- before separator -->
      <call-template name="wc:var-to-hstr">
        <with-param name="var" select="$pre" />
      </call-template>

      <text> for </text>

      <!-- after -->
      <call-template name="wc:var-to-hstr">
        <with-param name="var" select="substring-after( $var, '4' )" />
      </call-template>
    </when>

    <!-- no separator; continue -->
    <otherwise>
      <call-template name="wc:_var-to-hstr">
        <with-param name="var" select="$var" />
      </call-template>
    </otherwise>
  </choose>
</template>

<!-- var to human-readable string -->
<template name="wc:_var-to-hstr">
  <param name="var" />

  <!-- start by grabbing the prefix -->
  <variable name="prefix">
    <call-template name="wc:str-until-uc">
      <with-param name="str" select="$var" />
    </call-template>
  </variable>

  <!-- and grab the rest of the string after the prefix -->
  <variable name="remain" select="substring-after( $var, $prefix )" />

  <!-- convert the first char to lowercase so that we do not screw up the uc
       prefix substr on the next call -->
  <variable name="remain-recurse" select="
      concat(
        translate( substring( $remain, 1, 1 ), $wc:uc, $wc:lc ),
        substring( $remain, 2 )
      )
    " />

  <variable name="prelex" select="$wc:hlex//wc:lex[ @prefix=$prefix ]" />

  <choose>
    <when test="$prelex">
      <if test="not( $remain-recurse = '' )">
        <call-template name="wc:var-to-hstr">
          <with-param name="var" select="$remain-recurse" />
        </call-template>
      </if>

      <text> </text>
      <value-of select="$prelex/@to" />
    </when>

    <!-- no knowledge of prefix; output it and then recurse -->
    <otherwise>
      <variable name="strlex" select="$wc:hlex//wc:lex[ @str=$prefix ]" />

      <choose>
        <!-- if we recognize this text as a lexeme to be replaced, then do so
             -->
        <when test="$strlex">
          <value-of select="$strlex/@to" />
        </when>

        <!-- just output the text as-is -->
        <otherwise>
          <!-- ucfirst -->
          <value-of select="
              concat(
                translate( substring( $prefix, 1, 1 ), $wc:lc, $wc:uc ),
                substring( $prefix, 2 )
              )
            " />
        </otherwise>
      </choose>

      <if test="not( $remain-recurse = '' )">
        <text> </text>

        <call-template name="wc:var-to-hstr">
          <with-param name="var" select="$remain-recurse" />
        </call-template>
      </if>
    </otherwise>
  </choose>
</template>

<!-- get string prefix until reaching a upper-case char -->
<template name="wc:str-until-uc">
  <param name="str" />

  <variable name="char" select="substring( $str, 1, 1 )" />

  <choose>
    <when test="$str = ''">
      <!-- done; nothing else to do -->
    </when>

    <!-- did we find an upper-case char? -->
    <when test="translate( $char, $wc:uc, '' ) = ''">
      <!-- we're done; do nothing and do not output -->
    </when>

    <otherwise>
      <!-- output the char and recurse -->
      <value-of select="$char" />

      <call-template name="wc:str-until-uc">
        <with-param name="str" select="substring( $str, 2 )" />
      </call-template>
    </otherwise>
  </choose>
</template>

</stylesheet>
