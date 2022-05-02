<?xml version="1.0" encoding="utf-8"?>
<!--
  Compiles map fragments to produce a map from source data to a destination

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

  The source fields will be validated at compile-time to ensure that they exist;
  destination fields should be checked by the compiler and/or linker. The linker
  is responsible for assembling the fragments into a working map function.

  When linking, the special head and tail fragments of the topmost map should be
  used (that is, if A includes B and C, use A).

  TODO: Just generate a normal package and use the package system;
  this duplicates a lot of logic, and does so piecemeal and poorly.

  XXX: This is tightly coupled with the Program UI; refactor to support any type
  of source.
-->
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"

  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvp="http://www.lovullo.com">


<param name="map-noterminate" select="'no'" />

<!--
  Turn on/off unused param checks

  This is useful for, say, the global classifier, where a param may end up not
  being used if it's used in external classifications.
-->
<param name="unused-param-check" select="'true'" />

<!--
  Generate a function that maps a set of inputs to a set of outputs
-->
<template match="lvm:program-map" mode="lvmc:compile" priority="8">
  <param name="rater" />

  <variable name="program-ui" select="
      document( concat( @src, '.xml' ), . )/lvp:program
    " />

  <variable name="map" select="." />

  <variable name="vresult">
    <choose>
      <when test="$program-ui">
        <apply-templates select="." mode="lvmc:validate-ui">
          <with-param name="ui" select="$program-ui" />
        </apply-templates>
      </when>

      <otherwise>
        <message terminate="yes">
          <text>fatal: program UI source XML not found</text>
        </message>
      </otherwise>
    </choose>
  </variable>

  <if test="
      $vresult/lvmc:terminate
      and $map-noterminate = 'no'
    ">
    <message terminate="yes">!!! Terminating due to errors.</message>
  </if>

  <!-- we need to use an lv-namespaced node so that we are recognized
       consistently with the rest of the system -->
  <variable name="pkg">
    <lv:package name="{$__srcpkg}" lvmc:type="map">
      <!-- XXX: copied from expand.xsl! -->
      <attribute name="name" select="$__srcpkg" />
      <attribute name="__rootpath" select="$__relroot" />
      <attribute name="preproc:name" select="$__srcpkg" />

      <!-- initial symbol table; full table will be generated below -->
      <call-template name="lvmc:stub-symtable">
        <with-param name="type-prefix" select="'map'" />
      </call-template>

      <!-- copy source nodes -->
      <apply-templates mode="preproc:expand" select="node()" />
    </lv:package>
  </variable>

  <!-- process symbol table -->
  <variable name="pkg-with-symtable" as="element( lv:package )">
    <call-template name="preproc:gen-deps">
      <with-param name="pkg" as="element( lv:package )">
        <apply-templates select="$pkg" mode="preproc:sym-discover">
          <with-param name="orig-root" select="." />
        </apply-templates>
      </with-param>
    </call-template>
  </variable>

  <!-- final result with compiled fragments -->
  <lv:package>
    <sequence select="$pkg-with-symtable/@*,
                      $pkg-with-symtable/preproc:sym-deps/preceding-sibling::*,
                      $pkg-with-symtable/preproc:sym-deps" />

    <preproc:fragments>
      <!-- special fragment to be output as the head -->
      <preproc:fragment id=":map:___head">
        <!-- use a callback just in case we need to make portions of this async in the
             future -->
        <text>function( input, callback ) {</text>
        <text>var output = {};</text>
      </preproc:fragment>

      <!-- compile mapped -->
      <apply-templates select="./lvm:*" mode="lvmc:compile">
        <with-param name="rater"    select="$rater" />
        <with-param name="type"     select="'map'" />
        <with-param name="symtable" select="$pkg-with-symtable/preproc:symtable"
                    tunnel="yes"/>
      </apply-templates>

      <!-- special fragment to be output as the foot -->
      <preproc:fragment id=":map:___tail">
        <text>callback(output);</text>
        <text>};</text>
      </preproc:fragment>
    </preproc:fragments>

    <sequence select="$pkg-with-symtable/preproc:sym-deps/following-sibling::*" />
  </lv:package>
</template>


<!--
  Generate a function that maps a set of rater outputs

  TODO: This is essentailly the same as the input map; refactor.
-->
<template match="lvm:return-map" mode="lvmc:compile" priority="8">
  <param name="rater" />

  <!-- we don't have use for this right now, but it's required
       by other parts of this system -->
  <variable name="dummy-symtable" as="element( preproc:symtable )">
    <preproc:symtable lvmc:sym-ignore="true" />
  </variable>

  <variable name="pkg">
    <lv:package name="{$__srcpkg}" lvmc:type="retmap">
      <!-- XXX: copied from expand.xsl! -->
      <attribute name="name" select="$__srcpkg" />
      <attribute name="__rootpath" select="$__relroot" />
      <attribute name="preproc:name" select="$__srcpkg" />

      <!-- initial symbol table; full table will be generated below -->
      <call-template name="lvmc:stub-symtable">
        <with-param name="type-prefix" select="'retmap'" />
      </call-template>

      <!-- copy source nodes -->
      <apply-templates mode="preproc:expand" select="node()" />
    </lv:package>
  </variable>

  <!-- process symbol table -->
  <variable name="pkg-with-symtable" as="element( lv:package )">
    <call-template name="preproc:gen-deps">
      <with-param name="pkg" as="element( lv:package )">
        <apply-templates select="$pkg" mode="preproc:sym-discover">
          <with-param name="orig-root" select="." />
        </apply-templates>
      </with-param>
    </call-template>
  </variable>

  <!-- final result with compiled fragments -->
  <lv:package>
    <sequence select="$pkg-with-symtable/@*,
                      $pkg-with-symtable/preproc:sym-deps/preceding-sibling::*,
                      $pkg-with-symtable/preproc:sym-deps" />

    <preproc:fragments>
      <!-- special fragment to be output as the head -->
      <preproc:fragment id=":retmap:___head">
        <!-- use a callback just in case we need to make portions of this async in the
             future -->
        <text>function( input, callback ) {</text>
        <text>var output = {};</text>
      </preproc:fragment>

      <!-- compile mapped -->
      <apply-templates select="./lvm:*" mode="lvmc:compile">
        <with-param name="rater"    select="$rater" />
        <with-param name="type"     select="'retmap'" />
        <with-param name="symtable" select="$pkg-with-symtable/preproc:symtable"
                    tunnel="yes"/>
      </apply-templates>

      <!-- special fragment to be output as the foot -->
      <preproc:fragment id=":retmap:___tail">
        <text>callback(output);</text>
        <text>};</text>
      </preproc:fragment>
    </preproc:fragments>

    <sequence select="$pkg-with-symtable/preproc:sym-deps/following-sibling::*" />
  </lv:package>
</template>


<template name="lvmc:stub-symtable">
  <param name="type-prefix" select="'map'" />

  <preproc:symtable>
    <!-- purposely non-polluting.  @ignore-dup is intended to be
         temporary until static generation of these names is resolved;
         this will not cause problems, since the code is always the
         same (future bug pending!) -->
    <preproc:sym name=":{$type-prefix}:___head"
                 type="{$type-prefix}:head"
                 pollute="true"
                 ignore-dup="true"
                 no-deps="true" />
    <preproc:sym name=":{$type-prefix}:___tail"
                 type="{$type-prefix}:tail"
                 ignore-dup="true"
                 no-deps="true" />
  </preproc:symtable>
</template>


<template name="lvmc:mapsym">
  <param name="name" />
  <param name="from" />
  <param name="type-prefix" select="/lv:package/@lvmc:type" />
  <param name="no-deps" as="xs:boolean" select="false()" />

  <!-- allow mappings to be overridden after import, which allows defaults
       to be set and then overridden -->
  <preproc:sym name=":{$type-prefix}:{$name}" virtual="true"
               type="{$type-prefix}" pollute="true">

    <!-- for consistency and cleanliness, only copy over if set -->
    <if test="@override='true'">
      <copy-of select="@override" />
    </if>

    <copy-of select="@affects-eligibility" />

    <!-- only copy from data if present -->
    <if test="$from">
      <copy-of select="$from" />
    </if>

    <if test="$no-deps">
      <attribute name="no-deps" select="'true'" />
    </if>
  </preproc:sym>
</template>


<!--
  Get name of function associated with mapping method

  Note that this expands to an empty string if no processing is
  needed.  Since functions are applied using parenthesis, this has the
  effect of creating either a function application or a parenthesized
  expression, the latter of which simply returns the expression untouched.
-->
<function name="lvmc:get-method-func" as="xs:string">
  <param name="method" as="xs:string?" />

  <choose>
    <!-- default -->
    <when test="not( $method ) or ( $method = 'translate' )">
      <sequence select="''" />
    </when>

    <when test="$method = ( 'hash', 'uppercase' )">
      <sequence select="concat( 'map_method_', $method )" />
    </when>

    <otherwise>
      <message terminate="yes"
               select="concat( 'error: unknown map method `',
                               $method, '''' )" />
    </otherwise>
  </choose>
</function>


<!--
  Directly map an input to the output
-->
<template match="lvm:pass" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )"
         tunnel="yes" />
  <param name="type"     as="xs:string" />

  <preproc:fragment id=":{$type}:{@name}">
    <text>output['</text>
      <value-of select="@name" />
    <text>']=</text>
      <call-template name="lvmc:gen-input-default">
        <with-param name="sym"
                    select="lvmc:get-symbol( $symtable, $type, @name, @name )" />
        <with-param name="from" select="@name" />
      </call-template>
    <text>;</text>

    <!-- newline to make output reading and debugging easier -->
    <text>&#10;</text>
  </preproc:fragment>
</template>

<template match="lvm:pass" mode="preproc:symtable" priority="5">
  <call-template name="lvmc:mapsym">
    <with-param name="name" select="@name" />
    <with-param name="from">
      <preproc:from name="{@name}" />
    </with-param>
  </call-template>
</template>


<template match="lvm:pass" mode="preproc:depgen" priority="5">
  <preproc:sym-dep name=":map:{@name}" />
</template>

<template match="lvm:pass[ root(.)/@lvmc:type = 'retmap' ]"
          mode="preproc:depgen" priority="6">
  <preproc:sym-dep name=":retmap:{@name}">
    <preproc:sym-ref name="{@name}" lax="true" />
  </preproc:sym-dep>
</template>



<!--
  Maps an input to an output of a different name
-->
<template match="lvm:map[ @from ]" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )"
         tunnel="yes" />
  <param name="type" />

  <!-- if src and dest are identical, then it may as well be lvm:pass -->
  <if test="@to = @from">
    <message>
      <text>[map] notice: `</text>
        <value-of select="@to" />
      <!-- TODO: get namespace prefix from name() -->
      <text>' has a destination of the same name; use lvm:pass instead</text>
    </message>
  </if>

  <preproc:fragment id=":{$type}:{@to}">
    <text>output['</text>
    <value-of select="lvmc:escape-string( @to )" />
    <text>']=</text>
    <call-template name="lvmc:gen-input-default">
      <with-param name="sym"
                  select="lvmc:get-symbol( $symtable, $type, @to, @from )" />
      <with-param name="from" select="@from" />
    </call-template>
    <text>;</text>

    <!-- newline to make output reading and debugging easier -->
    <text>&#10;</text>
  </preproc:fragment>
</template>

<template name="lvmc:sym-from" match="lvm:map[ @from ]" mode="preproc:symtable" priority="5">
  <call-template name="lvmc:mapsym">
    <with-param name="name" select="@to" />
    <with-param name="from">
      <preproc:from name="{@from}" />
    </with-param>
  </call-template>
</template>

<template match="lvm:map[ @from
                          and root(.)/@lvmc:type = 'map' ]"
              mode="preproc:depgen" priority="5">
  <preproc:sym-dep name=":map:{@to}" />
</template>

<template match="lvm:map[ @from
                          and root(.)/@lvmc:type = 'retmap' ]"
              mode="preproc:depgen" priority="6">
  <preproc:sym-dep name=":retmap:{@to}">
    <preproc:sym-ref name="{@from}" lax="true" />
  </preproc:sym-dep>
</template>

<template match="lvm:map[ @from ]" mode="preproc:depgen" priority="4">
  <message terminate="yes"
               select="'internal error: unhandled lvm:map: ', ." />
</template>


<!--
  Triggers dependency generation on the source document, which contains far more
  information than our symbol table
-->
<template match="preproc:sym[ @type='map' ]" mode="preproc:depgen" priority="6">
  <variable name="name" select="substring-after( @name, ':map:' )" />
  <variable name="pkg" as="element( lv:package )"
                select="root(.)" />

  <apply-templates mode="preproc:depgen"
    select="$pkg/lvm:*[ @name=$name or @to=$name ]" />
</template>


<!-- FIXME: this is a cluster -->
<template match="preproc:sym[ @type='retmap' ]" mode="preproc:depgen" priority="6">
  <variable name="from" as="element( preproc:from )*"
                select="preproc:from" />

  <if test="$from">
    <variable name="src-name" as="xs:string"
                  select="substring-after( @name, ':retmap:' )" />

    <variable name="name" as="xs:string+"
                  select="$from/@name" />

    <variable name="pkg" as="element( lv:package )"
                  select="root(.)" />

    <variable name="src-node" as="element()+"
                  select="$pkg/lvm:*[ @name = $src-name
                                      or @to = $src-name ]" />

    <if test="count( $src-node ) gt count( $from )">
      <message terminate="yes"
                   select="'error: duplicate source identifier: ',
                           $src-name" />
    </if>

    <apply-templates mode="preproc:depgen"
                         select="$src-node" />
  </if>
</template>


<!--
  These guys have no dependencies; handle them to prevent depgen errors
-->
<template match="preproc:sym[ @type='map:head' or @type='map:tail' ]" mode="preproc:depgen" priority="2">
  <!-- do nothing -->
</template>
<template match="preproc:sym[ @type='retmap:head' or @type='retmap:tail' ]" mode="preproc:depgen" priority="2">
  <!-- do nothing -->
</template>


<!--
  Attempt to locate the expected symbol, and blow up otherwise.

  TODO: The retmap distinction muddies this; refactor to be agnostic
  (onus on caller perhaps).
 -->
<function name="lvmc:get-symbol" as="element( preproc:sym )?">
  <param name="symtable" as="element( preproc:symtable )" />
  <param name="type"     as="xs:string" />
  <param name="to"       as="xs:string" />
  <param name="from"     as="xs:string?" />

  <variable name="symname" as="xs:string?"
            select="if ( $type = 'retmap' ) then $from else $to" />

  <variable name="sym" as="element( preproc:sym )?"
            select="$symtable/preproc:sym[ @name=$symname and @src ]" />

  <!-- for error message display -->
  <variable name="srcdest" as="xs:string"
            select="if ( $type = 'retmap' ) then 'source' else 'destination'" />

  <if test="$symname and not( $sym ) and not( $symtable/@lvmc:sym-ignore )">
    <message terminate="yes"
                 select="concat(
                           'error: unknown ', $srcdest, ' identifier `',
                           string( $symname ),
                           ''' (did you import the package?)' )" />
  </if>

  <sequence select="$sym" />
</function>


<!--
  Generate a direct input mapping or, if a default exists for the field, use the
  default if the input is an empty string.
-->
<template name="lvmc:gen-input-default">
  <param name="sym" as="element( preproc:sym )" />
  <!-- use one or the other; latter takes precedence -->
  <param name="from" />
  <param name="from-str" />
  <param name="dim" select="number( $sym/@dim )" />

  <variable name="from-var">
    <choose>
      <when test="$from-str">
        <value-of select="$from-str" />
      </when>

      <otherwise>
        <sequence select="lvmc:gen-input( 'input', $from )" />
      </otherwise>
    </choose>
  </variable>

  <choose>
    <when test="$sym and $sym/@default and not( $sym/@default = '' )">
      <text>set_defaults(</text>
        <value-of select="$from-var" />
      <text>,'</text>
        <value-of select="lvmc:escape-string( $sym/@default )" />
      <text>',</text>
        <value-of select="$dim" />
      <text>)</text>
    </when>

    <otherwise>
      <value-of select="$from-var" />
    </otherwise>
  </choose>
</template>


<!--
  Maps a static value to the output
-->
<template match="lvm:map[ @value ]" mode="lvmc:compile" priority="5">
  <param name="type" as="xs:string" />

  <preproc:fragment id=":{$type}:{@to}">
    <text>output['</text>
      <value-of select="lvmc:escape-string( @to )" />
    <text>']='</text>
      <value-of select="normalize-space( @value )" />
    <text>';</text>

    <!-- newline to make output reading and debugging easier -->
    <text>&#10;</text>
  </preproc:fragment>
</template>

<template match="lvm:map[ @value ]" mode="preproc:symtable" priority="5">
  <call-template name="lvmc:mapsym">
    <with-param name="name" select="@to" />
    <with-param name="no-deps" select="true()" />
  </call-template>
</template>


<template match="lvm:map[*]" mode="lvmc:compile" priority="5">
  <param name="rater" />
  <param name="type" as="xs:string"/>

  <preproc:fragment id=":{$type}:{@to}">
    <text>output['</text>
      <value-of select="@to" />
    <text>']=</text>

      <apply-templates select="./lvm:*" mode="lvmc:compile">
        <with-param name="rater" select="$rater" />
        <with-param name="type"  select="$type" />
      </apply-templates>

    <text>;</text>

    <!-- newline to make output reading and debugging easier -->
    <text>&#10;</text>
  </preproc:fragment>
</template>

<template match="lvm:map[ * ]" mode="preproc:symtable" priority="5">
  <param name="to" select="@to" />

  <call-template name="lvmc:mapsym">
    <with-param name="name" select="$to" />
    <with-param name="from">
      <for-each select=".//lvm:from">
        <preproc:from name="{@name}" />
      </for-each>
    </with-param>
  </call-template>
</template>

<template match="/*[ @lvmc:type='retmap' ]/lvm:map[ * ]" mode="preproc:symtable" priority="6">
  <variable name="to" select="@to" />

  <call-template name="lvmc:mapsym">
    <with-param name="name" select="$to" />
    <with-param name="from">
      <for-each select=".//lvm:from">
        <preproc:from name="{@name}" />
      </for-each>
    </with-param>
  </call-template>
</template>

<template match="lvm:map[ * ]" mode="preproc:depgen" priority="5">
  <preproc:sym-dep name=":map:{@to}" />
</template>

<template match="lvm:map[ *
                     and root(.)/@lvmc:type = 'retmap' ]"
              mode="preproc:depgen" priority="6">
  <preproc:sym-dep name=":retmap:{@to}">
    <for-each select=".//lvm:from">
      <preproc:sym-ref name="{@name}" lax="true" />
    </for-each>
  </preproc:sym-dep>
</template>



<template match="lvm:const" mode="lvmc:compile" priority="5">
  <text>'</text>
    <value-of select="lvmc:escape-string( @value )" />
  <text>'</text>
</template>

<template match="lvm:map//lvm:set[@each]" mode="lvmc:compile" priority="5">
  <text>(function(){</text>
    <text>var ret=[];</text>
    <text>var len=</text>
    <value-of select="lvmc:gen-input( 'input', @each )" />
    <text>.length;</text>

    <text>for(var _i=0;_i&lt;len;_i++){</text>
      <text>var </text>
        <value-of select="@index" />
      <text>=_i;</text>

      <text>ret[_i]=</text>
        <apply-templates select="./lvm:*" mode="lvmc:compile" />
      <text>;</text>
    <text>}</text>

    <text>return ret;</text>
  <text>})()</text>
</template>

<template match="lvm:map//lvm:set[@ignore-empty='true']" mode="lvmc:compile" priority="3">
  <param name="type" as="xs:string"/>

  <text>(function(){</text>
    <text>var ret=[]; var tmp;</text>

    <for-each select="./lvm:*">
      <text>tmp=</text>
        <apply-templates select="." mode="lvmc:compile">
          <with-param name="type" select="$type" />
        </apply-templates>
      <text>;</text>

      <text>if(tmp&amp;&amp;tmp!=='0')ret.push(tmp);</text>
    </for-each>

    <text>return ret;</text>
  <text>})()</text>
</template>

<template match="lvm:map//lvm:set" mode="lvmc:compile" priority="2">
  <param name="type" as="xs:string"/>

  <text>[</text>
    <for-each select="./lvm:*">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <apply-templates select="." mode="lvmc:compile">
        <with-param name="type" select="$type" />
      </apply-templates>
    </for-each>
  <text>]</text>
</template>

<template match="lvm:map//lvm:static" mode="lvmc:compile" priority="5">
  <text>'</text>
    <value-of select="@value" />
  <text>'</text>
</template>


<template match="lvm:map//lvm:from[*]" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )"
         tunnel="yes" />
  <param name="type"     as="xs:string" />

  <variable name="to" select="ancestor::lvm:map/@to" />

  <variable name="nested-depth" as="xs:integer"
            select="count( ancestor::lvm:from )" />

  <variable name="sym" as="element( preproc:sym )?"
            select="lvmc:get-symbol( $symtable, $type, $to, @name )" />

  <!-- Saxon evaluates variables lazily.  Rather than using the
       Saxon-specific @saxon:assign="true" attribute above, we just need to
       use the value.  This conditional cannot be empty, otherwise it'll be
       optimized away. -->
  <if test="not( $sym )">
    <!-- Consequently, this should never be hit -->
    <message terminate="yes"
             select="concat( 'internal: unexpected condition in ',
                             'lvm:map//lvm:from processing of ',
                             $to, ' to ', @name )" />
  </if>

  <!-- Determine how many dimensions we _pretend_ a symbol has.  This is
       confusing and awkward with how this code is written.  One `from'
       element can represent either a scalar or vector mapping; the JS
       code below converts scalars to vectors at runtime. -->
  <variable name="effective-dim" as="xs:double"
            select="max( ( 0, number( $sym/@dim ) - 1 ) ) + 1" />

  <!-- The symbol dimensions are reduced by the current nesting level plus
       one to account for the outer loop at runtime.  -->
  <variable name="nested-dim" as="xs:double"
            select="$effective-dim - ( $nested-depth + 1 )" />

  <!-- Prevent mapping deeper than the number of available dimensions.  This
       check is confusing and awkward with how this code is written.  One
       `from' element can represent either a scalar or vector mapping. -->
  <if test="$nested-dim lt 0">
    <message terminate="yes"
             select="concat( 'error: `from'' nesting for ', $sym/@name,
                             ' must not exceed a depth of ',
                             $effective-dim )" />
  </if>

  <!-- TODO: support arbitrary depth -->
  <!-- oval = orig val -->
  <text>(function(oval){</text>
    <text>var val = Array.isArray(oval) ? oval : [oval===undefined?'':oval]; </text>
    <text>var ret = []; </text>

    <if test="$nested-depth = 0">
      <text>var curindex;</text>
    </if>

    <text>for ( var i = 0, l = val.length; i&lt;l; i++ ){</text>
      <if test="$nested-depth = 0">
        <text>curindex = i;</text>
      </if>

      <!-- note that we're casting the value to a string; this is important,
           since case comparisons are strict (===) -->
      <text>switch(''+val[i]){</text>
        <apply-templates mode="lvmc:compile">
          <with-param name="type" select="$type" />
        </apply-templates>

        <if test="not( lvm:default )">
          <text>default: ret.push(</text>
          <choose>
            <!-- give precedence to explicit default -->
            <when test="@default">
              <sequence select="concat( '''',
                                        lvmc:escape-string( @default ),
                                        '''' )" />
            </when>

            <otherwise>
              <call-template name="lvmc:gen-input-default">
                <with-param name="sym" select="$sym" />
                <with-param name="from-str">
                  <text>''+val[i]</text>
                </with-param>
                <!-- We have to reduce the nesting level by one because of
                     our outer loop, but we do not want to go below 0, which
                     _could_ happen if from is used with a scalar symbol
                     (see above nested-dim check) -->
                <with-param name="dim" select="$nested-dim" />
              </call-template>
            </otherwise>
          </choose>
          <text>);</text>
        </if>
      <text>}</text>
  <text>}</text>

  <choose>
    <when test="@scalar='true'">
      <text>return ret[0]; </text>
    </when>

    <otherwise>
      <text>return ret; </text>
    </otherwise>
  </choose>

  <text>})(</text>
    <value-of select="lvmc:gen-input( 'input', @name )" />
  <if test="@index">
    <text>[</text>
    <value-of select="@index" />
    <text>]</text>
  </if>

  <if test="$nested-depth gt 0">
    <text>[curindex]</text>
  </if>

  <text>)</text>
</template>


<template match="lvm:map//lvm:from" mode="lvmc:compile" priority="2">
  <sequence select="lvmc:value-ref( . )" />
</template>


<!--
  Generate source value lookup

  Nested value lookups are supported using `.' in `source'.
-->
<function name="lvmc:gen-input" as="xs:string">
  <param name="name"   as="xs:string" />
  <param name="source" as="xs:string" />

  <variable name="nested-prefix" as="xs:string"
            select="substring-before( $source, '.' )" />
  <variable name="nested-suffix" as="xs:string"
            select="substring-after( $source, '.' )" />

  <choose>
    <when test="$nested-prefix">
      <sequence select="lvmc:gen-input(
                          concat( '(', $name, '[''',
                                  lvmc:escape-string( $nested-prefix ),
                                  ''']||{})' ),
                          $nested-suffix )" />
    </when>

    <otherwise>
      <sequence select="concat( $name, '[''',
                                lvmc:escape-string( $source ),
                                ''']' )" />
    </otherwise>
  </choose>
</function>


<function name="lvmc:value-ref" as="xs:string">
  <param name="from" as="element( lvm:from )" />

  <variable name="nested" as="xs:boolean"
            select="exists( $from/ancestor::lvm:from )" />

  <variable name="name" as="xs:string"
            select="$from/@name" />
  <variable name="index" as="xs:string?"
            select="$from/@index" />


  <!-- index reference, if applicable -->
  <variable name="index-ref" as="xs:string"
            select="if ( $index ) then
                        concat( '[', $index, ']' )
                      else
                        ''" />

  <!-- additional index, if nested within another from -->
  <variable name="nested-ref" as="xs:string"
            select="if ( $nested ) then
                        '[curindex]'
                      else
                        ''" />

  <!-- compiled reference, including index and nested -->
  <variable name="ref" as="xs:string"
            select="concat(
                      lvmc:gen-input( 'input', $name ),
                      $index-ref,
                      $nested-ref )" />

  <!-- finally, wrap in any transformations -->
  <sequence select="lvmc:transformation-wrap(
                      $ref, $from/ancestor::lvm:transform )" />
</function>


<function name="lvmc:transformation-wrap" as="xs:string">
  <param name="value"     as="xs:string" />
  <param name="transform" as="element( lvm:transform )*" />

<!-- transformations (if any) as function applications -->
  <variable name="transform-methods" as="xs:string*"
            select="for $method in $transform/@method
                      return concat(
                        lvmc:get-method-func( $method ),
                        '(' )" />

  <!-- closing parenthesis for each -->
  <variable name="transform-close" as="xs:string*"
            select="for $_ in $transform-methods
                      return ')'" />

  <!-- wrap $ref in methods and closing parentheses -->
  <sequence select="concat(
                      string-join( $transform-methods, '' ),
                      $value,
                      string-join( $transform-close, '' ) )" />
</function>


<template match="lvm:from/lvm:default"
          mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )"
         tunnel="yes" />
  <param name="type"     as="xs:string" />

  <sequence select="concat(
                      'default:ret.push(',
                      string-join(
                        lvmc:concat-compile( element(), (), $symtable, $type),
                        '' ),
                      ');' )" />
</template>


<template match="lvm:map//lvm:value"
          mode="lvmc:compile" priority="5">
  <sequence select="concat( '''', text(), '''' )" />
</template>


<!--
  Key/value mapping
-->
<template mode="lvmc:compile" priority="5"
          match="lvm:map//lvm:from/lvm:translate[ @key ]">
  <param name="type" as="xs:string" />

  <text>case '</text>
    <value-of select="lvmc:escape-string( @key )" />
  <text>':</text>
    <apply-templates select="." mode="lvmc:compile-translate">
      <with-param name="type" select="$type" />
    </apply-templates>
  <text> break;</text>
</template>


<!--
  Skip transformations during initial encounter

  Transformations are applied within certain contexts; let those contexts
  apply them when ready.
-->
<template mode="lvmc:compile" priority="3"
          match="lvm:map//lvm:transform">
  <apply-templates mode="lvmc:compile" />
</template>


<template match="lvm:translate[ element() ]"
              mode="lvmc:compile-translate" priority="5">
  <param name="symtable" as="element( preproc:symtable )"
         tunnel="yes" />
  <param name="type"     as="xs:string" />

  <sequence select="concat(
                      'ret.push(',
                      string-join(
                        lvmc:concat-compile( element(), @empty, $symtable, $type ),
                        '' ),
                       ');' )" />
</template>


<function name="lvmc:concat-compile" as="xs:string+">
  <param name="children" as="element()+" />
  <param name="default"  as="xs:string?" />
  <param name="symtable" as="element( preproc:symtable )" />
  <param name="type"     as="xs:string" />

  <text>(function(){</text>
    <!-- end result should compile into a (dynamic) string -->
    <text>var result=</text>
      <for-each select="$children">
        <if test="position() > 1">
          <text> + </text>
        </if>

        <apply-templates mode="lvmc:compile" select=".">
          <with-param name="type" select="$type" />
          <with-param name="symtable" select="$symtable"
                      tunnel="yes"/>
        </apply-templates>
      </for-each>
    <text>;</text>

    <text>return (result === "") ? '</text>
      <sequence select="lvmc:escape-string( $default )" />
    <text>' : result;</text>
  <text>})()</text>
</function>


<function name="lvmc:escape-string" as="xs:string">
  <param name="str" as="xs:string?" />

  <sequence select="replace( $str, '''', '\\''' )" />
</function>


<template match="lvm:translate"
              mode="lvmc:compile-translate" priority="1">
  <text>ret.push('</text>
    <value-of select="lvmc:escape-string( normalize-space( @value ) )" />
  <text>');</text>
</template>


<template match="text()|comment()" mode="lvmc:compile" priority="1">
  <!-- strip all text and comments -->
</template>


<template match="*" mode="lvmc:compile" priority="1">
  <message terminate="yes">
    <text>fatal: invalid map: unexpected node </text>
    <apply-templates select="." mode="lvmc:pathout" />
  </message>
</template>


<template match="lvm:import|lvm:class" mode="lvmc:compile" priority="2">
  <!-- ignore -->
</template>


<!-- import symbols -->
<template match="lvm:import" mode="preproc:symtable" priority="5">
  <!-- original root passed to sym-discover -->
  <param name="orig-root" />

  <variable name="src" as="xs:string"
            select="@path" />

  <if test="not( @path )">
    <message terminate="yes"
             select="concat(
                       'fatal: import missing @path',
                       if ( @package ) then
                           ' (use instead of @package)'
                         else
                           () )" />
  </if>

  <!-- perform symbol import -->
  <call-template name="preproc:symimport">
    <with-param name="orig-root" select="$orig-root" />
    <with-param name="package" select="$src" />
    <with-param name="export" select="'true'" />
  </call-template>
</template>


<template match="*" mode="lvmc:pathout">
  <if test="parent::*">
    <apply-templates select="parent::*" mode="lvmc:pathout" />
  </if>

  <text>/</text>
  <value-of select="name()" />
</template>


<!--
  Outputs a simple pass-through map that may be used if no map is present

  This simply calls the callback with the given input after creating a new
  object with it as the prototype, ensuring that altered data does not impact
  the original data.
-->
<template name="lvmc:dummy-map">
  <param name="name" select="'map'" />

  <text>function </text>
    <value-of select="$name" />
  <text>( input, callback ) { </text>
    <!-- protect input against potential mutilation from classifier -->
    <text>var prot = function() {}; </text>
    <text>prot.prototype = input; </text>
    <text>callback( new prot() );</text>
  <text> }</text>
</template>



<!--
  Validates map between program and the rater, checking for errors that would
  cause significant problems.
-->
<template match="lvm:program-map" mode="lvmc:validate-rater">
  <param name="rater" />

  <variable name="map" select="." />

  <!--
    Get a list of all fields that have not been mapped
  -->
  <variable name="nomap" select="
      $rater/lv:param[
        not(
          @name=$map//lvm:pass/@name
          or @name=$map//lvm:map/@to
        )
      ]
    " />

  <!-- required and unmapped -->
  <variable name="req-nomap" select="
      $nomap[ not( @default ) or @default='' ]
    " />

  <!-- warning on non-mapped, but not required -->
  <for-each select="$nomap[ @default ]">
    <message>
      <text>! [map warning] unmapped optional field: </text>
      <value-of select="@name" />
    </message>
  </for-each>

  <!-- error on required non-mapped -->
  <for-each select="$req-nomap">
    <message>
      <text>!!! [map error] unmapped required field: </text>
      <value-of select="@name" />
    </message>
  </for-each>


  <if test="$unused-param-check = 'true'">
    <variable name="unknown" select="
        //lvm:pass[
          not( @name=$rater/lv:param/@name )
        ]
        |
        //lvm:map[
          not( @to=$rater/lv:param/@name )
        ]
        |
        //lvm:class[
          not( @name=$rater/lv:classify/@as )
        ]
      " />

    <!-- error on unknown -->
    <for-each select="$unknown">
      <message>
        <text>!!! [map error] unknown/unused destination identifier: </text>
        <value-of select="@name|@to" />
      </message>
    </for-each>

    <if test="count( $unknown )">
      <lvmc:terminate />
    </if>
  </if>


  <!-- fail. -->
  <if test="count( $req-nomap )">
    <lvmc:terminate />
  </if>
</template>


<template match="lvm:program-map" mode="lvmc:validate-ui">
  <param name="ui" />

  <variable name="knowns" as="xs:string*"
            select="$ui//lvp:question/@id,
                    $ui//lvp:external/@id,
                    $ui//lvp:flag/@id,
                    $ui//lvp:calc/@id,
                    for $id in $ui//lvp:meta/lvp:field/@id
                      return concat( 'meta:', $id )" />


  <!-- get a list of unknown source mappings -->
  <!-- TODO: this is a mess -->
  <variable name="unknown-pre" select="
      .//lvm:pass[ not( @name = $knowns ) ],
      .//lvm:map[ @from and not( @from = $knowns ) ],
      .//lvm:from[ not( @name = $knowns ) ],
      .//lvm:set[ @each and not( @each = $knowns ) ]
    " />

  <variable name="unknown"
    select="$unknown-pre[ not( @novalidate='true' ) ]" />


  <!-- error on unknown -->
  <for-each select="$unknown">
    <message>
      <text>!!! [map error] unknown source field: </text>
      <value-of select="@name|@from" />
    </message>
  </for-each>

  <if test="count( $unknown )">
    <lvmc:terminate />
  </if>
</template>


<!--
  Outputs source and dest mappings in a common, easily-referenced format useful
  for parsing
-->
<template match="lvm:program-map" mode="lvmc:source-dest-map" priority="5">
  <lvmc:map>
    <apply-templates select="./lvm:*" mode="lvmc:source-dest-map" />
  </lvmc:map>
</template>

<template match="lvm:pass" mode="lvmc:source-dest-map" priority="5">
  <lvmc:map from="{@name}" to="{@name}" elig="{@affects-eligibility}" />
</template>

<template match="lvm:map[ @from ]" mode="lvmc:source-dest-map" priority="5">
  <lvmc:map from="{@from}" to="{@to}" elig="{@affects-eligibility}" />
</template>
<template match="lvm:map/lvm:from" mode="lvmc:source-dest-map" priority="5">
  <lvmc:map from="{@name}" to="{ancestor::lvm:map/@to}"
    elig="{@affects-eligibility}" />
</template>
<template match="lvm:map//lvm:set/lvm:from" mode="lvmc:source-dest-map" priority="4">
  <!-- not included; not a one-to-one mapping -->
</template>

<template match="lvm:map[*]" mode="lvmc:source-dest-map" priority="5">
  <apply-templates select=".//lvm:*" mode="lvmc:source-dest-map" />
</template>

<template match="lvm:map//lvm:set" mode="lvmc:source-dest-map" priority="2">
  <!-- do nothing -->
</template>
<template match="lvm:map//lvm:static" mode="lvmc:source-dest-map" priority="2">
  <!-- do nothing -->
</template>
<template match="lvm:map//lvm:value" mode="lvmc:source-dest-map" priority="2">
  <!-- do nothing -->
</template>
<template match="lvm:map//lvm:translate" mode="lvmc:source-dest-map" priority="2">
  <!-- do nothing -->
</template>
<template match="lvm:map[ @value ]" mode="lvmc:source-dest-map" priority="2">
  <!-- no source -->
</template>
<template match="lvm:const" mode="lvmc:source-dest-map" priority="2">
  <!-- no source -->
</template>

<template match="lvm:class" mode="lvmc:source-dest-map" priority="2">
  <!-- not applicable -->
</template>


<template match="*" mode="lvmc:source-dest-map" priority="1">
  <message terminate="yes">
    <text>Unknown node: </text>
    <value-of select="name()" />
  </message>
</template>

</stylesheet>
