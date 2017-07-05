<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles map fragments to produce a map from source data to a destination

  Copyright (C) 2016 LoVullo Associates, Inc.

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
      <!-- initial symbol table; full table will be generated below -->
     <call-template name="lvmc:stub-symtable">
         <with-param name="type-prefix" select="'map'" />
     </call-template>

      <!-- copy all source nodes -->
      <sequence select="node()" />
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
                      $pkg-with-symtable/node()" />

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
        <with-param name="symtable" select="$pkg-with-symtable/preproc:symtable" />
        <with-param name="rater"    select="$rater" />
        <with-param name="type"     select="'map'" />
      </apply-templates>

      <!-- special fragment to be output as the foot -->
      <preproc:fragment id=":map:___tail">
        <text>callback(output);</text>
        <text>};</text>
      </preproc:fragment>
    </preproc:fragments>
  </lv:package>
</template>


<!--
  Generate a function that maps a set of rater outputs
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
      <!-- initial symbol table; full table will be generated below -->
      <call-template name="lvmc:stub-symtable">
        <with-param name="type-prefix" select="'retmap'" />
      </call-template>

      <!-- copy source data -->
      <copy-of select="*" />

      <preproc:fragments>
        <!-- special fragment to be output as the head -->
        <preproc:fragment id=":retmap:___head">
          <!-- use a callback just in case we need to make portions of this async in the
               future -->
          <text>function ( input, callback ) {</text>
            <text>var output = {};</text>
        </preproc:fragment>

        <apply-templates select="./lvm:*" mode="lvmc:compile">
          <with-param name="symtable" select="$dummy-symtable" />
          <with-param name="rater" select="$rater" />
          <with-param name="type" select="'retmap'" />
        </apply-templates>

        <!-- special fragment to be output as the foot -->
        <preproc:fragment id=":retmap:___tail">
            <text>callback(output);</text>
          <text>}</text>
        </preproc:fragment>
      </preproc:fragments>
    </lv:package>
  </variable>

  <!-- output the result after symbol processing -->
  <call-template name="preproc:gen-deps">
    <with-param name="pkg" as="element( lv:package )">
      <apply-templates select="$pkg" mode="preproc:sym-discover">
        <with-param name="orig-root" select="." />
      </apply-templates>
    </with-param>
  </call-template>
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
                 ignore-dup="true" />
    <preproc:sym name=":{$type-prefix}:___tail"
                 type="{$type-prefix}:tail"
                 ignore-dup="true" />
  </preproc:symtable>
</template>


<template name="lvmc:mapsym">
  <param name="name" />
  <param name="from" />
  <param name="type-prefix" select="/lv:package/@lvmc:type" />

  <!-- allow mappings to be overridden after import, which allows defaults
       to be set and then overridden -->
  <preproc:sym name=":{$type-prefix}:{$name}" virtual="true"
               keep="true" type="{$type-prefix}" pollute="true">

    <!-- for consistency and cleanliness, only copy over if set -->
    <if test="@override='true'">
      <copy-of select="@override" />
    </if>

    <copy-of select="@affects-eligibility" />

    <!-- only copy from data if present -->
    <if test="$from">
      <copy-of select="$from" />
    </if>
  </preproc:sym>
</template>


<!--
  Directly map an input to the output
-->
<template match="lvm:pass" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )" />
  <param name="type" />

  <preproc:fragment id=":{$type}:{@name}">
    <text>output['</text>
      <value-of select="@name" />
    <text>']=</text>
      <call-template name="lvmc:gen-input-default">
        <with-param name="symtable" select="$symtable" />
        <with-param name="to" select="@name" />
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
  <preproc:sym-ref name="{@name}" lax="true" />
</template>



<!--
  Maps an input to an output of a different name
-->
<template match="lvm:map[ @from ]" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )" />
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
    <value-of select="@to" />
    <text>']=</text>
    <call-template name="lvmc:gen-input-default">
      <with-param name="symtable" select="$symtable" />
      <with-param name="to" select="@to" />
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
  <!-- to the DSL -->
  <preproc:sym-ref name="{@to}" lax="true" />
</template>

<template match="lvm:map[ @from
                               and root(.)/@lvmc:type = 'retmap' ]"
              mode="preproc:depgen" priority="5">
  <!-- from the DSL -->
  <preproc:sym-ref name="{@from}" lax="true" />
</template>

<template match="lvm:map[ @from ]" mode="preproc:depgen" priority="4">
  <message terminate="yes"
               select="'internal error: unhandled lvm:map: ', ." />
</template>

<template match="/*[ @lvmc:type='retmap' ]//lvm:map[ @from ]" mode="preproc:depgen" priority="6">
  <preproc:sym-ref name="{@from}" lax="true" />
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
  Generate a direct input mapping or, if a default exists for the field, use the
  default if the input is an empty string

  XXX: This is broken; $rater is not provided at the entry point, and
  if it were, this needs to reference its symbol table.
-->
<template name="lvmc:gen-input-default">
  <param name="symtable" as="element( preproc:symtable )" />
  <param name="to" />
  <!-- use one or the other; latter takes precedence -->
  <param name="from" />
  <param name="from-str" />

  <variable name="sym" as="element( preproc:sym )?"
            select="$symtable/preproc:sym[ @name=$to and @src ]" />

  <if test="not( $sym ) and not( $symtable/@lvmc:sym-ignore )">
    <message terminate="yes"
                 select="concat(
                           'error: unknown destination identifier `',
                           string( $to ),
                           ''' (did you import the package?)' )" />
  </if>

  <variable name="from-var">
    <choose>
      <when test="$from-str">
        <value-of select="$from-str" />
      </when>

      <otherwise>
        <text>input['</text>
          <value-of select="$from" />
        <text>']</text>
      </otherwise>
    </choose>
  </variable>

  <choose>
    <when test="$sym and $sym/@default and not( $sym/@default = '' )">
      <text>set_defaults(</text>
        <value-of select="$from-var" />
      <text>,'</text>
        <value-of select="lvmc:escape-string( $sym/@default )" />
      <text>')</text>
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
  <param name="type" />

  <preproc:fragment id=":{$type}:{@to}">
    <text>output['</text>
      <value-of select="@to" />
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
  </call-template>
</template>


<template match="lvm:map[*]" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )" />
  <param name="rater" />
  <param name="type" />

  <preproc:fragment id=":{$type}:{@to}">
    <text>output['</text>
      <value-of select="@to" />
    <text>']=</text>

      <apply-templates select="./lvm:*" mode="lvmc:compile">
        <with-param name="symtable" select="$symtable" />
        <with-param name="rater" select="$rater" />
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
  <preproc:sym-ref name="{@to}" lax="true" />
</template>

<template match="lvm:map[ *
                     and root(.)/@lvmc:type = 'retmap' ]"
              mode="preproc:depgen" priority="6">
  <for-each select=".//lvm:from">
    <preproc:sym-ref name="{@name}" lax="true" />
  </for-each>
</template>



<template match="lvm:const" mode="lvmc:compile" priority="5">
  <text>'</text>
    <value-of select="@value" />
  <text>'</text>
</template>

<template match="lvm:map//lvm:set[@each]" mode="lvmc:compile" priority="5">
  <text>(function(){</text>
    <text>var ret=[];</text>
    <text>var len=input['</text>
      <value-of select="@each" />
    <text>'].length;</text>

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
  <text>(function(){</text>
    <text>var ret=[]; var tmp;</text>

    <for-each select="./lvm:*">
      <text>tmp=</text>
        <apply-templates select="." mode="lvmc:compile" />
      <text>;</text>

      <text>if(tmp&amp;&amp;tmp!=='0')ret.push(tmp);</text>
    </for-each>

    <text>return ret;</text>
  <text>})()</text>
</template>

<template match="lvm:map//lvm:set" mode="lvmc:compile" priority="2">
  <text>[</text>
    <for-each select="./lvm:*">
      <if test="position() > 1">
        <text>,</text>
      </if>

      <apply-templates select="." mode="lvmc:compile" />
    </for-each>
  <text>]</text>
</template>

<template match="lvm:map//lvm:static" mode="lvmc:compile" priority="5">
  <text>'</text>
    <value-of select="@value" />
  <text>'</text>
</template>


<template match="lvm:map//lvm:from[*]" mode="lvmc:compile" priority="5">
  <param name="symtable" as="element( preproc:symtable )" />

  <variable name="to" select="ancestor::lvm:map/@to" />

  <variable name="nested" as="xs:boolean"
            select="exists( ancestor::lvm:from )" />

  <!-- oval = orig val -->
  <text>(function(oval){</text>
    <text>var val = ( (oval||'').length ) ? oval : [oval]; </text>
    <text>var ret = []; </text>

    <if test="not( $nested )">
      <text>var curindex;</text>
    </if>

    <text>for ( var i = 0, l = val.length; i&lt;l; i++ ){</text>
      <if test="not( $nested )">
        <text>curindex = i;</text>
      </if>

      <!-- note that we're casting the value to a string; this is important,
           since case comparisons are strict (===) -->
      <text>switch(''+val[i]){</text>
        <apply-templates mode="lvmc:compile" />

        <if test="not( lvm:default )">
          <text>default: ret.push(</text>
          <choose>
            <!-- give precedence to explicit default -->
            <when test="@default">
              <sequence select="concat( '''',
                                        lvmc:escape-string( @default ),
                                        '''' )" />
            </when>

            <!-- otherwise, generate one -->
            <otherwise>
              <call-template name="lvmc:gen-input-default">
                <with-param name="symtable" select="$symtable" />
                <with-param name="to" select="$to" />
                <with-param name="from-str">
                  <text>''+val[i]</text>
                </with-param>
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

  <text>})(input['</text>
    <value-of select="@name" />
  <text>']</text>

  <if test="$nested">
    <text>[curindex]</text>
  </if>

  <text>)</text>
</template>


<template match="lvm:map//lvm:from" mode="lvmc:compile" priority="2">
  <variable name="nested" as="xs:boolean"
            select="exists( ancestor::lvm:from )" />

  <text>input['</text>
    <value-of select="@name" />
  <text>']</text>

  <choose>
    <when test="@index">
      <text>[</text>
      <value-of select="@index" />
      <text>]</text>
    </when>

    <when test="$nested">
      <text>[curindex]</text>
    </when>
  </choose>
</template>


<template match="lvm:from/lvm:default"
          mode="lvmc:compile" priority="5">
  <sequence select="concat(
                      'default:ret.push(',
                      string-join(
                        lvmc:concat-compile( element(), () ),
                        '' ),
                      ');' )" />
</template>


<template match="lvm:map//lvm:value"
          mode="lvmc:compile" priority="5">
  <sequence select="concat( '''', text(), '''' )" />
</template>


<template match="lvm:map//lvm:from/lvm:translate" mode="lvmc:compile" priority="5">
  <text>case '</text>
    <value-of select="@key" />
  <text>':</text>
    <apply-templates select="." mode="lvmc:compile-translate" />
  <text> break;</text>
</template>


<template match="lvm:translate[ element() ]"
              mode="lvmc:compile-translate" priority="5">
  <sequence select="concat(
                      'ret.push(',
                      string-join(
                        lvmc:concat-compile( element(), @empty ),
                        '' ),
                       ');' )" />
</template>


<function name="lvmc:concat-compile" as="xs:string+">
  <param name="children" as="element()+" />
  <param name="default"  as="xs:string?" />

  <text>(function(){</text>
    <!-- end result should compile into a (dynamic) string -->
    <text>var result=</text>
      <for-each select="$children">
        <if test="position() > 1">
          <text> + </text>
        </if>

        <apply-templates mode="lvmc:compile"
                         select="." />
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
    <value-of select="normalize-space( @value )" />
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
<template match="lvm:import[ @path ]" mode="preproc:symtable" priority="5">
  <!-- original root passed to sym-discover -->
  <param name="orig-root" />

  <variable name="src" as="xs:string"
            select="@path" />

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
                    $ui//lvp:calc/@id" />


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
