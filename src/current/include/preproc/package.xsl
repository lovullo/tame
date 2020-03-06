<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compile package

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

  This will preprocess a package XML suitable for compilation into an object
  file.
-->
<stylesheet version="1.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:map="http://www.w3.org/2005/xpath-functions/map"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:t="http://www.lovullo.com/rater/apply-template"
            xmlns:c="http://www.lovullo.com/calc"
            xmlns:w="http://www.lovullo.com/rater/worksheet"
            xmlns:lvv="http://www.lovullo.com/rater/validate"
            xmlns:ext="http://www.lovullo.com/ext"
            xmlns:util="http://www.lovullo.com/util">


<include href="../dslc-base.xsl" />

<!-- phases -->
<include href="macros.xsl" />
<include href="expand.xsl" />
<include href="symtable.xsl" />

<include href="../../compiler/fragments.xsl" />


<!-- begin preprocessing from an arbitrary node -->
<template name="preproc:pkg-compile" as="element( lv:package )"
              match="*" mode="preproc:compile" priority="1">
  <param name="orig-root" as="element()"
             select="root(.)/lv:package" />

  <param name="stopshort" />

  <!-- should be provided externally -->
  <if test="not( $__rseed ) or ( $__rseed = '' )">
    <message terminate="yes">
      <text>[preproc] error: missing random seed `__rseed'</text>
    </message>
  </if>

  <message>
    <text>[preproc] *beginning macro expansion...</text>
  </message>

  <!-- these can contain repass nodes, thus the element()+ -->

  <!-- macro expansion -->
  <variable name="stage1" as="element()+">
    <apply-templates select="." mode="preproc:macropass" />

    <message>
      <text>[preproc] *macro pass complete; expanding...</text>
    </message>
  </variable>

  <!-- expand shorthands, etc -->
  <variable name="stage2" as="element()+">
    <apply-templates select="$stage1"
      mode="preproc:expand" />

    <message>
      <text>[preproc] *expansion complete; generating symbol table...</text>
    </message>
  </variable>

  <variable name="stage3" as="element()+">
    <apply-templates select="$stage2"
      mode="preproc:sym-discover">

      <with-param name="orig-root" select="$orig-root" />
    </apply-templates>

    <message>
      <text>[preproc] *symbol table generated; checking for </text>
      <text>unprocessed templates...</text>
    </message>
  </variable>


  <!-- TODO: resolve this mess -->
  <variable name="stage3-pkg" as="element( lv:package )"
                select="$stage3/preproc:symtable/parent::*" />

  <!-- TODO: move me somewhere more appropriate and create an error
       system that is _guaranteed_ to catch everything -->
  <for-each select="$stage3-pkg/preproc:symtable/preproc:error">
    <message terminate="yes">
      <text>!!! [preproc] error: </text>
      <value-of select="." />
    </message>
  </for-each>


  <!-- determine if we should finish or simply return for further processing -->
  <choose>
    <when test="not( $stopshort )">
      <!-- template expansions may have been deferred until their symbols were made
           available -->
      <variable name="final" as="element( lv:package )">
        <call-template name="preproc:tpl-sym-recurse">
          <with-param name="package" select="$stage3-pkg" />
          <with-param name="orig-root" select="$orig-root" />
        </call-template>
      </variable>

      <variable name="extern-chk" as="element( preproc:error )*"
                    select="preproc:final-extern-check(
                              $final/preproc:symtable )" />


      <if test="$extern-chk">
        <for-each select="$extern-chk">
          <message>
            <text>!!! [preproc] error: </text>
            <value-of select="." />
          </message>
        </for-each>

        <message>~~~~[begin document dump]~~~~</message>
        <message select="$final" />
        <message>~~~~[end document dump]~~~~</message>

        <message select="'Aborting due to unresolved externs'" />

        <message terminate="yes"
                     select="'Document dumped.'" />
      </if>

      <!-- ensure that all template parameters have been expanded -->
      <apply-templates select="$final" mode="preproc:tpl-check" />

      <!-- perform validation before dependency generation to ensure that all
           dependencies are available -->
      <apply-templates select="$final" mode="preproc:pkg-validate" />

      <!-- determine how many passes have been made -->
      <!-- TODO: reintroduce
      <variable name="repass-count"
        select="count( $final//preproc:repass-record )" />
        -->

      <!-- assign unique ids to each node -->
      <variable name="idized" as="element( lv:package )">
        <apply-templates select="$final" mode="preproc:idize" />
      </variable>

      <!-- generate deps -->
      <variable name="depd" as="element( lv:package )">
        <apply-templates select="$idized" mode="preproc:gen-deps" />
      </variable>

      <!-- post-process symbol table to resolve any unknowns that require a
           dependency graph -->
      <variable name="resolvd" as="element( lv:package )">
        <apply-templates select="$depd" mode="preproc:resolv-syms">
          <with-param name="orig-root" select="$orig-root" />
        </apply-templates>
      </variable>


      <!-- compile fragments -->
      <message>
        <text>[preproc] compiling fragments...</text>
      </message>


      <apply-templates select="$resolvd"
                           mode="preproc:compile-fragments" />


      <!-- output a repass count, which could be a strong indicator of performance
           issues -->
      <!-- TODO: reintroduce
      <message>
        <text>[Preprocessor repass count: </text>
          <value-of select="$repass-count" />
        <text>] [Node count: </text>
          <value-of select="count( $final//* )" />
        <text>]</text>
      </message>
      -->
    </when>

    <!-- return for further processing -->
    <otherwise>
      <sequence select="$stage3" />
    </otherwise>
  </choose>
</template>


<!--
  A very primitive guard against unexpanded template parameters.
-->
<template match="*[ starts-with( @*, '@' ) ]" mode="preproc:tpl-check" priority="5">
  <message>
    <text>[preproc] fatal: unexpanded template parameter: </text>
    <sequence select="@*[ starts-with( ., '@' ) ]" />
  </message>

  <message>
    <text>[preproc] fatal: reference node: </text>
    <sequence select="." />
  </message>

  <message>~~~~[begin document dump]~~~~</message>
  <message select="root(.)" />
  <message>~~~~[end document dump]~~~~</message>

  <message terminate="yes">[preproc] notice: Document dumped.</message>
</template>


<!-- this should never happen; but is has, so here's a failsafe -->
<template match="lv:apply-template" mode="preproc:tpl-check" priority="5">
  <message>
    <text>[preproc] fatal: unexpanded template: </text>
    <sequence select="." />
  </message>

  <message>
    <text>[preproc] fatal: reference node: </text>
    <sequence select="." />
  </message>

  <message select="'[preproc] internal error: there is a bug in the',
                       'preprocessor; this should never happen!'" />

  <message>~~~~[begin document dump]~~~~</message>
  <message select="root(.)" />
  <message>~~~~[end document dump]~~~~</message>

  <message terminate="yes">[preproc] notice: Document dumped.</message>
</template>


<!-- skip things that cannot contain template applications -->
<template match="lv:template|lv:const|lv:typedef|lv:param-copy"
              mode="preproc:tpl-check" priority="9">
</template>

<template match="*" mode="preproc:tpl-check" priority="1">
  <apply-templates select="*" mode="preproc:tpl-check" />
</template>



<!--
  TODO: This needs to go away in the form of a more performant system
  that marks a repass as needed *without* scanning the entire tree.
-->
<template name="preproc:tpl-sym-recurse" as="element( lv:package )">
  <param name="package" as="element( lv:package )" />
  <param name="orig-root" as="element()" />

  <!-- number of iterations where no template applications have
       happened -->
  <param name="tpl-stall-count"
             tunnel="yes"
             select="0" />

  <!-- get a list of needed template applications (ignoring applications that
       are within templates and nested applications) -->
  <variable name="apply" as="element( lv:apply-template )*"
                select="$package//lv:apply-template[
                          not(
                            @name=$package/lv:template/@name
                            or ancestor::lv:template
                            or ancestor::lv:apply-template
                          )
                        ]" />
  <variable name="napply" select="count( $apply )" />

  <choose>
    <when test="$apply">
      <!-- get a list of required templates -->
      <variable name="req">
        <preproc:tpl>
          <for-each select="$apply">
            <copy>
              <sequence select="@name" />
            </copy>
          </for-each>
        </preproc:tpl>
      </variable>

      <variable name="requniq" as="element( lv:apply-template )*" select="
          $req//lv:apply-template[
            not( @name=preceding-sibling::lv:apply-template/@name
                 or @name=$package//lv:template/@name ) ]
        " />

      <message>
        <text>[preproc] </text>
        <value-of select="count( $apply )" />
        <text> template(s) still need application: </text>

        <for-each select="$apply">
          <if test="position() gt 1">
            <text>, </text>
          </if>

          <value-of select="@name" />
        </for-each>
      </message>

      <!-- load each of the requested templates (but only once) -->
      <variable name="tpls">
        <!-- TODO: we no longer need to load the templates; the
             template replacement looks up the template on-demand from
             the symbol table; we're no longer injecting them -->
        <apply-templates mode="preproc:tpl-from-sym" select="$requniq">
          <with-param name="orig-root" select="$orig-root" />
          <with-param name="symtable" select="$package/preproc:symtable" />
        </apply-templates>
      </variable>

      <!-- if we have recursed and have not decreased the application count at all,
           then we have a problem -->
      <if test="$requniq
                    and ( $package//preproc:sym-available )
                    and not( $package//preproc:repass[ @tpl-applied ] )">
        <message terminate="yes">
          <text>!!! [preproc] fatal: unable to locate symbols for </text>
          <text>remaining templates: </text>

          <for-each select="$requniq">
            <if test="position() > 1">
              <text>; </text>
            </if>

            <value-of select="@name" />
          </for-each>
        </message>
      </if>

      <!-- if there was an error during this part of the process, halt -->
      <if test="$tpls//preproc:error">
        <message terminate="yes">
          <text>!!! [preproc] fatal: terminating due to errors</text>
        </message>
      </if>

      <!-- perform expansion on the new package with the needed templates -->
      <variable name="result" as="element( lv:package )">
        <apply-templates select="$package" mode="preproc:compile">
          <with-param name="orig-root" select="$orig-root" />
          <with-param name="stopshort" select="true()" />
        </apply-templates>

        <message>
          <text>[preproc] *expansion complete (recursive)</text>
        </message>
      </variable>

      <!-- failsafe to prevent infinite recursion (5 (0-indexed)
           should be plenty, since everything in the system results in
           a template application in fewer steps -->
      <if test="$requniq and $tpl-stall-count eq 4">
        <message select="'!!! [preproc] internal: expansion deadlock!'" />
        <message
            select="'!!! [preproc] internal: stalled for 5 iterations'" />

        <sequence select="preproc:dump-document( $result )" />

        <message terminate="yes">
          <text></text>
          <text>!!! [preproc] fatal: expansion of remaining </text>
          <text>templates aborted (have all been imported?): </text>

          <for-each select="$requniq">
            <if test="position() > 1">
              <text>; </text>
            </if>

            <value-of select="@name" />
          </for-each>
        </message>
      </if>

      <!-- recurse to continue expanding if need be -->
      <call-template name="preproc:tpl-sym-recurse">
        <with-param name="orig-root" select="$orig-root" />
        <with-param name="package" select="$result" />
        <with-param name="tpl-stall-count"
                        tunnel="yes"
                        select="if ( $package//preproc:tpl-step ) then
                                  0
                                else
                                  $tpl-stall-count + 1" />
      </call-template>
    </when>


    <!-- expansion sequences and template short-hand expansions that
         have not yet taken place, due to one reason or another (too
         few passes: bug as far as I'm concerned) -->
    <when test="$package//lv:expand-sequence[
                      not( ancestor::lv:template
                           or ancestor::lv:apply-template ) ]
                    |$package//t:*[
                       not( ancestor::lv:template
                            or ancestor::lv:apply-template ) ]">
      <message select="'[preproc] pending expansions still present'" />

      <variable name="result" as="element( lv:package )">
        <apply-templates select="$package" mode="preproc:compile">
          <with-param name="orig-root" select="$orig-root" />
          <with-param name="stopshort" select="true()" />
        </apply-templates>
      </variable>

      <call-template name="preproc:tpl-sym-recurse">
        <with-param name="orig-root" select="$orig-root" />
        <with-param name="package" select="$result" />
      </call-template>
    </when>


    <!-- no further applications are necessary -->
    <otherwise>
      <!-- apply one final pass for the eligibility class generation -->
      <call-template name="preproc:elig-class-pass">
        <with-param name="package" select="$package" />
        <with-param name="orig-root" select="$orig-root" />
      </call-template>
    </otherwise>
  </choose>
</template>


<template name="preproc:elig-class-pass" as="element( lv:package )">
  <param name="package" as="element( lv:package )" />
  <param name="orig-root" as="element()" />

  <variable name="final-pkg" as="element( lv:package )">
    <apply-templates select="$package" mode="preproc:expand-elig-class">
      <with-param name="orig-root" select="$package" />
    </apply-templates>
  </variable>

  <!-- yield the final pass against the elig-class-augmented package -->
  <apply-templates select="$final-pkg" mode="preproc:compile">
    <with-param name="orig-root" select="$orig-root" />
    <with-param name="stopshort" select="true()" />
  </apply-templates>
</template>


<!--
  This is but a shell of its former self.

  We are no longer injecting templates, so this can be removed or
  adapted to do just enough to verify that the template symbols exist.
-->
<template match="lv:apply-template" mode="preproc:tpl-from-sym">
  <param name="orig-root" as="element()" />

  <param name="symtable" as="element( preproc:symtable )"
             select="root(.)/preproc:symtable" />

  <variable name="tplname" select="@name" />

  <variable name="sym" as="element( preproc:sym )?" select="
      $symtable/preproc:sym[
        @name=$tplname
        and @type='tpl'
      ]
    " />

  <!-- if we have a symbol table, then attempt to locate it -->
  <choose>
    <!-- if we have located the template, then we know its source (@src must be
         set, otherwise the template is defined in this package and should have
         been available to begin with) -->
    <when test="$sym">
      <preproc:sym-available for="{$tplname}" />
    </when>

    <!-- nothing we can do yet -->
    <otherwise>
      <message>
        <text>[preproc] template symbol not yet available: </text>
        <value-of select="$tplname" />
      </message>
    </otherwise>
  </choose>
</template>


<!-- TODO: we should use an attr besides _id; one more definitive -->
<template match="*[ @_id ]" mode="preproc:macropass" priority="9">
  <!-- already preprocessed -->
  <sequence select="." />
</template>
<template match="*[ @_id ]" mode="preproc:idize" priority="9">
  <!-- already preprocessed -->
  <sequence select="." />
</template>



<template match="preproc:repass-record" mode="preproc:idize" priority="9">
  <!-- no longer needed; remove -->
</template>


<!-- do not idize preproc nodes (unneeded waste of cycles) -->
<template match="preproc:*" mode="preproc:idize" priority="5">
  <sequence select="." />
</template>

<!-- do not idize templates (will cause processing problems) -->
<template match="lv:template" mode="preproc:idize" priority="5">
  <sequence select="." />
</template>


<!--
  Generates a unique id for each element and stores it as @_id

  This allows the node set to be copied and maintain its identity.
-->
<template match="*" mode="preproc:idize" priority="1">
  <variable name="id">
    <value-of select="generate-id(.)" />
  </variable>

  <copy>
    <sequence select="@*" />

    <attribute name="_id">
      <value-of select="$id" />
    </attribute>

    <apply-templates mode="preproc:idize" />
  </copy>
</template>


<template match="lv:package" mode="preproc:resolv-syms" as="element( lv:package )"
              priority="9">
  <param name="orig-root" as="element()" />
  <param name="rpcount" select="0" />

  <variable name="symtable-map" as="map( xs:string, element( preproc:sym ) )"
            select="map:merge(
                      for $sym in preproc:symtable/preproc:sym
                        return map{ string( $sym/@name ) : $sym } )" />

  <variable name="symdep-map" as="map( xs:string, element( preproc:sym-dep ) )"
            select="map:merge(
                      for $sym-dep in preproc:sym-deps/preproc:sym-dep
                        return map{ string( $sym-dep/@name ) : $sym-dep } )" />

  <!-- arbitrary; intended to prevent infinite recursion -->
  <!-- TODO: same method as for templates; ensure changes, but do not create
       arbitrary limit -->
  <if test="$rpcount = 100">
    <sequence select="preproc:dump-document( root() )" />

    <message terminate="yes">
      <text>[preproc] !!! recursion limit reached in resolving `</text>
        <value-of select="@name" />
      <text>' symbols</text>
    </message>
  </if>

  <variable name="result" as="element( lv:package )">
    <copy>
      <sequence select="@*" />

      <message>
        <text>[preproc] *resolving symbol attributes...</text>
      </message>

      <apply-templates mode="preproc:resolv-syms">
        <with-param name="orig-root" select="$orig-root" />
        <with-param name="symtable-map" select="$symtable-map" tunnel="yes" />
        <with-param name="symdep-map" select="$symdep-map" tunnel="yes" />
      </apply-templates>
    </copy>
  </variable>

  <variable name="repass"
    select="$result//preproc:symtable/preproc:repass" />

  <choose>
    <!-- repass scheduled; go for it -->
    <when test="$repass">
      <message>[preproc] *SYM REPASS*</message>
      <message>
        <text>[preproc] The following </text>
          <value-of select="count( $repass )" />
        <text> symbol(s) are still unresolved:</text>
      </message>

      <for-each select="$repass">
        <message>
          <text>[preproc] - </text>
          <value-of select="@ref" />
        </message>
      </for-each>

      <apply-templates select="$result" mode="preproc:resolv-syms">
        <with-param name="orig-root" select="$orig-root" />
        <with-param name="rpcount" select="$rpcount + 1" />
      </apply-templates>
    </when>

    <!-- no repass needed; done -->
    <otherwise>
      <sequence select="$result" />
    </otherwise>
  </choose>
</template>


<template match="preproc:symtable" mode="preproc:resolv-syms" priority="5">
  <param name="orig-root" as="element()" />

  <copy>
    <apply-templates mode="preproc:resolv-syms">
      <with-param name="orig-root" select="$orig-root" />
    </apply-templates>
  </copy>
</template>


<!--
  Calculate symbol dimensions by taking the highest dimension of its
  dependencies

  If all dependencies are not yet resolved, then schedule a repass.
-->
<template match="preproc:sym[ not( @src ) and @dim='?' ]" mode="preproc:resolv-syms" priority="5">
  <param name="orig-root" as="element()" />
  <param name="symtable-map" as="map(*)" tunnel="yes" />
  <param name="symdep-map" as="map(*)" tunnel="yes" />

  <variable name="name" select="@name" />
  <variable name="pkg"  as="element( lv:package )"
                select="root(.)" />

  <variable name="deps" as="element( preproc:sym-dep )?"
            select="$symdep-map( $name )" />

  <!-- TODO: make this fatal -->
  <if test="empty( $deps ) and not( @no-deps = 'true' )">
    <message select="concat( 'internal: failed to located dependencies for `',
                             $name, '''' )" />
  </if>

  <variable name="depsyms" as="element( preproc:sym )*"
            select="for $ref in $deps/preproc:sym-ref
                      return $symtable-map( $ref/@name )" />

  <choose>
    <!-- unresolved dependency dimensions; defer until next pass -->
    <when test="
        $depsyms/@dim = '?'
      ">
      <!-- schedule repass :x -->
      <sequence select="." />
      <preproc:repass src="preproc:sym resolv-syms"
                      ref="{$name}" />
    </when>

    <!-- all dependencies are resolved; calculate dimensions -->
    <otherwise>
      <variable name="max" as="xs:double"
                select="if ( empty( $depsyms ) ) then
                            0
                          else
                            max( $depsyms/@dim )" />

      <copy>
        <sequence select="@*" />
        <attribute name="dim" select="$max" />
        <sequence select="*" />
      </copy>
    </otherwise>
  </choose>
</template>


<template match="preproc:repass" mode="preproc:resolv-syms" priority="9">
  <!-- strip -->
</template>


<template match="*" mode="preproc:resolv-syms" priority="1">
  <sequence select="." />
</template>


<template match="lv:package" mode="preproc:pkg-validate">
  <variable name="symbol-map">
    <call-template name="get-symbol-map" />
  </variable>

  <variable name="err">
    <apply-templates select="." mode="lvv:validate">
      <with-param name="symbol-map" select="$symbol-map" />
    </apply-templates>
  </variable>

  <apply-templates select="$err//lvv:error" mode="preproc:handle-lvv-errors">
    <with-param name="document" select="." />
  </apply-templates>
</template>


<template match="*|text()" mode="preproc:pkg-validate">
</template>


<!-- errors should cause a failure -->
<template match="lvv:error" mode="preproc:handle-lvv-errors" priority="5">
  <param name="document" />

  <!-- output error -->
  <message>
    <text>!!! </text>
    <value-of select="@desc" />

    <if test="@path != ''">
      <text> (</text>
      <value-of select="@path" />
      <text>)</text>
    </if>

    <text>: </text>
    <value-of select="." />
  </message>

  <!-- terminate after we've output each error -->
  <if test="not( following-sibling::lvv:error )">
    <!-- dump document for debugging -->
    <sequence select="preproc:dump-document( $document )" />
    <message terminate="yes">Compilation failed due to validation errors.</message>
  </if>
</template>


<function name="preproc:dump-document">
  <param name="document" />

  <message>~~~~[begin document dump]~~~~</message>
  <message select="$document" />
  <message>~~~~[end document dump]~~~~</message>
  <message>internal: document dumped.</message>
</function>


<template match="node()|text()" mode="preproc:handle-lvv-errors" priority="1">
</template>

</stylesheet>
