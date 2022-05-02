<?xml version="1.0" encoding="utf-8"?>
<!--
  Produces CREATE statements to export package dependency graph into Neo4j

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

  Unlike the dot exporter, this is merciless: it'll export all the
  information that it can, with some minor cleanup to hide some unnecessary
  compiler implementation deatils (like classification splitting).  You are
expected to filter out what you're interested in when querying Neo4j.

  This also produces more realtionships than the dot format, including
  relationships between packges.  So Neo4j can be used as a static analysis
  tool down to the lowest levels of Tame.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:l="http://www.lovullo.com/rater/linker"
            xmlns:neo="http://www.lovullo.com/calc/dot"
            xmlns:st="http://www.lovullo.com/liza/proguic/util/struct"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<include href="util/serialize.xsl" />
<include href="include/preproc/path.xsl" />

<output method="text" />


<!--
  Newline character
-->
<variable name="nl" select="'&#10;'" />

<variable name="fqn-prefix" as="xs:string"
          select="'mega-demo/'" />

<variable name="neo:orig-root" as="document-node( element( lv:package ) )"
          select="/" />

<variable name="neo:orig-package" as="element( lv:package )"
          select="$neo:orig-root/lv:package" />


<!--
  Linked symbols

  This is expected to be executed in Neo4j _after_ package-level data has
  been added to the graph.
-->
<template match="lv:package[ l:dep ]" priority="7">
  <sequence select="concat(
                      ':param refs => ',
                      st:to-neo4j-attrs(
                        st:array( neo:linked-syms( l:dep ) ) ),
                      ';', $nl )" />
</template>


<function name="neo:linked-syms" as="element( st:item )+">
  <param name="deps" as="element( l:dep )" />

  <variable name="pkg-path" as="xs:string"
            select="preproc:get-path( $deps/ancestor::lv:package/@name )" />

  <sequence select="for $dep in $deps/preproc:sym
                      return st:item(
                               preproc:resolv-path(
                                 concat( $fqn-prefix, '/',
                                         $pkg-path, '/',
                                         $dep/@src, '/', $dep/@name ) ) )" />
</function>


<template match="lv:package" priority="5">
  <variable name="sym-deps" select="preproc:sym-deps" />

  <sequence select="concat(
                      ':param syms => ',
                      st:to-neo4j-attrs(
                        st:array(
                          for $sym in neo:get-package-syms( preproc:symtable )
                            return st:item( neo:dict-from-sym( $sym ) ) ) ),
                      ';',
                      $nl,

                      'CREATE (pkg:TamePackage ',
                      st:to-neo4j-attrs(
                        st:dict( (
                          st:item( concat( $fqn-prefix, '/', @name ),
                                   'fqn' ),
                          st:items-from-attrs( @* ) ) ) ),
                      ')',
                      $nl,
                      'WITH pkg',
                      $nl,
                      'UNWIND $syms AS symdata ',
                      'MERGE (sym:TameSymbol {fqn: symdata.fqn}) ',
                      'SET sym += symdata ',
                      'CREATE (pkg)-[:DEFINES]->(sym) ',
                      ';', $nl,
                      ':commit', $nl,

                      ':begin', $nl,
                      ':param deps => ',
                      st:to-neo4j-attrs(
                        st:array(
                          for $dep in neo:get-package-deps( . )
                            return st:item( neo:gen-dep-data( $dep ) ) ) ),
                      $nl,
                      'UNWIND $deps AS depdata ',
                      'MATCH (from:TameSymbol {fqn: depdata.from}) ',
                      'UNWIND depdata.to AS toname ',
                      'MERGE (to:TameSymbol {fqn: toname}) ',
                      'CREATE (from)-[:USES]->(to)',
                      ';', $nl )" />


  <!--
  <apply-templates select="preproc:sym-deps/preproc:sym-dep" />
  <apply-templates select="preproc:sym-deps/preproc:sym-dep/preproc:sym-ref" />
-->
</template>


<function name="neo:sym-src" as="xs:string">
  <param name="sym" as="element( preproc:sym )" />

  <sequence select="if ( $sym/@src and not( $sym/@src = '' ) ) then
                        neo:package-lookup( $sym/@src )/@name
                      else
                        $sym/ancestor::lv:package/@name" />
</function>


<function name="neo:sym-fqn" as="xs:string">
  <param name="sym" as="element( preproc:sym )" />

  <sequence select="concat( $fqn-prefix, '/',
                            neo:sym-src( $sym ), '/', $sym/@name )" />
</function>


<function name="neo:dict-from-sym" as="element( st:dict )">
  <param name="sym" as="element( preproc:sym )" />

  <variable name="attrs" as="attribute()+">
    <sequence select="$sym/@*" />

    <attribute name="fqn" select="neo:sym-fqn( $sym )" />
  </variable>

  <sequence select="st:dict( st:items-from-attrs( $attrs ) )" />
</function>


<function name="neo:package-lookup" as="element( lv:package )">
  <param name="src" as="xs:string?" />

  <sequence select="if ( $src and not( $src = '' ) ) then
                        document( concat( $src, '.xmlo' ), $neo:orig-root )/lv:*
                      else
                        $neo:orig-package" />
</function>


<function name="neo:is-mergable" as="xs:boolean">
  <param name="sym" as="element( preproc:sym )" />

  <!-- externs aren't resolved until linking, so we cannot hope to merge
       them here -->
  <sequence select="( exists( $sym/@preproc:generated-from )
                      or $sym/@type = 'cgen'
                      or ( $sym/@type = 'tpl'
                           and $sym/@preproc:generated = 'true' ) )
                    and not( $sym/@extern = 'true' )" />
</function>


<function name="neo:is-mergable-ref" as="xs:boolean">
  <param name="sym-ref" as="element( preproc:sym-ref )" />

  <variable name="name" as="xs:string"
            select="$sym-ref/@name" />

  <variable name="sym" as="element( preproc:sym )"
            select="$sym-ref/ancestor::lv:package/preproc:symtable
                      /preproc:sym[ @name = $name ]" />

  <sequence select="neo:is-mergable( $sym )" />
</function>


<function name="neo:is-mergable-dep" as="xs:boolean">
  <param name="sym-dep" as="element( preproc:sym-dep )" />

  <variable name="name" as="xs:string"
            select="$sym-dep/@name" />

  <variable name="sym" as="element( preproc:sym )"
            select="$sym-dep/ancestor::lv:package/preproc:symtable
                      /preproc:sym[ @name = $name ]" />

  <sequence select="neo:is-mergable( $sym )" />
</function>


<function name="neo:get-package-syms" as="element( preproc:sym )*">
  <param name="symtable" as="element( preproc:symtable )" />

  <sequence select="$symtable/preproc:sym[
                      not( @src )
                      and not( neo:is-mergable( . ) ) ]" />
</function>


<function name="neo:get-package-deps" as="element( preproc:sym-dep )*">
  <param name="package" as="element( lv:package )" />

  <sequence select="$package/preproc:sym-deps/preproc:sym-dep[
                      not( neo:is-mergable-dep( . ) ) ]" />
</function>


<function name="neo:get-sym-deps-by-ref" as="element( preproc:sym-ref )*">
  <param name="sym-ref" as="element( preproc:sym-ref )" />

  <variable name="name" as="xs:string"
            select="$sym-ref/@name" />

  <variable name="local-deps" as="element( preproc:sym-dep )?"
            select="$sym-ref/ancestor::preproc:sym-deps/preproc:sym-dep[
                      @name = $name ]" />

  <variable name="deps" as="element( preproc:sym-dep )"
            select="if ( exists( $local-deps ) ) then
                        $local-deps
                      else
                        neo:package-lookup-by-ref( $sym-ref )
                          /preproc:sym-deps/preproc:sym-dep[ @name = $name ]" />

  <sequence select="neo:get-sym-deps( $deps )" />
</function>


<function name="neo:package-lookup-by-ref" as="element( lv:package )">
  <param name="sym-ref" as="element( preproc:sym-ref )" />

  <variable name="name" as="xs:string"
            select="$sym-ref/@name" />

  <variable name="sym" as="element( preproc:sym )"
            select="$sym-ref/ancestor::lv:package/preproc:symtable
                      /preproc:sym[ @name = $name ]" />

  <sequence select="neo:package-lookup( $sym/@src )" />
</function>


<function name="neo:is-ignorable-ref" as="xs:boolean">
  <param name="sym-ref" as="element( preproc:sym-ref )" />

  <!-- much quicker check than using neo:is-mergable -->
  <sequence select="$sym-ref/@name = '_CMATCH_'" />
</function>


<function name="neo:get-sym-deps" as="element( preproc:sym-ref )*">
  <param name="sym-dep" as="element( preproc:sym-dep )" />

  <sequence select="for $ref in $sym-dep/preproc:sym-ref[
                                  not( neo:is-ignorable-ref( . ) ) ]
                      return if ( neo:is-mergable-ref( $ref ) ) then
                          neo:get-sym-deps-by-ref( $ref )
                        else
                          $ref" />
</function>


<function name="neo:gen-dep-data" as="element( st:dict )">
  <param name="dep" as="element( preproc:sym-dep )" />

  <variable name="refs" as="element( preproc:sym-ref )*"
            select="neo:get-sym-deps( $dep )" />

  <variable name="sym" as="element( preproc:sym )"
            select="$dep/ancestor::lv:package/preproc:symtable
                      /preproc:sym[ @name = $dep/@name ]" />

  <sequence select="st:dict( (
                      st:item( neo:sym-fqn( $sym ), 'from' ),
                      st:item(
                        st:array(
                          for $ref in $refs
                            return st:item( neo:sym-ref-fqn( $ref ) ) ),
                        'to' ) ) )" />
</function>


<function name="neo:sym-ref-fqn" as="xs:string">
  <param name="sym-ref" as="element( preproc:sym-ref )" />

  <variable name="sym" as="element( preproc:sym )"
            select="$sym-ref/ancestor::lv:package/preproc:symtable
                      /preproc:sym[ @name = $sym-ref/@name ]" />

  <sequence select="neo:sym-fqn( $sym )" />
</function>

</stylesheet>

