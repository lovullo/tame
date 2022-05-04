<?xml version="1.0" encoding="utf-8"?>
<!--
  Generates a symbol table from fully a expanded (preprocessed) package

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

  It is important that this table be generated after fully expanding all
  templates, macros, etc; otherwise, the table may be incomplete.

  The preproc:sym/@tex attribute is a TeX symbol used for typsetting. This
  process is not responsible for generating defaults; that should be done by the
  linker to ensure that there are no conflicts after all symbols are known.

  Here are the recognized types:
    rate   - lv:rate block
    gen    - generator (c:*/@generates)
    cgen   - class generator (lv:classify/@yields)
    class  - classification (lv:classify/@as)
    param  - global param (lv:param)
    lparam - local param (lv:function/lv:param)
    const  - global constant (lv:const; lv:enum/lv:item)
    tpl    - template (lv:template)
    type   - datatype (lv:typedef)
    func   - function

  Dimensions (think 0=point, 1=line, 2=plane, etc):
    0 - scalar
    1 - vector
    2 - matrix
    ...

  Symbols from imported packages will be consumed and added to the output,
  unless local; this has a similiar effect to including a C header file.
  External symbols are denoted by preproc:sym/@src, which specifies the name of
  the package from which it was imported. If an imported symbol conflicts with
  another symbol (imported or otherwise), an error will be produced. Symbols
  that are imported are implicitly marked as local.

  Certain symbols will "polute" the symbol table of every package that imports
  it, every package that imports that one, etc; this is for compatibility with
  the old system and will hopefully be phased out eventually. Pollution will
  reserve the symbol, but will not provide enough information about that symbol
  to be useful, which will ensure that a package has to import the symbol
  explicitly in order to actually make use of it.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:map="http://www.w3.org/2005/xpath-functions/map"
            xmlns:symtable="http://www.lovullo.com/tame/symtable"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:c="http://www.lovullo.com/calc"
            exclude-result-prefixes="xs symtable">


<include href="path.xsl" />
<include href="../../tame/src/symtable.xsl" />
<include href="../../tame/src/symtable/symbols.xsl" />


<!-- we will recurse through the entire tree rather than performing a series of
     xpaths that are likely to hit the same nodes many times -->
<template match="*" mode="preproc:symtable" priority="1">
  <apply-templates mode="preproc:symtable" />
</template>

<template match="text()" mode="preproc:symtable" priority="9">
  <!-- output nothing -->
</template>


<!-- an alternative way to invoke the preproc:sym-discover template; useful for
     use on variables -->
<template match="*" mode="preproc:sym-discover" as="element()">
  <param name="orig-root" />

  <call-template name="preproc:sym-discover">
    <with-param name="orig-root" select="$orig-root" />
  </call-template>
</template>


<template match="preproc:*" mode="preproc:sym-discover"
              as="element()" priority="9">
  <sequence select="." />
</template>


<!--
  Other systems may contribute to a symbol table by invoking this template and
  supplying the necessary preproc:symtable templates

  TODO: This guy needs some refactoring
-->
<template name="preproc:sym-discover" as="element()">
  <param name="orig-root" />

  <variable name="this-pkg" as="element( lv:package )"
                select="." />

  <copy>
    <sequence select="@*" />

    <variable name="new">
      <preproc:syms>
        <apply-templates mode="preproc:symtable">
          <!-- we only need this param for the root children, so this is the only
               template application that passes this -->
          <with-param name="orig-root" select="$orig-root" />
        </apply-templates>
      </preproc:syms>
    </variable>

    <!-- gather a list of overrides -->
    <variable name="overrides" select="
        $new/preproc:syms/preproc:sym[ @override='true' ]
      " />

    <!-- check for duplicates -->
    <variable name="symdup" as="element( preproc:sym )*"
                  select="symtable:find-duplicates( $new/preproc:syms )" />

    <!-- overrides that override nothing may be the sign of a bug (expectation
         of something that isn't there) -->
    <for-each select="$overrides[ not( @name=$symdup/@name ) ]">
      <message>
        <text>[preproc/symtable] warning: symbol /</text>
        <value-of select="$this-pkg/@name" />
        <text>/</text>
        <value-of select="@name" />
        <text> has @override set, but does not override anything</text>
      </message>
    </for-each>

    <!-- perform non-override duplicate checks (TODO: @ignore-dup is
         intended to be temporary while map __head and __tail
         generation conflicts are resolved)  -->
    <variable name="symdup-problems" as="element( preproc:sym )*" select="
        $symdup[
          not( @name = $overrides/@name
               and @virtual = 'true' )
          and not( @ignore-dup = 'true' ) ]
      " />

    <for-each select="$symdup-problems">
      <variable name="dupname" select="@name" />

      <choose>
        <!-- attempt to override a non-virtual symbol -->
        <when test="@name=$overrides/@name and not( @virtual='true' )">
          <message>
            <text>[preproc/symtable] error: cannot override non-virtual symbol /</text>
            <value-of select="$this-pkg/@name" />
            <text>/</text>
            <value-of select="@name" />
          </message>
        </when>

        <!-- just a plain duplicate -->
        <otherwise>
          <message>
            <text>[preproc/symtable] error: duplicate symbol /</text>
            <value-of select="$this-pkg/@name" />
            <text>/</text>
            <value-of select="@name" />
            <text> (defined in ./</text>

            <!-- output sources -->
            <for-each select="
                $new/preproc:syms/preproc:sym[
                  @name=$dupname
                  and @src
                  and not( @extern='true' )
                  and not( @name=$overrides/@name and @virtual='true' )
                ]
              ">
              <if test="position() gt 1">
                <text> and ./</text>
              </if>

              <value-of select="@src" />
            </for-each>
            <text>)</text>

            <!-- if virtual, suggest override as an alternative solution -->
            <if test="@virtual='true'">
              <text>; did you forget @override?</text>
            </if>
          </message>
        </otherwise>
      </choose>
    </for-each>

    <!-- terminate if any duplicates are found, dumping documents for debugging -->
    <if test="count( $symdup-problems ) gt 0">
      <message>~~~~[begin document dump]~~~~</message>
      <message select="$this-pkg" />
      <message>~~~~[end document dump]~~~~</message>

      <message>~~~~[begin symbol dump]~~~~</message>
      <message select="$new" />
      <message>~~~~[end symbol dump]~~~~</message>

      <message terminate="yes">
        <text>[preproc/symtable] fatal: aborting due to symbol errors</text>
      </message>
    </if>


    <variable name="result" as="element( preproc:symtable )">
      <preproc:symtable>
        <!-- copy any existing symbols table -->
        <preproc:syms>
          <sequence select="preproc:symtable/preproc:sym" />
          <sequence select="$new/preproc:syms/preproc:sym" />
        </preproc:syms>
      </preproc:symtable>
    </variable>

    <!-- output the symbols, checking for duplicates -->
    <preproc:symtable>
      <!-- validate imported externs -->
      <variable name="extresults" as="element( preproc:syms )">
        <call-template name="preproc:symtable-process-extern">
          <with-param name="result" select="$result" />
        </call-template>
      </variable>

      <sequence select="$extresults//preproc:error" />

      <!-- process symbols (except imported externs) -->
      <variable name="newresult" as="element( preproc:syms )">
        <call-template name="preproc:symtable-process-symbols">
          <with-param name="extresults" select="$extresults" />
          <with-param name="new" select="$new/preproc:syms" />
          <with-param name="this-pkg" select="$this-pkg" />
        </call-template>
      </variable>

      <apply-templates mode="preproc:symtable-complete"
                           select="$newresult/preproc:sym">
        <with-param name="syms" select="$newresult/preproc:sym" />
      </apply-templates>
    </preproc:symtable>

    <!-- copy all of the original elements after the symbol table; we're not
         outputting them as we go, so we need to make sure that we don't get
         rid of them; discard any existing symbol table -->
    <apply-templates mode="preproc:symtable-inject" />
  </copy>
</template>


<template match="preproc:symtable" mode="preproc:symtable-inject"
              priority="5">
  <!-- strip old symbol table -->
</template>

<template match="*" mode="preproc:symtable-inject">
  <sequence select="." />
</template>


<template name="preproc:symtable-process-extern"
              as="element( preproc:syms )">
  <param name="result" as="element( preproc:symtable )" />

  <variable name="syms" as="element( preproc:syms )"
                select="$result/preproc:syms" />

  <preproc:syms>
    <for-each select="$syms/preproc:sym[
                            @extern='true'
                            and @src
                            and not( @held ) ]">
      <variable name="name" select="@name" />

      <!-- our value may be concrete or an extern itself; we may also import
           a package with a concrete definition -->
      <variable name="ours" select="
          $syms/preproc:sym[
            @name=$name
            and (
              not( @src )
              or ( @dtype and @src and not( @extern ) )
            )
          ]
        " />

      <choose>
        <!-- we have our own symbol; ensure the important values match the
             expected (unless @dim has not yet been resolved) -->
        <!-- XXX: the @dim='?' check leaves room for a dimension mismatch to
             slip through; re-order checks (see package.xsl) as necessary to
             ensure that this doesn't happen -->
        <when test="$ours">
          <if test="
             not(
                @type=$ours/@type
                and (
                  not( @dtype or $ours/@dtype )
                  or @dtype = $ours/@dtype
                )
                and (
                  @dim=$ours/@dim
                  or $ours/@dim='?'
                )
              )
            ">

            <preproc:error>
              <text>extern mismatch: '</text>
                <value-of select="@name" />
              <text>' (imported from </text>
                <value-of select="@src" />
              <text>)</text>

              <for-each select="@type, @dtype, @dim">
                <variable name="aname" select="local-name()" />
                <text>; </text>

                <value-of select="local-name()" />
                <text>=</text>
                  <value-of select="$ours/@*[ local-name() = $aname ]" />
                <text>, </text>
                  <value-of select="." />
                <text> expected</text>
              </for-each>
            </preproc:error>
          </if>

          <!-- N.B.: there could potentially be multiple matches -->
          <!-- TODO: pollution should be removed and l:resolv-extern
               in the linker should look up the package that should
               include the resolved extern from the processing stack -->
          <preproc:sym pollute="true">
            <sequence select="$ours[1]/@*|*" />
          </preproc:sym>
        </when>

        <!-- we do not have our own symbol matching this extern;
             ignore this for now, as it may be handled by a future
             template expansion -->
        <otherwise>
          <preproc:sym held="true">
            <sequence select="@*" />
          </preproc:sym>
        </otherwise>
      </choose>
    </for-each>
  </preproc:syms>
</template>


<function name="preproc:final-extern-check" as="element(preproc:error )*">
  <param name="symtable" as="element( preproc:symtable )" />

  <!-- any remaining unresolved externs at this point are bad -->
  <for-each select="$symtable/preproc:sym[
                          @extern = 'true'
                          and @src ]">

    <!-- since @missing may be provided, let's include the actual
         symbol in the runlog for debugging -->
    <message select="'[preproc] missing extern: ', @name" />

    <preproc:error>
      <choose>
        <when test="@missing and not( @missing = '' )">
          <value-of select="normalize-space( @missing )" />
        </when>

        <otherwise>
          <text>unresolved extern '</text>
          <value-of select="@name" />
          <text>'</text>
        </otherwise>
      </choose>

      <text> (required by </text>
      <value-of select="@src" />
      <text>)</text>
    </preproc:error>
  </for-each>
</function>


<template name="preproc:symtable-process-symbols">
  <param name="extresults" as="element( preproc:syms )" />
  <param name="new"        as="element( preproc:syms )" />
  <param name="this-pkg"   as="element( lv:package )" />

  <variable name="cursym" as="element( preproc:sym )*"
                select="preproc:symtable/preproc:sym[
                          not( @held = 'true' ) ]" />

  <variable name="new-overrides-map" as="map( xs:string, element( preproc:sym ) )"
            select="map:merge(
                      for $sym in $new/preproc:sym[ @override = 'true' ]
                        return map{ string( $sym/@name ) : $sym } )" />

  <preproc:syms>
    <for-each-group select="$cursym,
                            $extresults/preproc:sym,
                            $new/preproc:sym[ not( @extern='true' and @src ) ]"
                    group-by="@name">
      <!-- Unfortuantely, <sort> in this context does not sort the resulting
           groups, so we must do so separately -->
      <variable name="sorted" as="element( preproc:sym )*">
        <perform-sort select="current-group()">
          <sort select="@local" />
          <sort select="@held" />
        </perform-sort>
      </variable>

      <!-- first symbol of this name with non-local taking precedence -->
      <variable name="first" select="$sorted[ 1 ]" />

      <variable name="name" select="current-grouping-key()" />
      <variable name="src" select="$first/@src" />

      <variable name="override" as="element( preproc:sym )?"
                select="$new-overrides-map( $name )" />

      <choose>
        <when test="$first/@pollute='true' and not( $first/@type )">
          <!-- we'll strip these out later -->
          <sequence select="$first" />
        </when>

        <!-- if we've gotten this far, then the override is good; clear it
             so as not to trigger override errors  -->
        <when test="$override">
          <preproc:sym>
            <sequence select="$override/@*[ not( name()='override' ) ]" />

            <!-- mark this has having been overridden for the linker (see
                 TAMER; we'll hopefully be getting rid of overrides in the
                 future) -->
            <attribute name="isoverride" select="'true'" />

            <sequence select="$override/*" />
          </preproc:sym>
        </when>

        <otherwise>
          <!-- this symbol is good; use it -->
          <sequence select="$first" />
        </otherwise>
      </choose>
    </for-each-group>
  </preproc:syms>
</template>


<template match="preproc:symtable" mode="preproc:symtable" priority="9">
  <!-- ignore existing symbol tables (for now at least) -->
</template>


<!-- do not re-import symbol tables that have already been imported -->
<template match="lv:import[
    @package=root(.)/preproc:symtable/preproc:sym[
      @dtype
    ]/@src
  ]"
  mode="preproc:symtable" priority="9">
</template>


<template name="preproc:symimport" match="lv:import[ @package ]" mode="preproc:symtable" priority="8">
  <param name="orig-root" />
  <param name="package" select="@package" />
  <param name="export" select="@export" />
  <param name="keep-classes" select="@keep-classes" />

  <variable name="path" as="xs:string"
                select="concat( $package, '.xmlo' )" />
  <variable name="syms"
                select="document( $path, $orig-root )/lv:*/preproc:symtable" />

  <variable name="import-path" select="$package" />

  <variable name="src-root" as="xs:string"
                select="ancestor::lv:package/@__rootpath" />
  <variable name="src-name" as="xs:string"
                select="ancestor::lv:package/@name" />

  <!-- if they're including a program package, do they realize what they're
       doing!? -->
  <!-- FIXME: @allow-nonpkg is no longer accurate terminology; change to
       @allow-nonprg -->
  <if test="
      not( @allow-nonpkg = 'true' )
      and $syms/parent::lv:package[ @program='true' ]
      ">
    <message terminate="yes">
      <text>[preproc/symtable] error: refusing to import non-package </text>
      <value-of select="$import-path" />
      <text>; use @allow-nonpkg to force (if you know what you are doing)</text>
    </message>
  </if>

  <!-- to keep everything consistent and to simplify package equality
       assertions, resolve relative paths -->
  <variable name="import-default-path" select="$import-path" />

  <!-- attempt to import symbols from the processed package -->
  <if test="not( $syms )">
    <message terminate="yes">
      <text>[preproc/symtable] internal error: </text>
      <text>failed to locate symbol table: </text>
      <value-of select="$path" />
    </message>
  </if>

  <!-- copy directly into symbol table, setting external source; local symbols
       will not be imported -->
  <for-each select="
      $syms/preproc:sym[
        not( @local='true' )
        or @pollute='true'
        or (
          ( @type='class' or @type='cgen' )
          and $keep-classes='true'
        )
      ]
    ">
    <copy>
      <choose>
        <!-- pollution should have only the name and pollution status copied
             over, which has the effect of reserving the symbol but not
             providing enough information to actually make use of it; only
             strip the data if this is a second-hand import -->
        <!-- TODO: this list has gotten too large, but reducing it will require
             refactoring other compilers and may reduce performance -->
        <when test="@pollute='true'
                        and @local='true'
                        and not( @extern='true' )">
          <sequence select="@name, @src, @pollute, @parent" />
        </when>

        <!-- copy all the symbol information -->
        <otherwise>
          <sequence select="@*" />
        </otherwise>
      </choose>

      <!-- all imported symbols are implicitly local (so including one package
           will not include symbols down the entire hierarchy), unless the
           symbol is explicitly marked global or @export was provided on the
           import node -->
      <if test="not( $export='true' )">
        <attribute name="local" select="'true'" />
      </if>

      <!-- determine the relative path to the import -->
      <attribute name="src">
        <choose>
          <!-- if no @src is set, then the answer is simple: the relative path is
               the import path -->
          <when test="not( @src )">
            <value-of select="$import-default-path" />
          </when>

          <!-- otherwise, we need to merge the import path into the existing
               relative path by prepending the import path (sans the package name
               itself) onto the existing relative path and resolving relative
               paths -->
          <otherwise>
            <sequence select="preproc:resolve-relative-import(
                                    $src-root,
                                    $src-name,
                                    $import-path,
                                    @src )" />
          </otherwise>
        </choose>
      </attribute>

      <!-- children should always be copied, unless poluting -->
      <if test="not( @pollute='true' and @local='true' )">
        <sequence select="preproc:*" />
      </if>
    </copy>
  </for-each>
</template>


<template match="lv:rate" mode="preproc:symtable" priority="5">
  <preproc:sym name="{@yields}" type="rate"
    local="{@local}" dtype="float" dim="0" tex="{@sym}">

    <if test="@preproc:generated = 'true'">
      <attribute name="local" select="'true'" />
    </if>
  </preproc:sym>

  <apply-templates mode="preproc:symtable" />
</template>


<template match="lv:const" mode="preproc:symtable" priority="5">
  <variable name="dim">
    <choose>
      <!-- TODO: matrix/vector predicate to support either type via
           @values -->
      <when test="./lv:set or @values">
        <text>2</text>
      </when>

      <when test="./lv:item">
        <text>1</text>
      </when>

      <otherwise>
        <text>0</text>
      </otherwise>
    </choose>
  </variable>

  <!-- TODO: remove magic support -->
  <preproc:sym name="{@name}"
    magic="{boolean( @magic='true' )}"
    type="const" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}">

    <!-- may or may not exist -->
    <sequence select="@value" />
  </preproc:sym>

  <!-- for performance, we will not recurse any further; the rest are simply
       data declarations -->
</template>


<template match="c:*[ @generates ]" mode="preproc:symtable" priority="5">
  <!-- it's possible that templates generating rate blocks will cause nested
       rate blocks, so only take the first ancestor -->
  <variable name="parent" as="element( lv:rate )"
                select="ancestor::lv:rate[1]" />

  <variable name="dim" as="xs:integer"
                select="if ( @dim ) then @dim else 1" />

  <preproc:sym name="{@generates}"
    parent="{$parent/@yields}"
    type="gen" dtype="float" dim="{$dim}" desc="{@desc}" tex="{@sym}">

    <if test="@preproc:generated = 'true'">
      <attribute name="local" select="'true'" />
    </if>
  </preproc:sym>

  <apply-templates mode="preproc:symtable" />
</template>


<!-- note the @dim value; this is determined later from its dependencies -->
<template match="lv:classify" mode="preproc:symtable" priority="5">
  <variable name="terminate" select="boolean( @terminate='true' )" />

  <variable name="is-generated" as="xs:boolean"
                select="@preproc:generated = 'true'" />

  <preproc:sym name=":class:{@as}"
    terminate="{$terminate}"
    type="class" dim="?" desc="{@desc}" yields="{@yields}"
    orig-name="{@as}">

    <if test="$is-generated">
      <attribute name="local" select="'true'" />
    </if>

    <!-- copy preprocessor metadata to symbol for easy reference -->
    <sequence select="@preproc:*" />
  </preproc:sym>

  <!-- generator if @yields is provided (note that we also have a @yields above
       to avoid scanning separate object files for such common information)
       -->
  <if test="@yields">
    <preproc:sym name="{@yields}"
      parent=":class:{@as}"
      terminate="{$terminate}"
      type="cgen" dtype="boolean" dim="?" desc="{@desc}">

      <if test="@preproc:yields-generated">
        <attribute name="preproc:generated" select="'true'" />
      </if>

      <!-- we only want to mark as local if $is-generated, not
           @preproc:yields-generated, beacuse otherwise class yield lookups
           would not always work in packages -->
      <if test="$is-generated">
        <attribute name="local" select="'true'" />
      </if>

      <sequence select="@preproc:*" />
    </preproc:sym>
  </if>

  <apply-templates mode="preproc:symtable" />
</template>


<template match="lv:typedef" mode="preproc:symtable" priority="5">
  <!-- FIXME: this is a kluge -->
  <variable name="dtype" as="xs:string?"
                select="if ( lv:base-type ) then
                          @name
                        else
                          lv:enum/@type
                          , lv:union/lv:typedef[1]/lv:enum/@type" />

  <if test="not( $dtype )">
    <message terminate="yes">
      <text>[preproc/symtable] internal error: </text>
      <text>failed to resolve type primitve of `</text>
      <value-of select="@name" />
      <text>'</text>
    </message>
  </if>

  <preproc:sym name="{@name}" dtype="{$dtype}"
    type="type" dim="0" desc="{@desc}" />

  <apply-templates mode="preproc:symtable" />
</template>


<template match="lv:typedef/lv:enum/lv:item" mode="preproc:symtable" priority="5">
  <variable name="dtype" select="parent::lv:enum/@type" />

  <preproc:sym name="{@name}" value="{@value}"
    type="const" dtype="{$dtype}" dim="0" desc="{@desc}" />
</template>


<template match="lv:function" mode="preproc:symtable" priority="5">
  <!-- default TeX symbol to the function name -->
  <variable name="tex">
    <choose>
      <when test="@sym">
        <value-of select="@sym" />
      </when>

      <otherwise>
        <text>\textrm{</text>
          <value-of select="@name" />
        <text>}</text>
      </otherwise>
    </choose>
  </variable>

  <!-- TODO: determine return data type from tail -->
  <!-- TODO: same for dim -->
  <!-- functions can have circular dependencies (recursion) -->
  <preproc:sym name="{@name}" type="func" dtype="float" dim="0" desc="{@desc}"
               tex="{$tex}" allow-circular="true">

    <!-- we need to include the argument order and symbol refs so that the
         compiler knows how to call the function -->
    <variable name="fname" select="@name" />
    <for-each select="lv:param">
      <preproc:sym-ref name=":{$fname}:{@name}" />
    </for-each>
  </preproc:sym>

  <apply-templates mode="preproc:symtable" />
</template>


<!--
  Function parameters are local to the function and are represented differently
  in the symbol table than most other symbols. In particular:
    - They have type lparam, not param
    - Their name begins with a colon, which is normally invalid (ensuring that
      there will be no naming conflicts)
    - The name following the colon is the concatenation of the function name,
      an underscore and the param name
-->
<template match="lv:function/lv:param" mode="preproc:symtable" priority="6">
  <!-- determine number of dimensions -->
  <variable name="dim">
    <call-template name="preproc:param-dim" />
  </variable>

  <variable name="fname" select="parent::lv:function/@name" />

  <preproc:sym name=":{$fname}:{@name}" parent="{$fname}" varname="{@name}"
    type="lparam" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}"
    no-deps="true">

    <!-- may or may not be defined -->
    <sequence select="@default" />
  </preproc:sym>
</template>


<!--
  Same concept as function params
-->
<template match="c:let/c:values/c:value" mode="preproc:symtable" priority="5">
  <variable name="name" select="@name" />

  <!-- determine number of dimensions -->
  <variable name="dim">
    <call-template name="preproc:param-dim" />
  </variable>

  <!-- the name is generated automatically by the preprocessor; the user cannot
       set it -->
  <variable name="lname" as="xs:string?"
                select="ancestor::c:let[1]/@name" />

  <!-- @lparent instead of @parent because the let does not actually exist
       as a symbol -->
  <preproc:sym name=":{$lname}:{@name}" local="true" varname="{@name}"
    type="lparam" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}"
    lparent="{$lname}" no-deps="true" />

  <apply-templates mode="preproc:symtable" />
</template>


<template match="lv:extern" mode="preproc:symtable" priority="5">
  <preproc:sym desc="{@name} extern" extern="true" missing="{@missing}">
    <!-- copy all the user-supplied params -->
    <sequence select="@*" />
  </preproc:sym>
</template>


<template match="preproc:sym[ @type='param' ]" mode="preproc:symtable-complete" priority="5">
  <param name="syms" as="element( preproc:sym )*" />

  <!-- attempt to derive type information from a typedef -->
  <!-- TODO: also check symbol table after import (post-process) -->
  <variable name="type" select="@dtype" />
  <variable name="typedef" as="element( preproc:sym )?"
                select="$syms[ @type = 'type'
                               and @name = $type ]" />

  <if test="not( $typedef and $typedef/@dtype )">
    <message terminate="yes">
      <text>[preproc/symtable] internal error: </text>
      <text>failed to resolve type: </text>
      <value-of select="$type" />
    </message>
  </if>

  <!-- complete datatype with primitive -->
  <copy>
    <sequence select="@*" />
    <attribute name="dtype" select="$typedef/@dtype" />
  </copy>
</template>


<template match="*" mode="preproc:symtable-complete" priority="1">
  <!-- symbol does not need completion -->
  <sequence select="." />
</template>

<!--
  Determines param dimension from its string definition:
    vector = 1-dimensional;
    matrix = 2-dimensional;
    otherwise, scalar = 0-dimensional

  Other dimensions are certainly supported, but @set's syntax does not support
  their specification.
-->
<template name="preproc:param-dim">
  <choose>
    <when test="@set = 'vector'">
      <text>1</text>
    </when>

    <when test="@set = 'matrix'">
      <text>2</text>
    </when>

    <otherwise>
      <text>0</text>
    </otherwise>
  </choose>
</template>

</stylesheet>
