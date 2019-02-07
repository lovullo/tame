<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Generates a symbol table from fully a expanded (preprocessed) package

  Copyright (C) 2016, 2018 R-T Specialty, LLC.

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
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:symtable="http://www.lovullo.com/tame/symtable"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  exclude-result-prefixes="xs symtable">


<xsl:include href="path.xsl" />
<xsl:include href="../../tame/src/symtable.xsl" />
<xsl:include href="../../tame/src/symtable/symbols.xsl" />


<!-- we will recurse through the entire tree rather than performing a series of
     xpaths that are likely to hit the same nodes many times -->
<xsl:template match="*" mode="preproc:symtable" priority="1">
  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>

<xsl:template match="text()" mode="preproc:symtable" priority="9">
  <!-- output nothing -->
</xsl:template>


<!-- an alternative way to invoke the preproc:sym-discover template; useful for
     use on variables -->
<xsl:template match="*" mode="preproc:sym-discover" as="element()">
  <xsl:param name="orig-root" />

  <xsl:call-template name="preproc:sym-discover">
    <xsl:with-param name="orig-root" select="$orig-root" />
  </xsl:call-template>
</xsl:template>


<xsl:template match="preproc:*" mode="preproc:sym-discover"
              as="element()" priority="9">
  <xsl:sequence select="." />
</xsl:template>


<!--
  Other systems may contribute to a symbol table by invoking this template and
  supplying the necessary preproc:symtable templates

  TODO: This guy needs some refactoring
-->
<xsl:template name="preproc:sym-discover" as="element()">
  <xsl:param name="orig-root" />

  <xsl:variable name="this-pkg" as="element( lv:package )"
                select="." />

  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:variable name="new">
      <xsl:message>
        <xsl:text>[preproc/symtable] discovering symbols...</xsl:text>
      </xsl:message>

      <preproc:syms>
        <xsl:apply-templates mode="preproc:symtable">
          <!-- we only need this param for the root children, so this is the only
               template application that passes this -->
          <xsl:with-param name="orig-root" select="$orig-root" />
        </xsl:apply-templates>
      </preproc:syms>
    </xsl:variable>

    <!-- gather a list of overrides -->
    <xsl:variable name="overrides" select="
        $new/preproc:syms/preproc:sym[ @override='true' ]
      " />

    <!-- check for duplicates -->
    <xsl:variable name="symdup" as="element( preproc:sym )*"
                  select="symtable:find-duplicates( $new/preproc:syms )" />

    <!-- overrides that override nothing may be the sign of a bug (expectation
         of something that isn't there) -->
    <xsl:for-each select="$overrides[ not( @name=$symdup/@name ) ]">
      <xsl:message>
        <xsl:text>[preproc/symtable] warning: symbol /</xsl:text>
        <xsl:value-of select="$this-pkg/@name" />
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text> has @override set, but does not override anything</xsl:text>
      </xsl:message>
    </xsl:for-each>

    <!-- perform non-override duplicate checks (TODO: @ignore-dup is
         intended to be temporary while map __head and __tail
         generation conflicts are resolved)  -->
    <xsl:variable name="symdup-problems" as="element( preproc:sym )*" select="
        $symdup[
          not( @name = $overrides/@name
               and @virtual = 'true' )
          and not( @ignore-dup = 'true' ) ]
      " />

    <xsl:for-each select="$symdup-problems">
      <xsl:variable name="dupname" select="@name" />

      <xsl:choose>
        <!-- attempt to override a non-virtual symbol -->
        <xsl:when test="@name=$overrides/@name and not( @virtual='true' )">
          <xsl:message>
            <xsl:text>[preproc/symtable] error: cannot override non-virtual symbol /</xsl:text>
            <xsl:value-of select="$this-pkg/@name" />
            <xsl:text>/</xsl:text>
            <xsl:value-of select="@name" />
          </xsl:message>
        </xsl:when>

        <!-- just a plain duplicate -->
        <xsl:otherwise>
          <xsl:message>
            <xsl:text>[preproc/symtable] error: duplicate symbol /</xsl:text>
            <xsl:value-of select="$this-pkg/@name" />
            <xsl:text>/</xsl:text>
            <xsl:value-of select="@name" />
            <xsl:text> (defined in ./</xsl:text>

            <!-- output sources -->
            <xsl:for-each select="
                $new/preproc:syms/preproc:sym[
                  @name=$dupname
                  and @src
                  and not( @extern='true' )
                  and not( @name=$overrides/@name and @virtual='true' )
                ]
              ">
              <xsl:if test="position() gt 1">
                <xsl:text> and ./</xsl:text>
              </xsl:if>

              <xsl:value-of select="@src" />
            </xsl:for-each>
            <xsl:text>)</xsl:text>

            <!-- if virtual, suggest override as an alternative solution -->
            <xsl:if test="@virtual='true'">
              <xsl:text>; did you forget @override?</xsl:text>
            </xsl:if>
          </xsl:message>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <!-- terminate if any duplicates are found, dumping documents for debugging -->
    <xsl:if test="count( $symdup-problems ) gt 0">
      <xsl:message>~~~~[begin document dump]~~~~</xsl:message>
      <xsl:message select="$this-pkg" />
      <xsl:message>~~~~[end document dump]~~~~</xsl:message>

      <xsl:message>~~~~[begin symbol dump]~~~~</xsl:message>
      <xsl:message select="$new" />
      <xsl:message>~~~~[end symbol dump]~~~~</xsl:message>

      <xsl:message terminate="yes">
        <xsl:text>[preproc/symtable] fatal: aborting due to symbol errors</xsl:text>
      </xsl:message>
    </xsl:if>


    <xsl:variable name="result" as="element( preproc:symtable )">
      <preproc:symtable>
        <!-- copy any existing symbols table -->
        <preproc:syms>
          <xsl:sequence select="preproc:symtable/preproc:sym" />
          <xsl:sequence select="$new/preproc:syms/preproc:sym" />
        </preproc:syms>
      </preproc:symtable>
    </xsl:variable>

    <!-- output the symbols, checking for duplicates -->
    <preproc:symtable>
      <!-- validate imported externs -->
      <xsl:variable name="extresults" as="element( preproc:syms )">
        <xsl:call-template name="preproc:symtable-process-extern">
          <xsl:with-param name="result" select="$result" />
        </xsl:call-template>
      </xsl:variable>

      <!-- remove duplicates (if any) -->
      <xsl:sequence select="
          $extresults/preproc:sym[
            not( @name=preceding-sibling::preproc:sym/@name )
          ]
          , $extresults//preproc:error
        " />

      <!-- process symbols (except imported externs) -->
      <xsl:variable name="newresult" as="element( preproc:syms )">
        <xsl:call-template name="preproc:symtable-process-symbols">
          <xsl:with-param name="extresults" select="$extresults" />
          <xsl:with-param name="new" select="$new/preproc:syms" />
          <xsl:with-param name="this-pkg" select="$this-pkg" />
        </xsl:call-template>
      </xsl:variable>

      <xsl:variable name="dedup" as="element( preproc:sym )*"
                    select="$newresult/preproc:sym[
                            not(
                              (
                                @pollute='true'
                                and not( @type )
                                and (
                                  @name=preceding-sibling::preproc:sym/@name
                                  or @name=$newresult/preproc:sym[ @type ]/@name
                                )
                              )
                              or (
                                @local = 'true'
                                and @name = following-sibling::preproc:sym[
                                  not( @local = 'true' )
                                ]/@name
                              )
                            )
                          ]" />


      <xsl:apply-templates mode="preproc:symtable-complete"
                           select="$dedup">
        <xsl:with-param name="syms" select="$dedup" />
      </xsl:apply-templates>
    </preproc:symtable>

    <xsl:message>
      <xsl:text>[preproc/symtable] done.</xsl:text>
    </xsl:message>

    <!-- copy all of the original elements after the symbol table; we're not
         outputting them as we go, so we need to make sure that we don't get
         rid of them; discard any existing symbol table -->
    <xsl:apply-templates mode="preproc:symtable-inject" />
  </xsl:copy>
</xsl:template>


<xsl:template match="preproc:symtable" mode="preproc:symtable-inject"
              priority="5">
  <!-- strip old symbol table -->
</xsl:template>

<xsl:template match="*" mode="preproc:symtable-inject">
  <xsl:sequence select="." />
</xsl:template>


<xsl:template name="preproc:symtable-process-extern"
              as="element( preproc:syms )">
  <xsl:param name="result" as="element( preproc:symtable )" />

  <xsl:variable name="syms" as="element( preproc:syms )"
                select="$result/preproc:syms" />

  <preproc:syms>
    <xsl:for-each select="$syms/preproc:sym[
                            @extern='true'
                            and @src
                            and not( @held ) ]">
      <xsl:variable name="name" select="@name" />

      <!-- our value may be concrete or an extern itself; we may also import
           a package with a concrete definition -->
      <xsl:variable name="ours" select="
          $syms/preproc:sym[
            @name=$name
            and (
              not( @src )
              or ( @dtype and @src and not( @extern ) )
            )
          ]
        " />

      <xsl:choose>
        <!-- we have our own symbol; ensure the important values match the
             expected (unless @dim has not yet been resolved) -->
        <!-- XXX: the @dim='?' check leaves room for a dimension mismatch to
             slip through; re-order checks (see package.xsl) as necessary to
             ensure that this doesn't happen -->
        <xsl:when test="$ours">
          <xsl:if test="
             not(
                @type=$ours/@type
                and @dtype=$ours/@dtype
                and (
                  @dim=$ours/@dim
                  or $ours/@dim='?'
                )
              )
            ">

            <preproc:error>
              <xsl:text>extern mismatch: '</xsl:text>
                <xsl:value-of select="@name" />
              <xsl:text>' (imported from </xsl:text>
                <xsl:value-of select="@src" />
              <xsl:text>)</xsl:text>

              <xsl:for-each select="@type, @dtype, @dim">
                <xsl:variable name="aname" select="local-name()" />
                <xsl:text>; </xsl:text>

                <xsl:value-of select="local-name()" />
                <xsl:text>=</xsl:text>
                  <xsl:value-of select="$ours/@*[ local-name() = $aname ]" />
                <xsl:text>, </xsl:text>
                  <xsl:value-of select="." />
                <xsl:text> expected</xsl:text>
              </xsl:for-each>
            </preproc:error>
          </xsl:if>

          <!-- N.B.: there could potentially be multiple matches -->
          <!-- TODO: pollution should be removed and l:resolv-extern
               in the linker should look up the package that should
               include the resolved extern from the processing stack -->
          <preproc:sym pollute="true">
            <xsl:sequence select="$ours[1]/@*|*" />
          </preproc:sym>
        </xsl:when>

        <!-- we do not have our own symbol matching this extern;
             ignore this for now, as it may be handled by a future
             template expansion -->
        <xsl:otherwise>
          <preproc:sym held="true">
            <xsl:sequence select="@*" />
          </preproc:sym>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </preproc:syms>
</xsl:template>


<xsl:function name="preproc:final-extern-check" as="element(preproc:error )*">
  <xsl:param name="symtable" as="element( preproc:symtable )" />

  <!-- any remaining unresolved externs at this point are bad -->
  <xsl:for-each select="$symtable/preproc:sym[
                          @extern = 'true'
                          and @src ]">

    <!-- since @missing may be provided, let's include the actual
         symbol in the runlog for debugging -->
    <xsl:message select="'[preproc] missing extern: ', @name" />

    <preproc:error>
      <xsl:choose>
        <xsl:when test="@missing and not( @missing = '' )">
          <xsl:value-of select="normalize-space( @missing )" />
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>unresolved extern '</xsl:text>
          <xsl:value-of select="@name" />
          <xsl:text>'</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:text> (required by </xsl:text>
      <xsl:value-of select="@src" />
      <xsl:text>)</xsl:text>
    </preproc:error>
  </xsl:for-each>
</xsl:function>


<xsl:template name="preproc:symtable-process-symbols">
  <xsl:param name="extresults" as="element( preproc:syms )" />
  <xsl:param name="new"        as="element( preproc:syms )" />
  <xsl:param name="this-pkg"   as="element( lv:package )" />

  <preproc:syms>
    <xsl:variable name="cursym" as="element( preproc:sym )*"
                  select="preproc:symtable/preproc:sym[
                            not( @held = 'true' ) ]" />

    <xsl:sequence select="$cursym" />

    <xsl:message>
      <xsl:text>[preproc/symtable] processing symbol table...</xsl:text>
    </xsl:message>

    <xsl:for-each select="$new/preproc:sym[ not( @extern='true' and @src ) ]">
      <xsl:variable name="name" select="@name" />
      <xsl:variable name="src" select="@src" />
      <xsl:variable name="dupall" select="
          (
            preceding-sibling::preproc:sym,
            $cursym,
            $extresults/preproc:sym
          )[
            @name=$name
          ]
        " />
      <xsl:variable name="dup" select="
          $dupall[
            not(
              @src=$src
              or ( not( @src ) and not( $src ) )
            )
          ]
        " />

      <xsl:choose>
        <xsl:when test="@pollute='true' and not( @type )">
          <!-- we'll strip these out later -->
          <xsl:sequence select="." />
        </xsl:when>

        <!-- note that dupall uses preceding-sibling, which will catch
             duplicates in that case even if @override is not set -->
        <xsl:when test="following-sibling::preproc:sym[ @name=$name and @override='true' ]">
          <!-- overridden; we're obsolete :( -->
        </xsl:when>

        <!-- if we've gotten this far, then the override is good; clear it -->
        <xsl:when test="@override='true'">
          <xsl:copy>
            <xsl:sequence select="@*[ not( name()='override' ) ], *" />
          </xsl:copy>
        </xsl:when>

        <!-- if we have already imported the symbol as local, but this one
             is non-local (exportable), then this one takes precedence -->
        <xsl:when test="not( @local = 'true' )
                          and $dupall[ @local = 'true' ]
                          and not( $dupall[ not( @local = 'true' ) ] )">
          <xsl:sequence select="." />
        </xsl:when>

        <xsl:when test="$dupall[ @type ]">
          <!-- there is already a symbol of this name from the same package;
               let's not add duplicates -->
        </xsl:when>

        <xsl:otherwise>
          <!-- this symbol is good; use it -->
          <xsl:sequence select="." />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </preproc:syms>
</xsl:template>


<xsl:template match="preproc:symtable" mode="preproc:symtable" priority="9">
  <!-- ignore existing symbol tables (for now at least) -->
</xsl:template>


<!-- do not re-import symbol tables that have already been imported -->
<xsl:template match="lv:import[
    @package=root(.)/preproc:symtable/preproc:sym[
      @dtype
    ]/@src
  ]"
  mode="preproc:symtable" priority="9">
</xsl:template>


<xsl:template name="preproc:symimport" match="lv:import[ @package ]" mode="preproc:symtable" priority="8">
  <xsl:param name="orig-root" />
  <xsl:param name="package" select="@package" />
  <xsl:param name="export" select="@export" />
  <xsl:param name="no-extclass" select="@no-extclass" />
  <xsl:param name="keep-classes" select="@keep-classes" />

  <xsl:variable name="path" as="xs:string"
                select="concat( $package, '.xmlo' )" />
  <xsl:variable name="syms"
                select="document( $path, $orig-root )/lv:*/preproc:symtable" />

  <xsl:variable name="import-path" select="$package" />

  <xsl:variable name="src-root" as="xs:string"
                select="ancestor::lv:package/@__rootpath" />
  <xsl:variable name="src-name" as="xs:string"
                select="ancestor::lv:package/@name" />

  <!-- if they're including a program package, do they realize what they're
       doing!? -->
  <!-- FIXME: @allow-nonpkg is no longer accurate terminology; change to
       @allow-nonprg -->
  <xsl:if test="
      not( @allow-nonpkg = 'true' )
      and $syms/parent::lv:package[ @program='true' ]
      ">
    <xsl:message terminate="yes">
      <xsl:text>[preproc/symtable] error: refusing to import non-package </xsl:text>
      <xsl:value-of select="$import-path" />
      <xsl:text>; use @allow-nonpkg to force (if you know what you are doing)</xsl:text>
    </xsl:message>
  </xsl:if>

  <!-- to keep everything consistent and to simplify package equality
       assertions, resolve relative paths -->
  <xsl:variable name="import-default-path" select="$import-path" />

  <xsl:message>
    <xsl:text>[preproc/symtable] importing symbol table of </xsl:text>
    <xsl:value-of select="$import-path" />
    <xsl:text>...</xsl:text>
  </xsl:message>

  <!-- attempt to import symbols from the processed package -->
  <xsl:if test="not( $syms )">
    <xsl:message terminate="yes">
      <xsl:text>[preproc/symtable] internal error: </xsl:text>
      <xsl:text>failed to locate symbol table: </xsl:text>
      <xsl:value-of select="$path" />
    </xsl:message>
  </xsl:if>

  <!-- copy directly into symbol table, setting external source; local symbols
       will not be imported -->
  <xsl:for-each select="
      $syms/preproc:sym[
        (
          not( @local='true' )
          or @pollute='true'
          or (
            ( @type='class' or @type='cgen' )
            and $keep-classes='true'
          )
        )
        and not( $no-extclass='true' and @extclass='true' )
      ]
    ">
    <xsl:copy>
      <xsl:choose>
        <!-- pollution should have only the name and pollution status copied
             over, which has the effect of reserving the symbol but not
             providing enough information to actually make use of it; only
             strip the data if this is a second-hand import -->
        <!-- TODO: this list has gotten too large, but reducing it will require
             refactoring other compilers and may reduce performance -->
        <xsl:when test="@pollute='true'
                        and @local='true'
                        and not( @extern='true' )">
          <xsl:sequence select="@name, @src, @pollute, @parent, @extclass" />
        </xsl:when>

        <!-- copy all the symbol information -->
        <xsl:otherwise>
          <xsl:sequence select="@*" />
        </xsl:otherwise>
      </xsl:choose>

      <!-- all imported symbols are implicitly local (so including one package
           will not include symbols down the entire hierarchy), unless the
           symbol is explicitly marked global or @export was provided on the
           import node -->
      <xsl:if test="not( $export='true' )">
        <xsl:attribute name="local" select="'true'" />
      </xsl:if>

      <!-- determine the relative path to the import -->
      <xsl:attribute name="src">
        <xsl:choose>
          <!-- if no @src is set, then the answer is simple: the relative path is
               the import path -->
          <xsl:when test="not( @src )">
            <xsl:value-of select="$import-default-path" />
          </xsl:when>

          <!-- otherwise, we need to merge the import path into the existing
               relative path by prepending the import path (sans the package name
               itself) onto the existing relative path and resolving relative
               paths -->
          <xsl:otherwise>
            <xsl:sequence select="preproc:resolve-relative-import(
                                    $src-root,
                                    $src-name,
                                    $import-path,
                                    @src )" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>

      <!-- children should always be copied, unless poluting -->
      <xsl:if test="not( @pollute='true' and @local='true' )">
        <xsl:sequence select="preproc:*" />
      </xsl:if>
    </xsl:copy>
  </xsl:for-each>
</xsl:template>


<xsl:template match="lv:rate" mode="preproc:symtable" priority="5">
  <xsl:variable name="external"  select="boolean( @external='true' )" />

  <preproc:sym name="{@yields}" type="rate"
    extclass="{$external}"
    local="{@local}" dtype="float" dim="0" tex="{@sym}">

    <xsl:if test="@preproc:generated = 'true'">
      <xsl:attribute name="local" select="'true'" />
    </xsl:if>
  </preproc:sym>

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<xsl:template match="lv:const" mode="preproc:symtable" priority="5">
  <xsl:variable name="dim">
    <xsl:choose>
      <!-- TODO: matrix/vector predicate to support either type via
           @values -->
      <xsl:when test="./lv:set or @values">
        <xsl:text>2</xsl:text>
      </xsl:when>

      <xsl:when test="./lv:item">
        <xsl:text>1</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>0</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- TODO: remove magic support -->
  <preproc:sym name="{@name}"
    magic="{boolean( @magic='true' )}"
    type="const" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}">

    <!-- may or may not exist -->
    <xsl:sequence select="@value" />
  </preproc:sym>

  <!-- for performance, we will not recurse any further; the rest are simply
       data declarations -->
</xsl:template>


<xsl:template match="c:*[ @generates ]" mode="preproc:symtable" priority="5">
  <!-- it's possible that templates generating rate blocks will cause nested
       rate blocks, so only take the first ancestor -->
  <xsl:variable name="parent" as="element( lv:rate )"
                select="ancestor::lv:rate[1]" />

  <xsl:variable name="dim" as="xs:integer"
                select="if ( @dim ) then @dim else 1" />

  <preproc:sym name="{@generates}"
    parent="{$parent/@yields}"
    type="gen" dtype="float" dim="{$dim}" desc="{@desc}" tex="{@sym}">

    <xsl:if test="@preproc:generated = 'true'">
      <xsl:attribute name="local" select="'true'" />
    </xsl:if>
  </preproc:sym>

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<!-- note the @dim value; this is determined later from its dependencies -->
<xsl:template match="lv:classify" mode="preproc:symtable" priority="5">
  <xsl:variable name="external"  select="boolean( @external='true' )" />
  <xsl:variable name="terminate" select="boolean( @terminate='true' )" />

  <xsl:variable name="is-generated" as="xs:boolean"
                select="@preproc:generated = 'true'" />

  <preproc:sym name=":class:{@as}"
    extclass="{$external}" terminate="{$terminate}"
    type="class" dim="?" desc="{@desc}" yields="{@yields}"
    orig-name="{@as}">

    <xsl:if test="$is-generated">
      <xsl:attribute name="local" select="'true'" />
    </xsl:if>

    <!-- copy preprocessor metadata to symbol for easy reference -->
    <xsl:sequence select="@preproc:*" />
  </preproc:sym>

  <!-- generator if @yields is provided (note that we also have a @yields above
       to avoid scanning separate object files for such common information)
       -->
  <xsl:if test="@yields">
    <preproc:sym name="{@yields}"
      parent=":class:{@as}"
      extclass="{$external}" terminate="{$terminate}"
      type="cgen" dtype="boolean" dim="?" desc="{@desc}">

      <xsl:if test="@preproc:yields-generated">
        <xsl:attribute name="preproc:generated" select="'true'" />
      </xsl:if>

      <!-- we only want to mark as local if $is-generated, not
           @preproc:yields-generated, beacuse otherwise class yield lookups
           would not always work in packages -->
      <xsl:if test="$is-generated">
        <xsl:attribute name="local" select="'true'" />
      </xsl:if>

      <xsl:sequence select="@preproc:*" />
    </preproc:sym>
  </xsl:if>

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<xsl:template match="lv:typedef" mode="preproc:symtable" priority="5">
  <!-- FIXME: this is a kluge -->
  <xsl:variable name="dtype" as="xs:string?"
                select="if ( lv:base-type ) then
                          @name
                        else
                          lv:enum/@type
                          , lv:union/lv:typedef[1]/lv:enum/@type" />

  <xsl:if test="not( $dtype )">
    <xsl:message terminate="yes">
      <xsl:text>[preproc/symtable] internal error: </xsl:text>
      <xsl:text>failed to resolve type primitve of `</xsl:text>
      <xsl:value-of select="@name" />
      <xsl:text>'</xsl:text>
    </xsl:message>
  </xsl:if>

  <preproc:sym name="{@name}" dtype="{$dtype}"
    type="type" dim="0" desc="{@desc}" />

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<xsl:template match="lv:typedef/lv:enum/lv:item" mode="preproc:symtable" priority="5">
  <xsl:variable name="dtype" select="parent::lv:enum/@type" />

  <preproc:sym name="{@name}" value="{@value}"
    type="const" dtype="{$dtype}" dim="0" desc="{@desc}" />
</xsl:template>


<xsl:template match="lv:function" mode="preproc:symtable" priority="5">
  <!-- default TeX symbol to the function name -->
  <xsl:variable name="tex">
    <xsl:choose>
      <xsl:when test="@sym">
        <xsl:value-of select="@sym" />
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>\textrm{</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- TODO: determine return data type from tail -->
  <!-- TODO: same for dim -->
  <!-- functions can have circular dependencies (recursion) -->
  <preproc:sym name="{@name}" type="func" dtype="float" dim="0" desc="{@desc}"
               tex="{$tex}" allow-circular="true">

    <!-- we need to include the argument order and symbol refs so that the
         compiler knows how to call the function -->
    <xsl:variable name="fname" select="@name" />
    <xsl:for-each select="lv:param">
      <preproc:sym-ref name=":{$fname}:{@name}" />
    </xsl:for-each>
  </preproc:sym>

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<!--
  Function parameters are local to the function and are represented differently
  in the symbol table than most other symbols. In particular:
    - They have type lparam, not param
    - Their name begins with a colon, which is normally invalid (ensuring that
      there will be no naming conflicts)
    - The name following the colon is the concatenation of the function name,
      an underscore and the param name
-->
<xsl:template match="lv:function/lv:param" mode="preproc:symtable" priority="6">
  <!-- determine number of dimensions -->
  <xsl:variable name="dim">
    <xsl:call-template name="preproc:param-dim" />
  </xsl:variable>

  <xsl:variable name="fname" select="parent::lv:function/@name" />

  <preproc:sym name=":{$fname}:{@name}" parent="{$fname}" varname="{@name}"
    type="lparam" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}"
    no-deps="true">

    <!-- may or may not be defined -->
    <xsl:sequence select="@default" />
  </preproc:sym>
</xsl:template>


<!--
  Same concept as function params
-->
<xsl:template match="c:let/c:values/c:value" mode="preproc:symtable" priority="5">
  <xsl:variable name="name" select="@name" />

  <!-- determine number of dimensions -->
  <xsl:variable name="dim">
    <xsl:call-template name="preproc:param-dim" />
  </xsl:variable>

  <!-- the name is generated automatically by the preprocessor; the user cannot
       set it -->
  <xsl:variable name="lname" as="xs:string?"
                select="ancestor::c:let[1]/@name" />

  <!-- @lparent instead of @parent because the let does not actually exist
       as a symbol -->
  <preproc:sym name=":{$lname}:{@name}" local="true" varname="{@name}"
    type="lparam" dtype="{@type}" dim="{$dim}" desc="{@desc}" tex="{@sym}"
    lparent="{$lname}" no-deps="true" />

  <xsl:apply-templates mode="preproc:symtable" />
</xsl:template>


<xsl:template match="lv:extern" mode="preproc:symtable" priority="5">
  <preproc:sym desc="{@name} extern" extern="true" missing="{@missing}">
    <!-- copy all the user-supplied params -->
    <xsl:sequence select="@*" />
  </preproc:sym>
</xsl:template>


<xsl:template match="preproc:sym[ @type='param' ]" mode="preproc:symtable-complete" priority="5">
  <xsl:param name="syms" as="element( preproc:sym )*" />

  <!-- attempt to derive type information from a typedef -->
  <!-- TODO: also check symbol table after import (post-process) -->
  <xsl:variable name="type" select="@dtype" />
  <xsl:variable name="typedef" as="element( preproc:sym )?"
                select="$syms[ @type = 'type'
                               and @name = $type ]" />

  <xsl:if test="not( $typedef and $typedef/@dtype )">
    <xsl:message terminate="yes">
      <xsl:text>[preproc/symtable] internal error: </xsl:text>
      <xsl:text>failed to resolve type: </xsl:text>
      <xsl:value-of select="$type" />
    </xsl:message>
  </xsl:if>

  <!-- complete datatype with primitive -->
  <xsl:copy>
    <xsl:sequence select="@*" />
    <xsl:attribute name="dtype" select="$typedef/@dtype" />
  </xsl:copy>
</xsl:template>


<xsl:template match="*" mode="preproc:symtable-complete" priority="1">
  <!-- symbol does not need completion -->
  <xsl:sequence select="." />
</xsl:template>

<!--
  Determines param dimension from its string definition:
    vector = 1-dimensional;
    matrix = 2-dimensional;
    otherwise, scalar = 0-dimensional

  Other dimensions are certainly supported, but @set's syntax does not support
  their specification.
-->
<xsl:template name="preproc:param-dim">
  <xsl:choose>
    <xsl:when test="@set = 'vector'">
      <xsl:text>1</xsl:text>
    </xsl:when>

    <xsl:when test="@set = 'matrix'">
      <xsl:text>2</xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <xsl:text>0</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
