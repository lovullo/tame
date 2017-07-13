<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Assembles code fragments into a final executable

  Copyright (C) 2016, 2017 LoVullo Associates, Inc.

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
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:l="http://www.lovullo.com/rater/linker"
  xmlns:log="http://www.lovullo.com/logger"
  xmlns:compiler="http://www.lovullo.com/rater/compiler"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<include href="../include/preproc/symtable.xsl" />
<include href="../include/util.xsl" />
<include href="js.xsl" />

<include href="linker/log.xsl" />

<!-- indentation makes dep lists easier to mentally process -->
<output indent="yes" />

<!-- optional fragments to include in exit block, comma-delimited
     (e.g. path/to/object/file/_fragment_)  -->
<param name="rater-exit-fragments" />

<!--
  Used to output a great deal of linker information for debugging

  THIS WILL HAVE A PERFORMANCE HIT!
-->
<param name="l:aggressive-debug" select="false()" />

<variable name="l:orig-root" as="document-node( element( lv:package ) )"
          select="/" />

<variable name="l:process-empty" as="element( l:pstack )">
  <l:pstack />
</variable>

<variable name="l:stack-empty" as="element( l:sym-stack )">
  <l:sym-stack />
</variable>


<template match="*" mode="l:link" priority="1">
  <call-template name="log:error">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>cannot link </text>
      <value-of select="name()" />
      <text>; must link program</text>
    </with-param>
  </call-template>
</template>


<!-- entry point (if run directly) -->
<template match="/" priority="1">
  <apply-templates select="/lv:*" mode="l:link" />
</template>


<!--
  We will only link program package

  Raters are similar to shared object files (that is, packages), but explicitly
  recognize the fact that linking should be done. They also contain definitions
  for exit points (lv:yields); think of it like defining a main() function.
-->
<template match="lv:package[ @program='true' ]" mode="l:link" priority="5">
  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>linking </text>
      <value-of select="@name" />
      <text>...</text>
    </with-param>
  </call-template>

  <!-- start by recursively discovering imported shared object files -->
  <variable name="pre-deps" as="element( l:dep )">
    <call-template name="log:debug">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>building dependency tree...</text>
      </with-param>
    </call-template>

    <l:dep>
      <!-- empty stack -->
      <apply-templates select="preproc:symtable" mode="l:depgen">
        <with-param name="stack" select="$l:stack-empty" as="element( l:sym-stack )" />
      </apply-templates>
    </l:dep>
  </variable>


  <!-- a single-pass post-processing of the deps to resolve any final issues -->
  <variable name="deps" as="element( l:dep )">
    <call-template name="log:debug">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>resolving dependency tree...</text>
      </with-param>
    </call-template>

    <apply-templates select="$pre-deps" mode="l:resolv-deps" />
  </variable>

  <copy>
    <copy-of select="@*" />

    <!-- copy the dependency tree -->
    <copy-of select="$deps" />

    <!-- if map data was provided, generate the map -->
    <variable name="maplink">
      <apply-templates select="." mode="l:map">
        <with-param name="deps" select="$deps" />
      </apply-templates>
    </variable>

    <if test="$maplink//l:map-error">
      <call-template name="log:error">
        <with-param name="name" select="'link'" />
      </call-template>
    </if>

    <!-- all good. -->
    <copy-of select="$maplink" />

    <!-- link. -->
    <l:exec>
      <apply-templates select="." mode="l:link-deps">
        <!-- TODO: remove this exception -->
        <with-param name="deps" select="
            $deps/preproc:sym[
              not(
                starts-with( @type, 'map' )
                or starts-with( @type, 'retmap' )
              )
            ]
          " />
      </apply-templates>
    </l:exec>
  </copy>
</template>


<template mode="l:depgen" as="element( preproc:sym )*"
          match="preproc:symtable">
  <param name="stack" as="element( l:sym-stack )"
         select="$l:stack-empty" />

  <!-- we care only of the symbols used by lv:yields, from which all
       dependencies may be derived (if it's not derivable from the yield
       symbol, then it must not be used); note that lv:yield actually compiles
       into a special symbol ___yield -->
  <variable name="yields" as="element( preproc:sym )+">
    <!-- TOOD: these shouldn't be a magical exception; map it -->
    <sequence select="preproc:sym[ @name = '___yield'
                                     or @name = '___worksheet'
                                     or @type = 'meta' ]" />

    <!-- TODO: messy; refactor this symbol situation -->
    <!-- this should be the sole source of outputs and, consequently,
         dependencies -->
    <sequence select="
        preproc:sym[
          @type='map' or @type='map:head' or @type='map:tail'
          or @type='retmap' or @type='retmap:head' or @type='retmap:tail'
        ]
      " />
  </variable>

  <!-- start at the top of the table and begin processing each symbol
       individually, generating a dependency tree as we go -->
  <variable name="result" as="element()+">
    <call-template name="l:depgen-sym">
      <with-param name="pending" select="$yields" />
      <with-param name="stack" select="$stack" as="element( l:sym-stack )" />
    </call-template>
  </variable>

  <!-- stack will contain additional metadata -->
  <sequence select="$result[ . instance of
                             element( preproc:sym ) ]" />
</template>


<template mode="l:resolv-deps" as="element( l:dep )"
          priority="9"
          match="l:dep">
  <copy>
    <apply-templates mode="l:resolv-deps" />
  </copy>
</template>


<!-- replace marks with the symbols that they reference (note that the linker
     will not add duplicates, so we needn't worry about them) -->
<template mode="l:resolv-deps" as="element( preproc:sym )"
          priority="8"
          match="preproc:sym[ @l:mark-inclass ]">
  <!-- FIXME: I sometimes return more than one symbol! -->
  <variable name="sym" as="element( preproc:sym )*"
            select="root(.)/preproc:sym[
                      @name = current()/@name ]" />

  <!-- sanity check; hopefully never necessary -->
  <if test="not( $sym )">
    <call-template name="log:internal-error">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>l:mark-class found for non-existing symbol </text>
        <value-of select="@name" />
        <text>; there is a bug in l:depgen-process-sym</text>
      </with-param>
    </call-template>
  </if>

  <!-- copy the element and mark as inclass (no need to check @src, since at
       this point, such conflicts would have already resulted in an error -->
  <preproc:sym>
    <sequence select="$sym/@*" />

    <!-- override attribute -->
    <attribute name="inclass" select="'true'" />
  </preproc:sym>
</template>


<!-- any now-inclass symbols should be stripped of their original position -->
<template mode="l:resolv-deps" priority="7"
          match="preproc:sym[
                   @name = root(.)/preproc:sym[ @l:mark-inclass ]
                             /@name ]">
  <!-- bye-bye -->
</template>


<template match="*" mode="l:resolv-deps" priority="1">
  <sequence select="." />
</template>


<!-- FIXME: I want element( l:preproc:sym ), but we also have
     l:mark-inclass -->
<template name="l:depgen-sym" as="element()*">
  <param name="pending" as="element( preproc:sym )*" />
  <param name="stack" as="element( l:sym-stack )" />
  <param name="path" as="xs:string"
         select="''" />
  <param name="processing" as="element( l:pstack )"
         select="$l:process-empty" />

  <variable name="pend-count" as="xs:integer"
            select="count( $pending )" />
  <variable name="stack-count" as="xs:integer"
            select="count( $stack/preproc:sym )" />
  <variable name="process-count" as="xs:integer"
            select="count( $processing/* )" />

  <choose>
    <!-- if there are no pending symbols left, then we are done; return the
         stack -->
    <when test="$pend-count = 0">
      <sequence select="$stack/*" />
    </when>


    <otherwise>
      <!-- take the first item from the pending list -->
      <variable name="cur" as="element( preproc:sym )"
                select="$pending[1]" />

      <!-- aggressive debugging data -->
      <if test="$l:aggressive-debug">
        <call-template name="log:debug">
          <with-param name="name" select="'link'" />
          <with-param name="msg">
            <text>(</text>
            <value-of select="$path" />
            <text>) </text>
            <value-of select="$pend-count" />
            <text>p - </text>
            <value-of select="$stack-count" />
            <text>s - </text>
            <value-of select="$process-count" />
            <text>r - </text>
            <value-of select="$cur/@name" />
            <text> [s:: </text>
            <value-of select="$stack/preproc:sym/@name" />
            <text> ::s] [r:: </text>
            <value-of select="$processing/preproc:sym/@name" />
            <text>::r]</text>
          </with-param>
        </call-template>
      </if>

      <apply-templates select="$cur" mode="l:depgen-process-sym">
        <with-param name="pending" select="$pending" />
        <with-param name="stack" select="$stack" />
        <with-param name="path" select="$path" />
        <with-param name="processing" select="
            if ( $cur/@l:proc-barrier = 'true' )
              then $l:process-empty
            else
              $processing
          " />
      </apply-templates>
    </otherwise>
  </choose>
</template>


<function name="l:resolv-extern" as="element( preproc:sym )">
  <param name="sym" as="element( preproc:sym )" />

  <variable name="name" as="xs:string"
            select="$sym/@name" />

  <!-- there is no reason (in the current implementation) that this
       should _not_ have already been resolved in the package being
       linked -->
  <variable name="pkg" as="element( lv:package )" select="
        $l:orig-root/lv:package" />

  <variable name="resolv" as="element( preproc:sym )?"
            select="$pkg/preproc:symtable/preproc:sym[
                      @name=$name ]" />

  <choose>
    <!-- if this symbol is not external, then we have found it -->
    <when test="$resolv and not( $resolv/@extern )">
      <sequence select="$resolv" />
    </when>

    <!-- if there is no more stack to check and we have not found the symbol,
         then this is a problem (this should never happen) -->
    <otherwise>
      <call-template name="log:internal-error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>unresolved extern </text>
          <value-of select="$name" />
          <text> (declared by `</text>
          <value-of select="$sym/@src" />
          <text>')</text>
        </with-param>
      </call-template>
    </otherwise>
  </choose>
</function>


<!--
  Resolves externs before processing them

  External symbols need special treatment; unlike other symbols, which are both
  declared and defined within the same package, externs are declared but their
  definitions are deferred to a later package. This allows using a value from
  another package in a C-style reverse-inheritence kind of way that is
  generally not a good idea, but sometimes necessary.

  When a package imports another package that declares an extern, the importing
  package must either declare that symbol as an extern or provide a definition.
  This is important, as it provides us with an easy means of resolution:
  traverse up the stack of symbols that are being processed, check their
  containing packages and see if there is a definition for the symbol. At some
  point, assuming that the shared object files are properly built, we must
  arrive at the definition. Since the symbol is defined within that package,
  the object file will also contain its dependency list.
-->
<template match="preproc:sym[ @extern='true' ]" mode="l:depgen-process-sym" priority="5">
  <param name="pending" as="element( preproc:sym )*" />
  <param name="stack" as="element( l:sym-stack )" />
  <param name="path" as="xs:string" />
  <param name="processing" as="element( l:pstack )" />

  <variable name="cur" select="." />

  <variable name="eresolv" as="element( preproc:sym )*"
            select="l:resolv-extern( $cur )" />

  <!-- were we able to resolve the symbol? -->
  <if test="empty( $eresolv )">
    <call-template name="log:internal-error">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>could not resolve external symbol `</text>
          <value-of select="$cur/@name" />
        <text>'</text>
      </with-param>
    </call-template>
  </if>

  <!-- in the event that we are importing symbols from program packages (which
       hopefully is a rare case), we may have external symbols that resolve to
       the same package; filter out duplicates -->
  <variable name="eresolv-uniq" as="element( preproc:sym )"
            select="$eresolv[
                      not( @src = $eresolv[ not( current() ) ]/@src ) ]" />

  <!-- did we find more than one? (that would be very bad and likely represents
       a symbol table generation bug) -->
  <if test="count( $eresolv-uniq ) gt 1">
    <call-template name="log:internal-error">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>ambiguous external symbol `</text>
          <value-of select="$cur/@name" />
        <text>'; resolution failed (found </text>
          <for-each select="$eresolv-uniq">
            <if test="position() > 1">
              <text>; </text>
            </if>
            <value-of select="@src" />
          </for-each>
        <text>); pulled in by: </text>

        <!-- help the user figure out how this happened -->
        <for-each select="$processing/preproc:sym">
          <if test="position() gt 1">
            <text> - </text>
          </if>
          <value-of select="concat( @src, '/', @name )" />
        </for-each>
      </with-param>
    </call-template>
  </if>

  <call-template name="log:debug">
    <with-param name="name" select="'link'" />
    <with-param name="msg"
                select="concat(
                          'external symbol ', $cur/@name,
                          ' resolved to ',
                          ( if ( $eresolv-uniq/@src ) then
                              $eresolv-uniq/@src
                            else '' ),
                          '/',
                          $eresolv-uniq/@name )" />
  </call-template>

  <!-- use the resolved symbol in place of the original extern -->
  <apply-templates select="$eresolv-uniq" mode="l:depgen-process-sym">
    <with-param name="pending" select="$pending" />
    <with-param name="stack" select="$stack" as="element( l:sym-stack )" />
    <with-param name="path" select="$path" />
    <with-param name="processing" select="$processing" />
  </apply-templates>
</template>


<template mode="l:depgen-process-sym" priority="1"
              match="preproc:sym">
  <param name="pending" as="element( preproc:sym )*" />
  <param name="stack" as="element( l:sym-stack )" />
  <param name="path" as="xs:string" />
  <param name="processing" as="element( l:pstack )" />

  <variable name="cur" as="element( preproc:sym )"
            select="." />

  <!-- determines if the compile destination for these dependencies will be
       within the classifier; this is the case for all class dependencies
       *unless* the class is external -->
  <variable name="inclass" as="xs:boolean"
            select="( ( $cur/@type='class' )
                      and not( $cur/@extclass='true' ) )
                    or $processing/preproc:sym[
                      @type='class'
                      and not( @extclass='true' ) ]" />

  <!-- perform circular dependency check and blow up if found (we cannot choose
       a proper linking order without a correct dependency tree); the only
       exception is if the circular dependency is a function, since that simply
       implies recursion, which we can handle just fine -->
  <variable name="circ" as="element( preproc:sym )*"
            select="$processing/preproc:sym[
                      @name=$cur/@name
                      and @src=$cur/@src ]" />

  <choose>
    <!-- non-function; fatal -->
    <when test="$circ
                    and $circ[ not( @type='func' ) ]
      ">
      <call-template name="l:err-circular">
        <with-param name="stack" select="$processing" />
        <with-param name="cur" select="$cur" />
      </call-template>
    </when>

    <!-- function; we've done all we need to, so do not re-link
         (recursive call) -->
    <when test="$circ">
      <!-- continue processing; leave stack unchanged -->
      <call-template name="l:depgen-sym">
        <with-param name="pending" select="remove( $pending, 1 )" />
        <with-param name="processing" select="$processing" />
        <with-param name="stack" select="$stack" as="element( l:sym-stack )"  />
      </call-template>
    </when>


    <!-- process; TODO: good refactoring point; large template -->
    <otherwise>
      <variable name="existing" as="element( preproc:sym )*"
                select="$stack/preproc:sym[
                          @name=$cur/@name ]" />

      <!-- TODO: this uses @name instead of @src because of map import
           paths; decide on one or the other -->
      <variable name="src-conflict" as="element( preproc:sym )*"
                select="if ( not( $cur/@name ) or $cur/@name = '' ) then
                          ()
                        else
                          $existing[ not( @name = $cur/@name ) ]" />

      <if test="$src-conflict">
        <call-template name="log:error">
          <with-param name="name" select="'link'" />
          <with-param name="msg">
            <text>symbol name is not unique: `</text>
            <value-of select="@name" />
            <text>' found in </text>
            <value-of select="$cur/@src" />

            <for-each select="$src-conflict">
              <text> and </text>
              <value-of select="@src" />
            </for-each>
          </with-param>
        </call-template>
      </if>

      <!-- determine if class already exists, but needs to be marked for
           inclusion within the classifier -->
      <variable name="needs-class-mark" as="xs:boolean"
                select="$existing
                          and $inclass
                          and not( $existing/@inclass='true' )
                          and not( $stack/preproc:sym[
                                     @l:mark-inclass
                                     and @name=$cur/@name
                                     and @src=$cur/@src ] )" />

      <!-- continue with the remainder of the symbol list -->
      <call-template name="l:depgen-sym">
        <with-param name="pending" select="remove( $pending, 1 )" />
        <with-param name="processing" select="$processing" />

        <with-param name="stack" as="element( l:sym-stack )">
          <!-- if this symbol already exists on the stack, then there is no use
               re-adding it (note that we check both the symbol name and its source
               since symbols could very well share a name due to exporting rules) -->
          <choose>
            <when test="not( $existing ) or $needs-class-mark">
              <!-- does this symbol have any dependencies? -->
              <variable name="deps" as="element( preproc:sym )*">
                <apply-templates select="$cur" mode="l:depgen-sym" />
              </variable>

              <!-- determine our path -->
              <variable name="mypath">
                <call-template name="preproc:get-path">
                  <with-param name="path" select="$cur/@src" />
                </call-template>
              </variable>

              <!-- augment each of the dep paths with our own (this ultimately
                   creates symbol paths relative to the rater) -->
              <variable name="deps-aug" as="element( preproc:sym )*">
                <call-template name="l:dep-aug">
                  <with-param name="cur" select="$cur" />
                  <with-param name="deps" select="$deps" />
                  <with-param name="inclass" select="$inclass" />
                  <with-param name="mypath" select="$mypath" />
                </call-template>
              </variable>

              <l:sym-stack>
                <!-- process the dependencies (note that this has the effect of
                     outputting the existing stack as well, which is why we have
                     not yet done so) -->
                <call-template name="l:depgen-sym">
                  <with-param name="pending" select="$deps-aug" />
                  <with-param name="stack" select="$stack" as="element( l:sym-stack )" />
                  <with-param name="path" select="$mypath" />
                  <with-param name="processing" as="element( l:pstack )">
                    <l:pstack>
                      <sequence select="$processing/*" />
                      <sequence select="$cur" />
                    </l:pstack>
                  </with-param>
                </call-template>

                <!-- finally, we can output ourself -->
                <choose>
                  <!-- existing symbol needs to be marked -->
                  <when test="$needs-class-mark">
                    <preproc:sym l:mark-inclass="true" name="{$cur/@name}" src="{$cur/@src}" />
                  </when>

                  <!-- new symbol -->
                  <otherwise>
                    <preproc:sym>
                      <sequence select="$cur/@*" />
                      <attribute name="inclass" select="$inclass" />
                    </preproc:sym>
                  </otherwise>
                </choose>
              </l:sym-stack>
            </when>


            <!-- already exists; leave stack unchanged -->
            <otherwise>
              <sequence select="$stack" />
            </otherwise>
          </choose>
        </with-param>
      </call-template>
    </otherwise>
  </choose>
</template>


<template name="l:dep-aug" as="element( preproc:sym )*">
  <param name="cur" as="element( preproc:sym )" />
  <param name="deps" as="element( preproc:sym )*" />
  <param name="inclass" as="xs:boolean"
         select="false()" />
  <param name="proc-barrier" as="xs:boolean"
         select="false()" />
  <param name="parent-name" as="xs:string"
         select="$cur/@name" />
  <param name="mypath">
    <!-- default -->
    <call-template name="preproc:get-path">
      <with-param name="path" select="$cur/@src" />
    </call-template>
  </param>

  <for-each select="$deps">
    <copy>
      <copy-of select="@*" />

      <variable name="newsrc">
        <choose>
          <!-- if a source path is provided, then we must take it
               relative to the current symbol's package's directory
               -->
          <when test="@src">
            <call-template name="preproc:resolv-path">
              <with-param name="path">
                <value-of select="$mypath" />

                <if test="$mypath and not( $mypath='' ) and @src and not( @src='' )">
                  <text>/</text>
                </if>

                <value-of select="@src" />
              </with-param>
            </call-template>
          </when>

          <!-- if no source path is set, then it exists in the same
               package as we do -->
          <otherwise>
            <value-of select="$cur/@src" />
          </otherwise>
        </choose>
      </variable>

      <!-- set new src path -->
      <attribute name="src" select="$newsrc" />

      <!-- flag for inclusion into classifier, if necessary -->
      <if test="$inclass">
        <attribute name="inclass" select="$inclass" />
      </if>

      <if test="$proc-barrier">
        <attribute name="l:proc-barrier" select="'true'" />
      </if>


      <if test="$l:aggressive-debug">
        <call-template name="log:debug">
          <with-param name="name" select="'link'" />
          <with-param name="msg">
            <value-of select="$parent-name" />
            <text> depends upon </text>
            <if test="@extern='true'">
              <text>external </text>
            </if>
            <value-of select="concat( @type, ' ', $newsrc, '/', @name )" />
          </with-param>
        </call-template>
      </if>
    </copy>
  </for-each>
</template>



<!-- TODO: some better way. -->
<template match="preproc:sym[ starts-with( @type, 'map' ) ]"
  mode="l:depgen-sym" priority="7">

  <!-- do not process deps -->
</template>


<template mode="l:depgen-sym" as="element( preproc:sym )*"
          match="preproc:sym[
                   @type='const' ]"
          priority="7">
  <!-- no need to link this; it has no code associated with it -->
</template>

<template mode="l:depgen-sym" as="element( preproc:sym )*"
          match="preproc:sym"
          priority="5">
  <!-- get the source package -->
  <variable name="pkg" as="element( lv:package )?" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), $l:orig-root )/lv:*
      else
        $l:orig-root/lv:package
    " />

  <variable name="name" as="xs:string"
            select="@name" />
  <variable name="deps" as="element( preproc:sym-dep )?"
            select="$pkg/preproc:sym-deps
                      /preproc:sym-dep[ @name=$name ]" />

  <!-- if we could not locate the dependencies, then consider this to be an
       error (even if there are no deps, there should still be a list dfn) -->
  <if test="not( $deps )">
    <call-template name="log:internal-error">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>could not locate dependency list for </text>
        <value-of select="@src" />
        <text>/</text>
        <value-of select="@name" />
      </with-param>
    </call-template>
  </if>

  <variable name="symtable" as="element( preproc:symtable )"
            select="$pkg/preproc:symtable" />

  <for-each select="
      $deps/preproc:sym-ref[
        not( @parent=$deps/preproc:sym-ref/@name )
      ]
    ">
    <variable name="sym-ref" as="xs:string"
              select="@name" />

    <variable name="sym" as="element( preproc:sym )?"
              select="$symtable/preproc:sym[ @name=$sym-ref ]" />

    <!-- if we cannot locate the referenced symbol, then that too is an error
         -->
    <if test="not( $sym )">
      <call-template name="log:internal-error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>failed locating dependency symbol `</text>
          <value-of select="$sym-ref" />
          <text>'</text>
          <text> from package </text>
          <value-of select="$pkg/@name" />
        </with-param>
      </call-template>
    </if>

    <!-- output the symbol, sans children -->
    <preproc:sym>
      <sequence select="$sym/@*" />
    </preproc:sym>
  </for-each>
</template>


<template mode="l:depgen-sym"
          match="*"
          priority="1">
  <message terminate="yes">
    <text>internal error: unknown symbol for l:depgen-sym: </text>
    <sequence select="." />
  </message>
</template>


<template name="l:err-circular">
  <param name="stack" />
  <param name="cur" />

  <call-template name="log:error">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>circular dependency </text>

      <text>(</text>
      <value-of select="concat( $cur/@src, '/', $cur/@name )" />
      <text>): </text>

      <for-each select="$stack//preproc:sym">
        <if test="position() > 1">
          <text> - </text>
        </if>

        <value-of select="concat( @src, '/', @name )" />
      </for-each>
    </with-param>
  </call-template>
</template>



<!--
  Links the object code for each dependency

  And here is where the magic happens. This will take the generated dependency
  list and, in order of the list, combine the object code from its source
  package's object file. The result is a fully linked executable.

  Not only is this one of the most interesting parts, this is also one of the
  fastest; all the hard work has already been done.

  Note: though the linked code is suitable for execution, it is up to a
  particular implementation to decide how it should be wrapped and invoked.
-->
<template match="lv:package" mode="l:link-deps">
  <param name="deps" />

  <!-- to make this executable, we must compile an entry point -->
  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>compiling entry point...</text>
    </with-param>
  </call-template>

  <apply-templates select="." mode="compiler:entry" />


  <apply-templates select="." mode="l:link-meta">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-worksheet">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-classifier">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-params">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-types">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-funcs">
    <with-param name="deps" select="$deps" />
  </apply-templates>
  <apply-templates select="." mode="l:link-rater">
    <with-param name="deps" select="$deps" />
  </apply-templates>

  <!-- common stuff -->
  <call-template name="compiler:static" />

  <!-- finally, finish up -->
  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>compiling exit...</text>
    </with-param>
  </call-template>

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <value-of select="@name" />
      <text> compilation complete.</text>
    </with-param>
  </call-template>
</template>


<function name="l:link-exit-fragments" as="xs:string*">
  <param name="paths"   as="xs:string" />
  <param name="context" as="node()" />

  <variable name="split" as="xs:string*"
            select="tokenize( $paths, ',' )" />

  <variable name="base-uri" as="xs:anyURI"
            select="base-uri( $context )" />

  <for-each select="$split">
    <variable name="fragment" as="xs:string?"
              select="l:get-fragment-by-path( ., $base-uri )" />

    <if test="empty( $fragment )">
      <call-template name="log:error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>fatal: missing exit fragment: </text>
          <value-of select="." />
        </with-param>
      </call-template>
    </if>

    <sequence select="$fragment" />
  </for-each>
</function>


<template match="lv:package" mode="l:link-meta">
  <param name="deps" as="element( preproc:sym )*" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking metadata...</text>
    </with-param>
  </call-template>

  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[ @type='meta' ]" />
  </apply-templates>
</template>


<template match="lv:package" mode="l:link-worksheet">
  <param name="deps" as="element( preproc:sym )*" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking worksheet...</text>
    </with-param>
  </call-template>

  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[ @type='worksheet' ]" />
  </apply-templates>
</template>


<template match="lv:package" mode="l:link-classifier">
  <param name="deps" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking classifier...</text>
    </with-param>
  </call-template>

  <!-- link everything that shall be a part of the classifier -->
  <apply-templates select="." mode="compiler:entry-classifier" />
  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[
          @inclass='true'
          and not( @type='param' )
          and not( @type='type' )
          and not( @type='meta' )
          and not( @type='worksheet' )
        ]
      " />
  </apply-templates>
  <apply-templates select="." mode="compiler:exit-classifier" />
</template>


<template match="lv:package" mode="l:link-params">
  <param name="deps" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking global params...</text>
    </with-param>
  </call-template>

  <!-- link all params -->
  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[ @type='param' ]
      " />
  </apply-templates>
</template>


<template match="lv:package" mode="l:link-types">
  <param name="deps" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking types...</text>
    </with-param>
  </call-template>

  <!-- link all params -->
  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[ @type='type' ]
      " />
  </apply-templates>
</template>


<template match="lv:package" mode="l:link-funcs">
  <param name="deps" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking functions...</text>
    </with-param>
  </call-template>

  <!-- link all params -->
  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[ @type='func' ]
      " />
  </apply-templates>
</template>


<template match="lv:package" mode="l:link-rater">
  <param name="deps" />

  <call-template name="log:info">
    <with-param name="name" select="'link'" />
    <with-param name="msg">
      <text>** linking rater...</text>
    </with-param>
  </call-template>

  <apply-templates select="." mode="compiler:entry-rater" />

  <!-- TODO: this list of exclusions is a mess -->
  <apply-templates select="." mode="l:do-link">
    <with-param name="symbols" select="
        $deps[
          not( @inclass='true' )
          and not( @type='param' )
          and not( @type='type' )
          and not( @type='func' )
          and not( @type='meta' )
          and not( @type='worksheet' )
        ]
      " />
  </apply-templates>

  <sequence select="l:link-exit-fragments(
                          $rater-exit-fragments,
                          . )" />

  <apply-templates select="." mode="compiler:exit-rater">
    <with-param name="symbols" select="$deps" />
  </apply-templates>
</template>


<template match="lv:package" mode="l:do-link">
  <param name="symbols" />

  <!-- link each of the dependencies -->
  <for-each select="$symbols">
    <call-template name="log:info">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>linking </text>
        <value-of select="concat( @type, ' ', @src, '/', @name )" />
        <text>...</text>
      </with-param>
    </call-template>

    <apply-templates select="." mode="l:link-deps" />
  </for-each>
</template>


<template match="preproc:sym[ @type='lparam' ]" mode="l:link-deps" priority="9">
  <!-- no object code for local params -->
</template>

<!-- priority of 7 because there may otherwise be some ambiguities
     (e.g. with lparam) -->
<template match="preproc:sym[ @parent ]" mode="l:link-deps" priority="7">
  <!-- if a parent is defined, then its symbol will have been sufficient -->
</template>

<template match="preproc:sym" mode="l:link-deps" priority="5">
  <!-- consult the source package for the last time... -->
  <variable name="pkg" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), $l:orig-root )/lv:*
      else
        $l:orig-root/lv:package
    " />

  <variable name="name" select="@name" />
  <variable name="objcode" as="xs:string?"
                select="l:get-fragment( $pkg, $name )" />

  <if test="empty( $objcode )">
    <if test="not( @type='param'
                   or ( @type='const' and @dim='0' )
                   or @type='tpl'
                   or @type='meta'
                   or not( @type='worksheet' ) )">
      <call-template name="log:internal-error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>missing object code for symbol </text>
          <value-of select="concat( @src, '/', @name )" />
        </with-param>
      </call-template>
    </if>
  </if>

  <copy-of select="$objcode" />
</template>


<function name="l:get-fragment-by-path" as="xs:string?">
  <param name="path"     as="xs:string" />
  <param name="base-uri" as="xs:anyURI" />

  <variable name="pkg-path" as="xs:string">
    <call-template name="preproc:get-path">
      <with-param name="path" select="$path" />
    </call-template>
  </variable>

  <variable name="fragment-name" as="xs:string">
    <call-template name="preproc:get-basename">
      <with-param name="path" select="$path" />
    </call-template>
  </variable>

  <variable name="package-uri" as="xs:anyURI"
            select="resolve-uri(
                      concat( $pkg-path, '.xmlo' ),
                      $base-uri )" />

  <variable name="doc" as="document-node()"
                select="doc( $package-uri )" />

  <if test="empty( $doc )">
    <call-template name="log:internal-error">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>could not locate package for exit-fragment: </text>
        <value-of select="$package-uri" />
      </with-param>
    </call-template>
  </if>

  <variable name="package" as="element()"
            select="$doc/*" />

  <sequence select="l:get-fragment(
                      $package,
                      $fragment-name )" />
</function>


<function name="l:get-fragment" as="xs:string?">
  <param name="package" as="element()" />
  <param name="name"    as="xs:string" />

  <variable name="fragment" as="element( preproc:fragment )?"
            select="$package/preproc:fragments/preproc:fragment[
                      @id = $name ]" />

  <sequence select="$fragment/text()" />
</function>


<template match="lv:package" mode="l:map" priority="5">
  <!-- it is important that we check against the dependencies actually compiled
       rather than the list of available symbols -->
  <param name="deps" as="element( l:dep )" />

  <variable name="syms" as="element( preproc:sym )*"
            select="preproc:symtable/preproc:sym" />

  <variable name="mapsyms" as="element( preproc:sym )*"
            select="$syms[ @type='map' ]" />
  <variable name="retmapsyms" as="element( preproc:sym )*"
            select="$syms[ @type='retmap' ]" />

  <!-- get head and tail -->
  <variable name="head"     select="$syms[ @type='map:head' ]" />
  <variable name="tail"     select="$syms[ @type='map:tail' ]" />
  <variable name="ret-head" select="$syms[ @type='retmap:head' ]" />
  <variable name="ret-tail" select="$syms[ @type='retmap:tail' ]" />

  <if test="count( $mapsyms ) gt 0">
    <call-template name="log:info">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>generating input map...</text>
      </with-param>
    </call-template>

    <if test="not( $head ) or not( $tail )">
      <call-template name="log:internal-error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>missing object code for input map head or tail</text>
        </with-param>
      </call-template>
    </if>

    <!-- input map -->
    <l:map-exec>
      <apply-templates select="$head" mode="l:link-deps" />
      <text>&#10;</text>
      <apply-templates select="$mapsyms" mode="l:map">
        <with-param name="symtable" select="$deps" />
        <!-- TODO -->
        <with-param name="ignore-error" select="true()" />
      </apply-templates>
      <apply-templates select="$tail" mode="l:link-deps" />
    </l:map-exec>
  </if>


  <!-- TODO: very similar to above; refactor -->
  <if test="count( $retmapsyms ) gt 0">
    <call-template name="log:info">
      <with-param name="name" select="'link'" />
      <with-param name="msg">
        <text>generating return map...</text>
      </with-param>
    </call-template>

    <if test="not( $ret-head ) or not( $ret-tail )">
      <call-template name="log:internal-error">
        <with-param name="name" select="'link'" />
        <with-param name="msg">
          <text>missing object code for return map head or tail</text>
        </with-param>
      </call-template>
    </if>

    <!-- return map -->
    <l:retmap-exec>
      <apply-templates select="$ret-head" mode="l:link-deps" />
      <text>&#10;</text>
      <apply-templates select="$retmapsyms" mode="l:map">
        <with-param name="type" select="'return'" />
        <with-param name="from" select="'input'" />
        <with-param name="symtable" select="$deps" />
      </apply-templates>
      <apply-templates select="$ret-tail" mode="l:link-deps" />
    </l:retmap-exec>
  </if>
</template>


<template match="preproc:sym" mode="l:map" priority="5">
  <apply-templates select="." mode="l:link-deps" />
</template>

</stylesheet>
