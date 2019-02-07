<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compile package

  Copyright (C) 2016 R-T Specialty, LLC.

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
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:lvv="http://www.lovullo.com/rater/validate"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:util="http://www.lovullo.com/util">


<xsl:include href="../dslc-base.xsl" />

<!-- phases -->
<xsl:include href="macros.xsl" />
<xsl:include href="expand.xsl" />
<xsl:include href="symtable.xsl" />

<xsl:include href="../../compiler/fragments.xsl" />


<!-- begin preprocessing from an arbitrary node -->
<xsl:template name="preproc:pkg-compile" as="element( lv:package )"
              match="*" mode="preproc:compile" priority="1">
  <xsl:param name="orig-root" as="element()"
             select="root(.)/lv:package" />

  <xsl:param name="stopshort" />

  <!-- should be provided externally -->
  <xsl:if test="not( $__rseed ) or ( $__rseed = '' )">
    <xsl:message terminate="yes">
      <xsl:text>[preproc] error: missing random seed `__rseed'</xsl:text>
    </xsl:message>
  </xsl:if>

  <xsl:message>
    <xsl:text>[preproc] *beginning macro expansion...</xsl:text>
  </xsl:message>

  <!-- these can contain repass nodes, thus the element()+ -->

  <!-- macro expansion -->
  <xsl:variable name="stage1" as="element()+">
    <xsl:apply-templates select="." mode="preproc:macropass" />

    <xsl:message>
      <xsl:text>[preproc] *macro pass complete; expanding...</xsl:text>
    </xsl:message>
  </xsl:variable>

  <!-- expand shorthands, etc -->
  <xsl:variable name="stage2" as="element()+">
    <xsl:apply-templates select="$stage1"
      mode="preproc:expand" />

    <xsl:message>
      <xsl:text>[preproc] *expansion complete; generating symbol table...</xsl:text>
    </xsl:message>
  </xsl:variable>

  <xsl:variable name="stage3" as="element()+">
    <xsl:apply-templates select="$stage2"
      mode="preproc:sym-discover">

      <xsl:with-param name="orig-root" select="$orig-root" />
    </xsl:apply-templates>

    <xsl:message>
      <xsl:text>[preproc] *symbol table generated; checking for </xsl:text>
      <xsl:text>unprocessed templates...</xsl:text>
    </xsl:message>
  </xsl:variable>


  <!-- TODO: resolve this mess -->
  <xsl:variable name="stage3-pkg" as="element( lv:package )"
                select="$stage3/preproc:symtable/parent::*" />

  <!-- TODO: move me somewhere more appropriate and create an error
       system that is _guaranteed_ to catch everything -->
  <xsl:for-each select="$stage3-pkg/preproc:symtable/preproc:error">
    <xsl:message terminate="yes">
      <xsl:text>!!! [preproc] error: </xsl:text>
      <xsl:value-of select="." />
    </xsl:message>
  </xsl:for-each>


  <!-- determine if we should finish or simply return for further processing -->
  <xsl:choose>
    <xsl:when test="not( $stopshort )">
      <!-- template expansions may have been deferred until their symbols were made
           available -->
      <xsl:variable name="final" as="element( lv:package )">
        <xsl:call-template name="preproc:tpl-sym-recurse">
          <xsl:with-param name="package" select="$stage3-pkg" />
          <xsl:with-param name="orig-root" select="$orig-root" />
        </xsl:call-template>
      </xsl:variable>

      <xsl:variable name="extern-chk" as="element( preproc:error )*"
                    select="preproc:final-extern-check(
                              $final/preproc:symtable )" />


      <xsl:if test="$extern-chk">
        <xsl:for-each select="$extern-chk">
          <xsl:message>
            <xsl:text>!!! [preproc] error: </xsl:text>
            <xsl:value-of select="." />
          </xsl:message>
        </xsl:for-each>

        <xsl:message>~~~~[begin document dump]~~~~</xsl:message>
        <xsl:message select="$final" />
        <xsl:message>~~~~[end document dump]~~~~</xsl:message>

        <xsl:message select="'Aborting due to unresolved externs'" />

        <xsl:message terminate="yes"
                     select="'Document dumped.'" />
      </xsl:if>

      <!-- ensure that all template parameters have been expanded -->
      <xsl:apply-templates select="$final" mode="preproc:tpl-check" />

      <!-- perform validation before dependency generation to ensure that all
           dependencies are available -->
      <xsl:apply-templates select="$final" mode="preproc:pkg-validate" />

      <!-- determine how many passes have been made -->
      <!-- TODO: reintroduce
      <xsl:variable name="repass-count"
        select="count( $final//preproc:repass-record )" />
        -->

      <!-- assign unique ids to each node -->
      <xsl:variable name="idized" as="element( lv:package )">
        <xsl:apply-templates select="$final" mode="preproc:idize" />
      </xsl:variable>

      <!-- generate deps -->
      <xsl:variable name="depd" as="element( lv:package )">
        <xsl:apply-templates select="$idized" mode="preproc:gen-deps" />
      </xsl:variable>

      <!-- post-process symbol table to resolve any unknowns that require a
           dependency tree -->
      <xsl:variable name="resolvd" as="element( lv:package )">
        <xsl:apply-templates select="$depd" mode="preproc:resolv-syms">
          <xsl:with-param name="orig-root" select="$orig-root" />
        </xsl:apply-templates>
      </xsl:variable>


      <!-- compile fragments -->
      <xsl:message>
        <xsl:text>[preproc] compiling fragments...</xsl:text>
      </xsl:message>


      <xsl:apply-templates select="$resolvd"
                           mode="preproc:compile-fragments" />


      <!-- output a repass count, which could be a strong indicator of performance
           issues -->
      <!-- TODO: reintroduce
      <xsl:message>
        <xsl:text>[Preprocessor repass count: </xsl:text>
          <xsl:value-of select="$repass-count" />
        <xsl:text>] [Node count: </xsl:text>
          <xsl:value-of select="count( $final//* )" />
        <xsl:text>]</xsl:text>
      </xsl:message>
      -->
    </xsl:when>

    <!-- return for further processing -->
    <xsl:otherwise>
      <xsl:sequence select="$stage3" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  A very primitive guard against unexpanded template parameters.
-->
<xsl:template match="*[ starts-with( @*, '@' ) ]" mode="preproc:tpl-check" priority="5">
  <xsl:message>
    <xsl:text>[preproc] fatal: unexpanded template parameter: </xsl:text>
    <xsl:sequence select="@*[ starts-with( ., '@' ) ]" />
  </xsl:message>

  <xsl:message>
    <xsl:text>[preproc] fatal: reference node: </xsl:text>
    <xsl:sequence select="." />
  </xsl:message>

  <xsl:message>~~~~[begin document dump]~~~~</xsl:message>
  <xsl:message select="root(.)" />
  <xsl:message>~~~~[end document dump]~~~~</xsl:message>

  <xsl:message terminate="yes">[preproc] notice: Document dumped.</xsl:message>
</xsl:template>


<!-- this should never happen; but is has, so here's a failsafe -->
<xsl:template match="lv:apply-template" mode="preproc:tpl-check" priority="5">
  <xsl:message>
    <xsl:text>[preproc] fatal: unexpanded template: </xsl:text>
    <xsl:sequence select="." />
  </xsl:message>

  <xsl:message>
    <xsl:text>[preproc] fatal: reference node: </xsl:text>
    <xsl:sequence select="." />
  </xsl:message>

  <xsl:message select="'[preproc] internal error: there is a bug in the',
                       'preprocessor; this should never happen!'" />

  <xsl:message>~~~~[begin document dump]~~~~</xsl:message>
  <xsl:message select="root(.)" />
  <xsl:message>~~~~[end document dump]~~~~</xsl:message>

  <xsl:message terminate="yes">[preproc] notice: Document dumped.</xsl:message>
</xsl:template>


<!-- skip things that cannot contain template applications -->
<xsl:template match="lv:template|lv:const|lv:typedef|lv:param-copy"
              mode="preproc:tpl-check" priority="9">
</xsl:template>

<xsl:template match="*" mode="preproc:tpl-check" priority="1">
  <xsl:apply-templates select="*" mode="preproc:tpl-check" />
</xsl:template>



<!--
  TODO: This needs to go away in the form of a more performant system
  that marks a repass as needed *without* scanning the entire tree.
-->
<xsl:template name="preproc:tpl-sym-recurse" as="element( lv:package )">
  <xsl:param name="package" as="element( lv:package )" />
  <xsl:param name="orig-root" as="element()" />

  <!-- number of iterations where no template applications have
       happened -->
  <xsl:param name="tpl-stall-count"
             tunnel="yes"
             select="0" />

  <!-- get a list of needed template applications (ignoring applications that
       are within templates and nested applications) -->
  <xsl:variable name="apply" as="element( lv:apply-template )*"
                select="$package//lv:apply-template[
                          not(
                            @name=$package/lv:template/@name
                            or ancestor::lv:template
                            or ancestor::lv:apply-template
                          )
                        ]" />
  <xsl:variable name="napply" select="count( $apply )" />

  <xsl:choose>
    <xsl:when test="$apply">
      <!-- get a list of required templates -->
      <xsl:variable name="req">
        <tpl>
          <xsl:for-each select="$apply">
            <xsl:copy>
              <xsl:sequence select="@name" />
            </xsl:copy>
          </xsl:for-each>
        </tpl>
      </xsl:variable>

      <xsl:variable name="requniq" as="element( lv:apply-template )*" select="
          $req//lv:apply-template[
            not( @name=preceding-sibling::lv:apply-template/@name
                 or @name=$package//lv:template/@name ) ]
        " />

      <xsl:message>
        <xsl:text>[preproc] </xsl:text>
        <xsl:value-of select="count( $apply )" />
        <xsl:text> template(s) still need application: </xsl:text>

        <xsl:for-each select="$apply">
          <xsl:if test="position() gt 1">
            <xsl:text>, </xsl:text>
          </xsl:if>

          <xsl:value-of select="@name" />
        </xsl:for-each>
      </xsl:message>

      <!-- load each of the requested templates (but only once) -->
      <xsl:variable name="tpls">
        <!-- TODO: we no longer need to load the templates; the
             template replacement looks up the template on-demand from
             the symbol table; we're no longer injecting them -->
        <xsl:apply-templates mode="preproc:tpl-from-sym" select="$requniq">
          <xsl:with-param name="orig-root" select="$orig-root" />
          <xsl:with-param name="symtable" select="$package/preproc:symtable" />
        </xsl:apply-templates>
      </xsl:variable>

      <!-- if we have recursed and have not decreased the application count at all,
           then we have a problem -->
      <xsl:if test="$requniq
                    and ( $package//preproc:sym-available )
                    and not( $package//preproc:repass[ @tpl-applied ] )">
        <xsl:message terminate="yes">
          <xsl:text>!!! [preproc] fatal: unable to locate symbols for </xsl:text>
          <xsl:text>remaining templates: </xsl:text>

          <xsl:for-each select="$requniq">
            <xsl:if test="position() > 1">
              <xsl:text>; </xsl:text>
            </xsl:if>

            <xsl:value-of select="@name" />
          </xsl:for-each>
        </xsl:message>
      </xsl:if>

      <!-- if there was an error during this part of the process, halt -->
      <xsl:if test="$tpls//preproc:error">
        <xsl:message terminate="yes">
          <xsl:text>!!! [preproc] fatal: terminating due to errors</xsl:text>
        </xsl:message>
      </xsl:if>

      <!-- perform expansion on the new package with the needed templates -->
      <xsl:variable name="result" as="element( lv:package )">
        <xsl:apply-templates select="$package" mode="preproc:compile">
          <xsl:with-param name="orig-root" select="$orig-root" />
          <xsl:with-param name="stopshort" select="true()" />
        </xsl:apply-templates>

        <xsl:message>
          <xsl:text>[preproc] *expansion complete (recursive)</xsl:text>
        </xsl:message>
      </xsl:variable>

      <!-- failsafe to prevent infinite recursion (5 (0-indexed)
           should be plenty, since everything in the system results in
           a template application in fewer steps -->
      <xsl:if test="$requniq and $tpl-stall-count eq 4">
        <xsl:message select="'!!! [preproc] internal: expansion deadlock!'" />
        <xsl:message
            select="'!!! [preproc] internal: stalled for 5 iterations'" />

        <xsl:sequence select="preproc:dump-document( $result )" />

        <xsl:message terminate="yes">
          <xsl:text></xsl:text>
          <xsl:text>!!! [preproc] fatal: expansion of remaining </xsl:text>
          <xsl:text>templates aborted (have all been imported?): </xsl:text>

          <xsl:for-each select="$requniq">
            <xsl:if test="position() > 1">
              <xsl:text>; </xsl:text>
            </xsl:if>

            <xsl:value-of select="@name" />
          </xsl:for-each>
        </xsl:message>
      </xsl:if>

      <!-- recurse to continue expanding if need be -->
      <xsl:call-template name="preproc:tpl-sym-recurse">
        <xsl:with-param name="orig-root" select="$orig-root" />
        <xsl:with-param name="package" select="$result" />
        <xsl:with-param name="tpl-stall-count"
                        tunnel="yes"
                        select="if ( $package//preproc:tpl-step ) then
                                  0
                                else
                                  $tpl-stall-count + 1" />
      </xsl:call-template>
    </xsl:when>


    <!-- expansion sequences and template short-hand expansions that
         have not yet taken place, due to one reason or another (too
         few passes: bug as far as I'm concerned) -->
    <xsl:when test="$package//lv:expand-sequence[
                      not( ancestor::lv:template
                           or ancestor::lv:apply-template ) ]
                    |$package//t:*[
                       not( ancestor::lv:template
                            or ancestor::lv:apply-template ) ]">
      <xsl:message select="'[preproc] pending expansions still present'" />

      <xsl:variable name="result" as="element( lv:package )">
        <xsl:apply-templates select="$package" mode="preproc:compile">
          <xsl:with-param name="orig-root" select="$orig-root" />
          <xsl:with-param name="stopshort" select="true()" />
        </xsl:apply-templates>
      </xsl:variable>

      <xsl:call-template name="preproc:tpl-sym-recurse">
        <xsl:with-param name="orig-root" select="$orig-root" />
        <xsl:with-param name="package" select="$result" />
      </xsl:call-template>
    </xsl:when>


    <!-- no further applications are necessary -->
    <xsl:otherwise>
      <!-- apply one final pass for the eligibility class generation -->
      <xsl:call-template name="preproc:elig-class-pass">
        <xsl:with-param name="package" select="$package" />
        <xsl:with-param name="orig-root" select="$orig-root" />
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="preproc:elig-class-pass" as="element( lv:package )">
  <xsl:param name="package" as="element( lv:package )" />
  <xsl:param name="orig-root" as="element()" />

  <xsl:variable name="final-pkg" as="element( lv:package )">
    <xsl:apply-templates select="$package" mode="preproc:expand-elig-class">
      <xsl:with-param name="orig-root" select="$package" />
    </xsl:apply-templates>
  </xsl:variable>

  <!-- yield the final pass against the elig-class-augmented package -->
  <xsl:apply-templates select="$final-pkg" mode="preproc:compile">
    <xsl:with-param name="orig-root" select="$orig-root" />
    <xsl:with-param name="stopshort" select="true()" />
  </xsl:apply-templates>
</xsl:template>


<!--
  This is but a shell of its former self.

  We are no longer injecting templates, so this can be removed or
  adapted to do just enough to verify that the template symbols exist.
-->
<xsl:template match="lv:apply-template" mode="preproc:tpl-from-sym">
  <xsl:param name="orig-root" as="element()" />

  <xsl:param name="symtable" as="element( preproc:symtable )"
             select="root(.)/preproc:symtable" />

  <xsl:variable name="tplname" select="@name" />

  <xsl:variable name="sym" as="element( preproc:sym )?" select="
      $symtable/preproc:sym[
        @name=$tplname
        and @type='tpl'
      ]
    " />

  <!-- if we have a symbol table, then attempt to locate it -->
  <xsl:choose>
    <!-- if we have located the template, then we know its source (@src must be
         set, otherwise the template is defined in this package and should have
         been available to begin with) -->
    <xsl:when test="$sym">
      <preproc:sym-available for="{$tplname}" />
    </xsl:when>

    <!-- nothing we can do yet -->
    <xsl:otherwise>
      <xsl:message>
        <xsl:text>[preproc] template symbol not yet available: </xsl:text>
        <xsl:value-of select="$tplname" />
      </xsl:message>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- TODO: we should use an attr besides _id; one more definitive -->
<xsl:template match="*[ @_id ]" mode="preproc:macropass" priority="9">
  <!-- already preprocessed -->
  <xsl:sequence select="." />
</xsl:template>
<xsl:template match="*[ @_id ]" mode="preproc:idize" priority="9">
  <!-- already preprocessed -->
  <xsl:sequence select="." />
</xsl:template>



<xsl:template match="preproc:repass-record" mode="preproc:idize" priority="9">
  <!-- no longer needed; remove -->
</xsl:template>


<!-- do not idize preproc nodes (unneeded waste of cycles) -->
<xsl:template match="preproc:*" mode="preproc:idize" priority="5">
  <xsl:sequence select="." />
</xsl:template>

<!-- do not idize templates (will cause processing problems) -->
<xsl:template match="lv:template" mode="preproc:idize" priority="5">
  <xsl:sequence select="." />
</xsl:template>


<!--
  Generates a unique id for each element and stores it as @_id

  This allows the node set to be copied and maintain its identity.
-->
<xsl:template match="*" mode="preproc:idize" priority="1">
  <xsl:variable name="id">
    <xsl:value-of select="generate-id(.)" />
  </xsl:variable>

  <xsl:copy>
    <xsl:sequence select="@*" />

    <xsl:attribute name="_id">
      <xsl:value-of select="$id" />
    </xsl:attribute>

    <xsl:apply-templates mode="preproc:idize" />
  </xsl:copy>
</xsl:template>


<xsl:template match="lv:package" mode="preproc:resolv-syms" as="element( lv:package )"
              priority="9">
  <xsl:param name="orig-root" as="element()" />
  <xsl:param name="rpcount" select="0" />

  <!-- arbitrary; intended to prevent infinite recursion -->
  <!-- TODO: same method as for templates; ensure changes, but do not create
       arbitrary limit -->
  <xsl:if test="$rpcount = 100">
    <xsl:sequence select="preproc:dump-document( root() )" />

    <xsl:message terminate="yes">
      <xsl:text>[preproc] !!! recursion limit reached in resolving `</xsl:text>
        <xsl:value-of select="@name" />
      <xsl:text>' symbols</xsl:text>
    </xsl:message>
  </xsl:if>

  <xsl:variable name="result" as="element( lv:package )">
    <xsl:copy>
      <xsl:sequence select="@*" />

      <xsl:message>
        <xsl:text>[preproc] *resolving symbol attributes...</xsl:text>
      </xsl:message>

      <xsl:apply-templates mode="preproc:resolv-syms">
        <xsl:with-param name="orig-root" select="$orig-root" />
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:variable>

  <xsl:variable name="repass"
    select="$result//preproc:symtable/preproc:repass" />

  <xsl:choose>
    <!-- repass scheduled; go for it -->
    <xsl:when test="$repass">
      <xsl:message>[preproc] *SYM REPASS*</xsl:message>
      <xsl:message>
        <xsl:text>[preproc] The following </xsl:text>
          <xsl:value-of select="count( $repass )" />
        <xsl:text> symbol(s) are still unresolved:</xsl:text>
      </xsl:message>

      <xsl:for-each select="$repass">
        <xsl:message>
          <xsl:text>[preproc] - </xsl:text>
          <xsl:value-of select="@ref" />
        </xsl:message>
      </xsl:for-each>

      <xsl:apply-templates select="$result" mode="preproc:resolv-syms">
        <xsl:with-param name="orig-root" select="$orig-root" />
        <xsl:with-param name="rpcount" select="$rpcount + 1" />
      </xsl:apply-templates>
    </xsl:when>

    <!-- no repass needed; done -->
    <xsl:otherwise>
      <xsl:sequence select="$result" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="preproc:symtable" mode="preproc:resolv-syms" priority="5">
  <xsl:param name="orig-root" as="element()" />

  <xsl:copy>
    <xsl:apply-templates mode="preproc:resolv-syms">
      <xsl:with-param name="orig-root" select="$orig-root" />
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>


<xsl:template match="preproc:sym[ not( @src ) and @dim='?' ]" mode="preproc:resolv-syms" priority="5">
  <xsl:param name="orig-root" as="element()" />

  <xsl:variable name="name" select="@name" />
  <xsl:variable name="pkg"  as="element( lv:package )"
                select="root(.)" />

  <xsl:variable name="deps" as="element( preproc:sym-dep )*" select="
      $pkg/preproc:sym-deps/preproc:sym-dep[ @name=$name ]
    " />

  <xsl:variable name="depsyms-unresolv" as="element( preproc:sym )*" select="
      $pkg/preproc:symtable/preproc:sym[
        @name=$deps/preproc:sym-ref/@name
      ]
    " />

  <xsl:variable name="depsyms-resolv">
    <xsl:for-each select="$depsyms-unresolv">
      <xsl:choose>
        <xsl:when test="not( @src )">
          <xsl:sequence select="." />
        </xsl:when>

        <!-- look up complete symbol -->
        <xsl:otherwise>
          <xsl:variable name="name" select="@name" />
          <xsl:variable name="sym" select="
            document( concat( @src, '.xmlo' ), $orig-root )
              /lv:package/preproc:symtable/preproc:sym[
                @name=$name
              ]
            " />

          <xsl:if test="not( $sym )">
            <xsl:message terminate="yes">
              <xsl:text>[preproc] !!! failed to look up symbol `</xsl:text>
                <xsl:value-of select="$name" />
              <xsl:text>'</xsl:text>
            </xsl:message>
          </xsl:if>

          <xsl:sequence select="$sym" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="depsyms" select="$depsyms-resolv/preproc:sym" />

  <xsl:choose>
    <!-- unresolved dependency dimensions; defer until next pass -->
    <xsl:when test="
        $depsyms/@dim = '?'
      ">
      <xsl:message>
        <xsl:text>[preproc] deferring `</xsl:text>
          <xsl:value-of select="$name" />
        <xsl:text>' dimensions with unresolved dependencies</xsl:text>
      </xsl:message>

      <!-- schedule repass :x -->
      <xsl:sequence select="." />
      <preproc:repass src="preproc:sym resolv-syms"
                      ref="{$name}" />
    </xsl:when>

    <!-- all dependencies are resolved; calculate dimensions -->
    <xsl:otherwise>
      <!-- sort dependencies so that the largest dimension is at the top -->
      <xsl:variable name="maxset">
        <xsl:for-each select="$depsyms">
          <xsl:sort select="@dim" data-type="number" order="descending" />
          <xsl:sequence select="." />
        </xsl:for-each>
      </xsl:variable>

      <xsl:variable name="max">
        <xsl:choose>
          <xsl:when test="count( $deps/preproc:sym-ref ) = 0">
            <!-- no dependencies, unknown size, so it's a scalar -->
            <xsl:text>0</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <!-- largest value -->
            <xsl:value-of select="$maxset/preproc:sym[1]/@dim" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <!-- failure? -->
      <xsl:if test="not( $max ) or $max = ''">
        <xsl:message terminate="yes">
          <xsl:text>[preproc] !!! failed to determine dimensions of `</xsl:text>
            <xsl:value-of select="$name" />
          <xsl:text>'</xsl:text>
        </xsl:message>
      </xsl:if>

      <!-- copy, substituting calculated dimensions -->
      <xsl:copy>
        <xsl:sequence select="@*" />
        <xsl:attribute name="dim" select="$max" />
        <xsl:sequence select="*" />
      </xsl:copy>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="preproc:repass" mode="preproc:resolv-syms" priority="9">
  <!-- strip -->
</xsl:template>


<xsl:template match="*" mode="preproc:resolv-syms" priority="1">
  <xsl:sequence select="." />
</xsl:template>


<xsl:template match="lv:package" mode="preproc:pkg-validate">
  <xsl:variable name="symbol-map">
    <xsl:call-template name="get-symbol-map" />
  </xsl:variable>

  <xsl:variable name="err">
    <xsl:apply-templates select="." mode="lvv:validate">
      <xsl:with-param name="symbol-map" select="$symbol-map" />
    </xsl:apply-templates>
  </xsl:variable>

  <xsl:apply-templates select="$err//lvv:error" mode="preproc:handle-lvv-errors">
    <xsl:with-param name="document" select="." />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="*|text()" mode="preproc:pkg-validate">
</xsl:template>


<!-- errors should cause a failure -->
<xsl:template match="lvv:error" mode="preproc:handle-lvv-errors" priority="5">
  <xsl:param name="document" />

  <!-- output error -->
  <xsl:message>
    <xsl:text>!!! </xsl:text>
    <xsl:value-of select="@desc" />

    <xsl:if test="@path != ''">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="@path" />
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:text>: </xsl:text>
    <xsl:value-of select="." />
  </xsl:message>

  <!-- terminate after we've output each error -->
  <xsl:if test="not( following-sibling::lvv:error )">
    <!-- dump document for debugging -->
    <xsl:sequence select="preproc:dump-document( $document )" />
    <xsl:message terminate="yes">Compilation failed due to validation errors.</xsl:message>
  </xsl:if>
</xsl:template>


<xsl:function name="preproc:dump-document">
  <xsl:param name="document" />

  <xsl:message>~~~~[begin document dump]~~~~</xsl:message>
  <xsl:message select="$document" />
  <xsl:message>~~~~[end document dump]~~~~</xsl:message>
  <xsl:message>internal: document dumped.</xsl:message>
</xsl:function>


<xsl:template match="node()|text()" mode="preproc:handle-lvv-errors" priority="1">
</xsl:template>

</xsl:stylesheet>
