<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Outputs rater summary, containing details for both the rater and its packages

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

  This is the core reason why the DSL was originally written: to provide
  extremely detailed renderings of all calculations.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvp="http://www.lovullo.com"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:lvv="http://www.lovullo.com/rater/validate"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
  xmlns:util="http://www.lovullo.com/util"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:l="http://www.lovullo.com/rater/linker"
  xmlns:ext="http://www.lovullo.com/ext"

  xmlns:exsl="http://exslt.org/common"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="exsl str">

<xsl:output method="html" />

<!-- EXSLT -->
<xsl:include href="include/exslt/str.tokenize.template.xsl" />

<xsl:include href="include/util.xsl" />
<xsl:include href="include/display.xsl" />
<xsl:include href="include/calc-display.xsl" />
<xsl:include href="include/entry-form.xsl" />


<!-- TODO: MOVE -->
<xsl:template match="*" mode="gen-uid">
  <xsl:value-of select="generate-id()" />
</xsl:template>

<!-- allows altering the path to certain framework files, such as the CSS
     and JS -->
<xsl:param name="fw-path" select="lv:package/@__rootpath" />

<xsl:variable name="program" select="/lv:package" />


<!--
  Generate HTML page for a given rater/package

  Low priority to permit overriding.

  @return HTML content
-->
<xsl:template match="lv:package" priority="1">
  <xsl:apply-templates select="." mode="init-compile" />
</xsl:template>

<xsl:template match="lv:package" mode="init-compile">
  <xsl:variable name="name" select="@name" />
  <xsl:variable name="root" select="." />

  <xsl:message>
    <xsl:text>[summary] processing </xsl:text>
    <xsl:value-of select="local-name()" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@name" />
    <xsl:text>... (please be patient)</xsl:text>
  </xsl:message>

  <xsl:variable name="ucname">
    <xsl:call-template name="util:ucfirst">
      <xsl:with-param name="str" select="@name" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="title">
    <xsl:value-of select="$ucname" />
    <xsl:text> </xsl:text>

    <!-- output document type -->
    <xsl:call-template name="util:ucfirst">
      <xsl:with-param name="str" select="local-name()" />
    </xsl:call-template>
  </xsl:variable>

  <!-- preprocess the summary information that'll be frequently used -->
  <!--
  <xsl:variable name="summarized_root">
    <xsl:message>[summary] preprocessing summary data...</xsl:message>
    <xsl:apply-templates select="$pre-root" mode="root-summarize" />
  </xsl:variable>
  <xsl:variable name="root" select="exsl:node-set( $summarized_root )" />
  -->

  <!-- intentional newline -->
  <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE html&gt;
 </xsl:text>

  <html>
    <head>
      <!-- determine style path -->
      <xsl:variable name="path">
        <xsl:choose>
          <xsl:when test="@core">
            <!-- same path -->
            <xsl:text></xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:text>rater/</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <link rel="stylesheet" href="{$fw-path}{$path}summary.css" type="text/css" />

      <title><xsl:value-of select="$title" /></title>
    </head>
    <body>
      <xsl:call-template name="gen-pkg-menu" />

      <!-- load preprocessed source file -->
      <xsl:message>[summary] typsetting self...</xsl:message>
      <xsl:variable name="pkg-self" select="
          document( concat( @__rootpath, @name, '.xmlo' ), . )/lv:*
        " />
      <xsl:apply-templates
        select="$pkg-self/lv:*" />

      <!-- get a list of unique packages and typeset them -->
      <!-- TODO: this logic is duplicated; see gen-pkg-menu -->
      <xsl:for-each select="
          /lv:*/l:dep/preproc:sym[
            @src
            and not( @src='' )
            and not(
              @src=preceding-sibling::preproc:sym/@src
            )
          ]
        ">

        <xsl:message>
          <xsl:text>[summary] typesetting package </xsl:text>
          <xsl:value-of select="@src" />
          <xsl:text>...</xsl:text>
        </xsl:message>

        <xsl:apply-templates
          select="document( concat( @src, '.xmlo' ), . )/lv:*/lv:*" />
      </xsl:for-each>

      <!-- some general information -->
      <xsl:call-template name="summary-info" />

      <!-- include rater externally (makes debugging easier; inlining is a
           clusterfuck -->
      <script type="text/javascript">
        <xsl:text>module = { exports: {} };</xsl:text>
      </script>
      <script type="text/javascript" src="{@__rootpath}{@name}.js"></script>

      <div id="test-data">
        <!-- values of interest -->
        <div class="test-summary">
          <table id="voi-container">
            <caption>Values of Interest</caption>
            <thead>
              <th>Name</th>
              <th>Value</th>
            </thead>
            <tbody id="voi-list">
            </tbody>
          </table>

          <!-- classifications at a glance -->
          <table id="class-overview">
            <caption>Classification Overview</caption>
            <tbody id="class-overview-list">
            </tbody>
          </table>
        </div>

        <xsl:apply-templates select="." mode="entry-form">
          <xsl:with-param name="root-pkg" select="$pkg-self" />
        </xsl:apply-templates>
      </div>

      <script type="text/x-mathjax-config">
        MathJax.Hub.Config({
          "HTML-CSS": { linebreaks: { automatic: true } },
                 SVG: { linebreaks: { automatic: true } },

          skipStartupTypeset: true,
        });
      </script>
      <script
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        integrity="sha512-tOav5w1OjvsSJzePRtt2uQPFwBoHt1VZcUq8l8nm5284LEKE9FSJBQryzMBzHxY5P0zRdNqEcpLIRVYFNgu1jw=="
        crossorigin="anonymous">
      </script>

      <!-- global functions for developers -->
      <script type="text/javascript">
        window.addEventListener( 'load', function()
        {
        // expected VOI order
        window.voi_order = [
          <xsl:apply-templates
            select="/lv:package[ @program='true' ]/l:dep/preproc:sym[ @type='rate' ]"
            mode="voi-order">

            <!-- the symbol table from the object file contains additional
                 symbols that the linker filtered out (such as generators) -->
            <xsl:with-param name="osym" select="
                document( concat( @__rootpath, $name, '.xmlo' ), $root )
                  /lv:package[ @program='true' ]/preproc:symtable
              " />
          </xsl:apply-templates>
        ];

        var rate_result = {};

        function styleMath( target )
        {
          if ( !target )
          {
            return;
          }

          if ( target.className.match( /\bmath-typeset-hover\b/ ) )
          {
            MathJax.Hub.Queue( [ "Typeset", MathJax.Hub, target ] );
            client.updateSummaryDebug( rate_result.debug, target );
          }
        }

        // defer other equation typesetting until it is actually needed (since,
        // in most instances, they will never even be viewed)
        window.onhashchange = function()
        {
          var destid = window.location.hash.substr( 1 );

          // special treatment (no, we do not want to return after doing this)
          if ( destid === 'prior' )
          {
            client.Prior.showLoad();
          }

          var dest = document.getElementById( destid );

          if ( dest )
          {
            styleMath( dest );
          }
        };

        // style on page load if we loaded a specific target
        var id = document.location.hash.replace( /^#/, '' );
        if ( id )
        {
          styleMath( document.getElementById( id ) );
        }

        client.onRate( function( result )
        {
          rate_result = result;
        } );

        // begin processing
        client.begin();
        } );
      </script>
    </body>
  </html>
</xsl:template>


<xsl:template name="gen-pkg-menu">
  <xsl:variable name="self" select="." />
  <xsl:variable name="name" select="@name" />

  <xsl:message>[summary] building package list...</xsl:message>

  <div class="menu" id="pkgmenu">
    <!-- build our own menu -->
    <h1 id="pkg-{@name}" class="sym-ref sym-pkg">
      <xsl:value-of select="@name" />
    </h1>

    <!-- general menu -->
    <ul>
      <li><a href="#test-data">Test Case</a></li>
      <li><a id="load-prior" href="#prior">Prior Test Cases</a></li>
      <li><a href="#__nb">N.B.</a></li>
    </ul>

    <xsl:apply-templates select="." mode="gen-menu">
      <xsl:with-param name="pkg" select="concat( @__rootpath, @name )" />
    </xsl:apply-templates>

    <!-- get a list of unique packages -->
    <xsl:for-each select="
        /lv:*/l:dep/preproc:sym[
          @src
          and not( @src='' )
          and not(
            @src=preceding-sibling::preproc:sym/@src
          )
        ]
      ">

      <xsl:message>
        <xsl:text>[summary] </xsl:text>
        <xsl:value-of select="$name" />
        <xsl:text> uses </xsl:text>
        <xsl:value-of select="@src" />
      </xsl:message>

      <!-- build package menu -->
      <xsl:variable name="result">
        <xsl:apply-templates select="$self" mode="gen-menu">
          <xsl:with-param name="src" select="@src" />
        </xsl:apply-templates>
      </xsl:variable>

      <xsl:if test="not( $result='' )">
        <xsl:variable name="pkg-name"
                      select="preproc:pkg-name( . )" />

        <h1 id="pkg-{$pkg-name}" class="sym-ref sym-pkg">
          <xsl:value-of select="concat( '/', $pkg-name )" />
        </h1>

        <xsl:copy-of select="$result" />
      </xsl:if>
    </xsl:for-each>
  </div>
</xsl:template>


<xsl:template name="get-menuitem-basic">
  <xsl:param name="src" />
  <xsl:param name="type" />
  <xsl:param name="title" />
  <xsl:param name="after" />

  <xsl:variable name="deps">
    <xsl:call-template name="get-menu-type">
      <xsl:with-param name="type" select="$type" />
      <xsl:with-param name="src" select="$src" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="$deps/*">
    <h2><xsl:value-of select="$title" /></h2>
    <ul>
      <xsl:for-each select="$deps/*">
        <xsl:sort select="@name" data-type="text" />
        <li>
          <a href="#{@name}" class="sym-ref sym-{$type}">
            <xsl:choose>
              <xsl:when test="$after">
                <xsl:value-of select="substring-after( @name, $after )" />
              </xsl:when>

              <xsl:otherwise>
                <xsl:value-of select="@name" />
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </li>
      </xsl:for-each>
    </ul>
  </xsl:if>
</xsl:template>


<xsl:template match="/lv:*" mode="gen-menu">
  <xsl:param name="src" />
  <xsl:param name="pkg" select="$src" />

  <xsl:variable name="syms" select="l:dep/preproc:sym" />

  <xsl:call-template name="get-menuitem-basic">
    <xsl:with-param name="title" select="'Types'" />
    <xsl:with-param name="type" select="'type'" />
    <xsl:with-param name="src" select="$src" />
  </xsl:call-template>

  <xsl:call-template name="get-menuitem-basic">
    <xsl:with-param name="title" select="'Parameters'" />
    <xsl:with-param name="type" select="'param'" />
    <xsl:with-param name="src" select="$src" />
  </xsl:call-template>

  <xsl:call-template name="get-menuitem-basic">
    <xsl:with-param name="title" select="'Global Constants'" />
    <xsl:with-param name="type" select="'const'" />
    <xsl:with-param name="src" select="$src" />
  </xsl:call-template>

  <xsl:call-template name="get-menuitem-basic">
    <xsl:with-param name="title" select="'Functions'" />
    <xsl:with-param name="type" select="'func'" />
    <xsl:with-param name="src" select="$src" />
  </xsl:call-template>

  <xsl:call-template name="get-menuitem-basic">
    <xsl:with-param name="title" select="'Classifications'" />
    <xsl:with-param name="type" select="'class'" />
    <xsl:with-param name="src" select="$src" />
    <xsl:with-param name="after" select="':class:'" />
  </xsl:call-template>


  <xsl:variable name="rblocks">
    <xsl:call-template name="get-menu-type">
      <xsl:with-param name="type" select="'rate'" />
      <xsl:with-param name="src" select="$src" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="$rblocks/*">
    <h2>Rate Blocks</h2>
    <ul>
      <xsl:variable name="rates" select="$rblocks/*" />

      <!-- to get the rate groups, we need to consult the source package -->
      <xsl:variable name="src" select="
          document( concat( $pkg, '.xmlo' ), . )/lv:*
        " />

      <xsl:if test="not( $src )">
        <xsl:message terminate="yes">
          <xsl:text>!!! failed to locate </xsl:text>
          <xsl:value-of select="@name" />
          <xsl:text> source document</xsl:text>
        </xsl:message>
      </xsl:if>

      <xsl:for-each select="$src/lv:rate[ @yields=$rates/@name ]">
        <li>
          <a href="#{@yields}" class="sym-ref sym-rate">
            <xsl:value-of select="@yields" />
          </a>
        </li>
      </xsl:for-each>
    </ul>
  </xsl:if>
</xsl:template>


<xsl:template name="get-menu-type">
  <xsl:param name="type" />
  <xsl:param name="src" />

  <!-- note that this does not output preprocessor-generated symbols, as that
       may yield a lot of clutter -->
  <xsl:copy-of select="
    l:dep/preproc:sym[
      @type=$type
      and not( @preproc:generated='true' )
      and (
        $src and @src=$src
        or (
          not( $src )
          and ( not( @src ) or @src='' )
        )
      )
      and not( starts-with( @orig-name, '__' ) )
      and not( starts-with( @orig-name, '--' ) )
    ]" />
</xsl:template>


<xsl:template match="lvv:error" priority="1">
  <li>
    <xsl:value-of select="@desc" />

    <xsl:if test="@path != ''">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="@path" />
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:text>: </xsl:text>
    <span class="content">
      <xsl:value-of select="." />
    </span>
  </li>
</xsl:template>

<xsl:template match="preproc:error" priority="1">
  <li>
    <xsl:text>Preprocessor error</xsl:text>

    <xsl:if test="@path != ''">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="@path" />
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:text>: </xsl:text>
    <span class="content">
      <xsl:value-of select="." />
    </span>
  </li>
</xsl:template>


<!--
  Generate package details

  The $path parameter is used to provide a link to navigate directly to the
  package XML, which could otherwise not be determined from the package XML
  itself.

  @param string path        path to included package
  @param string import-path path as it appears on lv:import (no extension)

  @return generated package HTML
-->
<xsl:template match="lv:package">
  <xsl:param name="path" />
  <xsl:param name="import-path" />

  <xsl:message>[summary] [<xsl:value-of select="@name" />]</xsl:message>

  <!-- used as an anchor -->
  <xsl:attribute name="id">
    <xsl:text>pkg-</xsl:text>
    <xsl:value-of select="$import-path" />
  </xsl:attribute>

  <p class="desc">
    <xsl:value-of select="@desc" />
  </p>

  <xsl:apply-templates select="*" />
</xsl:template>


<!--
  Generate list of package imports

  Both raters and packages may import other packages.

  @param Node root root node on which to scan for imports

  @return package imports HTML
-->
<xsl:template name="gen-imports">
  <!--
  <div class="imports">
    <xsl:for-each select="$packages">
      <xsl:if test="position() > 1">
        <xsl:text>, </xsl:text>
      </xsl:if>

      <a>
        <xsl:attribute name="href">
          <xsl:text>#pkg-</xsl:text>
          <xsl:value-of select="@package" />
        </xsl:attribute>

        <xsl:value-of select="@package" />
      </a>
    </xsl:for-each>

    <xsl:if test="not($packages)">
      <em>(none)</em>
    </xsl:if>
  </div>
  -->
</xsl:template>


<xsl:template name="_debug">
  <xsl:param name="text" />

  <xsl:message>
    <xsl:text>[summary] </xsl:text>
    <xsl:value-of select="$text" />
    <xsl:text> for </xsl:text>
    <xsl:value-of select="ancestor::lv:package/@name" />
  </xsl:message>
</xsl:template>


<xsl:function name="preproc:pkg-name">
  <xsl:param name="ref-sym" />

  <xsl:sequence select="if ( not( $ref-sym/@src ) ) then
                          root( $ref-sym )/lv:*/@name
                        else
                          document( concat( $ref-sym/@src, '.xmlo' ), $ref-sym )
                            /lv:*/@name" />
</xsl:function>


<xsl:template name="pkg-out">
  <xsl:variable name="name" select="/lv:*/@name" />

  <a href="#pkg-{$name}" class="pkg sym-ref sym-pkg">
    <xsl:text>/</xsl:text>
    <xsl:value-of select="$name" />
  </a>
</xsl:template>


<!--
  Generate parameter/constant list

  The list will contain symbols associated with each param/const, its type,
  [default] value and description.

  @return parameter/constant list HTML

  FIXME: this is broken!
-->
<xsl:template match="lv:param|lv:const|lv:item" priority="1">
  <xsl:variable name="class">
    <xsl:text>param</xsl:text>

    <!-- indicate if this param is used for classification -->
    <xsl:if test="@name = //lv:classify//lv:match/@on">
      <xsl:text> classifies</xsl:text>
    </xsl:if>
  </xsl:variable>

  <xsl:call-template name="_debug">
    <xsl:with-param name="text">
      <xsl:text>processing </xsl:text>
      <xsl:value-of select="local-name()" />
    </xsl:with-param>
  </xsl:call-template>

  <xsl:variable name="name" select="@name" />

  <xsl:variable name="deps" as="element( preproc:sym-dep )?"
                select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                          @name=$name ]" />

  <fieldset class="params math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:value-of select="@name" />
    </xsl:attribute>

    <xsl:variable name="type"
                  select="if ( local-name() = 'item' ) then
                              'const'
                            else
                              local-name()" />

    <legend class="sym-{$type}">
      <xsl:variable name="sym"
                    select="/lv:*/preproc:symtable
                              /preproc:sym[ @name=$name ]" />

      <xsl:variable name="tex" select="$sym/@tex" />

      <!-- only show symbol if it is defined (no need for a default since
           defaults are specific to a given block) -->
      <xsl:if test="$tex and not( $tex = '' )">
        <xsl:text>\(</xsl:text>
          <xsl:value-of select="$tex" />
        <xsl:text>\) </xsl:text>
      </xsl:if>

      <xsl:value-of select="@name" />

      <!-- type -->
      <span class="type">
        <xsl:variable name="parse">
          <xsl:call-template name="get-symbol">
            <xsl:with-param name="name"
                            select="if ( not( @type ) ) then
                                      $sym/@dtype
                                    else
                                      @type" />
            <xsl:with-param name="default" select="'\mathbb{R}'" />
          </xsl:call-template>
        </xsl:variable>

        <xsl:text> \(\in </xsl:text>
          <xsl:apply-templates select="$parse" mode="typeset-final">
            <xsl:with-param name="deps" select="$deps" />
          </xsl:apply-templates>
        <xsl:text>\)</xsl:text>

        <xsl:if test="@type">
          <xsl:text> (</xsl:text>
          <a>
            <xsl:attribute name="href">
              <xsl:text>#</xsl:text>
              <xsl:value-of select="@type" />
            </xsl:attribute>

            <xsl:value-of select="@type" />
          </a>

          <!-- if a set, display type -->
          <xsl:if test="@set">
            <xsl:text> </xsl:text>
            <xsl:value-of select="@set" />
          </xsl:if>
          <xsl:text>)</xsl:text>
        </xsl:if>
      </span>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <p class="desc">
      <xsl:value-of select="@desc" />
    </p>

    <!-- default value -->
    <xsl:if test="@default">
      <div class="default">
        <b>Default: </b>

        <!-- if a set, indicate-->
        <xsl:if test="@set">
          <xsl:text>{</xsl:text>
        </xsl:if>

        <xsl:choose>
          <xsl:when test="@default != ''">
            <xsl:value-of select="@default" />
          </xsl:when>

          <!-- we'll interpret an empty string as, well, an empty string
               (which is denoted by epsilon in computer science) -->
          <xsl:otherwise>
            <xsl:text>\(\epsilon\)</xsl:text>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:if test="@set">
          <xsl:text>}</xsl:text>
        </xsl:if>
      </div>
    </xsl:if>

    <!-- generate list of classifications that use this -->
    <xsl:apply-templates select="." mode="gen-class-use-list" />

    <!-- value (constants) -->
    <xsl:if test="@value">
      <div class="default">
        <b>Value: </b><xsl:value-of select="@value" />
      </div>
    </xsl:if>

    <xsl:choose>
      <!-- prevent ridiculous compilation times and output -->
      <xsl:when test="count( .//* ) > 200">
        <xsl:text>Too many values; not typesetting.</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <!-- set values (constants) -->
        <xsl:if test="./lv:item">
          <div class="default">
            <b>Values: </b>
            <xsl:text>\(\left[\begin{array}\\</xsl:text>

            <xsl:for-each select="./lv:item">
              <xsl:if test="position() > 1">
                <xsl:text>\\</xsl:text>
              </xsl:if>

              <xsl:value-of select="@value" />
            </xsl:for-each>

            <xsl:text>\end{array}\right]\)</xsl:text>
          </div>
        </xsl:if>

        <xsl:if test="./lv:set">
          <div class="default">
            <b>Values: </b>
            <xsl:text>\( \left[ \begin{array}\\</xsl:text>
              <xsl:for-each select="./lv:set">
                <xsl:if test="position() > 1">
                  <xsl:text>\\</xsl:text>
                </xsl:if>

                <xsl:for-each select="./lv:item">
                  <xsl:if test="position() > 1">
                    <xsl:text> &amp; </xsl:text>
                  </xsl:if>

                  <xsl:value-of select="@value" />
                </xsl:for-each>
              </xsl:for-each>
            <xsl:text>\end{array} \right] \)</xsl:text>
          </div>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:variable name="typedef"
                  select="ancestor::lv:typedef[ 1 ]" />
    <xsl:if test="exists( $typedef )">
      <p>Defined by type
        <a href="#{$typedef/@name}">
          <xsl:value-of select="$typedef/@name" />
        </a>
      </p>
    </xsl:if>
  </fieldset>
</xsl:template>


<xsl:template match="lv:param" mode="gen-class-use-list">
  <!-- FIXME: this used to make use of lv:required-params, which no longer
       exists -->
</xsl:template>


<xsl:template match="lv:rate" mode="gen-class-use-list">
  <a href="#{@yields}">
    <xsl:value-of select="@yields" />
  </a>
</xsl:template>

<xsl:template match="lv:function" mode="gen-class-use-list">
  <a href="#{@name}">
    <xsl:value-of select="@name" />
  </a>
</xsl:template>

<xsl:template match="lv:yield" mode="gen-class-use-list">
  <a href="#yields_premium">
    <xsl:text>(final premium)</xsl:text>
  </a>
</xsl:template>


<!--
  Generates a list of all user-defined types

  A heading will be output before the first typedef.

  @return typedef HTML
-->
<xsl:template match="lv:typedef[ not( lv:union ) ]">
  <xsl:param name="heading" select="true()" />

  <!-- output heading on first match -->
  <xsl:if test="( @name = ../lv:typedef[1]/@name ) and $heading">
    <xsl:call-template name="_debug">
      <xsl:with-param name="text">
        <xsl:text>processing typedefs</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <fieldset class="typedef math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:value-of select="@name" />
    </xsl:attribute>

    <legend class="sym-type">
      <xsl:value-of select="@desc" />

      <span class="name">
        <xsl:text> (</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>)</xsl:text>
      </span>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <!-- output its type, as determined by its first element (there should only
         ever be one) -->
    <div class="type">
      <strong>Type: </strong>
      <xsl:value-of select="local-name(*[1])" />

      <xsl:variable name="type_type">
        <xsl:value-of select="*[1]/@type" />
      </xsl:variable>

      <!-- output type of its type, if exists -->
      <xsl:if test="$type_type != ''">
        <xsl:text> (</xsl:text>
        <xsl:value-of select="$type_type" />
        <xsl:text>)</xsl:text>
      </xsl:if>
    </div>

    <xsl:apply-templates select="*" />
  </fieldset>

  <!-- items also need to be processed independently as constants
       (they are represented as such in the symbol table)  -->
  <xsl:apply-templates select=".//lv:item" />
</xsl:template>


<!--
  Recursively generates typedefs within a union

  @return union typedef HTML
-->
<xsl:template match="lv:typedef[ lv:union ]">
  <fieldset class="typedef math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:value-of select="@name" />
    </xsl:attribute>

    <legend class="sym-type">
      <xsl:value-of select="@desc" />

      <span class="name">
        <xsl:text> (</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>)</xsl:text>
      </span>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <p>Union of:</p>

    <ul>
      <xsl:for-each select="lv:union/lv:typedef">
        <li>
          <a href="#{@name}">
            <xsl:value-of select="@name" />
          </a>
        </li>
      </xsl:for-each>
    </ul>
  </fieldset>

  <!-- recurse on the nested typedefs -->
  <xsl:apply-templates select="lv:union/node()">
    <xsl:with-param name="heading" select="false()" />
  </xsl:apply-templates>
</xsl:template>


<!--
  Generates table of enumerated values

  @return enumerated value table HTML
-->
<xsl:template match="lv:typedef/lv:enum">
  <table class="enum">
    <thead>
      <th>Name</th>
      <th>Value</th>
      <th>Description</th>
    </thead>

    <tbody>
      <xsl:for-each select="lv:item">
        <tr>
          <td>
            <a href="#{@name}" class="sym-ref sym-const">
              <xsl:value-of select="@name" />
            </a>
          </td>
          <td><xsl:value-of select="@value" /></td>
          <td><xsl:value-of select="@desc" /></td>
        </tr>
      </xsl:for-each>
    </tbody>
  </table>
</xsl:template>


<!--
  Indicates a base type

  @return base type HTML
-->
<xsl:template match="lv:base-type">
  <p><em>Base type declaration (defined internally)</em></p>
</xsl:template>


<!--
  Do not output generated classifications

  In the case of lv:any|lv:all, the generated classifications are inlined as
  part of their parent.
-->
<xsl:template match="lv:classify[ @preproc:generated='true' ]" priority="5">
  <!-- no output -->
</xsl:template>


<!--
  Outputs plain-english classification information

  @return classification HTML
-->
<xsl:template match="lv:classify" priority="1">
  <xsl:variable name="as" select="@as" />

  <!-- output heading on first match -->
  <xsl:if test="@as = ../lv:classify[1]/@as">
    <xsl:call-template name="_debug">
      <xsl:with-param name="text">
        <xsl:text>processing classifications</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <fieldset class="class math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:text>:class:</xsl:text>
      <xsl:value-of select="@as" />
    </xsl:attribute>

    <legend class="sym-class">
      <xsl:value-of select="@desc" />
      <span class="name">
        <xsl:text> (</xsl:text>
        <span>
          <xsl:value-of select="@as" />
        </span>
        <xsl:text>)</xsl:text>
      </span>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <div class="ultra-breakdown">
      <fieldset>
        <xsl:apply-templates select="lv:match" />
      </fieldset>
    </div>

    <xsl:if test="@yields">
      <div class="yields">
        <xsl:attribute name="id">
          <xsl:value-of select="@yields" />
        </xsl:attribute>

        <span class="calc-yields">Yields: </span>

        <span class="sym-ref sym-cgen">
          <xsl:value-of select="@yields" />
        </span>
      </div>
    </xsl:if>
  </fieldset>
</xsl:template>


<!--
  Delimit multiple classifications with appropriate logical operation
-->
<xsl:template match="lv:match[
    preceding-sibling::lv:match
  ]"
  priority="8">

  <p>
    <em>
      <xsl:choose>
        <xsl:when test="parent::lv:classify/@any='true'">
          <xsl:text>or</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>and</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </em>
  </p>

  <xsl:apply-templates select="." mode="process-match" />
</xsl:template>


<!--
  Default case; process as-is
-->
<xsl:template match="lv:match" priority="1">
  <xsl:apply-templates select="." mode="process-match" />
</xsl:template>


<!--
  When matching on a generated classification, inline it

  This retains the style in which the classification was developed.
-->
<xsl:template match="lv:match[ @preproc:generated ]" priority="7" mode="process-match">
  <xsl:variable name="on" select="@on" />

  <!-- FIXME: it's assumed that generated classifications are always part of the
       same package (this is true right now, but may not be in the future!) -->
  <xsl:variable name="ref" select="
      //lv:classify[ @yields=$on ]/lv:match
    " />

  <xsl:if test="not( $ref )">
    <xsl:message>
      <xsl:text>warning: failed to locate generated class ref `</xsl:text>
        <xsl:value-of select="@on" />
      <xsl:text>'</xsl:text>
    </xsl:message>
  </xsl:if>

  <fieldset>
    <xsl:apply-templates select="$ref" />
  </fieldset>
</xsl:template>


<!--
  Outputs lv:match details in english

  @return plain-english match HTML
-->
<xsl:template match="lv:match" priority="5" mode="process-match">
  <xsl:variable name="on" select="@on" />
  <xsl:variable name="sym" select="preproc:sym-lookup( $on )" />

  <p class="debugid">
    <xsl:attribute name="id">
      <xsl:text>ubd-</xsl:text>
      <xsl:value-of select="@_id" />
    </xsl:attribute>

    <a class="sym-ref sym-{$sym/@type}">
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:choose>
          <xsl:when test="$sym/@parent">
            <xsl:value-of select="$sym/@parent" />
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="$sym/@name" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>

      <xsl:value-of select="@on" />
    </a>

    <xsl:variable name="on" select="@on" />
    <xsl:variable name="cref" select="//lv:classify[ @yields=$on ]" />

    <xsl:if test="$cref">
      <xsl:text> (</xsl:text>
        <xsl:value-of select="$cref/@desc" />
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:text> must </xsl:text>

    <xsl:apply-templates select="." mode="match-desc" />

    <xsl:if test="lv:assuming">
      <xsl:text>, assuming that:</xsl:text>

      <ul>
        <xsl:for-each select="lv:assuming/lv:that">
          <li>
            <!-- link to the ref -->
            <a>
              <xsl:attribute name="href">
                <xsl:text>#</xsl:text>
                <xsl:value-of select="@name" />
              </xsl:attribute>

              <xsl:value-of select="@name" />
            </a>

            <xsl:text> </xsl:text>
            <xsl:apply-templates select="." />
          </li>
        </xsl:for-each>
      </ul>
    </xsl:if>
  </p>
</xsl:template>


<xsl:template match="lv:assuming/lv:that[ @ignored ]" priority="5">
  <xsl:text>is ignored during classification</xsl:text>
</xsl:template>

<!-- we only do consts right now -->
<xsl:template match="lv:assuming/lv:that" priority="1">
  <xsl:text>has the value </xsl:text>
  <xsl:value-of select="@const" />
</xsl:template>


<!--
  Outputs a type match in plain english

  @param string anyOf type to match

  @return anyOf match HTML
-->
<xsl:template match="lv:match[@anyOf]" mode="match-desc">
  <xsl:variable name="anyOf" select="@anyOf" />

  <!-- attempt to locate the typedef -->
  <xsl:variable name="typedef" select="
      /lv:*/preproc:symtable/preproc:sym[ @name=$anyOf ]
    " />

  <xsl:text>match any value in </xsl:text>

  <xsl:choose>
    <xsl:when test="$typedef">
      <xsl:variable name="sym" select="$typedef/@sym" />

      <xsl:if test="$sym">
        <xsl:text>\(</xsl:text>
          <xsl:value-of select="$sym" />
        <xsl:text>\) (</xsl:text>
      </xsl:if>

      <a>
        <xsl:attribute name="href">
          <xsl:text>#</xsl:text>
          <xsl:value-of select="@anyOf" />
        </xsl:attribute>

        <xsl:value-of select="@anyOf" />
      </a>

      <xsl:if test="$sym">
        <xsl:text>)</xsl:text>
      </xsl:if>
    </xsl:when>

    <!-- if type does not eixst, output error message -->
    <xsl:otherwise>
      <span class="error">
        <xsl:text>[Undefined type: </xsl:text>
        <xsl:value-of select="@anyOf" />
        <xsl:text>]</xsl:text>
      </span>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="lv:match[@pattern]" mode="match-desc">
  <xsl:text>match the pattern: </xsl:text>
  <tt>
    <xsl:value-of select="@pattern" />
  </tt>
</xsl:template>


<!--
  Outputs a single-value match in plain english

  @return match HTML
-->
<xsl:template match="lv:match[@value]" mode="match-desc">
  <xsl:variable name="value" select="@value" />

  <xsl:variable name="sym"
                select="/lv:*/preproc:symtable/preproc:sym[ @name=$value ]" />

  <xsl:text>= </xsl:text>

  <a href="#{@value}" class="sym-ref sym-{$sym/@type}">
    <xsl:value-of select="@value" />
  </a>
</xsl:template>


<xsl:template match="lv:match[ ./c:* ]" mode="match-desc">
  <xsl:text> be </xsl:text>

  <xsl:text>\(</xsl:text>
    <xsl:apply-templates select="./c:*" mode="calc-iversons">
      <xsl:with-param name="recurse" select="false()" />
    </xsl:apply-templates>
  <xsl:text>\) </xsl:text>

  <xsl:apply-templates mode="match-desc"
                       select="c:*/c:*" />
</xsl:template>


<xsl:template match="c:value-of" mode="match-desc" priority="5">
  <xsl:variable name="name"
                select="@name" />

  <xsl:variable name="sym" as="element( preproc:sym )"
                select="/lv:*/preproc:symtable/preproc:sym[ @name=$name ]" />

  <a href="#{$name}" class="sym-ref sym-{$sym/@type}">
    <xsl:value-of select="$name" />
  </a>
</xsl:template>


<xsl:template match="c:const" mode="match-desc" priority="5">
  <xsl:text>\(</xsl:text>
  <xsl:value-of select="@value" />
  <xsl:text>\)</xsl:text>
</xsl:template>


<xsl:template match="c:*" mode="match-desc" priority="1">
  <xsl:message>
    <xsl:text>[summary] internal: unknown match calculation for `</xsl:text>
    <xsl:value-of select="ancestor::lv:classify/@as" />
    <xsl:text>'</xsl:text>
  </xsl:message>
</xsl:template>


<!--
  Output function details

  A heading will be output for the first function. The function name,
  description and variables, both local and global, will be included. The
  equation will be output in LaTeX format.

  Ultimately, very similar to lv:rate output.

  @return function HTML
-->
<xsl:template match="lv:function">
  <xsl:variable name="name" select="@name" />

  <!-- output heading on first match -->
  <xsl:if test="@name = ../lv:function[1]/@name">
    <xsl:call-template name="_debug">
      <xsl:with-param name="text">
        <xsl:text>processing functions</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:variable name="deps" as="element( preproc:sym-dep )"
                select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                          @name=$name ]" />

  <fieldset class="func math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:value-of select="@name" />
    </xsl:attribute>

    <legend class="sym-func">
      <xsl:text>\(</xsl:text>

      <xsl:choose>
        <xsl:when test="@sym">
          <xsl:value-of select="@sym" />
          <xsl:text>\;</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>\textrm{</xsl:text>
            <xsl:value-of select="@name" />
          <xsl:text>}</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:text>\)</xsl:text>

      <!-- if a symbol was provided, then include the name in parenthesis
           outside of the equation -->
      <xsl:if test="@sym">
        <xsl:text> (</xsl:text>
          <xsl:value-of select="@name" />
        <xsl:text>)</xsl:text>
      </xsl:if>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <div class="body">
      <p><em><xsl:value-of select="@desc" /></em></p>

      <!-- generate list of let statements to declare variables -->
      <xsl:apply-templates select="." mode="gen-let-list" />

      <br />
      <span class="calc-yields">Yields: </span>
      <xsl:text>\(</xsl:text>
        <xsl:variable name="result">
          <xsl:apply-templates select="./c:*" />
        </xsl:variable>

        <xsl:apply-templates select="$result" mode="typeset-final">
          <xsl:with-param name="deps" select="$deps" />
        </xsl:apply-templates>
      <xsl:text>\)</xsl:text>
    </div>

    <div class="right">
      <xsl:apply-templates select="." mode="gen-equation-details" />
    </div>

    <!-- provide extreme breakdown for debugging -->
    <xsl:apply-templates select="." mode="ultra-breakdown" />
  </fieldset>
</xsl:template>


<!--
  Output premium calculation details

  The title of the block will be generated from its classification. Each
  parameter used will be output in addition to context-specific constants.
  Functions will be displayed as calls using their full name, while an applied
  (single-level, not recursive) version of the function will appear separately
  for convenience (without its variable definitions).

  The equation itself will be output in LaTeX.

  Similar to the output of lv:function.

  @return premium calculation output
-->
<xsl:template match="lv:rate">
  <!-- we only do this so that other templates can explicitly do this -->
  <xsl:apply-templates select="." mode="gen-rate-block" />
</xsl:template>

<xsl:template match="lv:rate" mode="gen-rate-block">
  <xsl:variable name="root" select="/" />

  <xsl:variable name="name" select="@yields" />
  <xsl:variable name="deps" as="element( preproc:sym-dep )"
                select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                          @name=$name ]" />

  <fieldset class="rate math-typeset-hover">
    <!-- used as an anchor -->
    <xsl:attribute name="id">
      <xsl:value-of select="@yields" />
    </xsl:attribute>

    <legend class="sym-rate">
      <xsl:variable name="tex" select="
          /lv:*/preproc:symtable/preproc:sym[ @name=$name ]/@tex
        " />

      <!-- only show symbol if it is defined (no need for a default since
           defaults are specific to a given block) -->
      <xsl:if test="$tex and not( $tex = '' )">
        <xsl:text>\(</xsl:text>
          <xsl:value-of select="$tex" />
        <xsl:text>\) </xsl:text>
      </xsl:if>

      <xsl:text> </xsl:text>

      <span class="yields">
        <xsl:value-of select="@yields" />
      </span>

      <!-- output source package -->
      <xsl:call-template name="pkg-out" />
    </legend>

    <div class="body">
      <!-- applicability conditions (classifications) -->
      <div class="classes">
        <span class="prefix">Applicability: </span>

        <xsl:if test="not( lv:class )">
          <xsl:text>Always applicable.</xsl:text>
        </xsl:if>

        <xsl:for-each select="lv:class">
          <!-- have @no's come last -->
          <xsl:sort select="@no" data-type="text" order="ascending" />

          <xsl:variable name="cname" select="@ref" />
          <xsl:variable name="csymid" select="concat( ':class:', $cname )" />

          <xsl:if test="position() > 1">
            <xsl:text>, </xsl:text>
          </xsl:if>

          <!-- negate text if @no -->
          <xsl:if test="@no='true'">
            <xsl:choose>
              <xsl:when test="not( preceding-sibling::lv:class[ @no='true' ] )">
                <xsl:text>but not </xsl:text>
              </xsl:when>

              <xsl:otherwise>
                <xsl:text>nor </xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>

          <!-- this should not happen; if you found this by grepping for the
               error message, then have fun. -->
          <xsl:if test="@cname = ''">
            <xsl:message terminate="yes">
              <xsl:text>[summary] fatal: empty lv:class/@ref!</xsl:text>
            </xsl:message>
          </xsl:if>

          <!-- get class package -->
          <xsl:variable name="sym" select="preproc:sym-lookup( $csymid )" />

          <!-- output description -->
          <a href="#{$csymid}" class="sym-ref sym-class">
            <xsl:value-of select="$sym/@desc" />
          </a>
        </xsl:for-each>
      </div>

      <!-- generate list of let statements to declare variables -->
      <xsl:apply-templates select="." mode="gen-let-list">
        <xsl:with-param name="deps" select="$deps" />
        <xsl:with-param name="context" select="." />
      </xsl:apply-templates>

      <br />
      <span class="calc-yields">Yields: </span>
      <xsl:text>\(</xsl:text>
        <xsl:variable name="result">
          <xsl:apply-templates select="./c:*" />
        </xsl:variable>

        <xsl:apply-templates select="$result" mode="typeset-final">
          <xsl:with-param name="deps" select="$deps" />
        </xsl:apply-templates>
      <xsl:text>\)</xsl:text>
    </div>

    <div class="right">
      <xsl:apply-templates select="." mode="gen-equation-details" />
    </div>

    <!-- provide extreme breakdown for debugging -->
    <xsl:apply-templates select="." mode="ultra-breakdown" />
  </fieldset>
</xsl:template>


<xsl:template match="preproc:sym-ref" mode="typeset-final" priority="5">
  <xsl:param name="deps" as="element( preproc:sym-dep )?"/>

  <xsl:variable name="name" select="@name" />
  <xsl:variable name="tex" select="
      $deps/preproc:sym-ref[
        @name=$name
        or @varname=$name
      ]/@tex
    " />


  <xsl:text>{</xsl:text>
    <xsl:choose>
      <xsl:when test="@default and ( not( $tex ) or $tex='' )">
        <xsl:value-of select="@default" />
      </xsl:when>

      <xsl:when test="not( $tex ) or $tex=''">
        <xsl:message>
          <xsl:text>[summary] internal: missing TeX symbol for `</xsl:text>
            <xsl:value-of select="$name" />
          <xsl:text>' within the context of `</xsl:text>
            <xsl:value-of select="$deps/@name" />
          <xsl:text>'</xsl:text>
        </xsl:message>
        <xsl:text>?^!</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:choose>
          <!-- if the symbol contains an underscore, then wrap it in parenthesis
               so as not to confuse the reader with precedence rules -->
          <xsl:when test="contains( $tex, '_' )">
            <xsl:text>(</xsl:text>
              <xsl:value-of select="$tex" />
            <xsl:text>)</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="$tex" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template match="*" mode="typeset-final" priority="1">
  <xsl:param name="deps" as="element( preproc:sym-dep )"/>

  <xsl:copy>
    <xsl:copy-of select="@*" />

    <xsl:apply-templates mode="typeset-final">
      <xsl:with-param name="deps" select="$deps" />
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>

<xsl:template match="text()" mode="typeset-final" priority="1">
  <xsl:copy-of select="." />
</xsl:template>


<!--
  Break down the equation as much as possible, permitting the display of values
  for each individual portion.

  This is ridiculous. Seriously. This shit will blow your mind.
-->
<xsl:template match="lv:*" mode="ultra-breakdown" priority="5">
  <div class="ultra-breakdown">
    <h2>Calculation Breakdown</h2>
    <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
  </div>
</xsl:template>

<xsl:template match="lv:*" mode="ultra-breakdown" priority="1">
  <!-- not in debug mode; do nothing -->
</xsl:template>

<xsl:template match="c:arg|c:index" mode="ultra-breakdown" priority="5">
  <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
</xsl:template>

<xsl:template match="c:when/c:*" mode="ultra-breakdown" priority="5">
  <!-- do nothing with comparison operators; just process their children -->
  <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
</xsl:template>

<xsl:template match="c:let" mode="ultra-breakdown" priority="5">
  <fieldset>
    <legend>Scope boundary (let)</legend>

    <xsl:text>Each of the "let" statements below are only present </xsl:text>
    <xsl:text>within this scope boundary and exist to simplify the equation.</xsl:text>

    <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
  </fieldset>
</xsl:template>

<xsl:template match="c:let/c:values" mode="ultra-breakdown" priority="5">
  <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
</xsl:template>

<xsl:template match="c:let/c:values/c:value" mode="ultra-breakdown" priority="5">
  <xsl:variable name="parent"
                select="ancestor::lv:*[ 1 ]" />

  <xsl:variable name="pname"
                select="if ( $parent/@name ) then
                          $parent/@name
                        else
                          $parent/@yields" />

  <xsl:variable name="ref" select="@name" />

  <xsl:variable name="tex">
    <!-- generate symbol name -->
    <xsl:variable name="symname" select="
        concat( ':', ancestor::c:let[1]/@name, ':', $ref )
      " />

    <xsl:variable name="parse">
      <preproc:sym-ref name="{$symname}" />
    </xsl:variable>

    <xsl:variable name="deps" as="element( preproc:sym-dep )"
                  select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                            @name=$pname ]" />

    <xsl:apply-templates select="$parse" mode="typeset-final">
      <xsl:with-param name="deps" select="$deps" />
    </xsl:apply-templates>
  </xsl:variable>

  <xsl:call-template name="ultra-breakdown-set">
    <xsl:with-param name="c" select="./c:*" />

    <xsl:with-param name="label">
      <xsl:text>let \(</xsl:text>
        <xsl:value-of select="$tex" />
      <xsl:text>\) = </xsl:text>
      <xsl:value-of select="@desc" />
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="ultra-breakdown-set" match="c:*" mode="ultra-breakdown" priority="1">
  <xsl:param name="label" select="if ( @label ) then @label else @desc" />
  <xsl:param name="c" select="." />

  <xsl:variable name="from-tpl"
                select="preproc:from-template" />

  <fieldset>
    <xsl:if test="$from-tpl">
      <xsl:attribute name="class" select="'templated'" />
    </xsl:if>

    <legend class="debugid">
      <xsl:attribute name="id">
        <xsl:text>ubd-</xsl:text>
        <xsl:value-of select="$c/@_id" />
      </xsl:attribute>

      <xsl:if test="$label">
        <span class="label">
          <xsl:value-of select="$label" />
          <xsl:text> </xsl:text>
        </span>
      </xsl:if>

      <span class="uid">
        <xsl:text>(</xsl:text>
          <xsl:value-of select="$c/@_id" />
        <xsl:text>)</xsl:text>
      </span>

      <xsl:if test="$from-tpl">
        <xsl:for-each select="$from-tpl">
          <xsl:if test="position() gt 1">
            <xsl:text>, </xsl:text>
          </xsl:if>

          <span class="tplid sym-ref sym-tpl">
            <xsl:value-of select="@name" />
          </span>
        </xsl:for-each>
      </xsl:if>

      <xsl:if test="@name">
        <xsl:variable name="name" select="@name" />
        <xsl:variable name="sym"
                      select="/lv:*/preproc:symtable
                                /preproc:sym[ @name=$name ]" />

        <xsl:variable name="ref"
                      select="if ( $sym/@parent ) then
                                  $sym/@parent
                                else
                                  $name" />

        <xsl:text> </xsl:text>
        <a href="#{$ref}" class="sym-ref sym-{$sym/@type}">
          <xsl:value-of select="$name" />
        </a>
      </xsl:if>
    </legend>

    <xsl:apply-templates select="$c" mode="ultra-breakdown-equ" />
  </fieldset>
</xsl:template>

<xsl:template match="c:*" mode="ultra-breakdown-equ">
  <xsl:variable name="parent"
                select="ancestor::lv:*[ 1 ]" />

  <xsl:variable name="name"
                select="if ( $parent/@name ) then
                          $parent/@name
                        else
                          $parent/@yields" />

  <xsl:variable name="result">
    <xsl:text>\(</xsl:text>
      <xsl:apply-templates select=".">
        <xsl:with-param name="force-show" select="true()" />
        <xsl:with-param name="standalone" select="true()" />
      </xsl:apply-templates>
    <xsl:text>\)</xsl:text>
  </xsl:variable>

  <xsl:variable name="deps" as="element( preproc:sym-dep )"
                select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                         @name=$name] " />

  <xsl:apply-templates select="$result" mode="typeset-final">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>

  <xsl:apply-templates select="." mode="ultra-breakdown-inner" />
</xsl:template>

<xsl:template match="c:*" mode="ultra-breakdown-inner" priority="1">
  <xsl:apply-templates select="./c:*" mode="ultra-breakdown" />
</xsl:template>


<!--
  Generates additional information regarding equations (calculations)

  This information includes local (context-specific) constants, equation
  breakdowns when @label's are used and single-level function applications.

  @return additional information divs
-->
<xsl:template match="lv:function|lv:rate|lv:yield" mode="gen-equation-details">
  <xsl:variable name="root" select="/" />

  <xsl:variable name="name"
                select="if ( @name ) then @name else @yields" />

  <xsl:variable name="deps" as="element( preproc:sym-dep )"
                select="/lv:*/preproc:sym-deps/preproc:sym-dep[
                          @name=$name ]" />

  <xsl:if test=".//c:const">
    <div class="constants">
      <h4>Context-Specific Constants</h4>
      <ul>
        <xsl:for-each select=".//c:const">
          <li>
            <xsl:value-of select="@desc" />
            <xsl:text> = </xsl:text>
            <xsl:value-of select="@value" />
          </li>
        </xsl:for-each>
      </ul>
    </div>
  </xsl:if>

  <xsl:variable name="parts" select=".//c:*[@label]" />

  <xsl:if test="$parts">
    <h4>Summary Breakdown</h4>
    <div>
      <xsl:attribute name="class">
        <xsl:text>parts</xsl:text>

        <!-- indicate when we have a large number of parts to display (maybe the
             client will wish to render it in a column layout) -->
        <xsl:if test="count( $parts ) > 6">
          <xsl:text> many</xsl:text>
        </xsl:if>
      </xsl:attribute>

      <xsl:for-each select="$parts">
        <xsl:variable name="debug-id">
          <xsl:value-of select="@_id" />
        </xsl:variable>

        <div class="part debugid" id="{$debug-id}" title="{$debug-id}">
          <xsl:variable name="result">
            <xsl:text>\(</xsl:text>
              <!-- output the portion of the equation -->
              <xsl:apply-templates select=".">
                <xsl:with-param name="force-show" select="true()" />
              </xsl:apply-templates>
            <xsl:text>\)</xsl:text>
          </xsl:variable>

          <xsl:apply-templates select="$result" mode="typeset-final">
            <xsl:with-param name="deps" select="$deps" />
          </xsl:apply-templates>

          <div class="label">
            <xsl:value-of select="@label" />
          </div>
        </div>
      </xsl:for-each>
    </div>
  </xsl:if>

  <xsl:variable name="gen" select=".//c:*[ @generates ]" />

  <!-- generators? -->
  <xsl:if test="$gen">
    <div class="generators">
      <h4>Generators</h4>

      <xsl:for-each select="$gen">
        <div class="generator" id="{@generates}">
          <xsl:variable name="result">
            <xsl:text>\(</xsl:text>
              <xsl:text>\theta_{</xsl:text>
                <xsl:value-of select="preproc:tex-index( @index )" />
              <xsl:text>}</xsl:text>

              <!-- show equation -->
              <xsl:text>=</xsl:text>
              <xsl:apply-templates select="./c:*" />
            <xsl:text>\)</xsl:text>
          </xsl:variable>

          <xsl:apply-templates select="$result" mode="typeset-final">
            <xsl:with-param name="deps" select="$deps" />
          </xsl:apply-templates>

          <div class="desc">
            <xsl:value-of select="@desc" />
            <xsl:text> </xsl:text>
            <span class="sym-ref sym-gen">
              <xsl:value-of select="@generates" />
            </span>
          </div>
        </div>
      </xsl:for-each>
    </div>
  </xsl:if>
</xsl:template>


<!--
  Outputs equation that yields final premium

  The equation is formatted using LaTeX.

  @return final premium calculation HTML
-->
<xsl:template match="lv:yield">
  <div class="yield">
    <div class="body">
      <!-- generate list of let statements to declare variables -->
      <xsl:apply-templates select="." mode="gen-let-list" />

      <br />
      <xsl:text>\(</xsl:text>
      <xsl:apply-templates select="./c:*" />
      <xsl:text>\)</xsl:text>

      <!-- provide extreme breakdown for debugging -->
      <xsl:apply-templates select="." mode="ultra-breakdown" />
    </div>

    <div class="right">
      <xsl:apply-templates select="." mode="gen-equation-details" />
    </div>

    <br clear="both" />
  </div>
</xsl:template>


<xsl:template name="_get-single-class-desc">
  <xsl:param name="classname" />
  <xsl:param name="self" />
  <xsl:param name="imports" />

  <!-- locate description -->
  <xsl:variable name="desc">
    <xsl:value-of select="$self//lv:classify[@as=$classname]/@desc" />
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$desc">
      <!-- output the description that we found -->
      <a>
        <xsl:attribute name="href">
          <xsl:text>#:class:</xsl:text>
          <xsl:value-of select="$classname" />
        </xsl:attribute>

        <xsl:value-of select="$desc" />
      </a>
    </xsl:when>

    <!-- if no description could be found, output an error -->
    <xsl:otherwise>
      <span class="error">
        <xsl:text>[Undefined classification: </xsl:text>
        <xsl:value-of select="$classname" />
        <xsl:text>]</xsl:text>
      </span>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- basic summary info -->
<xsl:template name="summary-info">
  <div class="tcontent math-typeset-hover" id="__nb">
    <h2 class="nb">N.B.</h2>
    <p>
      This "Summary Page" provides both an overview of the rater as a whole
        and a breakdown of all of its details on an intimate level.
    </p>
    <dl>
      <dt>Iverson's Brackets</dt>
      <dd>
        <p>
          As is customary for many mathematical notations in CS, this system uses
          Iverson's convention (Iverson's brackets) to denote certain conditional
          expressions. It should be understood that the notation will produce a
          value of \( 1 \) if the expression is true; otherwise, it will be
          <em>strongly</em> \( 0 \) --- that is, even if the expression would
          be undefined, it will still yield \( 0 \).
        </p>
        <p>
          \( [ 1 \gt 0 ] = 1 \); \( [ 0 = 1 ] = 0 \); \( [ 5 \textrm{ is prime} ] = 1 \);
        </p>
        <p>
          \( \sum \limits_{1 \leq k \leq 5} k = \sum \limits_k k [ 1 \leq k \leq 5 ] \)
        </p>
      </dd>

      <dt>Arrays (Vectors, Matrices, etc.)</dt>
      <dd>
        <p>
          All sequences/arrays of values are represented as matrices.
          For one-dimensional arrays, column vectors are used; written
          horizontally, their notation is
          \(\left[\begin{array}\\x_0 &amp; x_1 &amp; \ldots &amp; x_n\end{array}\right]^T\),
          where the \(T\) means "transpose".
        </p>
        <p>
          In the equations represented above, it is to be assumed that undefined
          values in a vector are implicitly \(0\); this simplifies the representations of
          the various summations; they are not intended to be vigorous.
        </p>
        <p>
          For example: let \( x \) = \( \left[\begin{array}\\1 &amp; 2 &amp; 3\end{array}\right]^T \). Given the equation \(
          \sum_k x_k \), it is assumed that the solution is \( 1 + 2 + 3 = 6 \),
          not undefined. Formally, the former sum is to be interpreted as: \(
          \sum_{k=0}^n x_k \) where \( n \) is the length of vector \( x \), or \(
          \sum_k x_k [x_k \textrm{ is defined}] \) using Iverson's convention (the
          latter of which our first notation is based upon by simply omitting the
          brackets and implying their existence).
        </p>
      </dd>

      <dt>Counting Vectors</dt>
      <dd>
        Let \(\left|V\right|\) = the number of values within the vector \(V\); this notation is
        used within certain summations. You may also see the following notations:

        <ul>
          <li>
            \(\sum_{k} V_k\) to count the number of one-values in boolean vector
            \(V\) (e.g. if \(V\) denotes properties with swimming pools, we can
            count the number of swimming pools).
          </li>
          <li>
            \(\sum_{k=0}^{\left|V\right|-1} 1\) to count the number of values in vector \(V\).
          </li>
        </ul>
      </dd>

      <dt>Vector Arithmetic</dt>
      <dd>
        Only one type of vector arithmetic (dot products) is currently supported,
        but others may be done manually using sums and products. Dot products are
        denoted by \(a\cdot b\), where \(a\) and \(b\) are vectors.
      </dd>

      <dt>Subscript Precedence</dt>
      <dd>
        Subscripts should be applied from right to left. That is:
        \(V_{x_{y_z}}\) = \(V_{(x_{(y_z)})}\). In the event where a notation may
        be ambiguous (e.g. \(\theta_{1_x}\), since \(1_x\) could not possibly make
        sense in the context of this system), parenthesis will always be added to
        clarify intent.
      </dd>
    </dl>
  </div>
</xsl:template>


<xsl:template match="preproc:sym" mode="voi-order" priority="5">
  <xsl:param name="osym" />
  <xsl:variable name="ref" select="@name" />

  <!-- include generators first -->
  <xsl:variable name="nodes" select="
      $osym/preproc:sym[ @parent=$ref and @type='gen' ]
    " />
  <xsl:for-each select="$nodes">
    <xsl:text>['</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>',</xsl:text>
      <xsl:text>0</xsl:text>
      <!-- <xsl:value-of select="@depth" /> -->

    <!-- if a parent is available, then we should link to that instead -->
    <xsl:if test="@parent">
      <xsl:text>,'</xsl:text>
        <xsl:value-of select="@parent" />
      <xsl:text>'</xsl:text>
    </xsl:if>

    <xsl:text>],</xsl:text>
  </xsl:for-each>

  <!-- add ref itself after generators -->
  <xsl:text>['</xsl:text>
    <xsl:value-of select="$ref" />
  <xsl:text>',</xsl:text>
    <xsl:text>0</xsl:text>
    <!-- <xsl:value-of select="@depth" /> -->
  <xsl:text>],</xsl:text>
</xsl:template>

<xsl:template match="*" mode="voi-order" priority="1">
  <!-- nothing -->
</xsl:template>


<!-- imports are handled elsewhere, but we need this template to ensure that
     nothing is displayed for the element -->
<xsl:template match="lv:import">
  <!-- do nothing -->
</xsl:template>


<!-- preprocessor data -->
<xsl:template match="preproc:rate-deps">
  <h3>
    <xsl:text>Calculation Order (</xsl:text>
      <xsl:value-of select="count( ./preproc:flat/preproc:rate )" />
    <xsl:text>)</xsl:text>
  </h3>

  <ol class="calc-order">
    <xsl:for-each select="./preproc:flat/preproc:rate">
      <li>
        <a href="#{@ref}">
          <xsl:value-of select="@ref" />
        </a>
      </li>
    </xsl:for-each>
  </ol>
</xsl:template>


<xsl:template match="preproc:*">
  <!-- do nothing, for now (would be useful output), but we do have
       showPreprocessorOutput() -->
</xsl:template>


<xsl:template match="*">
  <!-- catch-all -->
</xsl:template>


<!--
  TODO: Everything should use this rather than consulting the symbol table
  directly!  Further, anything that does not return a symbol is likely not
  linked, in which case it should not be rendered at all.  Maybe we should
  output those somewhere.
-->
<xsl:function name="preproc:sym-lookup" as="element( preproc:sym )?">
  <xsl:param name="name" as="xs:string" />

  <!-- XXX: There's a linker bug where there may be duplicate symbols in
       l:dep! -->
  <xsl:sequence select="$program/l:dep/preproc:sym[ @name=$name ][1]" />
</xsl:function>


</xsl:stylesheet>
