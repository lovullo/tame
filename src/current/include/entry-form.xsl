<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Outputs HTML form that can be used to feed values to the rater for testing

  Copyright (C) 2016, 2017 R-T Specialty, LLC.

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
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:l="http://www.lovullo.com/rater/linker"
  xmlns:util="http://www.lovullo.com/util"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:preproc="http://www.lovullo.com/rater/preproc"

  xmlns:exsl="http://exslt.org/common"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="exsl str">


<!--
  Generate HTML entry form for testing

  Allows for collection of data to feed to the rater.

  The entry form will only be generated for raters, not other packages (since
  actual rating will need to be performed).

  @return form HTML
-->
<xsl:template match="lv:package" mode="entry-form">
  <xsl:param name="root-pkg" />

  <form class="entry-form">
    <h1>Rating Test Case</h1>

    <div class="foot">
      <p id="prior-message"></p>

      <div>
        <input type="submit" value="Calculate Premium" />
        <input type="reset" value="Reset" />
      </div>

      <div class="final-premium"></div>

      <div class="final-accept">
        <button id="final-accept-good">Looks Good!</button>
        <button id="final-accept-bad">Incorrect</button>
      </div>

      <div class="final-comments">
        <h1>Submit Test Case</h1>

        <p>Submission comments (please describe what you were testing, the
        desired result and, if the premium was incorrect, what went wrong):</p>

        <textarea id="final-comments"></textarea>

        <div id="final-expect-container">
          <p>Expected premium (if known; must be exact); this will allow us to
          automatically re-run this test when we believe that the problem has been
          fixed. <strong>Otherwise, you must re-test manually:</strong></p>
          <input type="text" id="final-expected" value="" />
          <em>(Only fill out if it does not hit the minimum premium.)</em>
        </div>

        <br />
        <label><input type="checkbox" id="final-waiting"> Requires Testing</input></label>

        <br />
        <button id="final-submit">Submit</button>
        <button id="final-submit-new">Submit As New Test Case</button>
        <button id="final-cancel">Nevermind. Cancel.</button>
      </div>
    </div>

    <!-- generate form fields for each param -->
    <xsl:for-each-group select="/lv:package/l:dep/preproc:sym[ @type='param' ]"
                        group-by="@src">
      <xsl:variable name="pkg"
                    select="if ( not( @src = '' ) ) then
                                document( concat( @src, '.xmlo' ), $root-pkg )/lv:*
                              else
                                ()" />

      <xsl:variable name="pkg-display"
                    select="if ( $pkg ) then
                                concat( $pkg/@desc, ' (', $pkg/@name, ')' )
                              else
                                ''" />

      <xsl:variable name="pkg-name" select="$pkg/@name" />

      <fieldset class="param-set">
        <legend data-pkg-name="{$pkg-name}">
          <a href="#pkg-{$pkg-name}" class="sym-ref sym-pkg sym-large">
            <xsl:value-of select="$pkg-display" />
          </a>
        </legend>

        <xsl:variable name="syms" as="element( preproc:sym )*">
          <xsl:perform-sort select="current-group()">
            <xsl:sort select="@desc"></xsl:sort>
          </xsl:perform-sort>
        </xsl:variable>

        <dl>
          <xsl:apply-templates mode="entry-form" select="$syms">
            <xsl:with-param name="root-pkg" select="$root-pkg" />
          </xsl:apply-templates>
        </dl>
      </fieldset>
    </xsl:for-each-group>
  </form>

  <script type="text/javascript" src="{$fw-path}/rater/scripts/entry-form.js"></script>
</xsl:template>


<!--
  Generate text and input for a global parameter

  @return parameter HTML
-->
<xsl:template match="preproc:sym" mode="entry-form">
  <xsl:param name="root-pkg" />

  <xsl:variable name="self" select="." />
  <xsl:variable name="package" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), . )/lv:*
      else
        $root-pkg
    " />

  <xsl:variable name="name">
    <xsl:value-of select="@name" />

    <!-- if this is a set, then we will need to generate an array of
         elements -->
    <xsl:if test="number(@dim) gt 0">
      <xsl:text>[]</xsl:text>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="param"
    select="$package/lv:param[ @name=$self/@name ]" />

  <dt id="param-{@name}">
    <span class="param-desc">
      <xsl:value-of select="@desc" />
    </span>

    <span class="param-id">
      <xsl:text> </xsl:text>
      <a href="#{@name}" class="sym-ref sym-param">
        <xsl:value-of select="@name" />
      </a>
    </span>
  </dt>

  <xsl:variable name="matrix">
    <xsl:if test="number(@dim) gt 1">
      <xsl:text> matrix</xsl:text>
    </xsl:if>
  </xsl:variable>

  <!-- generate field itself -->
  <dd id="param-input-{@name}">
    <div class="entry-row{$matrix}">
      <div class="entry-field">
        <xsl:apply-templates select="$param" mode="entry-form-field">
          <xsl:with-param name="name" select="$name" />
          <xsl:with-param name="sym" select="$self" />
          <xsl:with-param name="pkg" select="$package" />
        </xsl:apply-templates>

        <!-- if this is a set, add the ability to remove values -->
        <xsl:if test="number(@dim) gt 0">
          <button class="entry-rm">-</button>
        </xsl:if>
      </div>

      <xsl:if test="number(@dim) gt 1">
        <button class="entry-add-matrix">+</button>
      </xsl:if>
    </div>

    <!-- if this is a set, add ability to add values -->
    <xsl:if test="number(@dim) gt 0">
      <button class="entry-add">+</button>
    </xsl:if>

    <div class="entry-testcase-dfn"></div>
  </dd>
</xsl:template>


<!--
  Generate input field for integer parameters

  @return parameter HTML
-->
<xsl:template match="lv:param[@type='integer']" mode="entry-form-field">
  <xsl:param name="name" select="@name" />
  <input type="text" name="{$name}" value="{@default}" />
</xsl:template>


<!--
  Generate input field for float parameters

  @return parameter HTML
-->
<xsl:template match="lv:param[@type='float']" mode="entry-form-field">
  <xsl:param name="name" select="@name" />
  <input type="text" name="{$name}" value="{@default}" />
</xsl:template>


<!--
  Generate radio fields for boolean parameters

  @return parameter HTML
-->
<xsl:template match="lv:param[@type='boolean']" mode="entry-form-field">
  <xsl:param name="name" select="@name" />

  <xsl:variable name="default-y">
    <xsl:if test="@default = '1'">
      <xsl:text>selected</xsl:text>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="default-n">
    <xsl:if test="@default = '0'">
      <xsl:text>selected</xsl:text>
    </xsl:if>
  </xsl:variable>

  <select name="{$name}">
    <option selected="{$default-y}" value="1">Yes</option>
    <option selected="{$default-n}" value="0">No</option>
  </select>
</xsl:template>


<!--
  Handle parameters that are either of unknown or user-defined types

  @return parameter HTML
-->
<xsl:template match="lv:param" mode="entry-form-field">
  <xsl:param name="name" select="@name" />
  <xsl:param name="sym" />
  <xsl:param name="pkg" />

  <xsl:variable name="type" select="@type" />

  <!-- the typedef may or may not be in the same package as the param -->
  <xsl:variable name="typesym" select="
      $pkg/preproc:symtable/preproc:sym[
        @type='type'
        and @name=$type
      ]
    " />

  <!-- if the @src attribute is empty, then it resides within the same package
       -->
  <xsl:variable name="typesrc"
                select="if ( $typesym/@src ) then
                            $typesym/@src
                          else
                            $sym/@src" />

  <!-- load the typedef from the appropriate package -->
  <xsl:variable name="typepkg" select="
      if ( $typesrc and not( $typesrc='' ) ) then
        document( concat( $typesrc, '.xmlo' ), $sym )/lv:*
      else
        $pkg
    " />
  <!-- this makes maintinance more difficult, but speeds up searching large
       trees -->
  <xsl:variable name="typedef" select="
      $typepkg//lv:typedef[ @name=$type ]
      |$typepkg//lv:typedef/lv:union/lv:typedef[ @name=$type ]
    " />

  <xsl:choose>
    <xsl:when test="$typedef/lv:enum|$typedef/lv:union">
      <xsl:apply-templates select="." mode="entry-form-field-enum">
        <xsl:with-param name="name" select="$name" />
        <xsl:with-param name="typedef" select="$typedef" />
      </xsl:apply-templates>
    </xsl:when>

    <xsl:otherwise>
      <xsl:message>
        <xsl:text>[summary] warning: unknown param type `</xsl:text>
          <xsl:value-of select="$typesym/@src" />
          <xsl:text>/</xsl:text>
          <xsl:value-of select="@type" />
        <xsl:text>'</xsl:text>
      </xsl:message>

      <span class="error">
        <xsl:text>Unknown type: </xsl:text>
        <xsl:value-of select="@type" />
      </span>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  Generate HTML for enumerated lists

  @return parameter HTML
-->
<xsl:template match="lv:param" mode="entry-form-field-enum">
  <xsl:param name="name" select="@name" />
  <xsl:param name="typedef" />

  <xsl:variable name="type" select="@type" />

  <!-- get all fields, even if they're within a union -->
  <xsl:variable name="fields" select="$typedef//lv:enum/lv:item" />

  <select name="{$name}" value="{@default}">
    <option value=""></option>

    <xsl:for-each select="$fields">
      <xsl:variable name="value">
        <xsl:choose>
          <xsl:when test="@value">
            <xsl:value-of select="@value" />
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@name" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <option value="{$value}">
        <xsl:value-of select="@name" />
        <xsl:text>: </xsl:text>
        <xsl:value-of select="@desc" />
      </option>
    </xsl:for-each>
  </select>
</xsl:template>

</xsl:stylesheet>
