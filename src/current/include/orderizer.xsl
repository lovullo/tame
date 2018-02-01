<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  No, not "odorizer".

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

  This is a powerful system that takes an immensely complex (and insurmountable)
  task out of the programmer's hands. In particular, the system:

    - Uses the map to recognize the order in which params appear in a UI
    - Using that order, determines which fields in the UI could potentially
      cause fields that appear previous to them to become invalid (e.g. hide)
      due to classification criteria.
    - Based on those conflicts, constructs a custom classification that ignores
      only the conflicting portions, allowing a great deal of precision in
      controlling field validity up to that particular point in the UI.

  The result is highly-refined, custom-generated classifications per-question
  for only the criteria that are needed to ensure that fields cannot hide fields
  previous to them: A task that would otherwise be prohibitively complicated (or
  would otherwise have to be far too coarse) if done manually in systems with
  highly sophisticated classification schemes. Furthermore, the result is wholly
  deterministic.

  It should be noted that futher benefit can be realized when the map is altered
  or questions in the UI are reordered; the system will simply re-calculate new
  classifications that yield desirable results.
-->
<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvp="http://www.lovullo.com"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:lvmc="http://www.lovullo.com/rater/map/compiler"
  xmlns:gc="http://www.lovullo.com/calc/global-classifier"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:o="http://www.lovullo.com/rater/compiler/orderizer"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">



<!--
  Determines param conflicts based on UI ordering
-->
<xsl:template name="o:analyze">
  <xsl:param name="class-deps" />
  <xsl:param name="ui" />
  <xsl:param name="sdmap" />
  <xsl:param name="param-classes" />

  <xsl:variable name="rater" select="." />

  <!-- put fields in the order that they appear in the UI -->
  <!-- TODO: duplicate check (error) -->
  <xsl:variable name="ordered">
    <o:ordered>
      <xsl:for-each select="$ui//lvp:question">
        <xsl:variable name="id" select="@id" />
        <xsl:copy-of select="$sdmap/lvmc:map[ @from=$id ]" />
      </xsl:for-each>
    </o:ordered>
  </xsl:variable>

  <!-- the param-class list gives us the classes that are directly used; let's
       get a list of all classes that are used by those classes as well, which
       will simplify the queries to come -->
  <xsl:message>[orderizer] recursively discovering param classes...</xsl:message>
  <xsl:variable name="param-classes-expanded">
    <xsl:apply-templates select="$param-classes" mode="o:gen-param-reflist">
      <xsl:with-param name="class-deps" select="$class-deps" />
    </xsl:apply-templates>
  </xsl:variable>

  <xsl:variable name="initial">
    <o:analysis>
      <xsl:for-each select="$ordered//lvmc:*">
        <xsl:variable name="cur" select="." />
        <xsl:variable name="pref" select="@to" />

        <!-- progress indicator -->
        <xsl:message>
          <xsl:text>[orderizer] checking previous UI siblings: </xsl:text>
          <xsl:value-of select="@to" />
        </xsl:message>

        <!-- preceding -->
        <xsl:for-each select="
          $param-classes/gc:param[
            @ref=$cur/preceding-sibling::lvmc:map/@to
          ]">

          <!-- immediate conflicts (we check these separately rather than in the
               xpath above, which would be cleaner, so that they can be
               processed -->
          <xsl:variable name="conflicts" select="
              .//gc:class[
                @ref=$param-classes-expanded/o:param-refs/o:param[
                  @ref=$pref
                ]/o:class/@ref
              ]
            " />

          <xsl:if test="$conflicts">
            <o:conflict ref="{@ref}" relative-to="{$pref}">
              <xsl:for-each select="$conflicts">
                <!-- record the immediate problem -->
                <o:class ref="{@ref}" yields="{@yields}" />
              </xsl:for-each>
            </o:conflict>
          </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
    </o:analysis>
  </xsl:variable>

  <xsl:apply-templates select="$initial/o:analysis" mode="o:reduce-analysis" />
</xsl:template>


<xsl:template match="gc:param-classes" mode="o:gen-param-reflist" priority="5">
  <xsl:param name="class-deps" />

  <o:param-refs>
    <xsl:apply-templates select="gc:param" mode="o:gen-param-reflist">
      <xsl:with-param name="class-deps" select="$class-deps" />
    </xsl:apply-templates>
  </o:param-refs>
</xsl:template>

<xsl:template match="gc:param" mode="o:gen-param-reflist" priority="5">
  <xsl:param name="class-deps" />

  <o:param ref="{@ref}">
    <xsl:apply-templates select="gc:class" mode="o:gen-param-reflist">
      <xsl:with-param name="class-deps" select="$class-deps" />
    </xsl:apply-templates>
  </o:param>
</xsl:template>

<xsl:template match="gc:class" mode="o:gen-param-reflist" priority="5">
  <xsl:param name="class-deps" />

  <!-- well, this class is certainly used -->
  <o:class ref="{@ref}" />

  <xsl:variable name="ref" select="@ref" />

  <!-- but how about things that use this class? -->
  <xsl:apply-templates select="$class-deps/preproc:class[ @ref=$ref ]"
    mode="o:gen-param-reflist">
    <xsl:with-param name="class-deps" select="$class-deps" />
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="preproc:class" mode="o:gen-param-reflist" priority="5">
  <xsl:param name="class-deps" />

  <o:class ref="{@ref}" />

  <xsl:apply-templates mode="o:gen-param-reflist">
    <xsl:with-param name="class-deps" select="$class-deps" />
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="*" mode="o:gen-param-reflist" priority="1">
  <!-- do nothing -->
</xsl:template>


<xsl:template match="preproc:class" mode="o:get-indirect-params" priority="5">
  <xsl:apply-templates mode="o:get-indirect-params" />
</xsl:template>

<xsl:template match="preproc:class-dep" mode="o:get-indirect-params" priority="5">
  <xsl:variable name="ref" select="@ref" />

  <xsl:apply-templates
    select="ancestor::preproc:class-deps/preproc:class[ @ref=$ref ]"
    mode="o:get-indirect-params" />
</xsl:template>

<xsl:template match="preproc:rate-dep" mode="o:get-indirect-params" priority="5">
  <xsl:variable name="ref" select="@ref" />

  <xsl:apply-templates
    select="//preproc:rate-deps/preproc:rate[ @ref=$ref ]"
    mode="o:get-indirect-params" />
</xsl:template>

<xsl:template match="preproc:param" mode="o:get-indirect-params" priority="5">
  <xsl:copy-of select="." />
</xsl:template>

<xsl:template match="*" mode="o:get-indirect-params" priority="1">
  <!-- do nothing -->
</xsl:template>


<!--
  Combines initial analysis results, merging common refs and de-duplicating
  conflicts.
-->
<xsl:template match="o:analysis" mode="o:reduce-analysis">
  <!-- start by combining the contents of all unique refs -->
  <xsl:variable name="combined">
    <xsl:for-each select="
        o:conflict[
          not( @ref=preceding-sibling::o:conflict/@ref )
        ]
      ">

      <xsl:variable name="ref" select="@ref" />
      <xsl:variable name="dups" select="
          following-sibling::o:conflict[ @ref=$ref ]
        " />

      <o:conflict ref="{@ref}">
        <!-- add relativity nodes -->
        <xsl:for-each select="$dups|.">
          <o:relative-to ref="{@relative-to}" />
        </xsl:for-each>

        <!-- add class deps -->
        <o:reasons>
          <xsl:copy-of select="($dups|.)/o:class" />
        </o:reasons>
      </o:conflict>
    </xsl:for-each>
  </xsl:variable>

  <!-- now, remove duplicates resulting from the above operation -->
  <xsl:copy>
    <xsl:copy-of select="@*" />

    <xsl:apply-templates select="$combined/o:conflict"
      mode="o:reduce-analysis" />
  </xsl:copy>
</xsl:template>

<xsl:template match="o:conflict" mode="o:reduce-analysis">
  <xsl:copy>
    <xsl:copy-of select="@*" />

    <!-- unique relative-to nodes -->
    <xsl:copy-of select="
        o:relative-to[
          not( @ref=preceding-sibling::o:relative-to/@ref )
        ]
      " />

    <xsl:apply-templates select="o:reasons" mode="o:reduce-analysis" />
  </xsl:copy>
</xsl:template>

<xsl:template match="o:reasons" mode="o:reduce-analysis">
  <xsl:copy>
    <xsl:copy-of select="@*" />

    <!-- unique reasons -->
    <xsl:copy-of select="
        o:class[
          not( @ref=preceding-sibling::o:class/@ref )
        ]
      " />
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
