<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Inline value parser

  Will parse all attributes and text of the form "a{b}c", where `b' is some
  variable.
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">


<xsl:template match="@*|text()" mode="lvm:valparse">
  <xsl:call-template name="lvm:valparse">
    <xsl:with-param name="str" select="." />
  </xsl:call-template>
</xsl:template>


<xsl:template name="lvm:valparse">
  <xsl:param name="str" />

  <xsl:variable name="pre" select="substring-before( $str, '{' )" />
  <xsl:variable name="post" select="substring-after( $str, '}' )" />

  <!-- get stuff between the two -->
  <xsl:variable name="postpre" select="substring-after( $str, '{' )" />
  <xsl:variable name="val" select="substring-before( $postpre, '}' )" />


  <xsl:if test="$pre">
    <xsl:text>'</xsl:text>
      <!-- TODO: escape -->
      <xsl:value-of select="$pre" />
    <xsl:text>' . </xsl:text>
  </xsl:if>

  <xsl:choose>
    <!-- variable reference -->
    <xsl:when test="$val">
      <xsl:call-template name="lvmp:gen-val">
        <xsl:with-param name="name" select="$val" />
      </xsl:call-template>
    </xsl:when>

    <!-- static value; no variable -->
    <xsl:otherwise>
      <xsl:text>'</xsl:text>
        <xsl:value-of select="$str" />
      <xsl:text>'</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:if test="$post">
    <xsl:text> . '</xsl:text>
      <!-- TODO: escape -->
      <xsl:value-of select="$post" />
    <xsl:text>'</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  Generate a variable reference for later rendering

  If a variable of the form x.y is found, it is recursively processed with `y'
  as the variable and `x' as the context: a dictionary lookup.
-->
<xsl:template name="lvmp:gen-val">
  <xsl:param name="name" />

  <xsl:variable name="trans" select="
      ancestor::lvm:c1-map/lvmp:translate[ @name=$name ]
    " />

  <xsl:choose>
      <!-- name translation requested -->
    <xsl:when test="$trans">
      <xsl:call-template name="lvmp:do-gen-val">
        <xsl:with-param name="name" select="$trans/@to" />
      </xsl:call-template>
    </xsl:when>

    <!-- no translation; use name as-is -->
    <xsl:otherwise>
      <xsl:call-template name="lvmp:do-gen-val">
        <xsl:with-param name="name" select="$name" />
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="lvmp:do-gen-val">
  <xsl:param name="name" />

  <!-- we may have a variable of the form `x.y', in which case we should
       process `y' withint he context of `x' -->
  <xsl:variable name="rightmost" select="
      substring-after( $name, '.' )
    " />

  <xsl:choose>
    <!-- no more key references -->
    <xsl:when test="$rightmost = ''">
      <lvmp:value ref="{$name}" />
    </xsl:when>

    <!-- recursively process key reference -->
    <xsl:otherwise>
      <!-- determine the key context, which is the entire base sans the
           rightmost variable name -->
      <xsl:variable name="context" select="
          substring( $name, 1, (
            string-length( $name ) - string-length( $rightmost ) - 1
          ) )
        " />

      <lvmp:value ref="{$rightmost}" index-key="{$context}">
        <xsl:call-template name="lvmp:gen-val">
          <xsl:with-param name="name" select="$context" />
        </xsl:call-template>
      </lvmp:value>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
