<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Utility templates
-->

<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:util="http://www.lovullo.com/util"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:ext="http://www.lovullo.com/ext">


<xsl:variable name="_chrlower" select="'abcdefghijklmnopqrstuvwxyz'" />
<xsl:variable name="_chrupper" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

<!--
  Path to map directory
-->
<xsl:param name="map-root" select="'../map/'" />
<xsl:param name="retmap-root" select="concat( $map-root, 'return/' )" />


<xsl:template name="util:load-package">
  <xsl:param name="package" />
  <xsl:param name="self" />

  <xsl:variable name="path" select="concat($package, '.xml')" />
  <xsl:variable name="pkg" select="document( $path, $self )/lv:package" />

  <ext:package name="{$pkg/@name}">
    <!-- path, including extension -->
    <xsl:attribute name="path">
      <xsl:value-of select="$path" />
    </xsl:attribute>

    <!-- path, excluding extension (as it appears in @package) -->
    <xsl:attribute name="import-path">
      <xsl:value-of select="@package" />
    </xsl:attribute>

    <xsl:copy-of select="$pkg" />
  </ext:package>
</xsl:template>


<xsl:template match="lvm:*" mode="util:map-expand" priority="1">
  <xsl:param name="path" select="'.'" />

  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:apply-templates mode="util:map-expand">
      <xsl:with-param name="path" select="$path" />
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>

<!-- recursively inline imports -->
<xsl:template match="lvm:import" mode="util:map-expand" priority="5">
  <xsl:param name="path" select="'.'" />

  <xsl:apply-templates
    select="document( concat( @path, '.xml' ), . )/lvm:*/*"
    mode="util:map-expand">

    <xsl:with-param name="path" select="concat( $path, '/', @path )" />
  </xsl:apply-templates>
</xsl:template>

<!-- must be lower priority than lv:import -->
<xsl:template match="/lvm:*/lvm:*" mode="util:map-expand" priority="3">
  <xsl:param name="path" select="'.'" />

  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:attribute name="__src" select="$path" />

    <xsl:apply-templates mode="util:map-expand">
      <xsl:with-param name="path" select="$path" />
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>


<!--
  Converts first character to uppercase

  Functions like ucfirst in PHP

  @param string str string to ucfirst

  @return string provided string with first character converted to uppercase
-->
<xsl:template name="util:ucfirst">
  <xsl:param name="str" />

  <!-- convert first char to uppercase -->
  <xsl:value-of
    select="translate( substring( $str, 1, 1 ), $_chrlower, $_chrupper )" />

  <!-- remainder of string as it was provided -->
  <xsl:value-of select="substring( $str, 2 )" />
</xsl:template>


<!--
  Converts a string to uppercase
-->
<xsl:template name="util:uppercase">
  <xsl:param name="str" />
  <xsl:value-of select="translate( $str, $_chrlower, $_chrupper )" />
</xsl:template>

<!--
  Converts a string to lowercase
-->
<xsl:template name="util:lowercase">
  <xsl:param name="str" />
  <xsl:value-of select="translate( $str, $_chrupper, $_chrlower )" />
</xsl:template>


<xsl:template name="util:json">
  <xsl:param name="id" />
  <xsl:param name="value" />
  <xsl:param name="obj" />
  <xsl:param name="array" />

  <xsl:if test="$id">
    <xsl:text>"</xsl:text>
      <xsl:call-template name="util:json-escape">
        <xsl:with-param name="string" select="$id" />
      </xsl:call-template>
    <xsl:text>":</xsl:text>
  </xsl:if>

  <xsl:choose>
    <xsl:when test="$array">
      <xsl:text>[</xsl:text>
        <xsl:for-each select="$array/*">
          <xsl:if test="position() > 1">
            <xsl:text>,</xsl:text>
          </xsl:if>

          <xsl:value-of select="." />
        </xsl:for-each>
      <xsl:text>]</xsl:text>
    </xsl:when>

    <xsl:when test="$obj">
      <xsl:text>{</xsl:text>
        <xsl:for-each select="$obj/*">
          <xsl:if test="position() > 1">
            <xsl:text>,</xsl:text>
          </xsl:if>

          <xsl:value-of select="." />
        </xsl:for-each>
      <xsl:text>}</xsl:text>
    </xsl:when>

    <xsl:when test="$value">
      <xsl:text>"</xsl:text>
        <xsl:call-template name="util:json-escape">
          <xsl:with-param name="string" select="$value" />
        </xsl:call-template>
      <xsl:text>"</xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <xsl:message terminate="yes">[util] !!! invalid util:json</xsl:message>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="util:json-escape">
  <xsl:param name="string" />

  <xsl:value-of select="$string" />
</xsl:template>

</xsl:stylesheet>
