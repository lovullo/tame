<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Writes preprocessor output to disk to eliminate the overhead of reprocessing
  for each task that requires such output.

  Also performs validation.

  N.B.: Requires XSLT >=2.0
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:lvp="http://www.lovullo.com">

<xsl:output method="text" />

<xsl:variable name="nl" select="'&#10;'" />


<xsl:template match="lv:rater|lv:package">
  <!-- output deps, one per line -->
  <xsl:for-each select="lv:import[ @package ]">
    <xsl:value-of select="concat( @package, $nl )" />
  </xsl:for-each>
</xsl:template>

<xsl:template match="lvp:program">
  <!-- output deps, one per line -->
  <xsl:for-each select="lvp:import[ @package ]">
    <xsl:value-of select="concat( @package, $nl )" />
  </xsl:for-each>
</xsl:template>


<xsl:template match="lvm:program-map|lvm:return-map">
  <!-- output deps, one per line -->
  <xsl:for-each select="lvm:import[ @path ]">
    <xsl:value-of select="concat( @path, $nl )" />
  </xsl:for-each>
</xsl:template>


<xsl:template match="w:worksheet">
  <xsl:value-of select="concat( @package, $nl )" />
</xsl:template>


<xsl:template match="*">
  <!-- do nothing -->
</xsl:template>

</xsl:stylesheet>
