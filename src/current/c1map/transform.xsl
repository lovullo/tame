<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Transforms values
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">


<xsl:template match="lvmp:var[ @transform='proper-case' ]" priority="5"
  mode="lvmp:transform">

  <xsl:param name="value" />

  <xsl:text>ucwords(strtolower(</xsl:text>
    <xsl:copy-of select="$value" />
  <xsl:text>))</xsl:text>
</xsl:template>



<xsl:template match="lvmp:var[ not( @transform ) or @transform='' ]"
    mode="lvmp:transform" priority="3">

  <xsl:param name="value" />

  <!-- no transformation; do nothing -->
  <xsl:copy-of select="$value" />
</xsl:template>


<xsl:template match="lvmp:var" mode="lvmp:transform" priority="2">
  <xsl:message terminate="yes">
    <xsl:text>error: unknown transformation `</xsl:text>
      <xsl:value-of select="@transform" />
    <xsl:text>' for `</xsl:text>
      <xsl:value-of select="@name" />
    <xsl:text>'</xsl:text>
  </xsl:message>
</xsl:template>


<xsl:template match="lvmp:*" mode="lvmp:transform" priority="1">
  <xsl:message terminate="yes">
    <xsl:text>internal error: unexpected node for transformation: </xsl:text>
    <xsl:copy-of select="." />
  </xsl:message>
</xsl:template>

</xsl:stylesheet>

