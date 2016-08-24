<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Outputs graph visualization of dependencies in DOT format
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<xsl:import href="./attr-color.xsl" />
<xsl:import href="./attr-shape.xsl" />
<xsl:import href="./attr-extern.xsl" />
<xsl:import href="./attr-keep.xsl" />


<!--
  External nodes should be styled as such
-->
<xsl:template mode="dot:defnode-attr" priority="5" match="
    preproc:sym[ @src and not( @src='' ) ]
  ">

  <dot:attr name="label">
    <xsl:value-of select="@src" />
    <xsl:text>/\n</xsl:text>
    <xsl:value-of select="@name" />
  </dot:attr>

  <dot:attr name="tooltip">
    <xsl:value-of select="@src" />
    <xsl:text>/</xsl:text>
    <xsl:value-of select="@name" />
  </dot:attr>
</xsl:template>



<!--
  Default node attributes
-->
<xsl:template match="preproc:sym" mode="dot:defnode-attr" priority="1">
</xsl:template>


<!--
  Render an attribute list as a comma-delimited string

  Expects a tree of dot:attr nodes where @name is the name of the attribute, and
  its normalized text is the value. The value will be quoted; double quotes must
  be manually escaped prior to calling this template.
-->
<xsl:template name="dot:render-attr-list">
  <xsl:param name="attr-list" />

  <xsl:for-each select="$attr-list/dot:attr">
    <xsl:if test="position() > 1">
      <xsl:text>, </xsl:text>
    </xsl:if>

    <xsl:value-of select="@name" />
    <xsl:text>="</xsl:text>
      <xsl:value-of select="normalize-space( text() )" />
    <xsl:text>"</xsl:text>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
