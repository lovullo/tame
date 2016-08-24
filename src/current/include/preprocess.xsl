<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Preprocesses the XML

  This progress is aggressive; the resulting tree will follow the structure of
  the original XML, but will be heavily augmented and some parts rewritten.
-->

<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:preproc="http://www.lovullo.com/rater/preproc"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:t="http://www.lovullo.com/rater/apply-template"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:w="http://www.lovullo.com/rater/worksheet"
  xmlns:ext="http://www.lovullo.com/ext"
  xmlns:util="http://www.lovullo.com/util">


<xsl:include href="depgen.xsl" />
<xsl:include href="preproc/package.xsl" />


<!--
  Raters themselves get special treatment
-->
<xsl:template match="lv:rater" mode="preproc:compile" priority="9">
  <xsl:param name="orig-root" select="." />
  <xsl:param name="stopshort" />

  <xsl:message>
    <xsl:text>[preproc] [rater]</xsl:text>
  </xsl:message>

  <!-- handle package preprocessing -->
  <xsl:variable name="pkg-result">
    <xsl:call-template name="preproc:pkg-compile">
      <xsl:with-param name="orig-root" select="$orig-root" />
      <xsl:with-param name="stopshort" select="$stopshort" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:copy-of select="$pkg-result" />
</xsl:template>

</xsl:stylesheet>
