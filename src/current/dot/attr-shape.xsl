<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node shape based on symbol type
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Parameter
-->
<xsl:template mode="dot:attr-shape" priority="5"
  match="*[ @type='param' ]">

  <dot:attr name="shape">box</dot:attr>
</xsl:template>


<!--
  Classification
-->
<xsl:template mode="dot:attr-shape" priority="5"
  match="*[ @type='class' or @type='cgen' ]">

  <dot:attr name="shape">octagon</dot:attr>
</xsl:template>


<!--
  Function
-->
<xsl:template mode="dot:attr-shape" priority="5"
  match="*[ @type='func' ]">

  <dot:attr name="shape">component</dot:attr>
</xsl:template>


<!--
  Map
-->
<xsl:template mode="dot:attr-shape" priority="5"
  match="*[ @type='map' or @type='retmap' ]">

  <dot:attr name="shape">note</dot:attr>
</xsl:template>


<!--
  Default
-->
<xsl:template match="*" mode="dot:attr-shape" priority="1">
  <!-- default shape -->
</xsl:template>

</xsl:stylesheet>

