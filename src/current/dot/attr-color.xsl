<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node color based on symbol type
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  Parameter
-->
<xsl:template mode="dot:attr-color" priority="5"
  match="*[ @type='param' ]">

  <dot:attr name="color">#204a87</dot:attr>
</xsl:template>


<!--
  Param type
-->
<xsl:template mode="dot:attr-color" priority="5"
  match="*[ @type='type' ]">

  <dot:attr name="color">#729fcf</dot:attr>
</xsl:template>


<!--
  Classification
-->
<xsl:template mode="dot:attr-color" priority="5"
  match="*[ @type='class' or @type='cgen' ]">

  <dot:attr name="color">#4e9a06</dot:attr>
</xsl:template>


<!--
  Function
-->
<xsl:template mode="dot:attr-color" priority="5"
  match="*[ @type='func' ]">

  <dot:attr name="color">#c4a000</dot:attr>
</xsl:template>


<!--
  Map
-->
<xsl:template mode="dot:attr-color" priority="5"
  match="*[ @type='map' or @type='retmap' ]">

  <dot:attr name="color">#888a85</dot:attr>
</xsl:template>


<!--
  Default
-->
<xsl:template match="*" mode="dot:attr-color" priority="1">
  <!-- no color -->
</xsl:template>

</xsl:stylesheet>

