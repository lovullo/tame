<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node based on keep flag
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  External nodes should be styled as such
-->
<xsl:template mode="dot:attr-keep" priority="5" match="
    *[ @keep='true' ]
  ">

  <dot:attr name="fontcolor">red</dot:attr>
</xsl:template>



<!--
  Default node attributes
-->
<xsl:template match="preproc:sym" mode="dot:defnode-keep" priority="1">
</xsl:template>

</xsl:stylesheet>

