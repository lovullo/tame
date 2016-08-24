<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node based on locality
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"

  xmlns:dot="http://www.lovullo.com/calc/dot"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<!--
  External nodes should be styled as such
-->
<xsl:template mode="dot:attr-extern" priority="5" match="
    *[ @src and not( @src='' ) ]
  ">

  <dot:attr name="style">dashed</dot:attr>
</xsl:template>



<!--
  Default node attributes
-->
<xsl:template match="preproc:sym" mode="dot:defnode-attr" priority="1">
</xsl:template>

</xsl:stylesheet>

