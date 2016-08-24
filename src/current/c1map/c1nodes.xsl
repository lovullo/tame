<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Describes how ConceptOne nodes are handled in the output.

  Only nodes in the C1 XML namespace will be included in the output; all other
  nodes will be in error, except for nodes as part of the c1 map namespace,
  which are processed and will not be included in the output.

  The output is an array format used to generate the final XML at runtime; this
  format was not developed in conjunction with this project and is separate, so
  be sure that this compiler is updated if the format changes.
-->
<xsl:stylesheet version="2.0"
  xmlns:c1="http://www.epic-premier.com/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lvm="http://www.lovullo.com/rater/map/c1"
  xmlns:lvmp="http://www.lovullo.com/rater/map/c1/pp">

<!--
  Nodes with attributes or children are recursively processed and have the
  form:
    '>Name' => array( <recurse> )
-->
<xsl:template match="c1:*[*|@*]" priority="5">
  <!-- make the output a little bit sane -->
  <xsl:value-of select="$lvmp:nl" />

  <!-- defer node rendering; allows us to easily determine if there are
       siblings of the same name within a node boundary -->
  <lvmp:node name="{name()}" />
  <xsl:text> => </xsl:text>

  <lvmp:node-boundary>
    <xsl:apply-templates select="." mode="lvmp:c1-node-result" />
  </lvmp:node-boundary>
</xsl:template>


<!--
  The default behavior of c1 nodes is to simply output the nodes as-is, with
  variable substitutions.
-->
<xsl:template match="c1:*" mode="lvmp:c1-node-result" priority="1">
  <xsl:text>array( </xsl:text>
    <xsl:apply-templates select="@*|*" />
  <xsl:text>) </xsl:text>
</xsl:template>


<!--
  Text-only nodes are of the form:
    '>Name' => 'value'
-->
<xsl:template match="c1:*[text()]" priority="4">
  <!-- defer node rendering; allows us to easily determine if there are
       siblings of the same name within a node boundary -->
  <lvmp:node name="{name()}" />
  <xsl:text> => </xsl:text>

  <xsl:text></xsl:text>
    <!-- TODO: escape single quotes -->
    <xsl:apply-templates select="text()" mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>


<!--
  Attributes are of the format:
    '[Name]' => 'value'
-->
<xsl:template match="c1:*/@*" priority="5">
  <xsl:text>'[</xsl:text>
    <xsl:value-of select="name()" />
  <xsl:text>]' => </xsl:text>
    <xsl:apply-templates select="." mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>


<!-- alternative attribute format for special situations -->
<xsl:template match="lvm:attribute" priority="5">
  <xsl:text>'[</xsl:text>
    <xsl:value-of select="@name" />
  <xsl:text>]' => </xsl:text>
    <xsl:apply-templates select="@value" mode="lvm:valparse" />
  <xsl:text>, </xsl:text>
</xsl:template>


<xsl:template match="c1:*/@lvm:*" priority="6">
  <!-- discard all system attributes -->
  <!-- TODO: error once everything is properly implemented -->
</xsl:template>

</xsl:stylesheet>
