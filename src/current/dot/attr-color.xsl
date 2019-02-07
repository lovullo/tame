<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node color based on symbol type

  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

    This file is part of TAME.

    TAME is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
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

