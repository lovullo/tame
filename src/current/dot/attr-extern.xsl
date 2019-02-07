<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Styles node based on locality

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

