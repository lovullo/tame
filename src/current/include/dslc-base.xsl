<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Additional functionality provided by dslc

  Copyright (C) 2016 R-T Specialty, LLC.

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

  XSL does not provide every feature suitable for compilation (which is no
  suprise, since this was not its intended use case). As such, dslc provides
  additional features that are defined/abstracted within this file; every
  template that is intended for use with dslc should include this.
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
  Absolute path to root of TAME

  Relative paths between files in XSLT can be flaky because most operations
  are relative to the root stylesheet (not the filename of the stylesheet
  that a particular line of code is executing in).  Using absolute paths
  mitigates that definitively.
-->
<xsl:param name="__path-root" />


<!--
  Package source path, stripped of its extension

  XSL does not provide a means of exposing the file path (nor should it,
  really). This param will hold the source path of the package, sans its
  extension, relative to the project root that was used during compilation.

  I.e., given this source path:
    suppliers/common/foo.xml
  we would expect this value for __srcpkg:
    suppliers/common/foo

  By stripping the extension, we have the benefit of being void of any semantics
  that may be associated with it (e.g. xml vs xmlo vs xmle); rather, that
  information should be derived from the structe of the document itself and the
  path can be used as an identifier to describe the document as a whole,
  regardless of what form it is in.

  Consequently, no two files are able to have the same __srcpkg string; this
  value may therefore be used for disambiguation.
-->
<xsl:param name="__srcpkg" />


<!--
  Relative path to project root

  The project root is determined entirely by __srcpath by repeating the string
  "../" for the number of occurrances of "/".
-->
<xsl:param name="__relroot" />


<!--
  Random value that may be used to seed random values

  XSLT is deterministic and does not offer support for generating random values;
  its generate-id() function is not sufficient for cross-package generation.
-->
<xsl:param name="__rseed" />


<!--
  Root node of template on which stylesheet was invoked

  This points to the original, unprocessed document. This is especially
  important for `document' function calls, which use nodes as a reference
  point for resolving relative paths.
-->
<xsl:variable name="__entry-root" select="/" />



<!--
  Apply relative root to PATH

  If PATH is an absolute path, it will be prefixed with the relative root
  with the leading path delimiter stripped; otherwise, it will be echoed
  as-is.
-->
<xsl:template name="__apply-relroot">
  <xsl:param name="path" />

  <xsl:choose>
    <xsl:when test="starts-with( $path, '/' )">
      <xsl:value-of select="$__relroot" />
      <xsl:value-of select="substring-after( $path, '/' )" />
    </xsl:when>

    <xsl:otherwise>
      <xsl:value-of select="$path" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
