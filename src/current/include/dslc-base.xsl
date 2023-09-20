<?xml version="1.0" encoding="utf-8"?>
<!--
  Additional functionality provided by dslc

  Copyright (C) 2014-2023 Ryan Specialty, LLC.

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
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<!--
  Absolute path to root of TAME

  Relative paths between files in XSLT can be flaky because most operations
  are relative to the root stylesheet (not the filename of the stylesheet
  that a particular line of code is executing in).  Using absolute paths
  mitigates that definitively.
-->
<param name="__path-root" />


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
<param name="__srcpkg" />


<!--
  Relative path to project root

  The project root is determined entirely by __srcpath by repeating the string
  "../" for the number of occurrances of "/".
-->
<param name="__relroot" />

<!--
  A package-unique string

  You should use `preproc:pkg-generate-id` instead of this value directly.

  This value is deterministic, derived from `__srcpkg`, and so will not
  change between runs; it can be used to generate identifier names that are
  unique across packages, which is not something that we can rely on
  `generate-id()` for on its own.

  In practice, this can be concatenated with other generated strings,
  including `generate-id()`-derived strings.

  _There is no guarantee that this string will begin with a letter_, so you
  should generate your identifiers accordingly.

  See `DslCompiler.java` for implementation.
-->
<param name="__pkguniq" as="xs:string" />


<!--
  Root node of template on which stylesheet was invoked

  This points to the original, unprocessed document. This is especially
  important for `document' function calls, which use nodes as a reference
  point for resolving relative paths.
-->
<variable name="__entry-root" select="/" />



<!--
  Apply relative root to PATH

  If PATH is an absolute path, it will be prefixed with the relative root
  with the leading path delimiter stripped; otherwise, it will be echoed
  as-is.
-->
<template name="__apply-relroot">
  <param name="path" />

  <choose>
    <when test="starts-with( $path, '/' )">
      <value-of select="$__relroot" />
      <value-of select="substring-after( $path, '/' )" />
    </when>

    <otherwise>
      <value-of select="$path" />
    </otherwise>
  </choose>
</template>

<function name="preproc:pkg-generate-id" as="xs:string">
  <param name="refnode" as="node()" />

  <sequence select="concat(
                      '_pu', $__pkguniq, '_',
                      generate-id( $refnode ) )" />
</function>

</stylesheet>
