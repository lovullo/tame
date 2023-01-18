<?xml version="1.0" encoding="utf-8"?>
<!--
  Assembles code fragments into a final executable

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

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:map="http://www.w3.org/2005/xpath-functions/map"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:l="http://www.lovullo.com/rater/linker"
  xmlns:log="http://www.lovullo.com/logger"
  xmlns:compiler="http://www.lovullo.com/rater/compiler"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<template match="/" priority="9">
  <message terminate="yes"
           select="'error: the XSLT-based linker has been removed; use tameld'" />
</template>

</stylesheet>
