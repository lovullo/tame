<?xml version="1.0"?>
<!--
  Symbol table

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:f="http://mikegerwitz.com/hoxsl/apply"
            xmlns:symtable="http://www.lovullo.com/tame/symtable"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<import href="symtable.xsl.apply" />


<!--
  @node Symbol Table
  @appendix Symbol Table

  The @dfn{symbol table} holds declarations for each symbol known to
    a@tie{}particular package.
  Symbol tables are represented by @code{preproc:syms}
    elements.@footnote{
      The @code{preproc} namespace exists for legacy reasons;
        it will change in the future.}

  A @dfn{symbol} is an abstract representation of some object@mdash{
    }a calculation, classification, typedef, etc.@mdash{
    }containing the source location and useful metadata.
  @todo{Document symbol format and metadata.}
  Symbols are represented by @code{preproc:sym} elements.
-->


<!--
  Produce a list of duplicate symbols in@tie{}@var{$symtable},
    grouped by @code{@@name}.
  All duplicates will be returned@mdash{
    }that is, if @math{S_1} appears before duplicate @math{S_2}
      in the symbol table, both@tie{}@math{S_1} and@tie{}@math{S_2}
      will be returned.

  If two symbols have duplicate @code{@@name}s but the same
    @code{@@src},
    then they are not considered to be duplicates,
      @emph{unless} another duplicate symbol of the
      same@tie{}@code{@@name} is found with a different @code{@@src},
        in which case all symbols will be returned.
  An exception to this rule is made when both symbols lack a @code{@@src},
    meaning that they are both defined in the same package.
  This allows sloppy comparison on concatenated symbol tables before
    tidying it up.

  Externs are ignored, since they represent symbols that need to be
    satisfied at some point@mdash{
      }this will be checked during linking.

  Symbols (@code{preproc:sym} nodes) are returned by reference.

  This method name is ``find'' duplicates rather than ``get'' to
    emphasize that processing is performed, which is potentially
    intensive given a large symbol table@tie{}@var{$symtable}.
-->
<function name="symtable:find-duplicates" as="element( preproc:sym )*">
  <param name="symtable" as="element( preproc:syms )" />

  <for-each-group select="$symtable/preproc:sym[
                            not( @extern = 'true' ) ]"
                  group-by="@name">
    <!-- @src may be omitted to convey a local symbol -->
    <variable name="srcs" as="xs:string*"
              select="distinct-values(
                        for $sym in current-group()
                          return if ( exists( $sym/@src ) ) then
                                     $sym/@src
                                   else
                                     '.' )" />

    <sequence select="if ( count( $srcs ) gt 1 ) then
                          current-group()
                        else
                          if ( ( $srcs[ 1 ] = '.' )
                               and ( count( current-group() ) gt 1 ) ) then
                              current-group()
                            else
                              ()" />
  </for-each-group>
</function>


<!--
  @menu
  * Symbol Format::  Anatomy of a symbol table entry
  * Symbol Types::   Symbols describing various objects
  @end menu

  @include src/symtable/symbols.texi
-->

</stylesheet>
