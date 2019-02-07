<?xml version="1.0"?>
<!--
  Semantic analysis for symbol generation

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

    Permission is granted to copy, distribute and/or modify this document
    under the terms of the GNU Free Documentation License, Version 1.3 or
    any later version published by the Free Software Foundation; with no
    Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
    A copy of the license is included in the section entitled ``GNU Free
    Documentation License''.
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:lv="http://www.lovullo.com/rater"
            xmlns:symtable="http://www.lovullo.com/tame/symtable"
            xmlns:_symtable="http://www.lovullo.com/tame/symtable/_priv"
            xmlns:preproc="http://www.lovullo.com/rater/preproc"
            exclude-result-prefixes="symtable _symtable xs">

<import href="symbols.xsl.apply" />

<!--
  @macro sym{}
  @math{\\sigma}
  @end macro

  @macro obj{}
  @math{\\theta}
  @end macro
-->


<!--
  @node Symbol Format
  @section Symbol Format

  A @dfn{symbol} is nothing more than an abstract representation of an
    object,
      represented by a @code{preproc:sym} node in the symbol table
        (@pxref{Symbol Table}),
  For some symbol@tie{}@sym{} describing some object@tie{}@obj{}, the
    following attributes are recognized:

  @table @code
  @item name
  Unique identifier of@tie{}@sym{}.  @emph{Required.}

  @item type
  Type of object described by@tie{}@sym{}.
  Multiple symbols of different types may share the same@tie{}@obj{}.
  @emph{Required.}

  @item desc
  Human-readable description of@tie{}@obj{}. @emph{Required.}

  @item dim
  Dimensions of@tie{}@obj{} as an integer.
  See @ref{_symtable:str-to-dim#1} for supported strings.
  @emph{Required}

  @item dtype
  Reference to some symbol describing the datatype of@tie{}@sym{},
    if applicable.

  @item tex
  TeX code used to render@tie{}@sym{} in a math block.

  @item parent
  Reference to a symbol from which@tie{}@sym{} is derived.
  For example, a @code{cgen} symbol would have the associated
    @code{class} as its parent.

  @item extern
  Whether@tie{}@sym{} is unresolved in the given context.

  @item missing
  If extern, a human-readable message describing@tie{}@obj{}.
  This is useful to display to the user on error when@tie{}@sym{}
    is the result of generated code
      (e.g. @ref{Macro Expansion,,template expansion}).

  @item no-deps
  When @code{true},
    linker does not attempt to look up dependencies for this symbol.
  Otherwise,
    as a safeguard against compilation bugs,
    the linker will fail if a symbol is missing a dependency list.

  @item allow-circular
  Permit@tie{}@sym{} to be part of a cycle (@pxref{Dependency Graph}).
  This is desirable for recursive functions, but should otherwise be
    avoided like the plague.

  @item keep
  Always link@tie{}@sym{},
    even if disjoint from the program's dependency graph.
  @emph{This is marked for removal.}@footnote{
    ``Keeps'' cause various tricky and inelegant situations,
      cause unnecessary coupling of unrelated concerns,
      and create a performance nightmare for the linker.
    They were created as a kluge during a near-rewrite of @tame{}
      (which introduced the symbol table) to get around the fact that
      certain systems weren't in place to pull in symbols as
      dependencies for certain operations;
        it wasn't intended to stick.
    They will be removed entirely and will not be replaced with
      anything@mdash{
        }if a symbol is to be linked, it must be part of the
        dependency graph.}
  @end table

  An attribute not marked as required may be omitted.
  If an implementation wishes to attach additional metadata to a
    symbol,
      it @emph{must} use a different namespace.
-->


<!--
  Convert a string dimension representation into an integer.

  Standard dimensions are @samp{scalar}@tie{}(0),
    @samp{vector}@tie{}(1),
    and @samp{matrix}@tie{}(2).
  If no value is provided,
    then @samp{scalar} is assumed.
  All unknown strings will yield a value of @samp{-1}.@footnote{
    That's not to say that @tame{} can't support an arbitrary number
    of dimensions;
      this syntax just doesn't provide that utility.}
-->
<function name="_symtable:str-to-dim" as="xs:integer">
  <param name="str" as="xs:string?" />

  <choose>
    <when test="empty( $str ) or ( $str = 'scalar' )">
      <sequence select="0" />
    </when>

    <when test="$str = 'vector'">
      <sequence select="1" />
    </when>

    <when test="$str = 'matrix'">
      <sequence select="2" />
    </when>

    <otherwise>
      <sequence select="-1" />
    </otherwise>
  </choose>
</function>


<!--
  @node Symbol Types
  @section Symbol Types

  The @dfn{symbol type} describes the context in which a symbol may be
  used,
    and the attributes it supports.
  Generally, both the compiler and linker must be aware of a given
    symbol type:
      the compiler uses the symbol type to determine the type of code
        to generate,
      and the linker uses that information to determine what section
        the object code should be linked to.
  There are exceptions,
    noted in their individual sections.

  @tame{} aims to reduce the number of symbol types in favor of a
    generalized system with few primitives@mdash{
      }before defining a new symbol type,
        consider whether the existing can be reused in a novel way.
  Certain symbols will be consolidated in the future,
    time permitting.@footnote{
      @code{class} can be a special case of @code{rate}.
      @code{gen} can acquire the multidimensional capabilities of
        @code{cgen} and the latter can be removed.
      @code{param} and @code{lparam} can be merged if the former
        is defined by a wide "local" scope.
      @code{const} is a special case of @code{rate} or @code{cgen}.
      @code{func} can be eliminated if all current @code{rate} are
        considered to be immediate function applications;
          this would also allow for mocking inputs for testing and
          debugging.
        If it's not yet clear, @tame{} started as a very rigid,
          narrowly-focused system and began generalizing over time.}

  @table @code
  @item rate
  Calculation.@footnote{
    The term @dfn{rate} (a verb) is a consequence of @tame{}'s origin
      as an insurance rating system.
    This symbol type will be renamed to @code{calc} eventually.}
  Calculations yield a single scalar value.
  If a child @code{cgen} exists,
    the yield of the calculation is equivalent to the sum of all its
    elements.

  @item gen
  Generator.@footnote{
    The name is regrettable@mdash{
      }it originated from the concept of a generator function,
        but never turned out that way.
    @code{gen} is actually a list comprehension,
      and will likely be renamed in the future to reflect that.}
  Generators produce a vector element for each iteration of some
    calculation.
  Generators always have a parent @code{rate} symbol.

  @item class
  Classification.
  Classifications are universal or existential quantifiers most often
    used as predicates.

  @item cgen
  Classification generator.@footnote{
    See footnote for @code{gen}.}
  Analogous to @code{gen},
    produces a boolean element for each classification result.
  Unlike @code{gen}, the dimension of a given @code{cgen} is the
    largest of all of its predicates.

  @item param
  Global parameter.
  All TAME programs can be viewed as a function operating on a set of
    global inputs.
  Their domains are defined by their datatype,
    represented by the symbol attribute @code{dtype}
    (@pxref{Symbol Types}).

  @item lparam
  Local parameter.
  In contrast to @code{param}, local parameters are restricted to a
    defined scope (e.g. function parameters; let expressions).
  This symbol is not used by the linker.

  @item lparent
  Local parameter parent.
  This is set for @code{let} expressions in place of @code{@@parent}
    since the parent let does not actually generate a symbol.

  @item const
  Global constant.
  Constant values (of any dimension).
  These values may be inlined at the compiler's discretion.

  @item tpl
  Template definition.
  Templates define expandable text in the form of macros
    (@pxref{Macro Expansion}).
  This symbol is not used by the linker.

  @item type
  Datatype.
  A type describes the domain of a symbol.
  Types do not include dimension information@mdash{
    }such is determined by the @code{dim} symbol attribute
      (@pxref{Symbol Format}).

  @item func
  Function.
  @tame{} supports functions as a means of calculation reuse and
    abstraction.
  Unlike calculations,
    functions can recurse (see @code{allow-circular} in
      @ref{Symbol Format}).
  Functions are not first-class@mdash{
    }@tame{} is not a functional language.

  @item meta
  Arbitrary metadata about the program.
  These metadata are key-value and are intended to be compiled
    statically into the program for static reference without having to
    actually invoke the program.
  @end table


  @menu
  * Parameters: Parameter Symbols.              @code{param}
  * Templates: Template Symbols.                @code{tpl}
  * Program Metadata: Program Metadata Symbols. @code{meta}
  @end menu
-->


<!--
  @node Parameter Symbols
  @subsection Parameter Symbols

  Global parameters define all inputs to the program.
-->


<!--
  Produce a @code{param} symbol with the following attributes:

  @table @code
  @item name
  Name of the parameter as provided by @pkgns{param/@@name}.

  @item type
  The datatype defining the parameter's domain,
    as provided by @pkgns{param/@@type}.

  @item dim
  Numeric dimension converted from its string representation in
    @pkgns{param/@@set}.@footnote{
      The attribute name @pkgns{param/@@set} is unfortunate and simply
        incorrect terminology with how it is used.
      It will be changed in the future.}

  @item desc
  Description as provided by @pkgns{param/@@desc}.

  @item tex
  TeX symbol used when rendering parameter in an equation.

  @item keep
  Always @samp{true} to ensure that the symbol is retained after
    linking.
  @end table
-->
<template match="lv:param" mode="preproc:symtable" priority="5">
  <variable name="dim" as="xs:integer"
            select="_symtable:str-to-dim( @set )" />

  <preproc:sym type="param"
               name="{@name}"
               dim="{$dim}"
               desc="{@desc}"
               dtype="{@type}"
               default="{@default}"
               tex="{@sym}" />
</template>


<!--
  @node Template Symbols
  @subsection Template Symbols

  Templates produce a single @code{tpl}@tie{}symbol representing the
    macro itself.
  At the time of parsing,
    we do not care about the body of the template@mdash{
      }when applied, it will expand into code that will be further
        processed recursively during another pass to produce the
        appropriate symbols for that expansion.
-->

<!--
  Produce a @code{tpl} symbol with the following attributes:

  @table @code
  @item name
  Name of the template as provided by @pkgns{template/@@name}.

  @item dim
  Always @samp{0};
    templates are processed as macros before compilation.

  @item desc
  Template description as provided by @pkgns{template/@@desc}.
  @end table
-->
<template mode="preproc:symtable" priority="5"
          match="lv:template">
  <preproc:sym type="tpl"
               name="{@name}"
               dim="0"
               desc="{@desc}">
    <if test="@preproc:generated = 'true'">
      <attribute name="local" select="'true'" />
    </if>

    <sequence select="@preproc:*" />
  </preproc:sym>
</template>


<!--
  @node Program Metadata Symbols
  @subsection Program Metadata Symbols

  A basic key-value system allows for compiling static metadata into
    the program.
  These metadata can be referenced externally without having to run
    the program.
-->

<!--
  Produce a @code{meta} symbol for each @pkgns{meta/}@pkgns{prop} with
    the following attributes:

  @table @code
  @item name
  The metavalue name as provided by @pkgns{prop/@@name},
    prefixed with @samp{:meta:} to avoid conflicts with other
    symbols.

  @item desc
  Generic description including @code{name}.

  @item keep
  Always @samp{true}.
  @end table

  The @code{name} prefix enforces separation between the two
    compilation stages by preventing conflicts.
-->
<template mode="preproc:symtable" priority="5"
          match="lv:meta">
  <for-each select="lv:prop">
    <preproc:sym type="meta"
                 name=":meta:{@name}"
                 desc="Metavalue {@name}"
                 pollute="true"
                 no-deps="true" />
  </for-each>
</template>


</stylesheet>
