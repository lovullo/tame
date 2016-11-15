<?xml version="1.0"?>
<!--
  Semantic analysis for symbol generation

  Copyright (C) 2016 LoVullo Associates, Inc.

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
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

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
  Standard dimensions are scalar@tie{}(0),
    vector@tie{}(1),
    and matrix@tie{}(2).
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
-->


</stylesheet>
