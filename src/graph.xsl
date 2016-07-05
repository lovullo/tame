<?xml version="1.0"?>
<!--
  Dependency graph

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
-->
<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:f="http://mikegerwitz.com/hoxsl/apply"
            xmlns:graph="http://www.lovullo.com/tame/graph"
            xmlns:preproc="http://www.lovullo.com/rater/preproc">

<import href="../hoxsl/src/apply.xsl" />

<!--
  @node Dependency Graph
  @section Dependency Graph

  The dependency graph is a directed graph consisting of
    every known symbol,
    post-expansion (@pxref{Macro Expansion}).
  Cycles are produced only by function recursion and otherwise cause an
    error, so the graph is studied as a DAG (directed acyclic graph)
    with few exceptions.

  Vertices in the dependency graph are represented by
    @code{preproc:sym-dep} nodes,
    and edges by child @code{preproc:sym-ref} nodes.
  Graphs are represented by @code{preproc:sym-deps}.
  The graph of each package is considered to be a subgraph of the
    entire dependency graph for a particular system.@c
  @footnote{The node names are for compatibility with legacy systems
    and may change in the future; always use the graph API, and only
    use the node QNames for type checks.}
-->

<!--
  Retrieve dependenices for @var{$symbol} on the @var{$graph},
    using the lookup function @var{$lookup} to resolve external
    subgraphs.
  @var{$lookup} will be used only if the symbol cannot be
    found in @var{$graph},
    in which case the result of @var{$lookup} will used used in a
    recursive call as the new @var{$graph}.

  From a graph perspective,
    the dependencies are edges on the @var{$symbol} vertex.
-->
<function name="graph:dep-lookup" as="element( preproc:sym-dep )?">
  <param name="symbol" as="element( preproc:sym )" />
  <param name="graph"  as="element( preproc:sym-deps )" />
  <param name="lookup" />

  <variable name="deps" as="element( preproc:sym-dep )?"
            select="$graph/preproc:sym-dep
                      [ @name = $symbol/@name ]" />

  <sequence select="if ( exists( $deps ) ) then
                        $deps
                      else if ( $lookup ) then
                          graph:dep-lookup( $symbol,
                                            f:apply( $lookup, $symbol ),
                                            $lookup )
                        else
                          ()" />
</function>

</stylesheet>
