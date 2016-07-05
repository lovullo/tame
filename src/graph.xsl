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
  Produce a new graph that is the transpose of
    @var{$graph}@mdash{}that is,
    the graph @var{$graph} with the direction of all of its edges
    reversed.

  This is useful for processing what symbols are @emph{used by} other
    symbols.

  For example:

  @float Figure, fig:reverse-graph
  @verbatim
  G:  A->B->C->E
       \
        `->D

  G': A<-B<-C<-E
      ^
       `D
  @end verbatim
  @caption{G' is the transpose of G}
  @end float

  Edge attributes (@code{preproc:sym-ref/@@*)} will be set to the
    union of all attributes on all edges of the same @code{@@name} in
    @code{$graph}.
  @emph{If edge attributes do not share the same value,
    the behavior is undefined.}
-->
<function name="graph:reverse" as="element( preproc:sym-deps )">
  <param name="graph" as="element( preproc:sym-deps )" />

  <variable name="reversed" as="element( preproc:sym-dep )*">
    <for-each-group select="$graph//preproc:sym-ref"
                    group-by="@name">
      <preproc:sym-dep name="{@name}">
        <for-each select="current-group()/ancestor::preproc:sym-dep">
          <preproc:sym-ref>
            <sequence select="current-group()/@*" />

            <!-- keep our name (overrides the above) -->
            <attribute name="name" select="@name" />
          </preproc:sym-ref>
        </for-each>
      </preproc:sym-dep>
    </for-each-group>
  </variable>

  <preproc:sym-deps>
    <!-- vertices in $graph with no dependencies will not be in
         $reversed -->
    <for-each select="$graph/preproc:sym-dep[ not(
                        @name = $reversed/preproc:sym-ref/@name ) ]">
      <preproc:sym-dep name="{@name}" />
    </for-each>

    <sequence select="$reversed" />
  </preproc:sym-deps>
</function>


<!--
  Merge sequence of graphs @var{$graphs} into a single graph by taking
    the union of all vertices and edges.
  Directionality will be preserved.

  Edge attributes (@code{preproc:sym-ref/@@*)} will be set to the
    union of all attributes on all edges of the same @code{@@name}.
  @emph{If edge attributes do not share the same value,
    the behavior is undefined.}

  For example:

  @float Figure, fig:union-graph
  @verbatim
  G₁:  A->B->C
  G₂:  C->A
  G₃:  B->C->D

  G∪:  A->B->C->D
       ^____/
  @end verbatim
  @caption{(G₁ ∪ G₂ ∪ G₃)}
  @end float
-->
<function name="graph:union" as="element( preproc:sym-deps )*">
  <param name="graphs" as="element( preproc:sym-deps )*" />

  <preproc:sym-deps>
    <for-each-group select="$graphs/preproc:sym-dep"
                    group-by="@name">
      <preproc:sym-dep name="{@name}">
        <for-each-group select="current-group()/preproc:sym-ref"
                        group-by="@name">
          <preproc:sym-ref>
            <sequence select="current-group()/@*" />

            <!-- keep our name (overrides the above) -->
            <attribute name="name" select="@name" />
          </preproc:sym-ref>
        </for-each-group>
      </preproc:sym-dep>
    </for-each-group>
  </preproc:sym-deps>
</function>


<!--
@menu
* Package Subgraphs:: Managing package dependencies
@end menu
-->

<!--
  @node Package Subgraphs
  @subsection Package Subgraphs

  Each package has its own independent dependency graph.
  These vertices may have @dfn{virtual edges} to other packages'
    graphs@mdash{}edges that will be formed once combined the
    referenced graph;
      these edges are indicated with @code{preproc:sym-ref/@@src}.

  Graph operations are usually performed on single packages,
    but it is occionally necessary to traverse packages to recurisvely
    resolve dependencies.
  @ref{graph:dep-lookup} makes this easy:

  TODO: Generic graph functions.
-->

<!--
  Retrieve dependenices for @var{$symbol} on the @var{$graph},
    using the lookup function @var{$lookup} to resolve external
    subgraphs.
  @var{$lookup} will be used only if the symbol cannot be found in
    @var{$graph},
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
