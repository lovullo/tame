<?xml version="1.0"?>
<!--
  Dependency graph

  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.

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
<import href="graph.xsl.apply" />


<!--
  @node Dependency Graph
  @appendix Dependency Graph

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
  Create a graph from the given vertex set @var{$vertices}.
  The resulting graph will be normalized with duplicate vertices and
    edges removed,
    making it suitable for ad hoc graph generation.@c
  @footnote{This is done by calling @ttref{graph:union#1}.}
-->
<function name="graph:make-from-vertices"
          as="element( preproc:sym-deps )">
  <param name="vertices" as="element( preproc:sym-dep )*" />

  <variable name="graph" as="element( preproc:sym-deps )">
    <preproc:sym-deps>
        <sequence select="$vertices" />
    </preproc:sym-deps>
  </variable>

  <!-- dedupe/normalize -->
  <sequence select="graph:union( $graph )" />
</function>


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

  This function also removes duplicate vertices and edges,
    so it can be used with a single (or multiple) graphs to normalize
    and tidy things up.
  Any unknown XML nodes are removed.
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
  @section Package Subgraphs

  Each package has its own independent dependency graph.
  These vertices may have @dfn{virtual edges} to other packages'
    graphs@mdash{}edges that will be formed once combined the
    referenced graph;
      these edges are indicated with @code{preproc:sym-ref/@@src}.

  Graph operations are usually performed on single packages,
    but it is occionally necessary to traverse packages to recurisvely
    resolve dependencies.
  @ttref{graph:dep-lookup#3} makes this easy:

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

  Parameters are organized for partial application.
-->
<function name="graph:dep-lookup" as="element( preproc:sym-dep )?">
  <param name="lookup" />
  <param name="graph"  as="element( preproc:sym-deps )" />
  <param name="symbol" as="element( preproc:sym )" />

  <variable name="deps" as="element( preproc:sym-dep )?"
            select="$graph/preproc:sym-dep
                      [ @name = $symbol/@name ]" />

  <sequence select="if ( exists( $deps ) ) then
                        $deps
                      else if ( $lookup ) then
                          graph:dep-lookup( $lookup,
                                            f:apply( $lookup, $symbol ),
                                            $symbol )
                        else
                          ()" />
</function>

<!--
  @ttref{graph:dep-lookup#3} can be used together with the convenience
    function @ttref{graph:make-from-deps#2} to produce a graph that
    contains all dependencies for a given symbol list.
  Used together with @ttref{graph:reverse#1},
    a reverse dependency graph can be easily created that provides a
    useful ``used by'' relationship.
-->

<!--
  Create a dependency graph containing all dependencies of the given
    symbol list @var{$symbols}.
  The graph contains the union of the minimal subset of all package
    subgraphs@mdash{}only vertices representing a symbol in
    @var{$symbols} or its direct dependencies are included.

  This function is @emph{not} recursive;
    it assumes that the given symbol list @var{$symbols} is sufficient
    for whatever operation is being performed.

  The lookup function @var{$lookup} is invoked once per symbol in
    @var{$symbols} with the @code{preproc:sym} to look up.
  The final result is used to produce a new normalized graph,
    with any duplicate vertices and edges removed.
-->
<function name="graph:make-from-deps" as="element( preproc:sym-deps )*">
  <param name="lookup"  as="item()+" />
  <param name="symbols" as="element( preproc:sym )*" />

  <sequence select="graph:make-from-vertices(
                      for $symbol in $symbols
                        return f:apply( $lookup, $symbol ) )" />
</function>


<!--
@menu
* Graph Lookups:: Graph constructors using symbol lookups
@end menu
-->

<!--
  @node Graph Lookups
  @subsection Graph Lookups
-->

<!--
  The provided graph lookups are constructors that use symbols to
  locate a graph.  Using partial application, they are convenient for
  use in @ttref{graph:dep-lookup#3} to resolve external graphs.
-->

<!--
  Look up a graph on a document indicated by a source symbol
    @var{$symbol/@@src}.
  The document will be loaded relative to @var{$rel-node} with the
    file extension @var{$package-ext}.

  There are no restrictions on the root node of the document,
    but the @code{preproc:sym-deps} node is expected to be a child of
    the root.

  This function does not care if @var{$symbol} actually resolves to
    anything in the destination package@mdash{
      }such is up to the caller to decide.
  If the referenced document contains no graph
    (@code{preproc:sym-deps}), the empty sequence will be returned.
  If the referenced document does not exist,
    the result is implementation-defined.

  Customarily, @var{$doc-ext} is ``xmlo'' (the compiled object file)
    and @var{$rel-node} is the package from which @var{$symbol} was
    obtained.
-->
<function name="graph:lookup-from-doc" as="element( preproc:sym-deps )?">
  <param name="doc-ext"  as="xs:string" />
  <param name="rel-node" as="node()" />
  <param name="symbol"   as="element( preproc:sym )" />

  <variable name="src" as="xs:string?"
            select="$symbol/@src" />

  <sequence select="if ( $src ) then
                        document( concat( $src, '.', $doc-ext ),
                                  $rel-node )
                          /node()/preproc:sym-deps
                      else
                        ()" />
</function>

</stylesheet>
