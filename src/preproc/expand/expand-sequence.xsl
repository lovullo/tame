<?xml version="1.0"?>
<!--
  Expansion sequence

  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.

    This file is part of TAME.

    This program is free software: you can redistribute it and/or modify
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
  xmlns:eseq="http://www.lovullo.com/tame/preproc/expand/eseq"
  xmlns:_eseq="http://www.lovullo.com/tame/preproc/expand/eseq/_priv">

<!--
@node Expansion Sequence
@section Expansion Sequence

  An @dfn{expansion sequence} @math{E} is an ordered list of nodes
  @math{N_1,N_2,\ldots,N_m} satisfying the property that, given some
  node @math{N_x\in E} such that @math{m\geq x>1}, the node
  @math{N_@{x-1@}} must have already been fully expanded before
  expansion of @math{N_x} begins.  Such an ordering guarantee is
  generally unnecessary.

  Expansion sequences are initiated by invoking
  @ttref{eseq:expand-step#1} on any arbitrary node containing any number
  of children to be expanded in order.  Each call will proceed one
  step (detailed herein), eventually resulting in each node expanded
  and the expansion sequence node eliminated.
-->

<!--
  This fuction performs only a single pass, expected to be applied
  once for each pass as necessary.  This may change in the future once
  the preprocessor supports subtree expansion.

  The final result of the expansion is the result of expanding each
  child serially, requiring that expansion of the previous sibling be
  wholly completed before expansion of a node.  The parent expansion
  sequence node will be stripped from the result once empty.

  Note that this function accepts an empty XML sequence, and will
  return an empty sequence in such a case.
-->
<function name="eseq:expand-step" as="node()*">
  <param name="eseq" as="node()*" />

  <variable name="count" as="xs:integer"
            select="count( $eseq )" />

  <variable name="target" as="element()?"
            select="$eseq[ $count ]" />

  <!-- We must be careful in how we retain other nodes; since we
       accept a sequence of nodes, the @code{preceding-sibling}
       pseudo-selector cannot be used, since they are not part of the
       same tree. -->
  <sequence select="$eseq[ position() lt $count ]" />

  <apply-templates mode="_eseq:expand"
                   select="$target" />
</function>



<!--
  Throughout the process, we will consider only the head of the
  sequence for processing; we call the the @dfn{head node}.  Once the
  head is done expanding, it is @dfn{hoisted} out out of the
  sequence@mdash{}order maintained@mdash{}and processing continues
  with the next head.  If no such head exists, then we are done.

  This implementation sounds simple on the face of it, but is
  complicated by the fact that we are not performing this operation in
  one sweep@mdash{}due to the implementation of the preprocessor at
  the time that this was written, we must pass control back with each
  pass that might trigger an expansion, allowing the symbol table to
  be updated and other processing to take place.  At least that's the
  rationale; we wouldn't want to make such assumptions in this
  implementation.  But we do need to keep that in mind.

  With that, we will also want to keep the number of re-passes to a
  minimum; that means yielding back to the caller only when
  necessary.  Until a redesign of the preprocessor such that it will
  reprocess only as necessary, running a sequential expansion will
  @emph{always} be slower, and processing time will grow linearly with
  the number of expansion nodes, relative to the size of the entire
  package.

@menu
* Predicating Expansion::   Determining whether something needs
                            expansion, and delegating expansion

* Expansion Operations::    Operating on an abstract definition of
                            expansion

* Node Hoisting::           Handling of expanded nodes
@end menu

@node Predicating Expansion
@subsection Predicating Expansion

  To start, let's consider possible nodes.  We do not want to make any
  assumptions of what may or may not be expandable, so we will handle
  them all consistently.  The first question is whether the node is
  even expandable.
-->


<!--
  Our perspective is abstraction@mdash{}we do not know (nor should we)
  what the caller may consider to be expandable, or how it may be
  expanded.

  To solve this problem, we introduce an abstract predicate that must
  be overridden by an implementation to provide anything of
  value.  Otherwise, all we can do is assume that the node we have
  encountered cannot be expanded (what would we even do to expand
  it?).
-->

<!--
  @emph{An implementation must override this function.}

  This predicate determines exclusively whether a node should be
  expanded or hoisted.  Therefore, it should account for both nodes
  that @emph{cannot} be expanded (for example, text nodes may not be
  expanded), and nodes that have @emph{already been expanded} that can
  undergo no further expansion.

  The default implementation therefore will always yield false,
  meaning that no processing will take place.
-->
<function name="eseq:is-expandable" as="xs:boolean">
  <param name="node" as="node()" />

  <sequence select="false()" />
</function>


<!--
  With that question answered, we are now able to proceed:

  @enumerate
    @item If a node is not expandable, we can immediately hoist it
      (@pxref{Node Hoisting});
    @item Otherwise, we must allow the node to attempt to expand.
  @end enumerate

  The former allows us to save re-passes (and therefore improve
  performance), as well as the headache of handling every possible
  case.@footnote{Well, we deferred that complexity to the caller via
  our @code{eseq:is-expandable} predicate.}  The latter has an
  important consideration.

  Let's start with the first case.
-->

<!--
  When we encounter a head that is not expandable, it will be
  immediately hoisted, as there is no work to be done.

  The result of this operation will be a sequence of nodes, one of
  them being the hoisted node, and the last being the remaining
  expansion sequence.  @xref{Node Hoisting}.
-->
<template mode="_eseq:expand" as="node()+"
          match="*[ node()[1][
                      not( eseq:is-expandable( . ) ) ] ]">
  <sequence select="_eseq:hoist( . )" />
</template>


<!--
  As we only have an abstract view of the concept of ``expansion'', we
  need a way for an implementation to notify us whether a node needs
  further expansion, or is ready to be hoisted.  Fortunately, we
  already have that information because of how we defined
  @ttref{eseq:is-expandable#1}.  In other words, our previously declared
  processing will already take care of hoisting for us when necessary,
  so we need only continue to expand nodes as necessary.
-->

<!--
  We must continue to expand expandable nodes; otherwise, nested
  expansions would never take place.

  Once expansion is complete, by the definition of
  @ttref{eseq:is-expandable#1}, expansion will halt.
-->
<template mode="_eseq:expand" as="element()"
          match="*[ node()[1][
                      eseq:is-expandable( . ) ] ]">
  <sequence select="_eseq:expand-head( . )" />
</template>


<!--
  Up until this point, we have been assuming that there is an actual
  head node to process; this may not be the case.  In fact, we are
  guaranteed to encounter and empty expansion sequence at some point,
  because all nodes will have been hoisted (@pxref{Node Hoisting})!

  In this instance, we will consider our job to have been completed,
  and self-destruct.
-->


<!--
  Once there is no head node, expansion is complete and the sequence
  parent is no longer necessary.
-->
<template mode="_eseq:expand"
          match="*[ not( node() ) ]" />


<!--
  The above matches satisfy all possible conditions.

  @proof{Let the expansion sequence element be the context node.  As
  it is an element, it is matched by @code{*}, which is the context of
  each match.  The expansion sequence either has a head node, or it
  does not have a head node.  If it @emph{does} have a head node, then
  it can be defined as @code{node()[1]}; otherwise @code{node()}
  yields an empty sequence, and the final template is matched.  When
  the head node @emph{is} available, it is either expandable or
  non-expandable, determined by the predicate
  @ttref{eseq:is-expandable#1}.  Since the predicate returns a boolean,
  it must be either @code{false()} or @code{true()}, and so it
  must satisfy either the first or second template respectively.}

  We have therefore determined hoisting/expansion actions through use
  of a single predicate.

  An astute reader may have come to an uncomfortable realization:
  after all expansions are complete and the expansion sequence node
  itself is eliminated (per the final match above), then the node that
  was last expanded and hoisted will be considered to be the expansion
  sequence by @ttref{eseq:expand-step#1}.  This is true, but should not
  be a problem in practice: hoisting is intended to place nodes into
  context for the caller; it is expected that the caller will
  recognize when to invoke sequence expansion (likely on a pre-defined
  node type, which would no longer match after it is eliminated).  The
  discomfort comes from the fact that we cannot use this
  implementation recursively; this is a consequence of the current
  preprocessor implementation, and is subject to change in the future.
-->



<!--
@node Expansion Operations
@subsection Expansion Operations

  The concept of ``expansion'' is treated as an abstract operation
  (@pxref{Predicating Expansion}).  The system implementing expansion
  sequences should provide concrete definitions of what it means to
  expand a node, and what it means to check whether a node is
  expanded.

  Recall that expansion takes place only the on the head
  node.  Expanding the head therefore involves reproducing the
  expansion sequence with head expanded and siblings untouched.
-->

<!--
  Expanding the head produces a new expansion sequence with the head
  expanded and all its sibling nodes left untouched.  This produces
  the fundamental effect of the expansion sequence.

  A head node @emph{must} be available to satisfy the domain of the
  function.

  Actual expansion is left to
  @ref{eseq:expand-node#1,,@code{eseq:expand-node#1}}.
-->
<function name="_eseq:expand-head" as="element()">
  <param name="eseq" as="element()" />

  <variable name="head" as="node()"
            select="$eseq/node()[1]" />

  <!-- This @code{for-each} is purely to set the context for
       @code{copy}, since we do not know the sequence element
       name. -->
  <for-each select="$eseq">
    <copy>
      <sequence select="$eseq/@*,
                        eseq:expand-node( $head ),
                        $head/following-sibling::node()" />
    </copy>
  </for-each>
</function>


<!--
  Just as we defined an overridable expansion predicate, we will
  provide an overridable function that performs actual node
  expansion.

  Its default behavior is an important consideration: what if
  @ttref{eseq:is-expandable#1} is overridden but the implementation
  forgets to override @ttref{eseq:expand-node#1}?  If the default
  behavior were to simply echo back the node, it seems likely that we
  would never finish processing, since the very node that matched the
  predicate to begin with would remain unchanged.

  Ideally, we remain side-effect free (meaning no triggering of
  errors).  Instead, we return a single node in our own namespace that
  represents the error; this will likely trigger an error in the
  system, since the node is unrecognized.@footnote{If an error is
  @emph{not} triggered, then the system is not very sound.}
-->

<!--
  @emph{An implementation must override this function.}

  This function should perform whatever is necessary to expand the
  provided node.  Note that this call represents a single step in an
  expansion, so it need not result in a complete expansion; further
  processing will take place according to the result of the
  @ttref{eseq:is-expandable#1} predicate.

  If @ttref{eseq:is-expandable#1} is provided, but an override for this
  function is not, then the default behavior is to return a node in
  our namespace providing a description of the problem; this is to
  prevent infinite recursion/iteration.
-->
<function name="eseq:expand-node" as="node()*">
  <param name="node" as="node()" />

  <eseq:expand-error>
    <text>A node expansion was requested via `eseq:expand-node'</text>
    <text>, but no implementation was provided.  Please</text>
    <text>override the function.</text>
  </eseq:expand-error>
</function>


<!--
  The return type of @ttref{eseq:expand-node#1} produces an interesting
  concept.  Consider what may happen after an expansion:

  @enumerate
    @item Node expanded into a single node;
    @item Node expanded into nothing (producing no node); or
    @item Node expanded into multiple nodes.
  @end enumerate

  The first is obviously not an issue, since it keeps us consistent
  with what we have been doing.  In the second case, a node is removed
  rather than being hoisted, but we are otherwise in a state that we
  expect: less a node.  So will the case of expanding into multiple
  nodes cause any problems?

  It shouldn't, but it is worth a discussion to
  rationalize.  Expansion sequences exist to provide expansion
  guarantees; the system otherwise expands nodes as it can in an
  undefined manner.  Since that manner is undefined, providing it with
  stronger restrictions is acceptable: the newly expanded nodes will
  be processed in order as a consequence of becoming part of the
  expansion sequence, but they will otherwise be processed as
  normal.@footnote{In fact, this is a useful property, since it can be
  exploited by templates to create abstractions with ordering
  guarantees.  So consider it a feature!}

  After expansion, with the current preprocessor design, we have no
  choice but the yield control back to the caller to allow it to
  continue processing; the expansion may have yielded additional
  symbols that must be added to the symbol table, for example.  The
  process will be continued on the next call to
  @ttref{eseq:expand-step#1}.
-->



<!--
@node Node Hoisting
@subsection Node Hoisting

  @dfn{Hoisting} is the process of moving a fully expanded head node
  out of the expansion sequence; it is the final step of the process
  for a head node and is driven wholly by the
  @ttref{eseq:is-expandable#1} predicate.

  Unfortunately, we cannot continue processing immediately after
  hoisting for the same reasons that we cannot continue processing
  after expansion: after hoisting, the nodes may enter a proper
  context and acquire another meaning, which may result in, for
  example, additional symbols.
-->


<!--
  Hoisting removes the head node from the expansion sequence, leaving
  all other expansion nodes untouched.  The result is a sequence of
  two nodes, the last of which is the expansion sequence element.

  If no head node exists, the result is the single expansion sequence
  node unchanged.
-->
<function name="_eseq:hoist" as="node()+">
  <param name="eseq-node" as="element()" />

  <variable name="head" as="node()?"
            select="$eseq-node/node()[1]" />

  <sequence select="$head" />

  <!-- This @code{for-each} is purely to set the context for
       @code{copy}, since we do not know the sequence element
       name. -->
  <for-each select="$eseq-node">
    <copy>
      <sequence select="$eseq-node/@*
                        , $head/following-sibling::node()" />
    </copy>
  </for-each>
</function>

</stylesheet>
