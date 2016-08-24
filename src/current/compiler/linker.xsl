<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Compiles rater XML into JavaScript

  This stylesheet should be included by whatever is doing the processing and is
  responsible for outputting the generated code in whatever manner is
  appropriate (inline JS, a file, etc).
-->

<xsl:stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lv="http://www.lovullo.com/rater"
  xmlns:c="http://www.lovullo.com/calc"
  xmlns:l="http://www.lovullo.com/rater/linker"
  xmlns:log="http://www.lovullo.com/logger"
  xmlns:compiler="http://www.lovullo.com/rater/compiler"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">

<xsl:include href="../include/preproc/symtable.xsl" />
<xsl:include href="../include/util.xsl" />
<xsl:include href="js.xsl" />

<xsl:include href="linker/log.xsl" />

<!-- indentation makes dep lists easier to mentally process -->
<xsl:output indent="yes" />

<!-- optional fragments to include in exit block, comma-delimited
     (e.g. path/to/object/file/_fragment_)  -->
<xsl:param name="rater-exit-fragments" />

<!--
  Used to output a great deal of linker information for debugging

  THIS WILL HAVE A PERFORMANCE HIT!
-->
<xsl:param name="l:aggressive-debug" select="false()" />

<xsl:variable name="l:orig-root" as="document-node( element( lv:package ) )"
              select="/" />

<xsl:variable name="l:process-empty" as="element( l:pstack )">
  <l:pstack />
</xsl:variable>

<xsl:variable name="l:stack-empty" as="element( l:sym-stack )">
  <l:sym-stack />
</xsl:variable>


<xsl:template match="*" mode="l:link" priority="1">
  <xsl:call-template name="log:error">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>cannot link </xsl:text>
      <xsl:value-of select="name()" />
      <xsl:text>; must link program</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<!-- entry point (if run directly) -->
<xsl:template match="/" priority="1">
  <xsl:apply-templates select="/lv:*" mode="l:link" />
</xsl:template>


<!--
  We will only link program package

  Raters are similar to shared object files (that is, packages), but explicitly
  recognize the fact that linking should be done. They also contain definitions
  for exit points (lv:yields); think of it like defining a main() function.
-->
<xsl:template match="lv:package[ @program='true' ]" mode="l:link" priority="5">
  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>linking </xsl:text>
      <xsl:value-of select="@name" />
      <xsl:text>...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <!-- start by recursively discovering imported shared object files -->
  <xsl:variable name="pre-deps" as="element( l:dep )">
    <xsl:call-template name="log:debug">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>building dependency tree...</xsl:text>
      </xsl:with-param>
    </xsl:call-template>

    <l:dep>
      <!-- empty stack -->
      <xsl:apply-templates select="preproc:symtable" mode="l:depgen">
        <xsl:with-param name="stack" select="$l:stack-empty" as="element( l:sym-stack )" />
      </xsl:apply-templates>
    </l:dep>
  </xsl:variable>


  <!-- a single-pass post-processing of the deps to resolve any final issues -->
  <xsl:variable name="deps" as="element( l:dep )">
    <xsl:call-template name="log:debug">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>resolving dependency tree...</xsl:text>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:apply-templates select="$pre-deps" mode="l:resolv-deps" />
  </xsl:variable>

  <xsl:copy>
    <xsl:copy-of select="@*" />

    <!-- copy the dependency tree -->
    <xsl:copy-of select="$deps" />

    <!-- if map data was provided, generate the map -->
    <xsl:variable name="maplink">
      <xsl:apply-templates select="." mode="l:map">
        <xsl:with-param name="deps" select="$deps" />
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:if test="$maplink//l:map-error">
      <xsl:call-template name="log:error">
        <xsl:with-param name="name" select="'link'" />
      </xsl:call-template>
    </xsl:if>

    <!-- all good. -->
    <xsl:copy-of select="$maplink" />

    <!-- link. -->
    <l:exec>
      <xsl:apply-templates select="." mode="l:link-deps">
        <!-- TODO: remove this exception -->
        <xsl:with-param name="deps" select="
            $deps/preproc:sym[
              not(
                starts-with( @type, 'map' )
                or starts-with( @type, 'retmap' )
              )
            ]
          " />
      </xsl:apply-templates>
    </l:exec>
  </xsl:copy>
</xsl:template>


<xsl:template mode="l:depgen" as="element( preproc:sym )*"
              match="preproc:symtable">
  <xsl:param name="stack" as="element( l:sym-stack )"
             select="$l:stack-empty" />

  <!-- we care only of the symbols used by lv:yields, from which all
       dependencies may be derived (if it's not derivable from the yield
       symbol, then it must not be used); note that lv:yield actually compiles
       into a special symbol ___yield -->
  <xsl:variable name="yields" as="element( preproc:sym )+">
    <xsl:copy-of select="preproc:sym[ @name='___yield' ]" />

    <!-- also include anything derivable from any @keep symbol, either local
         or imported -->
    <xsl:copy-of select="preproc:sym[ @keep='true' ]" />

    <!-- TODO: these should be included as a consequence of the linking
         process, not as an exception -->
    <xsl:copy-of select="
        preproc:sym[
          @type='map' or @type='map:head' or @type='map:tail'
          or @type='retmap' or @type='retmap:head' or @type='retmap:tail'
        ]
      " />

    <!-- TODO: same as above -->
    <xsl:copy-of select="preproc:sym[ @name='___worksheet' ]" />
  </xsl:variable>

  <!-- start at the top of the table and begin processing each symbol
       individually, generating a dependency tree as we go -->
  <xsl:variable name="result" as="element()+">
    <xsl:call-template name="l:depgen-sym">
      <xsl:with-param name="pending" select="$yields" />
      <xsl:with-param name="stack" select="$stack" as="element( l:sym-stack )" />
    </xsl:call-template>
  </xsl:variable>

  <!-- stack will contain additional metadata -->
  <xsl:sequence select="$result[ . instance of
                                 element( preproc:sym ) ]" />
</xsl:template>


<xsl:template mode="l:resolv-deps" as="element( l:dep )"
              priority="9"
              match="l:dep">
  <xsl:copy>
    <xsl:apply-templates mode="l:resolv-deps" />
  </xsl:copy>
</xsl:template>


<!-- replace marks with the symbols that they reference (note that the linker
     will not add duplicates, so we needn't worry about them) -->
<xsl:template mode="l:resolv-deps" as="element( preproc:sym )"
              priority="8"
              match="preproc:sym[ @l:mark-inclass ]">
  <!-- FIXME: I sometimes return more than one symbol! -->
  <xsl:variable name="sym" as="element( preproc:sym )*"
                select=" root(.)/preproc:sym[
                          @name = current()/@name ]" />

  <!-- sanity check; hopefully never necessary -->
  <xsl:if test="not( $sym )">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>l:mark-class found for non-existing symbol </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>; there is a bug in l:depgen-process-sym</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <!-- copy the element and mark as inclass (no need to check @src, since at
       this point, such conflicts would have already resulted in an error -->
  <preproc:sym>
    <xsl:sequence select="$sym/@*" />

    <!-- override attribute -->
    <xsl:attribute name="inclass" select="'true'" />
  </preproc:sym>
</xsl:template>


<!-- any now-inclass symbols should be stripped of their original position -->
<xsl:template mode="l:resolv-deps" priority="7"
              match="preproc:sym[
                       @name = root(.)/preproc:sym[ @l:mark-inclass ]
                                 /@name ]">
  <!-- bye-bye -->
</xsl:template>


<xsl:template match="*" mode="l:resolv-deps" priority="1">
  <xsl:sequence select="." />
</xsl:template>


<!-- FIXME: I want element( l:preproc:sym ), but we also have
     l:mark-inclass -->
<xsl:template name="l:depgen-sym" as="element()*">
  <xsl:param name="pending" as="element( preproc:sym )*" />
  <xsl:param name="stack" as="element( l:sym-stack )" />
  <xsl:param name="path" as="xs:string"
             select="''" />
  <xsl:param name="processing" as="element( l:pstack )"
             select="$l:process-empty" />

  <xsl:variable name="pend-count" as="xs:integer"
                select="count( $pending )" />
  <xsl:variable name="stack-count" as="xs:integer"
                select="count( $stack/preproc:sym )" />
  <xsl:variable name="process-count" as="xs:integer"
                select="count( $processing/* )" />

  <xsl:choose>
    <!-- if there are no pending symbols left, then we are done; return the
         stack -->
    <xsl:when test="$pend-count = 0">
      <xsl:sequence select="$stack/*" />
    </xsl:when>


    <xsl:otherwise>
      <!-- take the first item from the pending list -->
      <xsl:variable name="cur" as="element( preproc:sym )"
                    select="$pending[1]" />

      <!-- aggressive debugging data -->
      <xsl:if test="$l:aggressive-debug">
        <xsl:call-template name="log:debug">
          <xsl:with-param name="name" select="'link'" />
          <xsl:with-param name="msg">
            <xsl:text>(</xsl:text>
            <xsl:value-of select="$path" />
            <xsl:text>) </xsl:text>
            <xsl:value-of select="$pend-count" />
            <xsl:text>p - </xsl:text>
            <xsl:value-of select="$stack-count" />
            <xsl:text>s - </xsl:text>
            <xsl:value-of select="$process-count" />
            <xsl:text>r - </xsl:text>
            <xsl:value-of select="$cur/@name" />
            <xsl:text> [s:: </xsl:text>
            <xsl:value-of select="$stack/preproc:sym/@name" />
            <xsl:text> ::s] [r:: </xsl:text>
            <xsl:value-of select="$processing/preproc:sym/@name" />
            <xsl:text>::r]</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <xsl:variable name="pkg-seen" as="xs:boolean"
                    select="(
                              ( $cur/@src = '' or not( $cur/@src ) )
                              and $stack/preproc:pkg-seen/@src = ''
                            )
                            or $cur/@src = $stack/preproc:pkg-seen/@src" />

      <xsl:variable name="newpending" as="element( l:pending )">
        <l:pending>
          <xsl:sequence select="$pending" />

          <!-- if this is the first time seeing this package, then pend its
               @keep's for processing -->
          <xsl:if test="not( $pkg-seen )">
            <xsl:message select="'[link] found package ', $cur/@src" />

            <xsl:variable name="document" as="element( lv:package )"
                          select="if ( not( $cur/@src or $cur/@src = '' ) ) then
                                    $l:orig-root/lv:package
                                  else
                                    document( concat( $cur/@src, '.xmlo' ),
                                              $l:orig-root )
                                      /lv:package" />

            <xsl:variable name="keeps" as="element( preproc:sym )*" select="
                  $document/preproc:symtable/preproc:sym[
                    (
                      @keep='true'
                      or ( $l:orig-root/lv:package/@auto-keep-imports='true'
                           and ( @type = 'class'
                                 or @type = 'cgen' ) )
                    )
                    and not(
                      $l:orig-root/lv:package/@no-extclass-keeps='true'
                      and @extclass='true'
                    )
                    and not( @name=$pending/@name )
                    and not( @name=$stack/preproc:sym/@name )
                  ]
              " />

            <xsl:variable name="keepdeps" as="element( preproc:sym )*">
              <xsl:call-template name="l:dep-aug">
                <xsl:with-param name="cur" select="$cur" />
                <xsl:with-param name="deps" select="$keeps" />
                <xsl:with-param name="proc-barrier" select="true()" />
                <xsl:with-param name="parent-name"
                  select="concat( 'package ', $cur/@src )" />
              </xsl:call-template>
            </xsl:variable>

            <xsl:sequence select="$keepdeps" />
          </xsl:if>
        </l:pending>
      </xsl:variable>


      <xsl:variable name="stack-seen" as="element( l:sym-stack )">
        <l:sym-stack>
          <xsl:if test="not( $pkg-seen )">
            <xsl:sequence select="$stack/*" />
            <preproc:pkg-seen src="{$cur/@src}" />
          </xsl:if>
        </l:sym-stack>
      </xsl:variable>

      <xsl:variable name="newstack" as="element( l:sym-stack )"
                    select="if ( $pkg-seen ) then
                              $stack
                            else
                              $stack-seen" />

      <xsl:apply-templates select="$cur" mode="l:depgen-process-sym">
        <xsl:with-param name="pending" select="$newpending/*" />
        <xsl:with-param name="stack" select="$newstack" />
        <xsl:with-param name="path" select="$path" />
        <xsl:with-param name="processing" select="
            if ( $cur/@l:proc-barrier = 'true' )
              then $l:process-empty
            else
              $processing
          " />
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:function name="l:resolv-extern" as="element( preproc:sym )">
  <xsl:param name="sym" as="element( preproc:sym )" />

  <xsl:variable name="name" as="xs:string"
                select="$sym/@name" />

  <!-- there is no reason (in the current implementation) that this
       should _not_ have already been resolved in the package being
       linked -->
  <xsl:variable name="pkg" as="element( lv:package )" select="
        $l:orig-root/lv:package" />

  <xsl:variable name="resolv" as="element( preproc:sym )?"
                select="$pkg/preproc:symtable/preproc:sym[
                          @name=$name ]" />

  <xsl:choose>
    <!-- if this symbol is not external, then we have found it -->
    <xsl:when test="$resolv and not( $resolv/@extern )">
      <xsl:sequence select="$resolv" />
    </xsl:when>

    <!-- if there is no more stack to check and we have not found the symbol,
         then this is a problem (this should never happen) -->
    <xsl:otherwise>
      <xsl:call-template name="log:internal-error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>unresolved extern </xsl:text>
          <xsl:value-of select="$name" />
          <xsl:text> (declared by `</xsl:text>
          <xsl:value-of select="$sym/@src" />
          <xsl:text>')</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:function>


<!--
  Resolves externs before processing them

  External symbols need special treatment; unlike other symbols, which are both
  declared and defined within the same package, externs are declared but their
  definitions are deferred to a later package. This allows using a value from
  another package in a C-style reverse-inheritence kind of way that is
  generally not a good idea, but sometimes necessary.

  When a package imports another package that declares an extern, the importing
  package must either declare that symbol as an extern or provide a definition.
  This is important, as it provides us with an easy means of resolution:
  traverse up the stack of symbols that are being processed, check their
  containing packages and see if there is a definition for the symbol. At some
  point, assuming that the shared object files are properly built, we must
  arrive at the definition. Since the symbol is defined within that package,
  the object file will also contain its dependency list.
-->
<xsl:template match="preproc:sym[ @extern='true' ]" mode="l:depgen-process-sym" priority="5">
  <xsl:param name="pending" as="element( preproc:sym )*" />
  <xsl:param name="stack" as="element( l:sym-stack )" />
  <xsl:param name="path" as="xs:string" />
  <xsl:param name="processing" as="element( l:pstack )" />

  <xsl:variable name="cur" select="." />

  <xsl:variable name="eresolv" as="element( preproc:sym )*"
                select="l:resolv-extern( $cur )" />

  <!-- were we able to resolve the symbol? -->
  <xsl:if test="empty( $eresolv )">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>could not resolve external symbol `</xsl:text>
          <xsl:value-of select="$cur/@name" />
        <xsl:text>'</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <!-- in the event that we are importing symbols from program packages (which
       hopefully is a rare case), we may have external symbols that resolve to
       the same package; filter out duplicates -->
  <xsl:variable name="eresolv-uniq" as="element( preproc:sym )"
                select="$eresolv[
                          not( @src = $eresolv[ not( current() ) ]/@src ) ]" />

  <!-- did we find more than one? (that would be very bad and likely represents
       a symbol table generation bug) -->
  <xsl:if test="count( $eresolv-uniq ) gt 1">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>ambiguous external symbol `</xsl:text>
          <xsl:value-of select="$cur/@name" />
        <xsl:text>'; resolution failed (found </xsl:text>
          <xsl:for-each select="$eresolv-uniq">
            <xsl:if test="position() > 1">
              <xsl:text>; </xsl:text>
            </xsl:if>
            <xsl:value-of select="@src" />
          </xsl:for-each>
        <xsl:text>); pulled in by: </xsl:text>

        <!-- help the user figure out how this happened -->
        <xsl:for-each select="$processing/preproc:sym">
          <xsl:if test="position() gt 1">
            <xsl:text> - </xsl:text>
          </xsl:if>
          <xsl:value-of select="concat( @src, '/', @name )" />
        </xsl:for-each>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:call-template name="log:debug">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg"
                    select="concat(
                              'external symbol ', $cur/@name,
                              ' resolved to ',
                              ( if ( $eresolv-uniq/@src ) then
                                  $eresolv-uniq/@src
                                else '' ),
                              '/',
                              $eresolv-uniq/@name )" />
  </xsl:call-template>

  <!-- use the resolved symbol in place of the original extern -->
  <xsl:apply-templates select="$eresolv-uniq" mode="l:depgen-process-sym">
    <xsl:with-param name="pending" select="$pending" />
    <xsl:with-param name="stack" select="$stack" as="element( l:sym-stack )" />
    <xsl:with-param name="path" select="$path" />
    <xsl:with-param name="processing" select="$processing" />
  </xsl:apply-templates>
</xsl:template>


<xsl:template mode="l:depgen-process-sym" priority="1"
              match="preproc:sym">
  <xsl:param name="pending" as="element( preproc:sym )*" />
  <xsl:param name="stack" as="element( l:sym-stack )" />
  <xsl:param name="path" as="xs:string" />
  <xsl:param name="processing" as="element( l:pstack )" />

  <xsl:variable name="cur" as="element( preproc:sym )"
                select="." />

  <!-- determines if the compile destination for these dependencies will be
       within the classifier; this is the case for all class dependencies
       *unless* the class is external -->
  <xsl:variable name="inclass" as="xs:boolean"
                select="( ( $cur/@type='class' )
                          and not( $cur/@extclass='true' ) )
                        or $processing/preproc:sym[
                          @type='class'
                          and not( @extclass='true' ) ]" />

  <!-- perform circular dependency check and blow up if found (we cannot choose
       a proper linking order without a correct dependency tree); the only
       exception is if the circular dependency is a function, since that simply
       implies recursion, which we can handle just fine -->
  <xsl:variable name="circ" as="element( preproc:sym )*"
                select="$processing/preproc:sym[
                          @name=$cur/@name
                          and @src=$cur/@src ]" />

  <xsl:choose>
    <!-- non-function; fatal -->
    <xsl:when test="$circ
                    and $circ[ not( @type='func' ) ]
      ">
      <xsl:call-template name="l:err-circular">
        <xsl:with-param name="stack" select="$processing" />
        <xsl:with-param name="cur" select="$cur" />
      </xsl:call-template>
    </xsl:when>

    <!-- function; we've done all we need to, so do not re-link
         (recursive call) -->
    <xsl:when test="$circ">
      <!-- continue processing; leave stack unchanged -->
      <xsl:call-template name="l:depgen-sym">
        <xsl:with-param name="pending" select="remove( $pending, 1 )" />
        <xsl:with-param name="processing" select="$processing" />
        <xsl:with-param name="stack" select="$stack" as="element( l:sym-stack )"  />
      </xsl:call-template>
    </xsl:when>


    <!-- process; TODO: good refactoring point; large template -->
    <xsl:otherwise>
      <xsl:variable name="existing" as="element( preproc:sym )*"
                    select="$stack/preproc:sym[
                              @name=$cur/@name ]" />

      <xsl:variable name="src-conflict" as="element( preproc:sym )*"
                    select="if ( not( $cur/@src ) or $cur/@src = '' ) then
                              ()
                            else
                              $existing[ not( @src = $cur/@src ) ]" />

      <xsl:if test="$src-conflict">
        <xsl:call-template name="log:error">
          <xsl:with-param name="name" select="'link'" />
          <xsl:with-param name="msg">
            <xsl:text>symbol name is not unique: `</xsl:text>
            <xsl:value-of select="@name" />
            <xsl:text>' found in</xsl:text>
            <xsl:value-of select="$cur/@src" />

            <xsl:for-each select="$src-conflict">
              <xsl:text> and </xsl:text>
              <xsl:value-of select="@src" />
            </xsl:for-each>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <!-- determine if class already exists, but needs to be marked for
           inclusion within the classifier -->
      <xsl:variable name="needs-class-mark" as="xs:boolean"
                    select="$existing
                              and $inclass
                              and not( $existing/@inclass='true' )
                              and not( $stack/preproc:sym[
                                         @l:mark-inclass
                                         and @name=$cur/@name
                                         and @src=$cur/@src ] )" />

      <!-- continue with the remainder of the symbol list -->
      <xsl:call-template name="l:depgen-sym">
        <xsl:with-param name="pending" select="remove( $pending, 1 )" />
        <xsl:with-param name="processing" select="$processing" />

        <xsl:with-param name="stack" as="element( l:sym-stack )">
          <!-- if this symbol already exists on the stack, then there is no use
               re-adding it (note that we check both the symbol name and its source
               since symbols could very well share a name due to exporting rules) -->
          <xsl:choose>
            <xsl:when test="not( $existing ) or $needs-class-mark">
              <!-- does this symbol have any dependencies? -->
              <xsl:variable name="deps" as="element( preproc:sym )*">
                <xsl:apply-templates select="$cur" mode="l:depgen-sym" />
              </xsl:variable>

              <!-- determine our path -->
              <xsl:variable name="mypath">
                <xsl:call-template name="preproc:get-path">
                  <xsl:with-param name="path" select="$cur/@src" />
                </xsl:call-template>
              </xsl:variable>

              <!-- augment each of the dep paths with our own (this ultimately
                   creates symbol paths relative to the rater) -->
              <xsl:variable name="deps-aug" as="element( preproc:sym )*">
                <xsl:call-template name="l:dep-aug">
                  <xsl:with-param name="cur" select="$cur" />
                  <xsl:with-param name="deps" select="$deps" />
                  <xsl:with-param name="inclass" select="$inclass" />
                  <xsl:with-param name="mypath" select="$mypath" />
                </xsl:call-template>
              </xsl:variable>

              <l:sym-stack>
                <!-- process the dependencies (note that this has the effect of
                     outputting the existing stack as well, which is why we have
                     not yet done so) -->
                <xsl:call-template name="l:depgen-sym">
                  <xsl:with-param name="pending" select="$deps-aug" />
                  <xsl:with-param name="stack" select="$stack" as="element( l:sym-stack )" />
                  <xsl:with-param name="path" select="$mypath" />
                  <xsl:with-param name="processing" as="element( l:pstack )">
                    <l:pstack>
                      <xsl:sequence select="$processing/*" />
                      <xsl:sequence select="$cur" />
                    </l:pstack>
                  </xsl:with-param>
                </xsl:call-template>

                <!-- finally, we can output ourself -->
                <xsl:choose>
                  <!-- existing symbol needs to be marked -->
                  <xsl:when test="$needs-class-mark">
                    <preproc:sym l:mark-inclass="true" name="{$cur/@name}" src="{$cur/@src}" />
                  </xsl:when>

                  <!-- new symbol -->
                  <xsl:otherwise>
                    <preproc:sym>
                      <xsl:sequence select="$cur/@*" />
                      <xsl:attribute name="inclass" select="$inclass" />
                    </preproc:sym>
                  </xsl:otherwise>
                </xsl:choose>
              </l:sym-stack>
            </xsl:when>


            <!-- already exists; leave stack unchanged -->
            <xsl:otherwise>
              <xsl:sequence select="$stack" />
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="l:dep-aug" as="element( preproc:sym )*">
  <xsl:param name="cur" as="element( preproc:sym )" />
  <xsl:param name="deps" as="element( preproc:sym )*" />
  <xsl:param name="inclass" as="xs:boolean"
             select="false()" />
  <xsl:param name="proc-barrier" as="xs:boolean"
             select="false()" />
  <xsl:param name="parent-name" as="xs:string"
             select="$cur/@name" />
  <xsl:param name="mypath">
    <!-- default -->
    <xsl:call-template name="preproc:get-path">
      <xsl:with-param name="path" select="$cur/@src" />
    </xsl:call-template>
  </xsl:param>

  <xsl:for-each select="$deps">
    <xsl:copy>
      <xsl:copy-of select="@*" />

      <xsl:variable name="newsrc">
        <xsl:choose>
          <!-- if a source path is provided, then we must take it
               relative to the current symbol's package's directory
               -->
          <xsl:when test="@src">
            <xsl:call-template name="preproc:resolv-path">
              <xsl:with-param name="path">
                <xsl:value-of select="$mypath" />

                <xsl:if test="$mypath and not( $mypath='' ) and @src and not( @src='' )">
                  <xsl:text>/</xsl:text>
                </xsl:if>

                <xsl:value-of select="@src" />
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>

          <!-- if no source path is set, then it exists in the same
               package as we do -->
          <xsl:otherwise>
            <xsl:value-of select="$cur/@src" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <!-- set new src path -->
      <xsl:attribute name="src" select="$newsrc" />

      <!-- flag for inclusion into classifier, if necessary -->
      <xsl:if test="$inclass">
        <xsl:attribute name="inclass" select="$inclass" />
      </xsl:if>

      <xsl:if test="$proc-barrier">
        <xsl:attribute name="l:proc-barrier" select="'true'" />
      </xsl:if>


      <xsl:if test="$l:aggressive-debug">
        <xsl:call-template name="log:debug">
          <xsl:with-param name="name" select="'link'" />
          <xsl:with-param name="msg">
            <xsl:value-of select="$parent-name" />
            <xsl:text> depends upon </xsl:text>
            <xsl:if test="@extern='true'">
              <xsl:text>external </xsl:text>
            </xsl:if>
            <xsl:value-of select="concat( @type, ' ', $newsrc, '/', @name )" />
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </xsl:copy>
  </xsl:for-each>
</xsl:template>



<!-- TODO: some better way. -->
<xsl:template match="preproc:sym[ starts-with( @type, 'map' ) or starts-with( @type, 'retmap' ) ]"
  mode="l:depgen-sym" priority="7">

  <!-- do not process deps -->
</xsl:template>


<xsl:template mode="l:depgen-sym" as="element()*"
              match="preproc:pkg-seen"
              priority="5">
  <xsl:sequence select="." />
</xsl:template>


<xsl:template mode="l:depgen-sym" as="element( preproc:sym )*"
              match="preproc:sym[
                       @type='const' ]"
              priority="7">
  <!-- no need to link this; it has no code associated with it -->
</xsl:template>

<xsl:template mode="l:depgen-sym" as="element( preproc:sym )*"
              match="preproc:sym"
              priority="5">
  <!-- get the source package -->
  <xsl:variable name="pkg" as="element( lv:package )?" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), $l:orig-root )/lv:*
      else
        $l:orig-root/lv:package
    " />

  <xsl:variable name="name" as="xs:string"
                select="@name" />
  <xsl:variable name="deps" as="element( preproc:sym-dep )?"
                select="$pkg/preproc:sym-deps
                          /preproc:sym-dep[ @name=$name ]" />

  <!-- if we could not locate the dependencies, then consider this to be an
       error (even if there are no deps, there should still be a list dfn) -->
  <xsl:if test="not( $deps )">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>could not locate dependency list for </xsl:text>
        <xsl:value-of select="@src" />
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@name" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:variable name="symtable" as="element( preproc:symtable )"
                select="$pkg/preproc:symtable" />

  <xsl:for-each select="
      $deps/preproc:sym-ref[
        not( @parent=$deps/preproc:sym-ref/@name )
      ]
    ">
    <xsl:variable name="sym-ref" as="xs:string"
                  select="@name" />

    <xsl:variable name="sym" as="element( preproc:sym )?"
                  select="$symtable/preproc:sym[ @name=$sym-ref ]" />

    <!-- if we cannot locate the referenced symbol, then that too is an error
         -->
    <xsl:if test="not( $sym )">
      <xsl:call-template name="log:internal-error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>failed locating dependency symbol `</xsl:text>
          <xsl:value-of select="$sym-ref" />
          <xsl:text>'</xsl:text>
          <xsl:text> from package </xsl:text>
          <xsl:value-of select="$pkg/@name" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <!-- output the symbol, sans children -->
    <preproc:sym>
      <xsl:sequence select="$sym/@*" />
    </preproc:sym>
  </xsl:for-each>
</xsl:template>


<xsl:template mode="l:depgen-sym"
              match="*"
              priority="1">
  <xsl:message terminate="yes">
    <xsl:text>internal error: unknown symbol for l:depgen-sym: </xsl:text>
    <xsl:sequence select="." />
  </xsl:message>
</xsl:template>


<xsl:template name="l:err-circular">
  <xsl:param name="stack" />
  <xsl:param name="cur" />

  <xsl:call-template name="log:error">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>circular dependency </xsl:text>

      <xsl:text>(</xsl:text>
      <xsl:value-of select="concat( $cur/@src, '/', $cur/@name )" />
      <xsl:text>): </xsl:text>

      <xsl:for-each select="$stack//preproc:sym">
        <xsl:if test="position() > 1">
          <xsl:text> - </xsl:text>
        </xsl:if>

        <xsl:value-of select="concat( @src, '/', @name )" />
      </xsl:for-each>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<!--
  Links the object code for each dependency

  And here is where the magic happens. This will take the generated dependency
  list and, in order of the list, combine the object code from its source
  package's object file. The result is a fully linked executable.

  Not only is this one of the most interesting parts, this is also one of the
  fastest; all the hard work has already been done.

  Note: though the linked code is suitable for execution, it is up to a
  particular implementation to decide how it should be wrapped and invoked.
-->
<xsl:template match="lv:package" mode="l:link-deps">
  <xsl:param name="deps" />

  <!-- to make this executable, we must compile an entry point -->
  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>compiling entry point...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates select="." mode="compiler:entry" />


  <xsl:apply-templates select="." mode="l:link-meta">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-worksheet">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-classifier">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-params">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-types">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-funcs">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="l:link-rater">
    <xsl:with-param name="deps" select="$deps" />
  </xsl:apply-templates>


  <!-- finally, finish up -->
  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>compiling exit...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:value-of select="@name" />
      <xsl:text> compilation complete.</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:function name="l:link-exit-fragments" as="xs:string*">
  <xsl:param name="paths"   as="xs:string" />
  <xsl:param name="context" as="node()" />

  <xsl:variable name="split" as="xs:string*"
                select="tokenize( $paths, ',' )" />

  <xsl:variable name="base-uri" as="xs:anyURI"
                select="base-uri( $context )" />

  <xsl:for-each select="$split">
    <xsl:variable name="fragment" as="xs:string?"
                  select="l:get-fragment-by-path( ., $base-uri )" />

    <xsl:if test="empty( $fragment )">
      <xsl:call-template name="log:error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>fatal: missing exit fragment: </xsl:text>
          <xsl:value-of select="." />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <xsl:sequence select="$fragment" />
  </xsl:for-each>
</xsl:function>


<xsl:template match="lv:package" mode="l:link-meta">
  <xsl:param name="deps" as="element( preproc:sym )*" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking metadata...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[ @type='meta' ]" />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:link-worksheet">
  <xsl:param name="deps" as="element( preproc:sym )*" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking worksheet...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[ @type='worksheet' ]" />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:link-classifier">
  <xsl:param name="deps" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking classifier...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <!-- link everything that shall be a part of the classifier -->
  <xsl:apply-templates select="." mode="compiler:entry-classifier" />
  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[
          @inclass='true'
          and not( @type='param' )
          and not( @type='type' )
          and not( @type='meta' )
          and not( @type='worksheet' )
        ]
      " />
  </xsl:apply-templates>
  <xsl:apply-templates select="." mode="compiler:exit-classifier" />
</xsl:template>


<xsl:template match="lv:package" mode="l:link-params">
  <xsl:param name="deps" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking global params...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <!-- link all params -->
  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[ @type='param' ]
      " />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:link-types">
  <xsl:param name="deps" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking types...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <!-- link all params -->
  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[ @type='type' ]
      " />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:link-funcs">
  <xsl:param name="deps" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking functions...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <!-- link all params -->
  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[ @type='func' ]
      " />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:link-rater">
  <xsl:param name="deps" />

  <xsl:call-template name="log:info">
    <xsl:with-param name="name" select="'link'" />
    <xsl:with-param name="msg">
      <xsl:text>** linking rater...</xsl:text>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:apply-templates select="." mode="compiler:entry-rater" />

  <!-- TODO: this list of exclusions is a mess -->
  <xsl:apply-templates select="." mode="l:do-link">
    <xsl:with-param name="symbols" select="
        $deps[
          not( @inclass='true' )
          and not( @type='param' )
          and not( @type='type' )
          and not( @type='func' )
          and not( @type='meta' )
          and not( @type='worksheet' )
        ]
      " />
  </xsl:apply-templates>

  <xsl:sequence select="l:link-exit-fragments(
                          $rater-exit-fragments,
                          . )" />

  <xsl:apply-templates select="." mode="compiler:exit-rater">
    <xsl:with-param name="symbols" select="$deps" />
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="lv:package" mode="l:do-link">
  <xsl:param name="symbols" />

  <!-- link each of the dependencies -->
  <xsl:for-each select="$symbols">
    <xsl:call-template name="log:info">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>linking </xsl:text>
        <xsl:value-of select="concat( @type, ' ', @src, '/', @name )" />
        <xsl:text>...</xsl:text>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:apply-templates select="." mode="l:link-deps" />
  </xsl:for-each>
</xsl:template>


<xsl:template match="preproc:sym[ @type='lparam' ]" mode="l:link-deps" priority="9">
  <!-- no object code for local params -->
</xsl:template>

<!-- priority of 7 because there may otherwise be some ambiguities
     (e.g. with lparam) -->
<xsl:template match="preproc:sym[ @parent ]" mode="l:link-deps" priority="7">
  <!-- if a parent is defined, then its symbol will have been sufficient -->
</xsl:template>

<xsl:template match="preproc:sym" mode="l:link-deps" priority="5">
  <!-- consult the source package for the last time... -->
  <xsl:variable name="pkg" select="
      if ( @src and not( @src='' ) ) then
        document( concat( @src, '.xmlo' ), $l:orig-root )/lv:*
      else
        $l:orig-root/lv:package
    " />

  <xsl:variable name="name" select="@name" />
  <xsl:variable name="objcode" as="xs:string?"
                select="l:get-fragment( $pkg, $name )" />

  <xsl:if test="empty( $objcode )">
    <xsl:if test="not( @type='param'
                       or ( @type='const' and @dim='0' )
                       or @type='tpl'
                       or @type='meta'
                       or not( @type='worksheet' ) )">
      <xsl:call-template name="log:internal-error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>missing object code for symbol </xsl:text>
          <xsl:value-of select="concat( @src, '/', @name )" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:if>

  <xsl:copy-of select="$objcode" />
</xsl:template>


<xsl:function name="l:get-fragment-by-path" as="xs:string?">
  <xsl:param name="path"     as="xs:string" />
  <xsl:param name="base-uri" as="xs:anyURI" />

  <xsl:variable name="pkg-path" as="xs:string">
    <xsl:call-template name="preproc:get-path">
      <xsl:with-param name="path" select="$path" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="fragment-name" as="xs:string">
    <xsl:call-template name="preproc:get-basename">
      <xsl:with-param name="path" select="$path" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="package-uri" as="xs:anyURI"
                select="resolve-uri(
                          concat( $pkg-path, '.xmlo' ),
                          $base-uri )" />

  <xsl:variable name="doc" as="document-node()"
                select="doc( $package-uri )" />

  <xsl:if test="empty( $doc )">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>could not locate package for exit-fragment: </xsl:text>
        <xsl:value-of select="$package-uri" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:variable name="package" as="element()"
                select="$doc/*" />

  <xsl:sequence select="l:get-fragment(
                          $package,
                          $fragment-name )" />
</xsl:function>


<xsl:function name="l:get-fragment" as="xs:string?">
  <xsl:param name="package" as="element()" />
  <xsl:param name="name"    as="xs:string" />

  <xsl:variable name="fragment" as="element( preproc:fragment )?"
                select="$package/preproc:fragments/preproc:fragment[
                         @id = $name ]" />

  <xsl:sequence select="$fragment/text()" />
</xsl:function>


<xsl:template match="lv:package" mode="l:map" priority="5">
  <!-- it is important that we check against the dependencies actually compiled
       rather than the list of available symbols -->
  <xsl:param name="deps" as="element( l:dep )" />

  <xsl:variable name="syms" as="element( preproc:sym )*"
                select="preproc:symtable/preproc:sym" />

  <xsl:variable name="mapsyms" as="element( preproc:sym )*"
                select="$syms[ @type='map' ]" />
  <xsl:variable name="retmapsyms" as="element( preproc:sym )*"
                select="$syms[ @type='retmap' ]" />

  <!-- get head and tail -->
  <xsl:variable name="head"     select="$syms[ @type='map:head' ]" />
  <xsl:variable name="tail"     select="$syms[ @type='map:tail' ]" />
  <xsl:variable name="ret-head" select="$syms[ @type='retmap:head' ]" />
  <xsl:variable name="ret-tail" select="$syms[ @type='retmap:tail' ]" />

  <xsl:if test="count( $mapsyms ) gt 0">
    <xsl:call-template name="log:info">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>generating input map...</xsl:text>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:if test="not( $head ) or not( $tail )">
      <xsl:call-template name="log:internal-error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>missing object code for input map head or tail</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <!-- input map -->
    <l:map-exec>
      <xsl:apply-templates select="$head" mode="l:link-deps" />
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates select="$mapsyms" mode="l:map">
        <xsl:with-param name="symtable" select="$deps" />
        <!-- TODO -->
        <xsl:with-param name="ignore-error" select="true()" />
      </xsl:apply-templates>
      <xsl:apply-templates select="$tail" mode="l:link-deps" />
    </l:map-exec>
  </xsl:if>


  <!-- TODO: very similar to above; refactor -->
  <xsl:if test="count( $retmapsyms ) gt 0">
    <xsl:call-template name="log:info">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>generating return map...</xsl:text>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:if test="not( $ret-head ) or not( $ret-tail )">
      <xsl:call-template name="log:internal-error">
        <xsl:with-param name="name" select="'link'" />
        <xsl:with-param name="msg">
          <xsl:text>missing object code for return map head or tail</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <!-- return map -->
    <l:retmap-exec>
      <xsl:apply-templates select="$ret-head" mode="l:link-deps" />
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates select="$retmapsyms" mode="l:map">
        <xsl:with-param name="type" select="'return'" />
        <xsl:with-param name="from" select="'input'" />
        <xsl:with-param name="symtable" select="$deps" />
      </xsl:apply-templates>
      <xsl:apply-templates select="$ret-tail" mode="l:link-deps" />
    </l:retmap-exec>
  </xsl:if>
</xsl:template>


<xsl:template match="preproc:sym" mode="l:map" priority="5">
  <xsl:param name="symtable" as="element( l:dep )" />
  <xsl:param name="type" as="xs:string"
             select="'input'" />
  <xsl:param name="from" as="xs:string"
             select="'destination'" />
  <xsl:param name="ignore-error" as="xs:boolean"
             select="false()" />

  <xsl:variable name="name" as="xs:string"
                select="@name" />
  <xsl:variable name="src" as="xs:string"
                select="@src" />

  <!-- map symbols must always be remote -->
  <xsl:variable name="pkg" as="element( lv:package )"
                select="document( concat( @src, '.xmlo' ), . )
                          /lv:package" />

  <!-- get map symbol dependencies -->
  <xsl:variable name="deps" as="element( preproc:sym-dep )*"
                select="$pkg/preproc:sym-deps/
                          preproc:sym-dep[ @name=$name ]" />

  <xsl:if test="not( $deps )">
    <xsl:call-template name="log:internal-error">
      <xsl:with-param name="name" select="'link'" />
      <xsl:with-param name="msg">
        <xsl:text>could not locate symbol dependencies: </xsl:text>
        <xsl:value-of select="concat( @src, '/', @name )" />
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <!-- FIXME: we should not have to check for @yields here; we may
       have to require imports in the map to satisfy normalization
       before-hand -->
  <xsl:variable name="unknown" as="element( preproc:sym-ref )*"
                select="$deps/preproc:sym-ref[
                          not( @name=$symtable/preproc:sym/@name
                               or @name=$symtable/preproc:sym/@yields ) ]" />

  <xsl:choose>
    <!-- ensure that every dependency is known (we only care that the symbol
         actually exists and is an input) -->
    <xsl:when test="$unknown and not( $ignore-error )">
      <xsl:for-each select="$unknown">
        <xsl:call-template name="log:error">
          <xsl:with-param name="terminate" select="'no'" />
          <xsl:with-param name="name" select="'link'" />
          <xsl:with-param name="msg">
            <xsl:value-of select="$type" />
            <xsl:text> map </xsl:text>
            <xsl:value-of select="$from" />
            <xsl:text> </xsl:text>
              <xsl:value-of select="@name" />
            <xsl:text> is not a known </xsl:text>
              <xsl:value-of select="$type" />
            <xsl:text> field for </xsl:text>
            <xsl:value-of select="concat( $src, '/', $name )" />
            <xsl:text>; ensure that it exists and is either used or has @keep set</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>

      <l:map-error />
    </xsl:when>

    <!-- good to go; link symbol -->
    <xsl:otherwise>
      <xsl:apply-templates select="." mode="l:link-deps" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
