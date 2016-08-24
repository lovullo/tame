<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  Operations on paths
-->
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:preproc="http://www.lovullo.com/rater/preproc">


<xsl:template name="preproc:get-path">
  <xsl:param name="path" />
  <xsl:param name="prev" select="''" />

  <xsl:variable name="first" select="substring-before( $path, '/' )" />
  <xsl:variable name="rest" select="substring-after( $path, '/' )" />

  <xsl:choose>
    <!-- if there's no $first, then there is no path separator, in which case
         we're done; if there's no rest, then there is a path separator, but it
         resulted in an empty string, meanaing that it ends in a path
         separator, in which case we are also done -->
    <xsl:when test="not( $first ) or not( $rest )">
      <!-- no more path separators; we're done -->
      <xsl:value-of select="$prev" />
    </xsl:when>

    <!-- keep recursing -->
    <xsl:otherwise>
      <xsl:call-template name="preproc:get-path">
        <xsl:with-param name="path" select="$rest" />
        <xsl:with-param name="prev">
          <xsl:if test="not( $prev = '' )">
            <xsl:value-of select="concat( $prev, '/' )" />
          </xsl:if>

          <xsl:value-of select="$first" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- FIXME: duplicate code with above -->
<xsl:template name="preproc:get-basename">
  <xsl:param name="path" />
  <xsl:param name="prev" select="''" />

  <xsl:variable name="first" select="substring-before( $path, '/' )" />
  <xsl:variable name="rest" select="substring-after( $path, '/' )" />

  <xsl:choose>
    <!-- if there's no $first, then there is no path separator, in which case
         we're done; if there's no rest, then there is a path separator, but it
         resulted in an empty string, meanaing that it ends in a path
         separator, in which case we are also done -->
    <xsl:when test="not( $first ) or not( $rest )">
      <!-- no more path separators; we're done -->
      <xsl:value-of select="$path" />
    </xsl:when>

    <!-- keep recursing -->
    <xsl:otherwise>
      <xsl:call-template name="preproc:get-basename">
        <xsl:with-param name="path" select="$rest" />
        <xsl:with-param name="prev">
          <xsl:if test="not( $prev = '' )">
            <xsl:value-of select="concat( $prev, '/' )" />
          </xsl:if>

          <xsl:value-of select="$first" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="preproc:resolv-path">
  <xsl:param name="path" />

  <!-- in order: strip //, process ../, strip ./ -->
  <xsl:call-template name="preproc:strip-sdot-path">
    <xsl:with-param name="path">
      <xsl:call-template name="preproc:resolv-rel-path">
        <xsl:with-param name="path">
          <xsl:call-template name="preproc:strip-extra-path">
            <xsl:with-param name="path" select="$path" />
          </xsl:call-template>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<!-- XXX: warning, this won't like 'foo../' -->
<xsl:template name="preproc:resolv-rel-path">
  <xsl:param name="path" />

  <!-- relative paths -->
  <xsl:variable name="before" select="substring-before( $path, '../' )" />
  <xsl:variable name="after" select="substring-after( $path, '../' )" />

  <xsl:choose>
    <xsl:when test="$before">
      <xsl:call-template name="preproc:resolv-rel-path">
        <xsl:with-param name="path">
          <!-- remove the last directory before the ../ -->
          <xsl:variable name="before-path">
            <xsl:call-template name="preproc:get-path">
              <xsl:with-param name="path" select="$before" />
            </xsl:call-template>
          </xsl:variable>

          <xsl:value-of select="$before-path" />

          <!-- the above get-path call will strip the trailing slash -->
          <xsl:if test="not( $before-path = '' ) and not( $after = '' )">
            <xsl:text>/</xsl:text>
          </xsl:if>

          <xsl:value-of select="$after" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>


    <!-- if there's no $before but there is an $after, then we must begin with
         '../', which we can do nothing with; output it and continue processing
         the remainder of the path -->
    <xsl:when test="$after">
      <xsl:text>../</xsl:text>

      <xsl:call-template name="preproc:resolv-rel-path">
        <xsl:with-param name="path" select="$after" />
      </xsl:call-template>
    </xsl:when>


    <!-- no relative paths remaining -->
    <xsl:otherwise>
      <xsl:value-of select="$path" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="preproc:strip-sdot-path">
  <xsl:param name="path" />

  <xsl:choose>
    <!-- the only time this should be called with an unresolved relative path
         is if it begins with one, in which case we'll simply output it and
         continue processing without it -->
    <xsl:when test="starts-with( $path, '../' )">
      <xsl:text>../</xsl:text>

      <!-- continue processing without it -->
      <xsl:call-template name="preproc:strip-sdot-path">
        <xsl:with-param name="path" select="substring-after( $path, '../' )" />
      </xsl:call-template>
    </xsl:when>


    <!-- path is safe for processing -->
    <xsl:otherwise>
      <xsl:variable name="a" select="substring-before( $path, './' )" />
      <xsl:variable name="b" select="substring-after( $path, './' )" />


      <xsl:choose>
        <!-- if we found one, recurse -->
        <xsl:when test="$a or $b">
          <xsl:call-template name="preproc:strip-sdot-path">
            <xsl:with-param name="path">
              <xsl:value-of select="$a" />

              <xsl:if test="$a and $b">
                <xsl:text>/</xsl:text>
              </xsl:if>

              <xsl:value-of select="$b" />
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>


        <!-- done -->
        <xsl:otherwise>
          <xsl:value-of select="$path" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="preproc:strip-extra-path">
  <xsl:param name="path" />

  <xsl:variable name="a" select="substring-before( $path, '//' )" />
  <xsl:variable name="b" select="substring-after( $path, '//' )" />


  <xsl:choose>
    <!-- if we found one, recurse -->
    <xsl:when test="$a or $b">
      <xsl:call-template name="preproc:strip-extra-path">
        <xsl:with-param name="path">
          <xsl:value-of select="$a" />

          <xsl:if test="$a and $b">
            <xsl:text>/</xsl:text>
          </xsl:if>

          <xsl:value-of select="$b" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>

    <!-- we're done! -->
    <xsl:otherwise>
      <xsl:value-of select="$path" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
