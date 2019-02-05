<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:bugs="urn:x-bugs"
                xmlns="http://www.w3.org/1999/xhtml"
                version="1.0">
  <xsl:output method="xml" indent="yes"/>
  <xsl:include href="common.xsl"/>

  <xsl:template match="bugs:table/bugs:row">
    <a href="?auth={$auth}&amp;t=list&amp;q=select%20bug_search(%27%27,%20%27%27,%2010,%200)">back to listing</a>
    <dl>
      <dt>ID</dt>
      <dd><xsl:copy-of select="bugs:id/text()" /></dd>
      <dt>Reported</dt>
      <dd><xsl:copy-of select="bugs:reported/text()" /></dd>
      <dt>Reporter</dt>
      <dd><xsl:copy-of select="bugs:reporter/text()" /></dd>
      <dt>Project</dt>
      <dd><xsl:copy-of select="bugs:project/text()" /></dd>
      <dt>Description</dt>
      <dd><pre><xsl:copy-of select="bugs:description/text()" /></pre></dd>
    </dl>
  </xsl:template>

</xsl:stylesheet>
