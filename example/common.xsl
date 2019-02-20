<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:pgx="urn:x-pgxhtml"
                xmlns="http://www.w3.org/1999/xhtml"
                version="1.0">
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="/">
    <xsl:text disable-output-escaping='yes'>&lt;!DOCTYPE html&gt;</xsl:text>
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Bugs</title>
      </head>
      <body>
        <xsl:apply-templates select="*" />
      </body>
    </html>
  </xsl:template>

  <xsl:template match="pgx:error">
    <h1>Error</h1>
    <dl>
      <xsl:apply-templates select="*" />
    </dl>
  </xsl:template>

  <xsl:template match="pgx:message">
    <dt>Message</dt>
    <dd><pre><xsl:copy-of select="text()" /></pre></dd>
  </xsl:template>

  <xsl:template match="pgx:db_error">
    <dt>DB error message</dt>
    <dd><pre><xsl:copy-of select="text()" /></pre></dd>
  </xsl:template>

  <xsl:template match="pgx:exec_status">
    <dt>ExecStatus</dt>
    <dd><xsl:copy-of select="text()" /></dd>
  </xsl:template>

  <xsl:template match="pgx:no_query">
    <p>No query is provided.</p>
  </xsl:template>

</xsl:stylesheet>
