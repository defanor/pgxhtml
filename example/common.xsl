<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:pgx="urn:x-pgxhtml"
                xmlns="http://www.w3.org/1999/xhtml"
                version="1.0">
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Bugs</title>
      </head>
      <body>
        <xsl:apply-templates select="*" />
      </body>
    </html>
  </xsl:template>

  <xsl:template match="pgx:sql_error">
    <h1>SQL error</h1>
    <dl>
      <dt>State</dt>
      <dd><xsl:copy-of select="pgx:state/text()" /></dd>
      <dt>Status</dt>
      <dd><xsl:copy-of select="pgx:status/text()" /></dd>
      <dt>Message</dt>
      <dd><xsl:copy-of select="pgx:message/text()" /></dd>
      <dt>Detail</dt>
      <dd><xsl:copy-of select="pgx:detail/text()" /></dd>
      <dt>Hint</dt>
      <dd><xsl:copy-of select="pgx:hint/text()" /></dd>
    </dl>
  </xsl:template>

  <xsl:template match="pgx:result_error">
    <h1>Result error</h1>
    <dl>
      <dt>Message</dt>
      <dd><xsl:copy-of select="pgx:message/text()" /></dd>
    </dl>
  </xsl:template>

  <xsl:template match="pgx:query_error">
    <h1>Query error</h1>
    <dl>
      <dt>Message</dt>
      <dd><xsl:copy-of select="pgx:message/text()" /></dd>
      <dt>Query</dt>
      <dd><xsl:copy-of select="pgx:query/text()" /></dd>
    </dl>
  </xsl:template>

  <xsl:template match="pgx:format_error">
    <h1>Format error</h1>
    <dl>
      <dt>Message</dt>
      <dd><xsl:copy-of select="pgx:message/text()" /></dd>
      <dt>Query</dt>
      <dd><xsl:copy-of select="pgx:query/text()" /></dd>
      <dt>Parameters</dt>
      <xsl:for-each select="pgx:param">
        <dd><xsl:copy-of select="text()" /></dd>
      </xsl:for-each>
    </dl>
  </xsl:template>

  <xsl:template match="pgx:error">
    <h1>Error</h1>
    <dl>
      <dt>Message</dt>
      <dd><xsl:copy-of select="pgx:message/text()" /></dd>
    </dl>
  </xsl:template>

</xsl:stylesheet>
