<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
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

  <xsl:template match="sql_error">
    <h1>SQL error</h1>
    <dl>
      <dt>State</dt>
      <dd><xsl:copy-of select="@state/text()" /></dd>
      <dt>Status</dt>
      <dd><xsl:copy-of select="@status/text()" /></dd>
      <dt>Message</dt>
      <dd><xsl:copy-of select="@message/text()" /></dd>
      <dt>Detail</dt>
      <dd><xsl:copy-of select="@detail/text()" /></dd>
      <dt>Hint</dt>
      <dd><xsl:copy-of select="@hint/text()" /></dd>
      <dt>Query template</dt>
      <dd><xsl:copy-of select="@template/text()" /></dd>
      <dt>Query parameters</dt>
      <dd><xsl:copy-of select="@parameters/text()" /></dd>
    </dl>
  </xsl:template>

</xsl:stylesheet>
