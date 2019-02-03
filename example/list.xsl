<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:bugs="urn:x-bugs"
                xmlns:str="http://exslt.org/strings"
                xmlns="http://www.w3.org/1999/xhtml"
                extension-element-prefixes="str"
                version="1.0">
  <xsl:output method="xml" indent="yes"/>
  <xsl:include href="common.xsl"/>
  <xsl:param name="project" />
  <xsl:param name="description" />
  <xsl:param name="limit" select="10" />
  <xsl:param name="offset" select="0" />

  <xsl:template match="bugs:table">
    <!-- Report form -->
    <h2>Report</h2>
    <form method="post" action="view.xhtml?q=insert%20into%20bugs%20(%20:fields%20)%20values%20(%20:values%20)%20returning%20xmlelement(name%20table,xmlattributes('urn:x-bugs'%20as%20xmlns),xmlelement(name%20row,xmlelement(name%20id,id),xmlelement(name%20reported,reported),xmlelement(name%20reporter,reporter),xmlelement(name%20project,project),xmlelement(name%20description,description)))">
      <dl>
        <dt><label for="report_project">Project</label></dt>
        <dd>
          <input type="text" name="project" id="report_project"
                 required="required" maxlength="128"
                 placeholder="Project name or URL" />
        </dd>
        <dt><label for="report_description">Description</label></dt>
        <dd>
          <textarea name="description" required="required"
                    id="report_description" maxlength="10240"
                    placeholder="Issue description" />
        </dd>
      </dl>
      <input type="submit" value="Report" />
    </form>

    <!-- Search form -->
    <h2>Search</h2>
    <form method="get" action="list.xhtml">
      <dl>
        <dt><label for="search_project">Project</label></dt>
        <dd>
          <input id="search_project" type="search" name="project"
                 value="{$project}" />
        </dd>
        <dt><label for="search_description">Description</label></dt>
        <dd>
          <input id="search_description" type="search" name="description"
                 value="{$description}" />
        </dd>
        <dt><label for="search_limit">Limit</label></dt>
        <dd>
          <input id="search_limit" type="number" name="limit" min="1"
                 value="{$limit}" />
        </dd>
        <dt><label for="search_offset">Offset</label></dt>
        <dd>
          <input id="search_offset" type="number" name="offset" min="0"
                 value="{$offset}" />
        </dd>
        <input type="hidden" name="q"
               value="select bug_search( q:project , q:description , q:limit , q:offset )" />
      </dl>
      <input type="submit" value="Search" />
    </form>

    <!-- Search results -->
    <table>
      <tr>
        <th>Reported</th>
        <th>Reporter</th>
        <th>Project</th>
        <th>Summary</th>
      </tr>
      <xsl:for-each select="bugs:row">
        <tr>
          <td><xsl:copy-of select="bugs:reported/text()" /></td>
          <td><xsl:copy-of select="bugs:reporter/text()" /></td>
          <td>
            <a href="list.xhtml?q=select%20bug_search(%20q:project%20,'',{$limit},{$offset})&amp;project={str:encode-uri(bugs:project/text(), true())}">
              <xsl:copy-of select="bugs:project/text()" />
            </a>
          </td>
          <td>
            <a href="view.xhtml?q=select%20query_to_xml('select%20*%20from%20bugs%20where%20id=''{bugs:id}''',false,false,'urn:x-bugs')">
              <xsl:copy-of select="bugs:summary/text()" />
            </a>
          </td>
        </tr>
      </xsl:for-each>
    </table>
  </xsl:template>

</xsl:stylesheet>
