<?xml version="1.0" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"/>

<xsl:template match="/">
  <html>
    <head>
      <title><xsl:value-of select="Code_Analysis_Tree/@name"/></title>
      <style type="text/css">
.Project {color:blue}
.File {text-indent:5px}
.Subprogram {color:grey; text-indent:10px}
      </style>
    </head>
    <body>
      <table border="1">
        <tr><th>Entities</th><th>Coverage</th><th>Percentage</th></tr>
        <xsl:apply-templates select="*"/>
      </table>
    </body>
  </html>
</xsl:template>

<xsl:template match="Code_Analysis_Tree|vfs_name|Line">
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="Project|Subprogram">
  <tr class="{name()}">
    <td><xsl:value-of select="@name"/></td>
    <td>
      <xsl:value-of select="@children"/>
      <xsl:text> lines (</xsl:text>
      <xsl:value-of select="@coverage"/>
      <xsl:text> not covered)</xsl:text>
    </td>
    <td>
      <xsl:value-of select=
         "format-number(number(@coverage) * 100 div number(@children),'#')"/>
    </td>
  </tr>
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="File[@status='FILE_NOT_FOUND']"/>

<xsl:template match="File[@status='VALID']">
  <tr class="{name()}">
    <td>
      <xsl:call-template name="file-name">
        <xsl:with-param name="path" select="vfs_name/text()"/>
      </xsl:call-template>
    </td>
    <td>
      <xsl:value-of select="@children"/>
      <xsl:text> lines (</xsl:text>
      <xsl:value-of select="@coverage"/>
      <xsl:text> not covered)</xsl:text>
    </td>
    <td>
      <xsl:value-of select=
         "format-number(number(@coverage) * 100 div number(@children),'#')"/>
    </td>
  </tr>
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template name="file-name">
  <xsl:param name="path"/>

  <xsl:if test="contains ($path, '/')">
    <xsl:call-template name="file-name">
      <xsl:with-param name="path" select="substring-after($path, '/')"/>
    </xsl:call-template>
  </xsl:if>

  <xsl:if test="not (contains ($path, '/'))">
    <xsl:value-of select="$path"/>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
