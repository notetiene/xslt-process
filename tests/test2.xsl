<?xml version="1.0"?>

<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
>

<xsl:template match="page">
 <xsl:variable name="a" select="h1"/>
 <xsl:variable name="b" select="'Title 123'"/>
 <xsl:apply-templates select="child::*"/>
</xsl:template>

<xsl:template match="h1">
 <xsl:variable name="a1" select=".">This is a bug!</xsl:variable>
 <h1><xsl:value-of select="$a1"/></h1>
</xsl:template>

</xsl:transform>
