<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
>

<xsl:variable name="g1" select="1"/>
<xsl:variable name="g2">this is a string</xsl:variable>

<xsl:template match="page">
 <xsl:variable name="a" select="h1"/>
 <xsl:variable name="b" select="'Title 123'"/>
 <xsl:apply-templates select="child::*"/>
</xsl:template>

<xsl:template match="h1">
 <xsl:variable name="a1" select="."/><!-- 132</xsl:variable> -->
</xsl:template>

</xsl:transform>
