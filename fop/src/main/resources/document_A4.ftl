[#ftl]

<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format" font-family="DejaVuSansCondensed" font-size="9pt" line-height="13pt">

<fo:layout-master-set>
    <fo:simple-page-master master-name="left" margin-right="1cm" margin-left="3cm" margin-bottom="1.3cm" margin-top="1.3cm" page-width="21cm" page-height="29.7cm" >
      <fo:region-body margin-top="1cm" margin-bottom="1cm"/>
      <fo:region-before extent="0.5cm"/>
      <fo:region-after extent="0.5cm"/>
    </fo:simple-page-master>

</fo:layout-master-set>

<fo:page-sequence id="title_page" master-reference="left">

	<fo:static-content flow-name="xsl-region-after">
	  <fo:block text-align="center" space-before="1cm" font-size="9pt"  letter-spacing="150%">${titlePageFooter}</fo:block>
	</fo:static-content>

	<fo:flow flow-name="xsl-region-body">
		
		<fo:block text-align="center"  font-size="10pt" >${titlePageHeader}</fo:block>
		
		<fo:block text-align="center" space-before="6cm" font-size="14pt">${titlePageTitle}</fo:block>
		
		<fo:block text-align="center" space-before="2cm" font-size="9pt">Wykonał:</fo:block>
		<fo:block text-align="center" space-before="0.2cm" font-size="12pt" font-weight="bold" letter-spacing="70%">${titlePageAuthor}</fo:block>
		
		<fo:block text-align="center" space-before="2cm" font-size="10pt">${titlePageAuthorInfo1}</fo:block>
		
		<fo:block text-align="center" space-before="0.3cm" font-size="10pt">${titlePageAuthorInfo2}</fo:block>
		
		
		
	</fo:flow>
</fo:page-sequence>

[#list chapters as ch]

<fo:page-sequence id="dokument_${ch_index}" master-reference="left">

<fo:static-content flow-name="xsl-region-before">
  <fo:block text-align-last="center" font-size="9pt" width="100%" border-bottom=".5pt solid gray">
  	${title}[#if ch.title?has_content] - ${ch.title}[/#if]
  </fo:block>
</fo:static-content>

<fo:static-content flow-name="xsl-region-after">
  <fo:block text-align-last="right" font-size="9pt" width="100%" border-top=".5pt solid gray">
    <fo:page-number/>
  </fo:block>
</fo:static-content>

<fo:flow flow-name="xsl-region-body">
	[#if ch.content?has_content]
		[#list ch.content as c]
			${c}
		[/#list]
	[/#if]
</fo:flow>
</fo:page-sequence>
[/#list]
</fo:root>


