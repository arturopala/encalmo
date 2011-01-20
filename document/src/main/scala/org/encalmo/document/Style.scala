package org.encalmo.document

import java.awt.Color

/**
 * DocumentComponent's style class
 * @author artur
 */
case class Style(
	font:FontStyle = DefaultFontStyle,
	paragraph:ParagraphStyle = DefaultParagraphStyle,
	color:Color = Color.BLACK,
	background:Color = Color.WHITE
){
	def use(f:FontStyle):Style = copy(font=f)
	def use(p:ParagraphStyle):Style = copy(paragraph=p)
	
	def useColor(c:Color):Style = copy(color=c)
	def useBackground(b:Color):Style = copy(background=b)
	
	def fontBold:Style = copy(font = font.makeBold)
	def fontItalic:Style = copy(font = font.makeItalic)
	def fontBigger:Style = copy(font = font++)
	def fontSmaller:Style = copy(font = font--)
	
	def hexColor:String = {
		Seq[Int](color.getRed,color.getGreen,color.getBlue).map(x => {val h = x.toHexString; if(h.size>1) h else {"0"+h}}).mkString
	}
	
	def hexBackground:String = {
		Seq[Int](background.getRed,background.getGreen,background.getBlue).map(x => x.toHexString).mkString
	}
	
	def paddingTop(d:Int) = copy(paragraph = paragraph.paddingTop(d))
	def paddingBottom(d:Int) = copy(paragraph = paragraph.paddingBottom(d))
	def paddingLeft(d:Int) = copy(paragraph = paragraph.paddingLeft(d))
	def paddingRight(d:Int) = copy(paragraph = paragraph.paddingRight(d))
	
	def marginTop(d:Int) = copy(paragraph = paragraph.marginTop(d))
	def marginBottom(d:Int) = copy(paragraph = paragraph.marginBottom(d))
	def marginLeft(d:Int) = copy(paragraph = paragraph.marginLeft(d))
	def marginRight(d:Int) = copy(paragraph = paragraph.marginRight(d))
	
	def setSpaceBefore(d:Int) = copy(paragraph = paragraph.setSpaceBefore(d))
	def setSpaceAfter(d:Int) = copy(paragraph = paragraph.setSpaceAfter(d))
	
	def indentStart(d:Int) = copy(paragraph = paragraph.indentStart(d))
	def indentEnd(d:Int) = copy(paragraph = paragraph.indentEnd(d))
}

/**
 * Default style object
 * @author artur.opala
 */
object DefaultStyle extends Style()