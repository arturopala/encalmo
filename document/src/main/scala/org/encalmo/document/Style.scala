package org.encalmo.document

import java.awt.Color

/**
 * DocumentComponent's style class
 * @author artur.opala
 */
case class Style(
	font:FontStyle = DefaultFontStyle,
	text:TextStyle = DefaultTextStyle,
	paragraph:ParagraphStyle = DefaultParagraphStyle,
	list:ListStyle = DefaultListStyle,
	color:Color = Color.BLACK,
	background:Color = null,
	classId:String = StyleIdGenerator()
){
	
	val hexColor:String = color match {
        case null => ""
		case _ => "#"+Seq[Int](color.getRed,color.getGreen,color.getBlue).map(x => {
			val h:String = x.toHexString
			if(h.size>1) h else ("0"+h)
		}).mkString
	}
	
	val hexBackground:String = background match {
	    case null => ""
	    case _ => "#"+Seq[Int](background.getRed,background.getGreen,background.getBlue).map(x => {
			val h:String = x.toHexString
			if(h.size>1) h else ("0"+h)
		}).mkString
	}
	
	def use(f:FontStyle):Style = copy(font=f)
	def use(p:ParagraphStyle):Style = copy(paragraph=p)
    def use(l:ListStyle):Style = copy(list=l)
    def use(t:TextStyle):Style = copy(text=t)
	
	def useColor(c:Color):Style = copy(color=c)
	def useBackground(b:Color):Style = copy(background=b)
	
	def fontBold:Style = copy(font = font.makeBold)
	def fontItalic:Style = copy(font = font.makeItalic)
	def fontBigger:Style = copy(font = font++)
	def fontSmaller:Style = copy(font = font--)
	def fontSize(d:Int):Style = copy(font = font.fontSize(d))
	def fontFamily(f:String):Style = copy(font = font.useFamily(f))
	
	def useAlign(d:String) = copy(text = text.useAlign(d))
	def useDecoration(d:String) = copy(text = text.useDecoration(d))
	def useTransform(d:String) = copy(text = text.useTransform(d))
	
	def useIndent(d:Int) = copy(text = text.useIndent(d))
	def useLineHeight(d:Int) = copy(text = text.useLineHeight(d))
	def useLetterSpacing(d:String) = copy(text = text.useLetterSpacing(d))
	def useWordSpacing(d:String) = copy(text = text.useWordSpacing(d))
	
	def paddingTop(d:Int) = copy(paragraph = paragraph.paddingTop(d))
	def paddingBottom(d:Int) = copy(paragraph = paragraph.paddingBottom(d))
	def paddingLeft(d:Int) = copy(paragraph = paragraph.paddingLeft(d))
	def paddingRight(d:Int) = copy(paragraph = paragraph.paddingRight(d))
	
	def marginTop(d:Int) = copy(paragraph = paragraph.marginTop(d))
	def marginBottom(d:Int) = copy(paragraph = paragraph.marginBottom(d))
	def marginLeft(d:Int) = copy(paragraph = paragraph.marginLeft(d))
	def marginRight(d:Int) = copy(paragraph = paragraph.marginRight(d))
	
	def useSpaceBefore(d:Int) = copy(paragraph = paragraph.useSpaceBefore(d))
	def useSpaceAfter(d:Int) = copy(paragraph = paragraph.useSpaceAfter(d))
	
	def indentStart(d:Int) = copy(paragraph = paragraph.indentStart(d))
	def indentEnd(d:Int) = copy(paragraph = paragraph.indentEnd(d))
	
	def usePaddings(bd:BoxDim) = copy(paragraph = paragraph.usePaddings(bd))
	def useMargins(bd:BoxDim) = copy(paragraph = paragraph.useMargins(bd))
	
	def paddings(d:Int) = copy(paragraph = paragraph.usePaddings(BoxDim(d,d,d,d)))
	def margins(d:Int) = copy(paragraph = paragraph.useMargins(BoxDim(d,d,d,d)))
	
	def useUnit(u:String) = copy(paragraph = paragraph.useUnit(u),text = text.useUnit(u))

	def hyphenateOn = copy(text = text.hyphenateOn)
	def hyphenateOff = copy(text = text.hyphenateOff)
	
	def width(d:Int) = copy(paragraph = paragraph.setWidth(d))
	def height(d:Int) = copy(paragraph = paragraph.setHeight(d))
	
	def useDistanceBetweenStarts(d:Int) = copy(list = list.useDistanceBetweenStarts(d))
    def useDistanceLabelSeparation(d:Int) = copy(list = list.useDistanceLabelSeparation(d))
    def useBullet(s:String) = copy(list = list.useBullet(s))
	
}

/**
 * Default style object
 * @author artur.opala
 */
object DefaultStyle extends Style()