package org.encalmo.style

import scala.language.postfixOps
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
	background:Color = null
){

    /** Unique style id */
    var classId:String = StyleClassIdGenerator()
	
	val hexColor:String = Style.toHex(color)
	
	val hexBackground:String = Style.toHex(background)
	
	def use(f:FontStyle):Style = copy(font=f)
	def use(p:ParagraphStyle):Style = copy(paragraph=p)
    def use(l:ListStyle):Style = copy(list=l)
    def use(t:TextStyle):Style = copy(text=t)
	
	def useColor(c:Color):Style = copy(color=c)
	def useBackground(b:Color):Style = copy(background=b)
	
	def fontBold:Style = copy(font = font.makeBold)
	def fontItalic:Style = copy(font = font.makeItalic)
	def fontBigger:Style = copy(font = font.++())
	def fontSmaller:Style = copy(font = font.--())
	def fontSize(d:Int):Style = copy(font = font.fontSize(d))
	def fontFamily(f:String):Style = copy(font = font.useFamily(f))
	
	def useAlign(d:String) = copy(text = text.useAlign(d))
	def useDecoration(d:String) = copy(text = text.useDecoration(d))
	def useTransform(d:String) = copy(text = text.useTransform(d))
	
	def useIndent(d:Int) = copy(text = text.useIndent(d))
	def useLineHeight(d:Int) = copy(text = text.useLineHeight(d))
	def useLetterSpacing(d:String) = copy(text = text.useLetterSpacing(d))
	def useWordSpacing(d:String) = copy(text = text.useWordSpacing(d))
	
	def paddingTop(d:Double) = copy(paragraph = paragraph.paddingTop(d))
	def paddingBottom(d:Double) = copy(paragraph = paragraph.paddingBottom(d))
	def paddingLeft(d:Double) = copy(paragraph = paragraph.paddingLeft(d))
	def paddingRight(d:Double) = copy(paragraph = paragraph.paddingRight(d))
	
	def marginTop(d:Double) = copy(paragraph = paragraph.marginTop(d))
	def marginBottom(d:Double) = copy(paragraph = paragraph.marginBottom(d))
	def marginLeft(d:Double) = copy(paragraph = paragraph.marginLeft(d))
	def marginRight(d:Double) = copy(paragraph = paragraph.marginRight(d))
	
	def useSpaceBefore(d:Double) = copy(paragraph = paragraph.useSpaceBefore(d))
	def useSpaceAfter(d:Double) = copy(paragraph = paragraph.useSpaceAfter(d))
	
	def indentStart(d:Double) = copy(paragraph = paragraph.indentStart(d))
	def indentEnd(d:Double) = copy(paragraph = paragraph.indentEnd(d))
	
	def usePaddings(bd:Box[Double]) = copy(paragraph = paragraph.usePaddings(bd))
	def useMargins(bd:Box[Double]) = copy(paragraph = paragraph.useMargins(bd))
	
	def paddings(d:Double) = copy(paragraph = paragraph.usePaddings(Box(d,d,d,d)))
	def margins(d:Double) = copy(paragraph = paragraph.useMargins(Box(d,d,d,d)))
    def borders(d:Border) = copy(paragraph = paragraph.useBorders(Box(d,d,d,d)))
    def borders(w:Double,s:String,c:Color) = {
        val b: Border = Border(w,s,c)
        copy(paragraph = paragraph.useBorders(Box(b,b,b,b)))
    }
	
	def useUnit(u:String) = copy(paragraph = paragraph.useUnit(u),text = text.useUnit(u))

	def hyphenateOn = copy(text = text.hyphenateOn)
	def hyphenateOff = copy(text = text.hyphenateOff)
	
	def width(d:Int) = copy(paragraph = paragraph.setWidth(d))
	def height(d:Int) = copy(paragraph = paragraph.setHeight(d))
	
	def useDistanceBetweenStarts(d:Int) = copy(list = list.useDistanceBetweenStarts(d))
    def useDistanceLabelSeparation(d:Int) = copy(list = list.useDistanceLabelSeparation(d))
    def useBullet(s:String) = copy(list = list.useBullet(s))
    
    def setClassId(s:String):Style = {classId = s;this}
	
	def withoutParagraphStyle:Style = copy(paragraph = DefaultParagraphStyle)
	
}

object Style {

    def toHex(c:Color):String = {
        c match {
            case null => ""
            case _ => "#"+Seq[Int](c.getRed,c.getGreen,c.getBlue).map(x => {
                val h:String = x.toHexString
                if(h.size>1) h else "0" + h
            }).mkString
        }
    }

}

/**
 * Default style object
 * @author artur.opala
 */
object DefaultStyle extends Style(){
    
    classId = "defaultStyle"
    
}