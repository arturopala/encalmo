package org.encalmo.style

import java.awt.Color

/**
 * Paragraph style class
 * @author artur.opala
 */
case class ParagraphStyle(
		margin:Box[Double] = new Box(0,0,0,0),
		padding:Box[Double] = new Box(0,0,0,0),
        border:Box[Border] = {
            val b = Border.default
            new Box(b,b,b,b)
        },
		spaceBefore:Double = 0,
		spaceAfter:Double = 0,
		startIndent:Double = 0,
		endIndent:Double = 0,
		width:Double = 0,
		height:Double = 0,
        unit:String = "pt"
	) {
	
	def paddingTop(d:Double) = copy(padding = padding.setTop(d))
	def paddingBottom(d:Double) = copy(padding = padding.setBottom(d))
	def paddingLeft(d:Double) = copy(padding = padding.setLeft(d))
	def paddingRight(d:Double) = copy(padding = padding.setRight(d))
	
	def marginTop(d:Double) = copy(margin = margin.setTop(d))
	def marginBottom(d:Double) = copy(margin = margin.setBottom(d))
	def marginLeft(d:Double) = copy(margin = margin.setLeft(d))
	def marginRight(d:Double) = copy(margin = margin.setRight(d))
	
	def useSpaceBefore(d:Double) = copy(spaceBefore = d)
	def useSpaceAfter(d:Double) = copy(spaceAfter = d)
	
	def indentStart(d:Double) = copy(startIndent = d)
	def indentEnd(d:Double) = copy(endIndent = d)
	
	def usePaddings(bd:Box[Double]) = copy(padding = bd)
	def useMargins(bd:Box[Double]) = copy(margin = bd)
    def useBorders(bd:Box[Border]) = copy(border = bd)
	
	def useUnit(u:String) = copy(unit = u)
	
	def setWidth(d:Double) = copy(width = d)
	def setHeight(d:Double) = copy(height = d)

    def borderTop(d:Border) = copy(border = border.setTop(d))
    def borderBottom(d:Border) = copy(border = border.setBottom(d))
    def borderLeft(d:Border) = copy(border = border.setLeft(d))
    def borderRight(d:Border) = copy(border = border.setRight(d))

}

/**
 * Default paragraph style
 * @author artur.opala
 */
object DefaultParagraphStyle extends ParagraphStyle

/**
 * Box side oriented dimensions
 * @author artur.opala
 */
case class Box[A](
		left:A,
		top:A,
		right:A,
		bottom:A
	){
    
    def apply(d:A):Box[A] = new Box[A](d,d,d,d)
    def apply(d1:A,d2:A):Box[A] = new Box[A](d1,d2,d1,d2)
	
	def setTop(d:A) = copy(top = d)
	def setBottom(d:A) = copy(bottom = d)
	def setLeft(d:A) = copy(left = d)
	def setRight(d:A) = copy(right = d)
	
}

case class Border(width:Double = 1,style:String = "solid",color:Color = Color.BLACK)  {

    def setWidth(w:Double): Border = copy(width = w)
    def setStyle(s:String): Border = copy(style = s)
    def setColor(c:Color): Border = copy(color = c)

}

object Border {
    val default = Border()
}