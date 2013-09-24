package org.encalmo.style

/**
 * Paragraph style class
 * @author artur.opala
 */
case class ParagraphStyle(
		margin:BoxDim = BoxDim(),
		padding:BoxDim = BoxDim(),
		spaceBefore:Double = 0,
		spaceAfter:Double = 0,
		startIndent:Double = 0,
		endIndent:Double = 0,
		width:Double = 0,
		height:Double = 0,
		unit:String = "pt",
        border:BoxDim = BoxDim()
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
	
	def usePaddings(bd:BoxDim) = copy(padding = bd)
	def useMargins(bd:BoxDim) = copy(margin = bd)
    def useBorders(bd:BoxDim) = copy(border = bd)
	
	def useUnit(u:String) = copy(unit = u)
	
	def setWidth(d:Double) = copy(width = d)
	def setHeight(d:Double) = copy(height = d)

    def borderTop(d:Double) = copy(border = border.setTop(d))
    def borderBottom(d:Double) = copy(border = border.setBottom(d))
    def borderLeft(d:Double) = copy(border = border.setLeft(d))
    def borderRight(d:Double) = copy(border = border.setRight(d))

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
case class BoxDim(
		left:Double = 0,
		top:Double = 0,
		right:Double = 0,
		bottom:Double = 0
	){
    
    def apply(d:Double):BoxDim = BoxDim(d,d,d,d)
    def apply(d1:Double,d2:Double):BoxDim = BoxDim(d1,d2,d1,d2)
	
	def setTop(d:Double) = copy(top = d)
	def setBottom(d:Double) = copy(bottom = d)
	def setLeft(d:Double) = copy(left = d)
	def setRight(d:Double) = copy(right = d)
	
}