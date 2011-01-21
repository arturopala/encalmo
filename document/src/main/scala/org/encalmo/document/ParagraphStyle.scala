package org.encalmo.document

/**
 * Paragraph style class
 * @author artur.opala
 */
case class ParagraphStyle(
		val margin:BoxDim = BoxDim(),
		val padding:BoxDim = BoxDim(),
		val spaceBefore:Int = 0,
		val spaceAfter:Int = 0,
		val startIndent:Int = 0,
		val endIndent:Int = 0,
		val unit:String = "pt"
	) {
	
	def paddingTop(d:Int) = copy(padding = padding.setTop(d))
	def paddingBottom(d:Int) = copy(padding = padding.setBottom(d))
	def paddingLeft(d:Int) = copy(padding = padding.setLeft(d))
	def paddingRight(d:Int) = copy(padding = padding.setRight(d))
	
	def marginTop(d:Int) = copy(margin = margin.setTop(d))
	def marginBottom(d:Int) = copy(margin = margin.setBottom(d))
	def marginLeft(d:Int) = copy(margin = margin.setLeft(d))
	def marginRight(d:Int) = copy(margin = margin.setRight(d))
	
	def useSpaceBefore(d:Int) = copy(spaceBefore = d)
	def useSpaceAfter(d:Int) = copy(spaceAfter = d)
	
	def indentStart(d:Int) = copy(startIndent = d)
	def indentEnd(d:Int) = copy(endIndent = d)
	
	def usePaddings(bd:BoxDim) = copy(padding = bd)
	def useMargins(bd:BoxDim) = copy(margin = bd)
	
	def useUnit(u:String) = copy(unit = u)

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
		left:Int = 0,
		top:Int = 0,
		right:Int = 0,
		bottom:Int = 0
	){
	
	def setTop(d:Int) = copy(top = d)
	def setBottom(d:Int) = copy(bottom = d)
	def setLeft(d:Int) = copy(left = d)
	def setRight(d:Int) = copy(right = d)
	
}