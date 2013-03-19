package org.encalmo.style

/**
 * Text style class
 * @author artur.opala
 * @param align start, end, center and justify 
 * @param decoration none | [ [ underline | no-underline] || [ overline | no-overline ] || [ line-through | no-line-through ] || [ blink | no-blink ] ] |
 * @param transform capitalize | uppercase | lowercase | none | inherit 
 */
case class TextStyle(
	align:String = null,
	decoration:String = null,
	transform:String = null,
	indent:Int = 0,
	lineHeight:Int = 0,
	letterSpacing:String = null,
	wordSpacing:String = null,
	unit:String = "pt",
	hyphenate:Boolean = false
) {
	
	val ALIGN_START = "start"
	val ALIGN_END = "end"
	val ALIGN_CENTER = "center"
	val ALIGN_JUSTIFY = "justify"
		
	val DECORATE_UNDERLINE = "underline"
	val DECORATE_OVERLINE = "overline"
	val DECORATE_LINE_THROUGH = "line-through"
		
	val TRANSFORM_UPPERCASE = "uppercase"
	val TRANSFORM_LOWERCASE = "lowercase"
	val TRANSFORM_CAPITALIZE = "capitalize"
	
	def useAlign(d:String) = copy(align = d)
	def useDecoration(d:String) = copy(decoration = d)
	def useTransform(d:String) = copy(transform = d)
	
	def useIndent(d:Int) = copy(indent = d)
	def useLineHeight(d:Int) = copy(lineHeight = d)
	def useLetterSpacing(d:String) = copy(letterSpacing = d)
	def useWordSpacing(d:String) = copy(wordSpacing = d)
	
	def useUnit(u:String) = copy(unit = u)
	
	def hyphenateOn = copy(hyphenate = true)
	def hyphenateOff = copy(hyphenate = false)
	
}

/**
 * Text style companion object
 * @author artur.opala
 */
object DefaultTextStyle extends TextStyle