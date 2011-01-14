package org.encalmo.document

/**
 * DocumentComponent's style class
 * @author artur
 */
case class Style(
	font:FontStyle = DefaultFontStyle
){
	def withFont(f:FontStyle):Style = copy(font=f)
	def makeFontBold:Style = copy(font = font.makeBold)
	def makeFontItalic:Style = copy(font = font.makeItalic)
	def makeFontBigger:Style = copy(font = font++)
	def makeFontSmaller:Style = copy(font = font--)
}

/**
 * Default style object
 * @author artur.opala
 */
object DefaultStyle extends Style()