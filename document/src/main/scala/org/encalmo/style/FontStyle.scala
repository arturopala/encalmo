package org.encalmo.style

/**
 * Font style class
 * @author artur.opala
 */
case class FontStyle(
	family:String="Stix",
	size:Int=11,
	bold:Boolean=false,
	italic:Boolean=false
){
	def fontSize(s:Int):FontStyle = copy(size=s)
	def useFamily(f:String):FontStyle = copy(family=f)
	def makeBold:FontStyle = copy(bold=true)
	def makeItalic:FontStyle = copy(italic=true)
	def makeNormal:FontStyle = copy(italic=false,bold=false)
	def +(s:Int):FontStyle = copy(size=size+s)
	def -(s:Int):FontStyle = copy(size=size-s)
	def ++():FontStyle = copy(size=size+1)
	def --():FontStyle = copy(size=size-1)
}
	
/**
 * Default font style object
 * @author artur.opala
 */
object DefaultFontStyle extends FontStyle()