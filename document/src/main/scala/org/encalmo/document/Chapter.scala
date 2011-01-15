package org.encalmo.document

/**
 * Chapter component class
 * @author artur.opala
 */
class Chapter(myStyle:Style, val title:String, val header:Section, val footer:Section, flow:DocumentComponent*) 
extends DocumentComponentSeq(myStyle, flow:_*) {
	
	override def toString = "Chapter("+myStyle+","+title+","+header+","+footer+","+flow.mkString(",")+")"

}

/**
 * Chapter class companion object
 * @author artur.opala
 */
object Chapter {
	
	def apply(myStyle:Style, title:String, header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(myStyle,title,header,footer,flow:_*)
	}
	
	def apply(title:String, header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(null,title,header,footer,flow:_*)
	}
	
	def apply(header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(null,"",header,footer,flow:_*)
	}
	
	def unapply(c:Chapter) = Some(c.myStyle,c.title,c.header,c.footer,c.flow)
	
}