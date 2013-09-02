package org.encalmo.document
import org.encalmo.style.Style

/**
 * Chapter component class
 * @author artur.opala
 */
class Chapter(customStyle: Option[Style], val title:String, val header:Section, val footer:Section, flow:DocumentComponent*)
extends DocumentComponentSeq(customStyle, flow:_*) with BlockComponent {
	
	override def toString = "Chapter("+customStyle+","+title+","+header+","+footer+","+flow.mkString(",")+")"
	
}

/**
 * Chapter class companion object
 * @author artur.opala
 */
object Chapter {
	
	def apply(myStyle:Style, title:String, header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(Option(myStyle),title,header,footer,flow:_*)
	}
	
	def apply(title:String, header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(None,title,header,footer,flow:_*)
	}
	
	def apply(header:Section, footer:Section, flow:DocumentComponent*):Chapter = {
		new Chapter(None,"",header,footer,flow:_*)
	}
	
	def unapply(c:Chapter) = Some(c.customStyle,c.title,c.header,c.footer,c.flow)
	
}