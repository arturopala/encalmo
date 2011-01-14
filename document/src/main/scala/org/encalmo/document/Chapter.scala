package org.encalmo.document

/**
 * Chapter component class
 * @author artur.opala
 */
class Chapter(myStyle:Style, title:String, header:Section, footer:Section, flow:DocumentComponent*) 
extends DocumentComponentSeq(flow:_*) {

}

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
	
}