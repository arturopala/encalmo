package org.encalmo.document

/**
 * Document class
 * @author artur
 */
class Document(myStyle:Style, val title:String, flow:DocumentComponent*) 
extends DocumentComponentSeq(myStyle, flow:_*) with TextContent {
	
	override def toString = "Document("+myStyle+","+title+","+flow.mkString(",")+")"
	
	override def textContent:String = title
	
}

/**
 * Document class companion object
 * @author artur.opala
 */
object Document {
	
	def apply(myStyle:Style, title:String, flow:DocumentComponent*) = {
		new Document(myStyle, title, flow:_*)
	}
	
	def apply(title:String, flow:DocumentComponent*) = {
		new Document(null, title, flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new Document(null, "", flow:_*)
	}
	
	def unapply(d:Document) = Some(d.myStyle,d.title,d.flow)
	
}