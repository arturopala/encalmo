package org.encalmo.document

/**
 * Text component class
 */
class Text(myStyle:Style, val text:String) 
extends DocumentComponent(myStyle) {
	
	override def toString = "Text("+myStyle+","+text+")"
	
}

/**
 * Text class companion object
 * @author artur.opala
 */
object Text {
	
	def apply(mystyle:Style, text:String) = {
		new Text(mystyle,text)
	}
	
	def apply(text:String) = {
		new Text(null,text)
	}
	
	def unapply(t:Text) = Some((t.myStyle,t.text))
	
}