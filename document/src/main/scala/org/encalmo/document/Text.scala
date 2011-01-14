package org.encalmo.document

/**
 * Text component class
 */
class Text(mystyle:Style, text:String) 
extends DocumentComponent

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
	
}