package org.encalmo.document
import org.encalmo.style.Style

/**
 * Text component class
 */
class Text(customStyle: Option[Style], val text:String)
extends DocumentComponent(customStyle) with TextContent with InlineComponent {
	
	override def toString = "Text("+customStyle+","+text+")"
	
	override def textContent:String = text
	
}

/**
 * Text class companion object
 * @author artur.opala
 */
object Text {
	
	def apply(mystyle:Style, text:String) = {
		new Text(Option(mystyle),text)
	}
	
	def apply(text:String) = {
		new Text(None,text)
	}
	
	def unapply(t:Text) = Some((t.customStyle,t.text))
	
}