package org.encalmo.document
import org.encalmo.style.Style

/**
 * Text component class
 */
class Text(customStyle: Option[Style], val text:String, val dictionary:Option[String] = None)
extends DocumentComponent(customStyle) with TextContent with InlineComponent {
	
	override def toString = "Text("+customStyle.map(_ + ",").getOrElse("") + text + dictionary.map("," + _).getOrElse("") +")"
	
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

    def apply(customStyle:Style, text:String, dictionary:String) = {
        new Text(Option(customStyle),text,Option(dictionary))
    }

    def apply(text:String, dictionary:String) = {
        new Text(None,text,Option(dictionary))
    }

    def apply(text:String, dictionary:Option[String]) = {
        new Text(None,text,dictionary)
    }
	
	def unapply(t:Text) = Some((t.customStyle,t.text,t.dictionary))
	
}