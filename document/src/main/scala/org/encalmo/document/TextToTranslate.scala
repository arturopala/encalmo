package org.encalmo.document
import org.encalmo.style.Style

/**
 * TextToTranslate component class
 */
class TextToTranslate(customStyle: Option[Style], text:String, val dictionary:String)
extends Text(customStyle,text)

/**
 * TextToTranslate class companion object
 * @author artur.opala
 */
object TextToTranslate{
	
	def apply(customStyle:Style, text:String, dictionary:String) = {
		new TextToTranslate(Option(customStyle),text,dictionary)
	}
	
	def apply(text:String, dictionary:String) = {
		new TextToTranslate(None,text,dictionary)
	}

    def apply(text:String, dictionary:Option[String]) = {
        new TextToTranslate(None,text,dictionary.getOrElse(null))
    }
	
	def unapply(t:TextToTranslate) = Some((t.customStyle,t.text,t.dictionary))
	
}