package org.encalmo.document
import org.encalmo.style.Style

/**
 * TextToTranslate component class
 */
class TextToTranslate(myStyle:Style, text:String, val dictionary:String) 
extends Text(myStyle,text)

/**
 * TextToTranslate class companion object
 * @author artur.opala
 */
object TextToTranslate{
	
	def apply(mystyle:Style, text:String, dictionary:String) = {
		new TextToTranslate(mystyle,text,dictionary)
	}
	
	def apply(text:String, dictionary:String) = {
		new TextToTranslate(null,text,dictionary)
	}
	
	def unapply(t:TextToTranslate) = Some((t.myStyle,t.text,t.dictionary))
	
}