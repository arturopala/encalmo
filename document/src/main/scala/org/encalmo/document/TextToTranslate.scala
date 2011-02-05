package org.encalmo.document

/**
 * TextToTranslate component class
 */
class TextToTranslate(myStyle:Style, text:String) 
extends Text(myStyle,text)

/**
 * TextToTranslate class companion object
 * @author artur.opala
 */
object TextToTranslate{
	
	def apply(mystyle:Style, text:String) = {
		new TextToTranslate(mystyle,text)
	}
	
	def apply(text:String) = {
		new TextToTranslate(null,text)
	}
	
	def unapply(t:TextToTranslate) = Some((t.myStyle,t.text))
	
}