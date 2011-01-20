package org.encalmo.document

/**
 * NumSection enumerator strategy
 * @author artur.opala
 */
trait Enumerator extends EnumeratorProvider {
	
	def apply(level:Int, item:Int):String
	
	/** Enumerator style */
	def style:Style = null
	
	def toDocumentComponent:DocumentComponent = {
		new EnumeratorComponent(this)
	}
	
	def enumerator:Enumerator = this
	
}

/**
 * Enumerator factory
 * @author artur.opala
 */
object Enumerator {
	
	/** Standard numbers-only enumerator */
	def apply():Enumerator = new Enumerator {
		def apply(level:Int, item:Int):String = String.valueOf(item)
	}
	
	/** Standard numbers-only enumerator */
	def apply(s:Style):Enumerator = new Enumerator {
		def apply(level:Int, item:Int):String = String.valueOf(item)
		override def style:Style = s
	}
	
}

/**
 * DocumentComponent holding enumerator, acting as an EnumeratorProvider
 * @author artur.opala
 */
class EnumeratorComponent(val enumerator:Enumerator)
extends DocumentComponent(null) with EnumeratorProvider with NonVisualDocumentComponent