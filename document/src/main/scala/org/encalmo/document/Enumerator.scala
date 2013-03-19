package org.encalmo.document
import org.encalmo.style.Style

/**
 * NumSection enumerator strategy
 * @author artur.opala
 */
trait Enumerator extends EnumeratorProvider {
	
	/** Enumerator style */
	def style:Style = null
	
	def toDocumentComponent:DocumentComponent = {
		new EnumeratorComponent(this)
	}
	
	def enumerator:Enumerator = this
        
    def apply(level:Int, item:Int):String = String.valueOf(item)
    
    def string(positions:Seq[Int]):String = {
	    (for(i <- 0 to (positions.size-1)) yield this(i,positions(i))).mkString(".")
    }
	
}

/**
 * Enumerator factory
 * @author artur.opala
 */
object Enumerator {
	
	/** Standard numbers-only enumerator */
	def apply():Enumerator = new Enumerator{}
	
	/** Standard numbers-only enumerator */
	def apply(s:Style):Enumerator = new Enumerator {
		
		override def style:Style = s
	}
	
}

/**
 * DocumentComponent holding enumerator, acting as an EnumeratorProvider
 * @author artur.opala
 */
class EnumeratorComponent(val enumerator:Enumerator)
extends DocumentComponent(null) with EnumeratorProvider with NonVisualComponent

/**
 * Default enumerator singleton
 * @author artur.opala
 */
object DefaultEnumerator extends Enumerator