package org.encalmo.document

import org.encalmo.common._

/**
 * DocumentComponent trait
 * @author artur
 */
abstract class DocumentComponent(val myStyle:Style) extends Travelable[DocumentComponent] {
    
    /** Parent component */
    var parent:Option[DocumentComponent] = None
    
    /** Component's resolved style */
    final def style:Style = {
    	if(myStyle!=null){
    		myStyle
    	}else{
    		if(parent.isDefined) parent.get.style else DefaultStyle
    	}
    }
    
    /**
     * Travels internal structure of the expression 
     * @param t traveler
     */
  	override def travel(parent:Node[DocumentComponent] = null, traveler:Traveler[DocumentComponent], position:Int=0):Unit = {
		val n = Node(parent,this,position)
		traveler.onEnter(n)
		traveler.onExit(n)
  	}

}

/**
 * Empty content singleton
 * @author artur
 */
object EmptyDocumentComponent extends DocumentComponent(null){
	
	override def toString = "EmptyDocumentComponent"
	
}