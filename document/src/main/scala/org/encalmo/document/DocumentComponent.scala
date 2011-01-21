package org.encalmo.document

import org.encalmo.common._
import annotation.tailrec

/**
 * DocumentComponent trait
 * @author artur.opala
 */
abstract class DocumentComponent(private val dcStyle:Style) extends TreeLikeWithParent[DocumentComponent] {
    
    /** Component's own style declaration */
    def myStyle:Style = dcStyle
    
    /** Component's resolved style */
    @tailrec
    final def style:Style = {
    	if(myStyle!=null){
    		myStyle
    	}else{
    		if(!parent.isDefined) DefaultStyle else parent.get.style
    	}
    }

}

/**
 * Empty content singleton
 * @author artur.opala
 */
object EmptyDocumentComponent extends DocumentComponent(null){
	
	override def toString = "EmptyDocumentComponent"
	
}