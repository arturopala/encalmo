package org.encalmo.document

import org.encalmo.style.Style

/**
 * Document's components sequence abstract class
 * @author artur.opala
 */
abstract class DocumentComponentSeq(customStyle: Option[Style], val flow:DocumentComponent*)
extends DocumentComponent(customStyle) {
    
    final override def children:Seq[DocumentComponent] = flow
    
}

/**
 * Empty document's components sequence singleton
 * @author artur.opala
 */
object EmptyDocumentComponentSeq extends DocumentComponentSeq(null){
	
	override def toString = "EmptyDocumentComponentSeq"
	
}