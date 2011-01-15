package org.encalmo.document

import org.encalmo.common._
/**
 * Document's components sequence abstract class
 * @author artur.opala
 */
abstract class DocumentComponentSeq(myStyle:Style, val flow:DocumentComponent*) 
extends DocumentComponent(myStyle) {

    for (f <- flow) {
        f.parent = Some(this)
    }
    
    final override def travel(parent:Node[DocumentComponent] = null, traveler:Traveler[DocumentComponent], position:Int=0):Unit = {
		val n = Node(parent,this,position)
		traveler.onEnter(n)
		val zip = (flow.+:(null)).zip(flow.:+(null)).zipWithIndex
		zip.foreach(z => {
			val e = z._1._2 //current node
			val pe = z._1._1 //previous node
			val i = z._2 //position index
			if(e!=null){
				if(pe!=null){
					traveler.onBetweenChildren(n,pe,e)
				}
				traveler.onBeforeChildEnter(n,i,e)
				e.travel(n,traveler,i)
				traveler.onAfterChildExit(n,i,e)
			}
		})
		traveler.onExit(n)
    }
    
}

/**
 * Empty document's components sequence singleton
 * @author artur.opala
 */
object EmptyDocumentComponentSeq extends DocumentComponentSeq(null){
	
	override def toString = "EmptyDocumentComponentSeq"
	
}