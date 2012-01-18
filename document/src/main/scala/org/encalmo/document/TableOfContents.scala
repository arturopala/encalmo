package org.encalmo.document

import annotation.tailrec

/**
 * TableOfContents section component class
 * @author artur.opala
 */
case class TableOfContents(val title:String, tocStyle:Style = null) extends DocumentComponent(tocStyle) {
    
    lazy val parentDocument:Option[Document] = parentOfType[Document](classOf[Document])
	
}