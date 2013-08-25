package org.encalmo.document

import org.encalmo.style.Style

/**
 * TableOfContents section component class
 * @author artur.opala
 */
case class TableOfContents(title:String, tocStyle:Style = null) extends DocumentComponent(tocStyle) {
    
    lazy val parentDocument:Option[Document] = parentOfType[Document](classOf[Document])
	
}