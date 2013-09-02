package org.encalmo.document

import org.encalmo.style.Style

/**
 * TableOfContents section component class
 * @author artur.opala
 */
case class TableOfContents(title:String, override val customStyleOfComponent: Option[Style] = None) extends DocumentComponent(customStyleOfComponent) {
    
    lazy val parentDocument:Option[Document] = parentOfType[Document](classOf[Document])
	
}