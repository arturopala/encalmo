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
    lazy val style:Style = {
    	if(myStyle!=null){
    		myStyle
    	}else{
    		if(!parent.isDefined) {
    			DefaultStyle 
    		} else {
    			parent.get.style
    		}
    	}
    }
    
    lazy val isFirstBlockComponent = isFirstChildrenOfType[BlockComponent](classOf[BlockComponent])
    lazy val isFirstInlineComponent = isFirstChildrenOfType[InlineComponent](classOf[InlineComponent])
    /** Parent document */
    lazy val document:Option[Document] = parentOfType[Document](classOf[Document])
    
    def styleClassId:Option[String] = document match {
        case None => Some(style.classId)
        case Some(d) => d.stylesConfig.matchStyleClassId(style)
    }
    
    def myStyleClassId:Option[String] = myStyle match {
        case null => None
        case _ => {
            document match {
                case None => Some(myStyle.classId)
                case Some(d) => d.stylesConfig.matchStyleClassId(myStyle)
            }
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