package org.encalmo.document

import org.encalmo.common._
import annotation.tailrec
import org.encalmo.common.AdHocTraveler
import org.encalmo.common.Node
import scala.collection.mutable.{Set,LinkedHashSet}
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle

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
    
    lazy val styleClassId:Option[String] = document match {
        case None => Some(style.classId)
        case Some(d) => d.stylesConfig.matchStyleClassId(style)
    }
    
    lazy val myStyleClassId:Option[String] = myStyle match {
        case null => None
        case _ => {
            document match {
                case None => Some(myStyle.classId)
                case Some(d) => d.stylesConfig.matchStyleClassId(myStyle)
            }
        }
    }
    
	lazy val allStyles:Set[Style] = {
	    val stylesSet:Set[Style] = LinkedHashSet()
	    if(dcStyle!=null){
        	stylesSet.add(dcStyle)
        }
	    val t = AdHocTraveler[DocumentComponent](onEnterFx = Some({n:Node[DocumentComponent] => {
	        val style = n.element.dcStyle
	        if(style!=null){
	        	stylesSet.add(style)
	        }
	    }}))
	    this.travel(traveler = t)
	    stylesSet
	}
}

/**
 * Empty content singleton
 * @author artur.opala
 */
object EmptyDocumentComponent extends DocumentComponent(null){
	
	override def toString = "EmptyDocumentComponent"
	
}