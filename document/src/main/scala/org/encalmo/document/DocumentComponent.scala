package org.encalmo.document

import org.encalmo.common._
import org.encalmo.common.AdHocVisitor
import org.encalmo.common.Node
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle
import scala.collection.mutable

/**
 * DocumentComponent trait
 * @author artur.opala
 */
abstract class DocumentComponent(protected val customStyleOfComponent: Option[Style] = None) extends TreeNodeWithParentRef[DocumentComponent] {

    assert(customStyleOfComponent!=null,"Document component style option MUST not be null: "+this)

    /** Component's own style declaration */
    def customStyle:Style = customStyleOfComponent.getOrElse(null)
    
    /** Component's resolved style */
    //TODO @tailrec
    lazy val style:Style = {
    	if(customStyle!=null){
    		customStyle
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
    
    lazy val customStyleClassId:Option[String] = customStyle match {
        case null => None
        case _ => {
            document match {
                case None => Some(customStyle.classId)
                case Some(d) => d.stylesConfig.matchStyleClassId(customStyle)
            }
        }
    }
    
	lazy val allStyles:mutable.Set[Style] = {
	    val stylesSet:mutable.Set[Style] = mutable.LinkedHashSet()
        customStyleOfComponent.map(stylesSet.add)
	    val t = AdHocVisitor[DocumentComponent](onEnterFx = Some({node: Node[DocumentComponent] => {
	        node.element.customStyleOfComponent.map(stylesSet.add)
	    }}))
	    this.visit(visitor = t)
	    stylesSet
	}
}

/**
 * Empty content singleton
 * @author artur.opala
 */
object EmptyDocumentComponent extends DocumentComponent(None){
	
	override def toString = "EmptyDocumentComponent"
	
}