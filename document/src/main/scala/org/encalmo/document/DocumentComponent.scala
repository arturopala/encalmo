package org.encalmo.document

/**
 * DocumentComponent trait
 * @author artur
 */
trait DocumentComponent {
    
    /** Parent component */
    var parent:Option[DocumentComponent] = None
    
    /** Component's custom style if declared */
    def mystyle:Style = null
    
    /** Component's resolved style */
    final def style:Style = {
    	if(mystyle!=null){
    		mystyle
    	}else{
    		if(parent.isDefined) parent.get.style else DefaultStyle
    	}
    }

}

/**
 * Empty content singleton
 * @author artur
 */
object EmptyDocumentComponent extends DocumentComponent