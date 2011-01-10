package org.encalmo.document

/**
 * Content trait
 * @author artur
 */
trait DocumentComponent {
    
    /** Parent component */
    var parent:Option[DocumentComponent] = None
    
    /** Component's style */
    var style:Option[Style] = if(parent.isDefined) parent.get.style else None

}

/**
 * Empty content singleton
 * @author artur
 */
object EmptyDocumentComponent extends DocumentComponent 