package org.encalmo.graphics

/**
 * Graphics element's sequence base class
 */
abstract class GraphicsElementSeq(val flow:GraphicsElement*) extends GraphicsElement {
    
    final override def children:Seq[GraphicsElement] = flow
    
}