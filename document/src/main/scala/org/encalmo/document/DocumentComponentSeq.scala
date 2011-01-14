package org.encalmo.document

/**
 * Document's components sequence abstract class
 * @author artur.opala
 */
abstract class DocumentComponentSeq(flow:DocumentComponent*) 
extends DocumentComponent {

    for (f <- flow) {
        f.parent = Some(this)
    }
    
}

/**
 * Empty document's components sequence singleton
 * @author artur.opala
 */
object EmptyDocumentComponentSeq extends DocumentComponentSeq