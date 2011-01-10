package org.encalmo.document

/**
 * Document class
 * @author artur
 */
class Document(title:String,flow:DocumentComponent*) extends DocumentComponentSeq(flow:_*) 