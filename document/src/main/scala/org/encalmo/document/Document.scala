package org.encalmo.document

/**
 * Document class
 * @author artur
 */
case class Document(title:String,flow:DocumentComponent*) 
extends DocumentComponentSeq(flow:_*) 