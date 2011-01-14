package org.encalmo.document

/**
 * Section component class
 * @author artur.opala
 */
class Section(myStyle:Style, flow:DocumentComponent*) 
extends DocumentComponentSeq(flow:_*)

/**
 * Section class companion object
 * @author artur.opala
 */
object Section {
	
	def apply(myStyle:Style, flow:DocumentComponent*) = {
		new Section(myStyle,flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new Section(null,flow:_*)
	}
	
	def apply(myStyle:Style):Section = {
		new Section(myStyle)
	}
	
	def apply():Section = {
		new Section(null)
	}
	
}

object EmptySection extends Section(null)