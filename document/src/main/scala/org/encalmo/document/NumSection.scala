package org.encalmo.document

/**
 * Numbered section component class
 * @author artur.opala
 */
class NumSection(myStyle:Style, flow:DocumentComponent*) 
extends Section(myStyle,flow:_*)

/**
 * NumSection class companion object
 * @author artur.opala
 */
object NumSection {
	
	def apply(myStyle:Style, flow:DocumentComponent*) = {
		new NumSection(myStyle,flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new NumSection(null,flow:_*)
	}
	
}