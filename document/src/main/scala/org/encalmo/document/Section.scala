package org.encalmo.document
import org.encalmo.style.Style

/**
 * Section component class
 * @author artur.opala
 */
class Section(myStyle:Style, flow:DocumentComponent*) 
extends DocumentComponentSeq(myStyle,flow:_*) with BlockComponent {
	
	override def toString = "Section("+myStyle+","+flow.mkString(",")+")"
	
}

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
	
	def unapply(s:Section) = Some(s.myStyle,s.flow)
	
}

object EmptySection extends Section(null) {
	
	override def toString = "EmptySection"
		
}