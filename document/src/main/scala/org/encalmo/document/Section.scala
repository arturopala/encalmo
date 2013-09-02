package org.encalmo.document
import org.encalmo.style.Style

/**
 * Section component class
 * @author artur.opala
 */
class Section(customStyle: Option[Style], flow:DocumentComponent*)
extends DocumentComponentSeq(customStyle,flow:_*) with BlockComponent {
	
	override def toString = "Section("+customStyle+","+flow.mkString(",")+")"
	
}

/**
 * Section class companion object
 * @author artur.opala
 */
object Section {
	
	def apply(customStyle:Style, flow:DocumentComponent*) = {
		new Section(Option(customStyle),flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new Section(None,flow:_*)
	}
	
	def apply(customStyle:Style):Section = {
		new Section(Option(customStyle))
	}
	
	def apply():Section = {
		new Section(None)
	}
	
	def unapply(s:Section) = Some(s.customStyle,s.flow)
	
}

object EmptySection extends Section(None) {
	
	override def toString = "EmptySection"
		
}