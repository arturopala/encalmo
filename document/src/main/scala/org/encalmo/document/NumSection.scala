package org.encalmo.document

import annotation.tailrec
import org.encalmo.document.StyledPlaces._

/**
 * Numbered section component class
 * @author artur.opala
 */
class NumSection(val nsStyle:Style, val myEnumerator:Enumerator, flow:DocumentComponent*) 
extends Section(nsStyle,flow:_*) {
	
	override def myStyle:Style = nsStyle
	
	override def toString = "NumSection("+myStyle+","+flow.mkString(",")+")"
	
	/** Section's resolved enumerator */
	@tailrec
    final def enumerator:Enumerator = {
    	if(myEnumerator!=null){
    		myEnumerator
    	}else{
    		val pns = parentOfType[NumSection](classOf[NumSection])
    		if(!pns.isDefined){
    			parentOrSiblingOfType[EnumeratorProvider](classOf[EnumeratorProvider])
    			.getOrElse(NumSection.defaultEnumerator).asInstanceOf[EnumeratorProvider].enumerator
    		}else{
    			pns.get.enumerator
    		}
    	}
    }
	
	/**
	 * Resolves style for this numbered section
	 */
	final def resolveStyle(level:Int):Style = {
		if(nsStyle!=null){
			nsStyle
		}else{
			val slo = parentOrSiblingOfType[StyleManager](classOf[StyleManager])
			if(slo.isDefined){
				val sm = slo.get
				level match {
					case 1 => sm.get(STYLED_PLACE_NUM_SECTION_01).getOrElse(resolveStyle(sm))
					case 2 => sm.get(STYLED_PLACE_NUM_SECTION_02).getOrElse(resolveStyle(sm))
					case 3 => sm.get(STYLED_PLACE_NUM_SECTION_03).getOrElse(resolveStyle(sm))
					case 4 => sm.get(STYLED_PLACE_NUM_SECTION_04).getOrElse(resolveStyle(sm))
					case 5 => sm.get(STYLED_PLACE_NUM_SECTION_05).getOrElse(resolveStyle(sm))
					case 6 => sm.get(STYLED_PLACE_NUM_SECTION_06).getOrElse(resolveStyle(sm))
					case 7 => sm.get(STYLED_PLACE_NUM_SECTION_07).getOrElse(resolveStyle(sm))
					case 8 => sm.get(STYLED_PLACE_NUM_SECTION_08).getOrElse(resolveStyle(sm))
					case 9 => sm.get(STYLED_PLACE_NUM_SECTION_09).getOrElse(resolveStyle(sm))
					case 10 => sm.get(STYLED_PLACE_NUM_SECTION_10).getOrElse(resolveStyle(sm))
					case _ => resolveStyle(sm)
				}
			}else{
				super.style
			}
		}
	}
	
	private def resolveStyle(sm:StyleManager):Style = {
		sm.get(STYLED_PLACE_NUM_SECTION).getOrElse(super.style)
	}
}

/**
 * NumSection class companion object
 * @author artur.opala
 */
object NumSection {
	
	val defaultEnumerator = Enumerator()
	
	def apply(myStyle:Style, enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(myStyle, enumerator, flow:_*)
	}
	
	def apply(myStyle:Style, flow:DocumentComponent*) = {
		new NumSection(myStyle, null, flow:_*)
	}
	
	def apply(enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(null, enumerator, flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new NumSection(null, null, flow:_*)
	}
	
	def unapply(s:NumSection) = Some(s.nsStyle,s.myEnumerator,s.flow)
	
}