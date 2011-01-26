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
				resolveStyle(slo.get,level)
			}else{
				super.style
			}
		}
	}
	
	@tailrec
	private def resolveStyle(sm:StyleManager, level:Int):Style = {
		if(level>10){
			resolveStyle(sm,10)
		}else{
			if(level<0){
				sm.get(STYLED_PLACE_NUM_SECTION).getOrElse(super.style)
			}else{
				sm.get(NumSection.level2styledPlacesMap(level)) match {
					case Some(style) => style
					case None => resolveStyle(sm,level-1)
				}
			}
		}
	}
	
}

/**
 * NumSection class companion object
 * @author artur.opala
 */
object NumSection {
	
	val level2styledPlacesMap:Map[Int,StyledPlace] = Map(
			0 -> STYLED_PLACE_NUM_SECTION_LEVEL_00,
			1 -> STYLED_PLACE_NUM_SECTION_LEVEL_01,
			2 -> STYLED_PLACE_NUM_SECTION_LEVEL_02,
			3 -> STYLED_PLACE_NUM_SECTION_LEVEL_03,
			4 -> STYLED_PLACE_NUM_SECTION_LEVEL_04,
			5 -> STYLED_PLACE_NUM_SECTION_LEVEL_05,
			6 -> STYLED_PLACE_NUM_SECTION_LEVEL_06,
			7 -> STYLED_PLACE_NUM_SECTION_LEVEL_07,
			8 -> STYLED_PLACE_NUM_SECTION_LEVEL_08,
			9 -> STYLED_PLACE_NUM_SECTION_LEVEL_09,
			10 -> STYLED_PLACE_NUM_SECTION_LEVEL_10
	)
	
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