package org.encalmo.document

import annotation.tailrec

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
			val slo = parentOrSiblingOfType[StyleList](classOf[StyleList])
			if(slo.isDefined){
				val ss = slo.get.itemStyle
				if(ss.size>level){
					ss(level)
				}else{
					ss.last
				}
			}else{
				super.style
			}
		}
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