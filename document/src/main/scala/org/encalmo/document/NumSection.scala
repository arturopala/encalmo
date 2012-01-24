package org.encalmo.document

import annotation.tailrec
import org.encalmo.style.Style
import org.encalmo.style.StylesConfig

/**
 * Numbered section component class
 * @author artur.opala
 */
class NumSection(val title:Option[String], val nsStyle:Style, val myEnumerator:Enumerator, flow:DocumentComponent*) 
extends Section(nsStyle,flow:_*) {
    
    lazy val parentNumSection = parentOfType[NumSection](classOf[NumSection])
    lazy val parentEnumeratorProvider = parentOrSiblingOfType[EnumeratorProvider](classOf[EnumeratorProvider])
	
	override def toString = "NumSection("+nsStyle+","+flow.mkString(",")+")"
	
	/** Section's resolved enumerator */
	@tailrec
    lazy val enumerator:Enumerator = {
    	if(myEnumerator!=null){
    		myEnumerator
    	}else{
    		if(!parentNumSection.isDefined){
    			parentEnumeratorProvider.getOrElse(NumSection.defaultEnumerator).asInstanceOf[EnumeratorProvider].enumerator
    		}else{
    			parentNumSection.get.enumerator
    		}
    	}
    }
    
    lazy val enumeratorLevel:Int = countParentsOfTypeUntil[NumSection](classOf[NumSection],(x)=>{x.enumerator.eq(this.enumerator)})
    
    override lazy val myStyle:Style = {
    	Option(nsStyle).getOrElse(resolveStyle(enumeratorLevel))
    }
	
	/**
	 * Resolves style for this numbered section
	 */
    private def resolveStyle(level:Int):Style = {
		Option(nsStyle).getOrElse(
		    document match {
		        case None => null
		        case Some(d) => resolveStyle(d.stylesConfig,level)
		    }
		)
	}
	
	@tailrec
	private def resolveStyle(sc:StylesConfig, level:Int):Style = {
		if(level>10){
			resolveStyle(sc,10)
		}else{
			if(level<0){
				sc.level(0).getOrElse(sc.numsection.getOrElse(null))
			}else{
				sc.level(level) match {
					case Some(style) => style
					case None => resolveStyle(sc,level-1)
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
	
	val defaultEnumerator = Enumerator()
	
	def apply(myStyle:Style, enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(None, myStyle, enumerator, flow:_*)
	}
	
	def apply(myStyle:Style, flow:DocumentComponent*) = {
		new NumSection(None, myStyle, null, flow:_*)
	}
	
	def apply(enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(None, null, enumerator, flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new NumSection(None, null, null, flow:_*)
	}
	
	def apply(title:String, flow:DocumentComponent*) = {
		new NumSection(Some(title), null, null, flow:_*)
	}
	
	def unapply(s:NumSection) = Some(s.nsStyle,s.myEnumerator,s.flow)
	
}