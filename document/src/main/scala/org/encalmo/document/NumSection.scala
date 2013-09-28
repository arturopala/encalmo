package org.encalmo.document

import annotation.tailrec
import org.encalmo.style.Style
import org.encalmo.style.StylesConfig

/**
 * Numbered section component class
 * @author artur.opala
 */
class NumSection(val title:Option[String], customStyleOfNumSection: Option[Style], val myEnumerator:Enumerator, flow:DocumentComponent*)
extends Section(customStyleOfNumSection,flow:_*) {

    val expressionCounter: Counter = Counter(1)

    lazy val parentNumSection = parentOfType[NumSection](classOf[NumSection])
    lazy val parentEnumeratorProvider = parentOrSiblingOfType[EnumeratorProvider](classOf[EnumeratorProvider])
	
	override def toString = "NumSection("+customStyleOfComponent+","+flow.mkString(",")+")"
	
	/** Section's resolved enumerator */
	//TDOD @tailrec
    lazy val enumerator:Enumerator = {
    	if(myEnumerator!=null){
    		myEnumerator
    	}else{
    		if(!parentNumSection.isDefined){
    			parentEnumeratorProvider.getOrElse(NumSection.defaultEnumerator).enumerator
    		}else{
    			parentNumSection.get.enumerator
    		}
    	}
    }
    
    lazy val enumeratorLevel:Int = {
        val c = countParentsOfTypeUntil[NumSection](classOf[NumSection],(x)=>{x.enumerator.eq(this.enumerator)})
        c
    }
    
    override lazy val customStyle:Style = {
    	customStyleOfComponent.getOrElse(resolveStyle(enumeratorLevel))
    }
	
	/**
	 * Resolves style for this numbered section
	 */
    private def resolveStyle(level:Int):Style = {
        customStyleOfComponent.getOrElse(
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
				sc.level(0)
			}else{
				sc.level(level)
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
	
	def apply(customStyle: Style, enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(None, Option(customStyle), enumerator, flow:_*)
	}
	
	def apply(customStyle:Style, flow:DocumentComponent*) = {
		new NumSection(None, Option(customStyle), null, flow:_*)
	}
	
	def apply(enumerator:Enumerator, flow:DocumentComponent*) = {
		new NumSection(None, None, enumerator, flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new NumSection(None, None, null, flow:_*)
	}
	
	def apply(title:String, flow:DocumentComponent*) = {
		new NumSection(Some(title), None, null, flow:_*)
	}
	
	def unapply(s:NumSection) = Some(s.customStyleOfComponent,s.myEnumerator,s.flow)
	
}