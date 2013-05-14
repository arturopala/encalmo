package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

object IBeamSectionSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "section_ibeam"
    
    val ID = symbol("ID").makeNonPrintable
    //Dimensions for detailing
    val tw = symbol(BasicSymbols.t|BasicSymbols.w) unit "mm"
    val tf = symbol(BasicSymbols.t|BasicSymbols.f) unit "mm"
    val r = symbol(BasicSymbols.r) unit "mm"
    val r2 = symbol(BasicSymbols.r|2) unit "mm"
    val hw = symbol(BasicSymbols.h|BasicSymbols.w) unit "mm"
    val bf = symbol(BasicSymbols.b|BasicSymbols.f) unit "mm"
    val hd = symbol(BasicSymbols.h|BasicSymbols.d) unit "mm"
    val ss = symbol(BasicSymbols.s|BasicSymbols.s) unit "mm"
    val pmin = symbol(BasicSymbols.p|"min") unit "mm"
    val pmax = symbol(BasicSymbols.p|"max") unit "mm"
    val phi = symbol(BasicSymbols.phi) unit "mm"
    //Classification ENV 1993-1-1
    val ctf = symbol("c/t"|"f")
    val ctw = symbol("c/t"|"w")
        
}

object IBeamSectionExpressions extends MapContext {
	
	import SectionSymbols._
	import IBeamSectionSymbols._
	
	this(Wzd) = Wz
	this(Wzg) = Wz
	this(Wyd) = Wy
	this(Wyg) = Wy
	this(hw) = h-2*tf
	this(bf) = (b-tw)/2
	this(ctf) = (bf-r)/tf
	this(ctw) = (hw-2*r)/tw
	this(AVz) = max(A-2*b*tf+(tw+2*r)*tf,1.2*hw*tw)

}

/** Section's shape trait */
class IBeamSection(id:String, val name:String = "IBeamSection") extends Section(id) {

	import SectionSymbols._
	import IBeamSectionSymbols._
	
	this add IBeamSectionExpressions
	
	this(ID) = text(id)
	
	override def label = this(ID)
	
	def info = NumSection(TextToTranslate(name,IBeamSectionSymbols.dictionary),id,
		Evaluate(h,b,tw,tf,hw,bf,A,Iy,Iz,Wy,Wz,Wypl,Wzpl,m)
	)
	
}
