package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

object IBeamSectionSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "section_ibeam"
    
    val ID = symbol("ID").makeNonPrintable
    val tw = symbol(BasicSymbols.t|BasicSymbols.w) unit "m"
    val tf = symbol(BasicSymbols.t|BasicSymbols.f) unit "m"
    val r = symbol(BasicSymbols.r) unit "m"
    val hw = symbol(BasicSymbols.h|BasicSymbols.w) unit "m"
    val bf = symbol(BasicSymbols.b|BasicSymbols.f) unit "m"
    val ctf = symbol("c/t"|"f")
    val ctw = symbol("c/t"|"w")
    val AV = symbol("A"|"V") unit "m2"
        
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
	this(AV) = max(A-2*b*tf+(tw+2*r)*tf,1.2*hw*tw)

}

/** Section's shape trait */
class IBeamSection(id:String) extends Section(id) {

	import SectionSymbols._
	import IBeamSectionSymbols._
	
	this add IBeamSectionExpressions
	
	this(ID) = text(id)
	
	def info = NumSection(TextToTranslate("BeamSection",SectionSymbols.dictionary),id,
		Evaluate(Seq(h,b,tw,tf,A,Iy,Iz,Wy,Wz,Wypl,Wzpl,m),this)
	)
	

}

object IBeamSection {

	import SectionSymbols._
	import IBeamSectionSymbols._

	lazy val IPE500 = new IBeamSection("IPE500"){
		this(h)=0.5
		this(b)=0.2
		this(tw)=10.2E-3
		this(tf)=16E-3
		this(r)=21E-3
		this(A)=116E-4
		this(Iy)=48200E-8
		this(Iz)=2142E-8
		this(Wy)=1928E-6
		this(Wz)=214E-6
		this(iy)=20.4E-2
		this(iz)=4.31E-2
		this(m)=91.1
		this(u)=1.744
		this(Wypl)=2194E-6
		this(Wzpl)=336E-6
		this(It)=89.1E-8
		this(Iomega)=1.25-6
		lock
	}

}