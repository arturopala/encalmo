package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{Evaluate, TextToTranslate, NumSection}

trait IBeamSectionSymbols extends SymbolConfigurator {

    val ibeamDict = "section_ibeam"

    val ID = symbol("ID").makeNonPrintable dict ibeamDict
    //Dimensions for detailing
    val tw = symbol(BasicSymbols.t|BasicSymbols.w) unit "mm" dict ibeamDict
    val tf = symbol(BasicSymbols.t|BasicSymbols.f) unit "mm" dict ibeamDict
    val r = symbol(BasicSymbols.r) unit "mm" dict ibeamDict
    val r2 = symbol(BasicSymbols.r|2) unit "mm" dict ibeamDict
    val hw = symbol(BasicSymbols.h|BasicSymbols.w) unit "mm" dict ibeamDict
    val bf = symbol(BasicSymbols.b|BasicSymbols.f) unit "mm" dict ibeamDict
    val hd = symbol(BasicSymbols.h|BasicSymbols.d) unit "mm" dict ibeamDict
    val ss = symbol(BasicSymbols.s|BasicSymbols.s) unit "mm" dict ibeamDict
    val pmin = symbol(BasicSymbols.p|"min") unit "mm" dict ibeamDict
    val pmax = symbol(BasicSymbols.p|"max") unit "mm" dict ibeamDict
    val phi = symbol(BasicSymbols.phi) unit "mm" dict ibeamDict
    //Classification ENV 1993-1-1
    val ctf = symbol("c/t"|"f") dict ibeamDict
    val ctw = symbol("c/t"|"w") dict ibeamDict
        
}

/** Section's shape trait */
class IBeamSection(name:String, val sectionType: String) extends Section(name) with IBeamSectionSymbols {
	
	this(ID) = text(name)

    this(Wzd) = Wz
    this(Wzg) = Wz
    this(Wyd) = Wy
    this(Wyg) = Wy
    this(hw) = h-2*tf
    this(bf) = (b-tw)/2
    this(ctf) = (bf-r)/tf
    this(ctw) = (hw-2*r)/tw
    this(AVz) = max(A-2*b*tf+(tw+2*r)*tf,1.2*hw*tw)

    def label = this(ID)

	def info = NumSection(TextToTranslate(sectionType,ibeamDict),name,
		Evaluate(h,b,tw,tf,hw,bf,A,Iy,Iz,Wy,Wz,Wypl,Wzpl,m)
	)
	
}
