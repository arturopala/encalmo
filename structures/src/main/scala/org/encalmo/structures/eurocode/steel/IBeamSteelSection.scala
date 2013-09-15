package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{Evaluate, Text, NumSection}

trait IBeamSteelSectionSymbols extends SymbolConfigurator {

    val ibeamDict = "section_ibeam"

    //Dimensions of details
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
        
}

/** Section's shape trait */
class IBeamSteelSection(name:String, val sectionType: String) extends SteelSection(name) with IBeamSteelSectionSymbols {

    this(Wzd) = Wz
    this(Wzg) = Wz
    this(Wyd) = Wy
    this(Wyg) = Wy
    this(hw) = h-2*tf
    this(bf) = (b-tw)/2
    this(ctf) = (bf-r)/tf
    this(ctw) = (hw-2*r)/tw
    this(AVz) = max(A-2*b*tf+(tw+2*r)*tf,1.2*hw*tw)

    epsi := 0.814
    alphaw := 0.25
    alphaf := 0.25
    Thetaw := -0.5
    ksigma := 1

    Cw1 := rangeChoice4LE(ctw,1,72*epsi,2,83*epsi,3,124*epsi,4)
    Cw2 := rangeChoice4LE(ctw,1,33*epsi,2,38*epsi,3,42*epsi,4)
    Cw3 := rangeChoice4LE(ctw,1,rangeChoiceLE(alphaw,(36*epsi)/alphaw,0.5,(396*epsi)/(13*alphaw-1)),2,rangeChoiceLE(alphaw,(41.5*epsi)/alphaw,0.5,(456*epsi)/(13*alphaw-1)),3,rangeChoiceLE(Thetaw,(62*epsi*(1-Thetaw)*sqrt(-Thetaw)),-1,(42*epsi)/(0.67+0.33*Thetaw)),4)

    Cf1 := rangeChoice4LE(ctf,1,9*epsi,2,10*epsi,3,14*epsi,4)
    Cf2 := rangeChoice4LE(ctf,1,(9*epsi)/alphaf,2,(10*epsi)/alphaf,3,21*epsi*sqrt(ksigma),4)
    Cf3 := rangeChoice4LE(ctf,1,(9*epsi)/(alphaf*sqrt(alphaf)),2,(10*epsi)/(alphaf*sqrt(alphaf)),3,21*epsi*sqrt(ksigma),4)

    C1 := max(Cf1,Cw1)
    C2 := max(Cf1,Cw2)
    C3 := max(Cf2,Cf3,Cw3)

    wy := rangeChoiceLE(h/b,rangeChoiceLE(tf,text("b"),Number(100,SI.mm),text("d")),1.2,rangeChoiceLE(tf,text("a"),Number(40,SI.mm),text("b")))
    wz := rangeChoiceLE(h/b,rangeChoiceLE(tf,text("c"),Number(100,SI.mm),text("d")),1.2,rangeChoiceLE(tf,text("b"),Number(40,SI.mm),text("c")))

	def info = NumSection(Text(sectionType,ibeamDict),name,
		Evaluate(h,b,tw,tf,hw,bf,A,Iy,Iz,Wy,Wz,Wypl,Wzpl,m)
	)
	
}
