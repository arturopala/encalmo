package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

trait CorrugatedSteelSheetSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "corrugatedSteelSheet"
	
	val ID = symbol("ID").makeNonPrintable
	val hp = symbol(BasicSymbols.h|p) unit "m"
	val br = symbol(BasicSymbols.b|r) unit "m"
	val bs = symbol(BasicSymbols.b|s) unit "m"
	val bo = symbol(BasicSymbols.b|o) unit "m"
	val bb = symbol(BasicSymbols.b|b) unit "m"
	val t = symbol(BasicSymbols.t) unit "m"
	val Iminus = symbol(I!"-") unit "m4/m"
	val Iplus = symbol(I!"+") unit "m4/m"
	val Ap = symbol(A|p) unit "m2/m"
	val eminus = symbol(e!"-") unit "m"
	val eplus = symbol(e!"+") unit "m"
	val ep = symbol(e|p) unit "m"
	val Qcck = symbol(Q|"cc,k") unit "N/m2"
	val Qccd = symbol(Q|"cc,d") unit "N/m2"
	val gammaG = symbol(gamma|"G")

}

class CorrugatedSteelSheet(id:String, val steel:Steel) 
extends Calculation(Option(id)) with CorrugatedSteelSheetSymbols {

   def info = NumSection(TextToTranslate("CorrugatedSteelSheet",dictionary),id,
		Evaluate(Seq(t,hp,br,bs,bo,bb,Ap,Iminus,Iplus,eminus,eplus,ep),this),
		AssertionGE("EN 1994-1-1 3.5(2)",this,t,0.7E-3),
		AssertionLE("EN 1994-1-1 9.1.1(2)",this,br/bs,0.6)
	)
	
	this add steel
	this(ID) = text(id)
	this(Qcck) = Ap*Steel.gammas
	this(Qccd) = Qcck*gammaG
	
}

object CorrugatedSteelSheet extends CorrugatedSteelSheetSymbols {
	
}

object FLORSTROP {

	import CorrugatedSteelSheet._

	lazy val T59_Z_075 = new CorrugatedSteelSheet("FLORSTROP T59 Z 0.75", Steel.S280GD){
		this(hp) = 59E-3
		this(br) = 44.6E-3
		this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
		this(t) = 0.75E-3
		this(Iminus) = 34.87E-8
        this(Iplus) = 54.42E-8
        this(Ap) = 12.53E-4
        this(eminus) = 3.06E-2
        this(eplus) = 4.03E-2
        this(ep) = 3.64E-2
	}
	
	lazy val T59_Z_088 = new CorrugatedSteelSheet("FLORSTROP T59 Z 0.88", Steel.S280GD){
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 0.88E-3
        this(Iminus) = 42.65E-8
        this(Iplus) = 63.66E-8
        this(Ap) = 15.04E-4
        this(eminus) = 3.16E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
    }
	
	lazy val T59_Z_100 = new CorrugatedSteelSheet("FLORSTROP T59 Z 1.0", Steel.S280GD){
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.0E-3
        this(Iminus) = 49.61E-8
        this(Iplus) = 73.23E-8
        this(Ap) = 17.35E-4
        this(eminus) = 3.27E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
    }
	
    lazy val T59_Z_125 = new CorrugatedSteelSheet("FLORSTROP T59 Z 1.25", Steel.S280GD){
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.25E-3
        this(Iminus) = 65.84E-8
        this(Iplus) = 96.48E-8
        this(Ap) = 22.17E-4
        this(eminus) = 3.46E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
    }

}