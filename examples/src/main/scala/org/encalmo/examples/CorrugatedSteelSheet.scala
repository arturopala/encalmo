package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document.Evaluate
import org.encalmo.document.Section

trait CorrugatedSteelSheetSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "corrugatedSteelSheet"
	
	val ID = symbol("ID").makeNonPrintable
	val t = symbol(BasicSymbols.t) unit "m"
	val Iminus = symbol(I!"-") unit "m4"
	val Iplus = symbol(I!"+") unit "m4"
	val Ap = symbol(A|p) unit "m2"
	val eminus = symbol(e!"-") unit "m"
	val eplus = symbol(e!"+") unit "m"
	val ep = symbol(e|p) unit "m"

}

class CorrugatedSteelSheet(id:String, val steel:Steel) 
extends Calculation(Option(id)) with CorrugatedSteelSheetSymbols {
	
	this add steel
	this(ID) = text(id)
	
	lazy val info = Section(
		Evaluate(Seq(ID,t),this),
		steel.info,
		Evaluate(Seq(Ap,Iminus,Iplus,eminus,eplus,ep),this)
	)
	
}

object CorrugatedSteelSheet {
	
}

object FLORSTROP {

	import CorrugatedSteelSheet._

	lazy val T59_Z = new CorrugatedSteelSheet("FLORSTROP T59 Z", Steel.S280GD){
		this(t) = 59E-3
	}

}