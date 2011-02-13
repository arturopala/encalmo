package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Steel symbols */
trait SteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "steel"
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val E = symbol(BasicSymbols.E) unit "Pa"
	val fyp = symbol(f|"yp") unit "Pa"
	val fypd = symbol(f|"yp,d") unit "Pa"
	val gammaM0 = symbol(gamma|"M,0")
	val gammaM1 = symbol(gamma|"M,1")
}

/** Steel context */
class Steel(id:String) extends Calculation(Option(id)) with SteelSymbols {
	
	def info = NumSection(TextToTranslate("Steel",dictionary),id,
		Evaluate(Seq(fyp,gammaM0,fypd,E),this)
	)
	
	this(CLASS) = text(id)
	this(fypd) = fyp/gammaM0
}

/** Steel library */
object Steel extends SteelSymbols {
	

	lazy val S355 = new Steel("S355"){
		this(E) = 210E9
		this(fyp) = 355E6
	}
	
	lazy val S280GD = new Steel("S280 GD"){
		this(E) = 210E9
		this(fyp) = 280E6
	}
}