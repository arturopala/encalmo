package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Actions symbols */
trait ActionsSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "actions"
	
	val gammaG = symbol(gamma|"G")
	val gammaQ = symbol(gamma|"Q")
	
	
}

/** Actions context */
class Actions(id:String) extends Calculation(Option(id)) with ActionsSymbols {
	
	def info = NumSection(TextToTranslate("Actions",dictionary),id,
		Evaluate(Seq(gammaG,gammaQ),this)
	)
	
	
}

/** Actions library */
object Actions extends ActionsSymbols {
	
}