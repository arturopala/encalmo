package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Actions symbols */
object ActionsSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "actions"
	
	val gammaG = symbol(gamma|"G")
	val gammaQ = symbol(gamma|"Q")
	
}