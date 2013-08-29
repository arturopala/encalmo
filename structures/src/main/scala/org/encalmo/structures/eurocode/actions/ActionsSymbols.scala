package org.encalmo.structures.eurocode.actions

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.document._

/** Actions symbols */
trait ActionsSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val actionsDict = "actions"
	
	val gammaG = symbol(gamma|"G", actionsDict)
	val gammaQ = symbol(gamma|"Q", actionsDict)
	
}
