package org.encalmo.calculation

import org.encalmo.expression.Symbol

/**
 * Handy symbols's configurator
 * @author artur.opala
 */
trait SymbolConfigurator {
	
	val dictionary:String
	val contextId:String
	
	def symbol(symbol:Symbol):Symbol = {
		symbol.dictionary(dictionary).id(contextId)
	}

}