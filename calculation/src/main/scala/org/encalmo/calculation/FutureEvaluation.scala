package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Future evaluation expression
 * @author artur.opala
 */
case class FutureEvaluation(calc:Calculation,symbol:Symbol) extends Expression with SymbolLike {
	
	override def eval = calc.evaluate(symbol)
	
	def resolve = calc.resolve(symbol)
	
	def substitute = calc.substitute(symbol)

}