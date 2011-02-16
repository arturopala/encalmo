package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Future expression
 * @author artur.opala
 */
case class FutureExpression(er:ExpressionResolver,symbol:Symbol) extends Expression with SymbolLike {
	
	override def eval = er.evaluate(symbol)
	
	def resolve = er.resolve(symbol)
	
	def substitute = er.substitute(symbol)

}