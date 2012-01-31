package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Expression bound to the specific resolver
 * @author artur.opala
 */
case class BoundExpression(er:ExpressionResolver,symbol:Symbol) extends Expression with SymbolLike {
	
	override def eval = er.evaluate(symbol)
	
	def resolve = er.resolve(symbol)
	
	def substitute = er.substitute(symbol)

}