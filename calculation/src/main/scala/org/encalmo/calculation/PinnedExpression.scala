package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Expression pinned to the specific resolver
 * @author artur.opala
 */
case class PinnedExpression(context:ExpressionResolver, symbol:Symbol) extends Expression with SymbolLike {
	
	override def eval() = {
        context.evaluate(symbol)(new ResultsCache())
    }
	
	def resolve(cache: ResultsCache) = {
        context.resolve(symbol)(cache)
    }
	
	def substitute(cache: ResultsCache) = {
        context.substitute(symbol)(cache)
    }

}