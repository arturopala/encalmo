package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Expression pinned to the specific context
 * @author artur.opala
 */
case class PinnedExpression(context:Context, symbol:Symbol) extends Expression with SymbolLike {
	
	override def eval() = {
        context.evaluate(symbol)(new ResultsCache())
    }
	
	def expand(cache: ResultsCache) = {
        context.expand(symbol)(cache)
    }
	
	def substitute(cache: ResultsCache) = {
        context.substitute(symbol)(cache)
    }

}