package org.encalmo.calculation

import org.encalmo.expression._

/** Expression trait functionality extension */
case class ExpressionExt(expression:Expression) {

	def at(map:Map[Symbol,Expression]):EvalAt = new EvalAt(expression, Context(map))
	def at(er:ExpressionResolver):EvalAt = new EvalAt(expression, er)
	def at(args:(Symbol,Expression)*):EvalAt = new EvalAt(expression, Context(args:_*))
    
}