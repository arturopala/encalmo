package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Expression evaluating other expression with provided expression resolver
 * @author artur.opala
 */
class EvalAt(val expr:Expression, val er:ExpressionResolver) extends Expression {
    
    override def children = Seq(expr)
	
	override def eval() = {
	    er.evaluate(expr)(new ResultsCache())
	}
	
	override def map(f:Transformation):Expression = {
		val ve = f(expr.map(f))
        if(ve==expr) f(this) else f(new EvalAt(ve, er))
	}
	
	def resolve = {
        er.resolve(expr)(new ResultsCache())
    }
	
	def substitute = {
        er.substitute(expr)(new ResultsCache())
    }
	
	override def toString = "EvalAt("+expr+","+er+")"
  
}

/**
 * Eval expression factory
 */
object EvalAt{
  
	def apply(expr:Expression, er:ExpressionResolver) = new EvalAt(expr, er)
	
	def apply(expr:Expression, map:Map[Symbol,Expression]) = new EvalAt(expr, Context(map))
	
	def apply(expr:Expression, args:(Symbol,Expression)*) = new EvalAt(expr, Context(args:_*))
	
	def unapply(ev:EvalAt) = Some(ev.expr, ev.er)
  
}