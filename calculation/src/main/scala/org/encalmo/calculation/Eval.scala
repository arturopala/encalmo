package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Expression evaluating other expression with provided expression resolver
 * @author artur.opala
 */
class Eval(val expr:Expression, val er:ExpressionResolver) extends Expression {
    
    override def children = Seq(expr)
	
	override def eval = {
	    er.evaluate(expr)
	}
	
	override def map(f:Transformation):Expression = {
		val ve = f(expr.map(f));
		if(ve==expr) f(this) else f(new Eval(ve,er))
	}
	
	def resolve = er.resolve(expr)
	
	def substitute = er.substitute(expr)
	
	override def toString = "Eval("+expr+","+er+")"
  
}

/**
 * Eval expression factory
 */
object Eval{
  
	def apply(expr:Expression, er:ExpressionResolver) = new Eval(expr,er)
	
	def apply(expr:Expression, map:Map[Symbol,Expression]) = new Eval(expr,Context(map))
	
	def apply(expr:Expression, args:(Symbol,Expression)*) = new Eval(expr,Context(args:_*))
	
	def unapply(ev:Eval) = Some(ev.expr, ev.er)
  
}