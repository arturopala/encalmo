package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Result component class
 * @author artur.opala
 */
class Result(myStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(myStyle) {
	
	/** Resolves this expressions to sequences of ExpressionToPrint objects */
	def resolve:Seq[Seq[ExpressionToPrint]] = {
		for(e <- expr) yield resolveExpression(e)
	}
	
	/** Function to implement */
	def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		val evaluated = calc.evaluate(e)
		Seq[ExpressionToPrint](ExpressionToPrint(evaluated,myStyle,null,null,ExpressionToPrint.TYPE_EXPRESSION_EVALUATED))
	}
	
}

/**
 * Result: expression evaluated
 * @author artur.opala
 */
object Result {
	
	def apply(calc:Calculation, expr:Expression*):Result = {
		new Result(null,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*):Result = {
		new Result(mystyle,calc,expr:_*)
	}
	
}