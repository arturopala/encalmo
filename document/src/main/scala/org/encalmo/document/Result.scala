package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation
import org.encalmo.document.StyledPlaces._

/**
 * Result component class
 * @author artur.opala
 */
class Result(myStyle:Style, calc:Calculation, expr:Expression*) 
extends Expr(myStyle,calc,expr:_*){
	
	override def toString = "Result("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	/** Function to implement */
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		val evaluated = calc.evaluate(e)
		Seq[ExpressionToPrint](ExpressionToPrint(evaluated,resolveStyle(myStyle,STYLED_PLACE_EXPRESSION_EVALUATED),null,null))
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