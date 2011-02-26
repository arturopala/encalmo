package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Result inline component class
 * @author artur.opala
 */
class Result(rStyle:Style, calc:Calculation, expr:Expression*) 
extends InlineExpr(rStyle,calc,expr:_*){
    
    override lazy val myStyle:Style = rStyle
	
	override def toString = "Result("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	/** Function to implement */
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		val evaluated = calc.evaluate(e)
		e match {
			case s:Symbol if s.hasUnit => {
				Seq[ExpressionToPrint](ExpressionToPrint(evaluated,style,null,/*s.unit.getOrElse(null)*/null,parentStylesConfig))
			}
			case _ => {
				Seq[ExpressionToPrint](ExpressionToPrint(evaluated,style,null,null,parentStylesConfig))
			}
		}
		
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