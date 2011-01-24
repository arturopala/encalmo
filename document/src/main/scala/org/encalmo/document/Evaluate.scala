package org.encalmo.document

import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Transformations
import org.encalmo.expression.Operation2
import org.encalmo.expression.OperationN
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Evaluate: symbol = resolved = evaluated
 */
class Evaluate(myStyle:Style, val styleOfResolved:Style, val styleOfEvaluated:Style, calc:Calculation, expr:Expression*) 
extends Expr(myStyle,calc,expr:_*){
	
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		var se = e match {
			case s:Symbol => Seq[ExpressionToPrint](ExpressionToPrint(e,myStyle,null,null,ExpressionToPrint.TYPE_SYMBOL))
			case _ => Seq[ExpressionToPrint](ExpressionToPrint(e,myStyle,null,null,ExpressionToPrint.TYPE_EXPRESSION_UNRESOLVED))
		}
		val resolved = calc.resolve(e)
		if(resolved!=e){
			se = se :+ ExpressionToPrint(resolved,styleOfResolved,"=",null,ExpressionToPrint.TYPE_EXPRESSION_RESOLVED)
		}
		val evaluation1 = resolved match {
			case o:Operation2 => {
				o.copy(calc.evaluate(o.l),calc.evaluate(o.r))
			}
			case o:OperationN => {
				o.copy(o.args.map(calc.evaluate(_)):_*)
			}
			case _ => calc.evaluate(resolved)
		}
		val evaluated = calc.evaluate(evaluation1)
		if(evaluation1!=evaluated){
			se = se :+ ExpressionToPrint(evaluation1,styleOfResolved,"=",null,ExpressionToPrint.TYPE_EXPRESSION_INTERMEDIATE_EVALUATION)
		}
		if(evaluated!=resolved){
			se = se :+ ExpressionToPrint(evaluated,styleOfEvaluated,"=",null,ExpressionToPrint.TYPE_EXPRESSION_EVALUATED)
		}
		se
	}
}

/**
 * Evaluate: symbol = resolved = evaluated
 * @author artur.opala
 */
object Evaluate {
	
	def apply(mystyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,styleOfResolved,styleOfEvaluated,calc,expr:_*)
	}
	
	def apply(mystyle:Style, style2:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,style2,style2,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,mystyle,mystyle,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
		new Evaluate(null,null,null,calc,expr:_*)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.calc,e.expr)
	
}