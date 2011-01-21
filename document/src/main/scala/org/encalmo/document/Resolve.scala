package org.encalmo.document

import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Resolve: symbol = resolved
 */
class Resolve(myStyle:Style, val styleOfResolved:Style, calc:Calculation, expr:Expression*) 
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
		se
	}
}

/**
 * Resolve: symbol = resolved
 * @author artur.opala
 */
object Resolve {
	
	def apply(styleOfSymbol:Style, styleOfResolved:Style, calc:Calculation, expr:Expression*) = {
		new Resolve(styleOfSymbol,styleOfResolved,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*) = {
		new Resolve(mystyle,mystyle,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
		new Resolve(null,null,calc,expr:_*)
	}
	
	def unapply(e:Resolve) = Some(e.myStyle, e.styleOfResolved ,e.calc,e.expr)
	
}