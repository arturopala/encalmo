package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.expression.Transformations
import org.encalmo.expression.Operation2
import org.encalmo.expression.OperationN
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Resolve: symbol = resolved
 */
class Resolve(myStyle:Style, val styleOfResolved:Style, calc:Calculation, expr:Expression*) 
extends Expr(myStyle,calc,expr:_*){
	
	override def toString = "Resolve("+myStyle+","+styleOfResolved+","+calc+","+expr.mkString(",")+")"
	
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		var se = Seq[ExpressionToPrint]()
		var ue = e // unresolved expression
		if(e.isInstanceOf[Symbol]){
			se = se :+ ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null)
			ue = calc.getRawExpression(e.asInstanceOf[Symbol]) match {
				case Some(x) => x
				case None => e
			}
			if(ue!=e){
				se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null)
			}
		}else{
			se = se :+ ExpressionToPrint(ue,resolveStyle(myStyle,StylesConfigSymbols.EXPR_UNRESOLVED),null,null)
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