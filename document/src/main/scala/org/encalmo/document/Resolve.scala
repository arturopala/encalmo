package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.expression.Transformations
import org.encalmo.expression.Operation2
import org.encalmo.expression.OperationN
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation
import org.encalmo.style.Style
import org.encalmo.style.StylesConfigSymbols
import org.encalmo.calculation.Context

/**
 * Resolve: symbol = resolved
 */
class Resolve(
        myStyle:Style, 
        val styleOfResolved:Style, 
        val isPrintDescription:Boolean, 
        val variant:Int, 
        context:Context, 
        expressions:Expression*) 
extends BlockExpr(myStyle,context,expressions:_*){
	
	override def toString = "Resolve("+myStyle+","+styleOfResolved+","+context+","+expressions.mkString(",")+")"
	
	override def prepareExpressionToPrint(e:Expression):Seq[ExpressionToPrint] = {
		var se = Seq[ExpressionToPrint]()
		var ue = e // unresolved expression
		if(e.isInstanceOf[Symbol]){
			se = se :+ ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig)
			ue = context.getRawExpression(e.asInstanceOf[Symbol]) match {
				case Some(x) => x
				case None => e
			}
			if(ue!=e){
				se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null,parentStylesConfig)
			}
		}else{
			se = se :+ ExpressionToPrint(ue,resolveStyle(myStyle,StylesConfigSymbols.EXPR_UNRESOLVED),null,null,parentStylesConfig)
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
		new Resolve(styleOfSymbol,styleOfResolved,true,1,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*) = {
		new Resolve(mystyle,mystyle,true,1,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
		new Resolve(null,null,true,1,calc,expr:_*)
	}
	
	def apply(isPrintDescription:Boolean, calc:Calculation, expr:Expression*) = {
		new Resolve(null,null,isPrintDescription,1,calc,expr:_*)
	}
	
	def apply(isPrintDescription:Boolean, variant:Int, calc:Calculation, expr:Expression*) = {
		new Resolve(null,null,isPrintDescription,variant,calc,expr:_*)
	}
	
	def unapply(e:Resolve) = Some(e.myStyle, e.styleOfResolved ,e.context)
	
}