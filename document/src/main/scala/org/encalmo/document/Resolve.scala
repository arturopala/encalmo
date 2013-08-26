package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, Calculation, ContextFactory}
import org.encalmo.style.Style

/**
 * Resolve: symbol = resolved
 */
class Resolve(
        myStyle:Style, 
        val styleOfResolved:Style, 
        val isPrintDescription:Boolean, 
        val variant:Int, 
        context: Context,
        expressions:Expression*) 
extends BlockExpr(myStyle,context,expressions:_*){
	
	override def toString = "Resolve("+myStyle+","+styleOfResolved+","+context+","+expressions.mkString(",")+")"
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