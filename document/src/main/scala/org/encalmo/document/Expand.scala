package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, Calculation, ContextFactory}
import org.encalmo.style.Style

/**
 * Expand: symbol = resolved
 */
class Expand(
        myStyle:Style, 
        val styleOfResolved:Style, 
        val isPrintDescription:Boolean, 
        val variant:Int,
        expressions:Expression*)(implicit context: Context)
extends BlockExpr(myStyle,expressions:_*)(context){
	
	override def toString = "Expand("+myStyle+","+styleOfResolved+","+context+","+expressions.mkString(",")+")"
}

/**
 * Expand: symbol = resolved
 * @author artur.opala
 */
object Expand {
	
	def apply(styleOfSymbol:Style, styleOfResolved:Style, expr:Expression*)(implicit context: Context) = {
		new Expand(styleOfSymbol,styleOfResolved,true,1,expr:_*)(context)
	}
	
	def apply(mystyle:Style, expr:Expression*)(implicit context: Context) = {
		new Expand(mystyle,mystyle,true,1,expr:_*)(context)
	}
	
	def apply(expr:Expression*)(implicit context: Context) = {
		new Expand(null,null,true,1,expr:_*)(context)
	}
	
	def apply(isPrintDescription:Boolean, expr:Expression*)(implicit context: Context) = {
		new Expand(null,null,isPrintDescription,1,expr:_*)(context)
	}
	
	def apply(isPrintDescription:Boolean, variant:Int, expr:Expression*)(implicit context: Context) = {
		new Expand(null,null,isPrintDescription,variant,expr:_*)(context)
	}
	
	def unapply(e:Expand) = Some(e.myStyle, e.styleOfResolved ,e.context)
	
}