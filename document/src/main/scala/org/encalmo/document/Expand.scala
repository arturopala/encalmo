package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, Calculation, ContextFactory}
import org.encalmo.style.Style

/**
 * Expand: symbol = resolved
 */
class Expand(
        customStyle: Option[Style],
        val styleOfResolved: Option[Style],
        val isPrintDescription:Boolean, 
        val variant:Int,
        expressions:Expression*)(implicit context: Context)
extends BlockExpr(customStyle,expressions:_*)(context){
	
	override def toString = "Expand("+customStyle+","+styleOfResolved+","+context+","+expressions.mkString(",")+")"
}

/**
 * Expand: symbol = resolved
 * @author artur.opala
 */
object Expand {
	
	def apply(styleOfSymbol:Style, styleOfResolved:Style, expr:Expression*)(implicit context: Context) = {
		new Expand(Option(styleOfSymbol),Option(styleOfResolved),true,1,expr:_*)(context)
	}
	
	def apply(customStyle:Style, expr:Expression*)(implicit context: Context) = {
		new Expand(Option(customStyle),Option(customStyle),true,1,expr:_*)(context)
	}
	
	def apply(expr:Expression*)(implicit context: Context) = {
		new Expand(None,None,true,1,expr:_*)(context)
	}
	
	def apply(isPrintDescription:Boolean, expr:Expression*)(implicit context: Context) = {
		new Expand(None,None,isPrintDescription,1,expr:_*)(context)
	}
	
	def apply(isPrintDescription:Boolean, variant:Int, expr:Expression*)(implicit context: Context) = {
		new Expand(None,None,isPrintDescription,variant,expr:_*)(context)
	}
	
	def unapply(e:Expand) = Some(e.customStyle, e.styleOfResolved ,e.context)
	
}