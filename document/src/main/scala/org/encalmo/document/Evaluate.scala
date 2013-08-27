package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation._
import scala.Some
import org.encalmo.style.Style

/**
 * Evaluate: symbol = unresolved = substituted = partially evaluated = evaluated
 */
class Evaluate(
        customStyle:Style,
        val styleOfResolved:Style, 
        val styleOfEvaluated:Style, 
        val isPrintDescription:Boolean,
        expressions:Expression*)(implicit context: Context)
extends BlockExpr(customStyle,expressions:_*){
	
	override def toString = "Evaluate("+customStyle+","+styleOfResolved+","+styleOfEvaluated+","+context+","+expressions.mkString(",")+")"
	
}

/**
 * Evaluate: symbol = resolved = evaluated
 * @author artur.opala
 */
object Evaluate {
	
	def apply(customStyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(customStyle,styleOfResolved,styleOfEvaluated,true,expressions:_*)(context)
	}
	
	def apply(customStyle:Style, style2:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(customStyle,style2,style2,true,expressions:_*)(context)
	}
	
	def apply(customStyle:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(customStyle,customStyle,customStyle,true,expressions:_*)(context)
	}
	
	def apply(expressions:Expression*)(implicit context: Context) = {
        new Evaluate(null,null,null,true,expressions:_*)(context)
    }
	
	def apply(isPrintDescription:Boolean, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(null,null,null,isPrintDescription,expressions:_*)(context)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
	
}