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
        context:Context, 
        expressions:Expression*) 
extends BlockExpr(customStyle,context,expressions:_*){
	
	override def toString = "Evaluate("+customStyle+","+styleOfResolved+","+styleOfEvaluated+","+context+","+expressions.mkString(",")+")"
	
}

/**
 * Evaluate: symbol = resolved = evaluated
 * @author artur.opala
 */
object Evaluate {
	
	def apply(customStyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,styleOfResolved,styleOfEvaluated,true,calc,expressions:_*)
	}
	
	def apply(customStyle:Style, style2:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,style2,style2,true,calc,expressions:_*)
	}
	
	def apply(customStyle:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,customStyle,customStyle,true,calc,expressions:_*)
	}
	
	def apply(expressions:Expression*)(implicit context:Context) = {
        new Evaluate(null,null,null,true,context,expressions:_*)
    }
	
	def apply(isPrintDescription:Boolean, calc:Calculation, expressions:Expression*) = {
		new Evaluate(null,null,null,isPrintDescription,calc,expressions:_*)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
	
}