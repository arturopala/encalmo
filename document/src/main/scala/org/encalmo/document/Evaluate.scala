package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation._
import scala.Some
import org.encalmo.style.Style

/**
 * Evaluate given expressions
 */
class Evaluate(
        customStyle: Option[Style],
        val styleOfResolved: Option[Style],
        val styleOfEvaluated: Option[Style],
        val isPrintDescription:Boolean,
        expressions:Expression*)(implicit context: Context)
extends BlockExpr(customStyle,expressions:_*){
	
	override def toString = "Evaluate("+customStyle+","+styleOfResolved+","+styleOfEvaluated+","+isPrintDescription+","+expressions.mkString(",")+")("+context+")"
	
}

/**
 * Evaluate given expressions
 * @author artur.opala
 */
object Evaluate {
	
	def apply(customStyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(Option(customStyle),Option(styleOfResolved),Option(styleOfEvaluated),true,expressions:_*)(context)
	}
	
	def apply(customStyle:Style, style2:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(Option(customStyle),Option(style2),Option(style2),true,expressions:_*)(context)
	}
	
	def apply(customStyle:Style, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(Option(customStyle),Option(customStyle),Option(customStyle),true,expressions:_*)(context)
	}
	
	def apply(expressions:Expression*)(implicit context: Context) = {
        new Evaluate(None,None,None,true,expressions:_*)(context)
    }
	
	def apply(isPrintDescription:Boolean, expressions:Expression*)(implicit context: Context) = {
		new Evaluate(None,None,None,isPrintDescription,expressions:_*)(context)
	}
	
	def unapply(e:Evaluate) = Some(e.customStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
	
}