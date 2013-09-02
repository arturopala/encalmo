package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, Calculation}
import org.encalmo.style.Style

/**
 * Result inline component class
 * @author artur.opala
 */
class Result(customStyle: Option[Style], expr:Expression*)(implicit context: Context)
extends InlineExpr(customStyle,expr:_*)(context){
	
	override def toString = s"Result($customStyle,${expr.mkString(",")})($context)"
	
}

/**
 * Result: expression evaluated
 * @author artur.opala
 */
object Result {
	
	def apply(expr:Expression*)(implicit context: Context):Result = {
		new Result(None,expr:_*)(context)
	}
	
	def apply(customStyle:Style,expr:Expression*)(implicit context: Context):Result = {
		new Result(Option(customStyle),expr:_*)(context)
	}
	
}