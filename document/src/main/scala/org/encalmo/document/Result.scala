package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, Calculation}
import org.encalmo.style.Style

/**
 * Result inline component class
 * @author artur.opala
 */
class Result(rStyle:Style, expr:Expression*)(implicit context: Context)
extends InlineExpr(rStyle,expr:_*)(context){
    
    override lazy val myStyle:Style = rStyle
	
	override def toString = s"Result($myStyle,${expr.mkString(",")})($context)"
	
}

/**
 * Result: expression evaluated
 * @author artur.opala
 */
object Result {
	
	def apply(expr:Expression*)(implicit context: Context):Result = {
		new Result(null,expr:_*)(context)
	}
	
	def apply(mystyle:Style,expr:Expression*)(implicit context: Context):Result = {
		new Result(mystyle,expr:_*)(context)
	}
	
}