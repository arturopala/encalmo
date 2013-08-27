package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.style.Style
import org.encalmo.calculation.{Context, ContextFactory}

/**
 * Symb inline component class
 * @author artur.opala
 */
class Symb(sStyle:Style, expr:Expression*)(implicit context: Context)
extends InlineExpr(sStyle,expr:_*)(context){
	
    override lazy val myStyle:Style = sStyle
    
	override def toString = "Symb("+myStyle+","+context+","+expr.mkString(",")+")"
    
}

/**
 * Symb: symbol inline
 * @author artur.opala
 */
object Symb {
	
	def apply(expr:Expression*)(implicit context:Context):Symb = {
		new Symb(null,expr:_*)(context)
	}
	
	def apply(customStyle:Style,expr:Expression*)(implicit context:Context):Symb = {
		new Symb(customStyle,expr:_*)(context)
	}
	
}