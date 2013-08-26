package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.style.Style
import org.encalmo.calculation.{Context, ContextFactory}

/**
 * Symb inline component class
 * @author artur.opala
 */
class Symb(sStyle:Style, context: Context, expr:Expression*)
extends InlineExpr(sStyle,context,expr:_*){
	
    override lazy val myStyle:Style = sStyle
    
	override def toString = "Symb("+myStyle+","+context+","+expr.mkString(",")+")"
    
}

/**
 * Symb: symbol inline
 * @author artur.opala
 */
object Symb {
	
	def apply(expr:Expression*)(implicit context:Context):Symb = {
		new Symb(null,context,expr:_*)
	}
	
	def apply(customStyle:Style,expr:Expression*)(implicit context:Context):Symb = {
		new Symb(customStyle,context,expr:_*)
	}
	
}