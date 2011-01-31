package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Symb inline component class
 * @author artur.opala
 */
class Symb(sStyle:Style, expr:Expression*) 
extends InlineExpr(sStyle,null,expr:_*){
	
    override lazy val myStyle:Style = sStyle
    
	override def toString = "Symb("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	/** Function to implement */
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		Seq[ExpressionToPrint](ExpressionToPrint(e,style,null,null,parentStylesConfig))
	}
    
}

/**
 * Symb: symbol inline
 * @author artur.opala
 */
object Symb {
	
	def apply(expr:Expression*):Symb = {
		new Symb(null,expr:_*)
	}
	
	def apply(mystyle:Style,expr:Expression*):Symb = {
		new Symb(mystyle,expr:_*)
	}
	
}