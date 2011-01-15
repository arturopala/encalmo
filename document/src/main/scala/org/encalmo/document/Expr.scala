package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.Calculation

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(myStyle:Style, text:String, val calc:Calculation, val expr:Expression*) 
extends Text(myStyle,text) {

	override def toString = "Expr("+myStyle+","+text+","+calc+","+expr.mkString(",")+")"
	
}

/**
 * Expr class companion object
 * @author artur.opala
 */
object Expr {
	
	def apply(mystyle:Style, text:String, calc:Calculation, expr:Expression*) = {
		new Expr(mystyle,text,calc,expr:_*)
	}
	
	def apply(text:String, calc:Calculation, expr:Expression*) = {
		new Expr(null,text,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
		new Expr(null,null,calc,expr:_*)
	}
	
	def unapply(e:Expr) = Some(e.myStyle,e.text,e.calc,e.expr)
	
}