package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.Calculation

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(mystyle:Style, text:String, calc:Calculation, expr:Expression*) extends Text(mystyle,text) {

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
	
}