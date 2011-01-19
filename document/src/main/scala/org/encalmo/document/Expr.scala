package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.calculation.Calculation

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(myStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(myStyle) {

	override def toString = "Expr("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	def resolve:Seq[Seq[Expression]] = {
		var result = LinkedList[Seq[Expression]]()
		for(e <- expr) {
			result = result :+ resolveExpression(e)
		}
		result
	}
	
	private def resolveExpression(e:Expression):Seq[Expression] = {
		var se = LinkedList[Expression]()
		se = se :+ e
		val re = calc.resolve(e)
		if(re!=e){
			se = se :+ re
		}
		se
	}
	
}

/**
 * Expr class companion object
 * @author artur.opala
 */
object Expr {
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*) = {
		new Expr(mystyle,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
		new Expr(null,calc,expr:_*)
	}
	
	def unapply(e:Expr) = Some(e.myStyle,e.calc,e.expr)
	
}