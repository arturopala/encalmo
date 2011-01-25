package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation
import org.encalmo.document.StyledPlaces._

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(myStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(myStyle) {

	override def toString = "Expr("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	/** Resolves this expressions to sequences of ExpressionToPrint objects */
	final def resolve:Seq[Seq[ExpressionToPrint]] = {
		for(e <- expr) yield resolveExpression(e)
	}
	
	/** Function to implement */
	def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		e match {
			case s:Symbol => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,STYLED_PLACE_EXPRESSION_SYMBOL),null,null))
			case _ => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,STYLED_PLACE_EXPRESSION_EVALUATED),null,null))
		}
	}
	
	def isForceLineBreak:Boolean = true
	
	/**
	 * Resolves style for this expression
	 */
	final def resolveStyle(definedStyle:Style, styledPlace:StyledPlace):Style = {
		if(definedStyle!=null){
			definedStyle
		}else{
			val slo:Option[StyleManager] = parentOrSiblingOfType[StyleManager](classOf[StyleManager])
			slo match {
				case Some(sm) => sm.get(styledPlace).getOrElse(resolveStyle(sm))
				case None => null
			}
		}
	}
	
	private def resolveStyle(sm:StyleManager):Style = {
		sm.get(STYLED_PLACE_EXPRESSION).getOrElse(null)
	}
	
}

/**
 * Expr: expression or symbol or value
 * @author artur.opala
 */
object Expr {
	
	def apply(calc:Calculation, expr:Expression*):Expr = {
		new Expr(null,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*):Expr = {
		new Expr(mystyle,calc,expr:_*)
	}
	
}