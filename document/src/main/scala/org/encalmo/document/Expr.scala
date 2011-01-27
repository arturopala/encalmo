package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(myStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(myStyle) {

	override def toString = "Expr("+myStyle+","+calc+","+expr.mkString(",")+")"
	
	lazy val parentStylesConfig:Option[StylesConfig] = parentOrSiblingOfType[StylesConfig](classOf[StylesConfig])
	
	/** Resolves this expressions to sequences of ExpressionToPrint objects */
	final def resolve:Seq[Seq[ExpressionToPrint]] = {
		for(e <- expr) yield resolveExpression(e)
	}
	
	/** Function to implement */
	def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		e match {
			case s:Symbol => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null))
			case _ => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_EVALUATED),null,null))
		}
	}
	
	def isForceLineBreak:Boolean = true
	
	/**
	 * Resolves style for this expression
	 */
	final def resolveStyle(definedStyle:Style, part:StylesConfigSymbols.Value):Style = {
		Option(definedStyle).getOrElse(
			parentStylesConfig match {
				case Some(psc) => psc.expressions.part(part).getOrElse(psc.expressions.expression.getOrElse(null))
				case None => null
			}
		)
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