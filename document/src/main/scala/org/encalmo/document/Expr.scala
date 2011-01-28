package org.encalmo.document
import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Expression component class
 * @author artur.opala
 */
class Expr(val exStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(exStyle) {

	override def toString = "Expr("+exStyle+","+calc+","+expr.mkString(",")+")"
	
	lazy val parentStylesConfig:Option[StylesConfig] = parentOrSiblingOfType[StylesConfig](classOf[StylesConfig])
	
	override lazy val myStyle:Style = {
        Option(exStyle).getOrElse(
            parentStylesConfig match {
                case Some(psc) => psc.expressions.expression.getOrElse(null)
                case None => null
            }
        )
    }
	
	/** Resolves this expressions to sequences of ExpressionToPrint objects */
	final def resolve:Seq[Seq[ExpressionToPrint]] = {
		for(e <- expr) yield resolveExpression(e)
	}
	
	/** Function to implement */
	def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		e match {
			case s:Symbol => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig))
			case _ => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_EVALUATED),null,null,parentStylesConfig))
		}
	}
	
	def isForceLineBreak:Boolean = true
	def isPrintDescription:Boolean = true
	
	/**
	 * Resolves style for this expression part
	 */
	final def resolveStyle(defaultStyle:Style, part:StylesConfigSymbols.Value):Style = {
		Option(defaultStyle).getOrElse(
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