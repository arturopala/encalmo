package org.encalmo.document
import scala.collection.mutable.LinkedList
import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation
import org.encalmo.style.Style
import org.encalmo.style.StylesConfigSymbols
import org.encalmo.style.StylesConfig

/**
 * Expression component class
 * @author artur.opala
 */
abstract class Expr(val exStyle:Style, val calc:Calculation, val expr:Expression*) 
extends DocumentComponent(exStyle) {

	override def toString = "Expr("+exStyle+","+calc+","+expr.mkString(",")+")"
	
	lazy val parentStylesConfig:Option[StylesConfig] = document.map(_.stylesConfig)
	
	override lazy val myStyle:Style = {
        Option(exStyle).getOrElse(
            parentStylesConfig match {
                case Some(psc) => psc.expression.getOrElse(null)
                case None => null
            }
        )
    }
	
	/** Resolves this expressions to sequences of ExpressionToPrint objects */
	final def resolve:Seq[Seq[ExpressionToPrint]] = {
		for(e <- expr) yield prepareExpressionToPrint(e)
	}
	
	/** Function to implement */
	def prepareExpressionToPrint(e:Expression):Seq[ExpressionToPrint] = {
		e match {
			case s:Symbol => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig))
			case _ => Seq[ExpressionToPrint](ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_EVALUATED),null,null,parentStylesConfig))
		}
	}
	
	/**
	 * Resolves style for this expression part
	 */
	final def resolveStyle(defaultStyle:Style, part:StylesConfigSymbols.Value):Style = {
		Option(defaultStyle).getOrElse(
			parentStylesConfig match {
				case Some(psc) => psc.part(part).getOrElse(psc.expression.getOrElse(null))
				case None => null
			}
		)
	}
	
}

/**
 * Block-style expression
 * @author artur.opala
 */
abstract class BlockExpr(exStyle:Style, calc:Calculation, expr:Expression*) 
extends Expr(exStyle,calc,expr:_*) with BlockComponent {
	
	def isPrintDescription:Boolean
	
}

/**
 * Inline-style expression
 * @author artur.opala
 */
abstract class InlineExpr(exStyle:Style, calc:Calculation, expr:Expression*) 
extends Expr(exStyle,calc,expr:_*) with InlineComponent {
	
}