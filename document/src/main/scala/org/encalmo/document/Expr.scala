package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation.{Context, ContextFactory}
import org.encalmo.style.Style
import org.encalmo.style.StylesConfig

/**
 * Document component representing expression sequence
 * @author artur.opala
 */
abstract class Expr (
        val customStyle:Style,
        val expressions:Expression*)(implicit val context: Context)
extends DocumentComponent(customStyle) with StylesResolver {

    assert(context!=null, "Context reference MUST not be null")

	override def toString = "Expr("+customStyle+","+context+","+expressions.mkString(",")+")"
	
	lazy val parentStylesConfig:Option[StylesConfig] = document.map(_.stylesConfig)
	
	override lazy val myStyle:Style = {
        Option(customStyle).getOrElse(
            parentStylesConfig match {
                case Some(psc) => psc.expression.getOrElse(null)
                case None => null
            }
        )
    }
}

/**
 * Block-style expression
 * @author artur.opala
 */
abstract class BlockExpr(customStyle:Style, expressions:Expression*)(implicit context: Context)
extends Expr(customStyle,expressions:_*)(context) with BlockComponent {
	
	def isPrintDescription:Boolean
	
}

/**
 * Inline-style expression
 * @author artur.opala
 */
abstract class InlineExpr(customStyle:Style, expressions:Expression*)(implicit context: Context)
extends Expr(customStyle,expressions:_*)(context) with InlineComponent {
	
}