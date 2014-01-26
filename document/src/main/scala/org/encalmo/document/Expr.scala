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
        customStyle: Option[Style],
        val expressions:Expression*)(implicit val context: Context)
extends DocumentComponent(customStyle) with ExpressionHolderComponent {

    assert(context!=null, "Context reference MUST not be null")

	override def toString = "Expr("+customStyle+","+context+","+expressions.mkString(",")+")"
}

/**
 * Block-style expression
 * @author artur.opala
 */
abstract class BlockExpr(customStyle: Option[Style], expressions:Expression*)(implicit context: Context)
extends Expr(customStyle,expressions:_*)(context) with BlockComponent {
	
	def isPrintDescription:Boolean
	
}

/**
 * Inline-style expression
 * @author artur.opala
 */
abstract class InlineExpr(customStyle: Option[Style], expressions:Expression*)(implicit context: Context)
extends Expr(customStyle,expressions:_*)(context) with InlineComponent {
	
}