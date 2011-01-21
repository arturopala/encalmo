package org.encalmo.document

import org.encalmo.expression._

/**
 * Expression to print holder
 * @author artur.opala
 */
case class ExpressionToPrint(
		expression:Expression,
		style:Style,
		prefix:String,
		suffix:String,
		extype:Int
)

object ExpressionToPrint {
	
	val TYPE_SYMBOL:Int = 0
	val TYPE_EXPRESSION_UNRESOLVED:Int = 1
	val TYPE_EXPRESSION_RESOLVED:Int = 2
	val TYPE_EXPRESSION_INTERMEDIATE_EVALUATION:Int = 3
	val TYPE_EXPRESSION_EVALUATED:Int = 4
	
}