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
	suffix:String
)