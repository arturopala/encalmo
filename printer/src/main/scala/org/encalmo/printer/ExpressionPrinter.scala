package org.encalmo.printer

import org.encalmo.expression._

/**
 * Expression printers trait
 * A - output type
 * B - result type
 * @author artur.opala
 */
trait ExpressionPrinter[A<:Output[B],B] {
	
	def print(e:Expression,output:A):A
	
}